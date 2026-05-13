# ==============================================================================
# 临床研究数据模拟流程 (完整版)
# 功能：
#   - 参数化模拟系统
#   - 支持任意层数和自定义比例
#   - 支持自定义治疗组分配比例 (1:1, 2:1, 3:1等)
#   - 支持每层每组的均值矩阵定义
#   - 自动计算随机列表大小
#   - 计算分层因素的 Standardized Mean Difference (SMD)
#   - Excel 输出汇总统计
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 加载必要的库
# ------------------------------------------------------------------------------
# install.packages(c("openxlsx", "truncnorm", "pbapply")) # 如未安装

library(openxlsx)   # Excel 输出
library(pbapply)    # 进度条

# ------------------------------------------------------------------------------
# 1. 辅助函数：动态生成所有可能的区组排列
# ------------------------------------------------------------------------------
generate_all_blocks <- function(block_size, ratio = c(1, 1)) {
  if (length(ratio) != 2) stop("ratio 必须是长度为 2 的向量")
  ratio_sum <- sum(ratio)
  if (block_size %% ratio_sum != 0) stop(sprintf("区组大小 %d 必须能被比例总和 %d 整除", block_size, ratio_sum))
  
  n_group1 <- as.integer((ratio[1] / ratio_sum) * block_size)
  n_group2 <- as.integer((ratio[2] / ratio_sum) * block_size)
  
  positions <- combn(block_size, n_group1)
  n_blocks <- ncol(positions)
  block_list <- vector("list", n_blocks)
  
  for (i in 1:n_blocks) {
    block <- rep(2, block_size)
    block[positions[, i]] <- 1
    block_list[[i]] <- block
  }
  return(block_list)
}

# ------------------------------------------------------------------------------
# 2. 辅助函数：生成单层随机列表
# ------------------------------------------------------------------------------
generate_rand_list <- function(n_per_strata, block_size = 4, ratio = c(1, 1)) {
  blocks <- generate_all_blocks(block_size, ratio)
  n_blocks_needed <- ceiling(n_per_strata / block_size)
  selected_indices <- sample(length(blocks), n_blocks_needed, replace = TRUE)
  selected_blocks <- blocks[selected_indices]
  rand_vec <- unlist(selected_blocks)
  return(rand_vec[1:n_per_strata])
}

# ------------------------------------------------------------------------------
# 3. 辅助函数：生成多层随机列表（按A因素分层）
# ------------------------------------------------------------------------------
generate_stratified_rand_lists <- function(n_strata, n_per_strata, block_size = 4, ratio = c(1, 1)) {
  rand_lists <- vector("list", n_strata)
  for (s in 1:n_strata) {
    rand_lists[[s]] <- generate_rand_list(n_per_strata, block_size, ratio)
  }
  return(rand_lists)
}


# ------------------------------------------------------------------------------
# 4. 辅助函数：计算分层因素的 Standardized Mean Difference (SMD)
# ------------------------------------------------------------------------------
calculate_strata_smd <- function(enrolled_trt, enrolled_strata) {
  n1 <- sum(enrolled_trt == 1)
  n2 <- sum(enrolled_trt == 2)
  
  if (n1 == 0 || n2 == 0) {
    return(NA)
  }
  
  # 获取分层数量
  n_strata <- length(unique(enrolled_strata))
  
  # 存储每层的 SMD
  smd_values <- numeric(n_strata)
  
  for (s in 1:n_strata) {
    # 计算该层在两组中的比例
    prop_trt1 <- mean(enrolled_strata[enrolled_trt == 1] == s)
    prop_trt2 <- mean(enrolled_strata[enrolled_trt == 2] == s)
    
    # 计算平均比例
    p_pool <- (prop_trt1 + prop_trt2) / 2
    
    # 计算 SMD
    if (p_pool > 0 && p_pool < 1) {
      smd_values[s] <- abs(prop_trt1 - prop_trt2) / sqrt(p_pool * (1 - p_pool))
    } else {
      smd_values[s] <- 0  # 比例为 0 或 1 时，SMD 为 0
    }
  }
  
  # 返回每层的 SMD 和平均/最大/中位数
  list(
    smd_per_strata = smd_values,
    mean_smd = mean(smd_values),
    max_smd = max(smd_values),
    median_smd = median(smd_values)
  )
}

# ------------------------------------------------------------------------------
# 5. 辅助函数：矩阵与字符串转换
# ------------------------------------------------------------------------------
matrix_to_string <- function(mat) {
  paste(apply(mat, 1, function(row) paste(row, collapse = ",")), collapse = ";")
}

string_to_matrix <- function(str, n_strata) {
  rows <- strsplit(str, ";")[[1]]
  mat <- matrix(as.numeric(unlist(strsplit(rows, ","))), 
                nrow = n_strata, 
                ncol = 2, 
                byrow = TRUE)
  return(mat)
}

# ------------------------------------------------------------------------------
# 6. 核心函数：执行单次试验（支持分层/非分层随机化）
# ------------------------------------------------------------------------------
run_single_trial <- function(
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  SD_COMMON = 15,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEAN_MATRIX = NULL,
  IS_STRATIFIED = TRUE  # 兼容参数，实际不影响（总是同时生成两套）
) {
  # 验证参数
  if (abs(sum(STRATA_PROPORTIONS) - 1) > 1e-6) {
    stop("STRATA_PROPORTIONS 的和必须等于 1")
  }
  if (length(STRATA_PROPORTIONS) != STRATA_LEVELS) {
    stop("STRATA_PROPORTIONS 的长度必须等于 STRATA_LEVELS")
  }
  
  # 验证或创建 MEAN_MATRIX
  if (is.null(MEAN_MATRIX)) {
    MU_TRT1 <- 10
    MU_TRT2 <- 15
    MEAN_MATRIX <- matrix(
      c(MU_TRT1, MU_TRT2),
      nrow = STRATA_LEVELS,
      ncol = 2,
      byrow = TRUE
    )
  }
  
  if (nrow(MEAN_MATRIX) != STRATA_LEVELS) {
    stop(sprintf("MEAN_MATRIX 的行数 (%d) 必须等于 STRATA_LEVELS (%d)", 
                 nrow(MEAN_MATRIX), STRATA_LEVELS))
  }
  if (ncol(MEAN_MATRIX) != 2) {
    stop("MEAN_MATRIX 必须有 2 列（对应 2 个治疗组）")
  }
  
  # ========== 自动计算随机列表大小 ==========
  RAND_LIST_SIZE_PER_STRATA <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  RAND_LIST_SIZE_TOTAL <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  
  # --- Step 1: 生成入组人群 ---
  pool_strata <- sample(1:STRATA_LEVELS, 
                        size = N_POOL, 
                        replace = TRUE, 
                        prob = STRATA_PROPORTIONS)
  
  # --- Step 2: 生成两套随机表 ---
  # 分层区组随机化
  rand_lists <- generate_stratified_rand_lists(
    n_strata = STRATA_LEVELS, 
    n_per_strata = RAND_LIST_SIZE_PER_STRATA,
    block_size = BLOCK_SIZE,
    ratio = TREATMENT_RATIO
  )
  ptr_list <- rep(0, STRATA_LEVELS)
  
  # 仅区组随机化（无分层）
  rand_list_global <- generate_rand_list(
    n_per_strata = RAND_LIST_SIZE_TOTAL,
    block_size = BLOCK_SIZE,
    ratio = TREATMENT_RATIO
  )
  ptr_global <- 0
  
  # 预分配存储空间
  enrolled_trt_strat <- numeric(TARGET_N)
  enrolled_trt_simple <- numeric(TARGET_N)
  enrolled_strata <- numeric(TARGET_N)
  
  total_enrolled <- 0
  
  # --- Step 3: 模拟入组流程 ---
  for (i in 1:N_POOL) {
    # 核心停止条件
    if (total_enrolled >= TARGET_N) break
    
    s <- pool_strata[i]
    
    # 检查两种分配是否都可行
    if (ptr_list[s] >= length(rand_lists[[s]])) next      # 该层分层随机表耗尽
    if (ptr_global >= length(rand_list_global)) break     # 全局随机表耗尽，终止
    
    # 分配患者
    total_enrolled <- total_enrolled + 1
    
    # 分层随机化
    ptr_list[s] <- ptr_list[s] + 1
    enrolled_trt_strat[total_enrolled] <- rand_lists[[s]][ptr_list[s]]
    
    # 简单随机化
    ptr_global <- ptr_global + 1
    enrolled_trt_simple[total_enrolled] <- rand_list_global[ptr_global]
    
    # 记录层信息
    enrolled_strata[total_enrolled] <- s
  }
  
  # --- 截取实际入组数据 ---
  enrolled_trt_strat <- enrolled_trt_strat[1:total_enrolled]
  enrolled_trt_simple <- enrolled_trt_simple[1:total_enrolled]
  enrolled_strata <- enrolled_strata[1:total_enrolled]
  # 两套设计的层信息相同
  enrolled_strata_strat <- enrolled_strata
  enrolled_strata_simple <- enrolled_strata
  
  # --- Step 4: 计算分层因素的 SMD ---
  strata_balance_smd_strat <- calculate_strata_smd(enrolled_trt_strat, enrolled_strata_strat)
  strata_balance_smd_unstrat <- calculate_strata_smd(enrolled_trt_simple, enrolled_strata_simple)
  
  # Step 5: 模拟临床结果（两套独立结局）
  outcomes_strat <- numeric(total_enrolled)
  outcomes_unstrat <- numeric(total_enrolled)
  
  for (i in 1:total_enrolled) {
    mu_strat <- MEAN_MATRIX[enrolled_strata_strat[i], enrolled_trt_strat[i]]
    outcomes_strat[i] <- rnorm(1, mean = mu_strat, sd = SD_COMMON)
    
    mu_unstrat <- MEAN_MATRIX[enrolled_strata_simple[i], enrolled_trt_simple[i]]
    outcomes_unstrat[i] <- rnorm(1, mean = mu_unstrat, sd = SD_COMMON)
  }
  
  # Step 6: 统计分析（区分两种设计）
  analyze_dataset <- function(outcomes, enrolled_trt, enrolled_strata_A, 
                             is_stratified_design = TRUE) {
    
    n1 <- sum(enrolled_trt == 1)
    n2 <- sum(enrolled_trt == 2)
    imbalance <- abs(n1 - n2)
    
    outcomes_trt1 <- outcomes[enrolled_trt == 1]
    outcomes_trt2 <- outcomes[enrolled_trt == 2]
    mean1 <- mean(outcomes_trt1)
    mean2 <- mean(outcomes_trt2)
    md <- mean2 - mean1
    
    # 初始化返回值
    res <- list(
      n1 = n1, n2 = n2, imbalance = imbalance,
      md_unstrat = NA, se_unstrat = NA, z_unstrat = NA, p_unstrat = NA,
      md_strat = NA, se_strat = NA, z_strat = NA, p_strat = NA,
      md_strat_z = NA, se_strat_z = NA, z_strat_z = NA, p_strat_z = NA,
      method_strat = NA, method_strat_z = NA, pooled_variance = NA, df_residual = NA
    )
    
    if (n1 > 1 & n2 > 1) {
      
      # ========== 未分层分析（所有设计都计算，作为基准）==========
      sd1 <- sd(outcomes_trt1)
      sd2 <- sd(outcomes_trt2)
      pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
      se_unstrat <- pooled_sd * sqrt(1/n1 + 1/n2)
      z_unstrat <- md / se_unstrat
      p_unstrat <- 2 * pnorm(-abs(z_unstrat))
      
      res$md_unstrat <- md
      res$se_unstrat <- se_unstrat
      res$z_unstrat <- z_unstrat
      res$p_unstrat <- p_unstrat
      res$pooled_variance <- pooled_sd^2
      
      # ========== 分层分析（根据设计类型选择方法）==========
      if (is_stratified_design) {
        # =====================================================
        # 分层随机化设计：使用ANCOVA（共同方差，最优）
        # =====================================================
        if (length(unique(enrolled_strata_A)) >= 2) {
          fit_ancova <- lm(outcomes ~ factor(enrolled_trt) + factor(enrolled_strata_A))
          coef_sum <- summary(fit_ancova)$coefficients
          
          trt_row <- grep("enrolled_trt", rownames(coef_sum))[1]
          if (length(trt_row) > 0 && !is.na(trt_row)) {
            res$md_strat <- coef_sum[trt_row, "Estimate"]
            res$se_strat <- coef_sum[trt_row, "Std. Error"]
            res$z_strat <- res$md_strat / res$se_strat
            res$p_strat <- 2 * pnorm(-abs(res$z_strat))
            res$method_strat <- "ANCOVA"
            res$pooled_variance <- summary(fit_ancova)$sigma^2
            res$df_residual <- fit_ancova$df.residual
          }
        } else {
          res$method_strat <- "Insufficient strata levels"
        }
        
        # =====================================================
        # 分层随机化设计：使用分层Z检验（Inverse Variance Weighted）
        # =====================================================
        strata_levels <- unique(enrolled_strata_A)
        md_s <- numeric()
        w_s <- numeric()
        
        for (s in strata_levels) {
          idx1 <- enrolled_trt == 1 & enrolled_strata_A == s
          idx2 <- enrolled_trt == 2 & enrolled_strata_A == s
          n1s <- sum(idx1)
          n2s <- sum(idx2)
          if (n1s >= 2 && n2s >= 2) {
            m1s <- mean(outcomes[idx1])
            m2s <- mean(outcomes[idx2])
            v1s <- var(outcomes[idx1])
            v2s <- var(outcomes[idx2])
            md_s <- c(md_s, m2s - m1s)
            var_md_s <- v1s/n1s + v2s/n2s
            w_s <- c(w_s, 1/var_md_s)
          }
        }
        
        if (length(w_s) > 0 && sum(w_s) > 0) {
          res$md_strat_z <- sum(w_s * md_s) / sum(w_s)
          res$se_strat_z <- sqrt(1 / sum(w_s))
          res$z_strat_z <- res$md_strat_z / res$se_strat_z
          res$p_strat_z <- 2 * pnorm(-abs(res$z_strat_z))
          res$method_strat_z <- "Stratified Z-test"
        }
        
      } else {
        # 不进行分层分析（单层或强制不分层）
        res$md_strat <- md
        res$se_strat <- se_unstrat
        res$z_strat <- z_unstrat
        res$p_strat <- p_unstrat
        res$method_strat <- "Unstratified only"
      }
      
    } else {
      # 样本不足
      res$method_strat <- "Insufficient data"
    }
    
    return(res)
  }
  
  # ========== 调用分析 ==========
  
  # 分层随机化设计：ANCOVA分析 + 分层Z检验 + 未分层分析
  res_strat_design <- analyze_dataset(
    outcomes = outcomes_strat,
    enrolled_trt = enrolled_trt_strat,
    enrolled_strata_A = enrolled_strata_strat,
    is_stratified_design = TRUE
  )
  
  # 简单随机化设计：未分层分析
  res_simple_design_unstrat <- analyze_dataset(
    outcomes = outcomes_unstrat,
    enrolled_trt = enrolled_trt_simple,
    enrolled_strata_A = enrolled_strata_simple,
    is_stratified_design = FALSE
  )
  
  # 返回结果
  list(
    summary = data.frame(
      # 分层随机化设计结果（分层分析 ANCOVA）
      md_strat_design = res_strat_design$md_strat,
      se_strat_design = res_strat_design$se_strat,
      p_strat_design = res_strat_design$p_strat,
      
      # 分层随机化设计结果（分层Z检验）
      md_strat_z = res_strat_design$md_strat_z,
      se_strat_z = res_strat_design$se_strat_z,
      p_strat_z = res_strat_design$p_strat_z,
      
      # 分层随机化设计结果（未分层分析）
      md_strat_unstrat = res_strat_design$md_unstrat,
      se_strat_unstrat = res_strat_design$se_unstrat,
      p_strat_unstrat = res_strat_design$p_unstrat,
      
      # 简单随机化设计结果
      md_simple_design = res_simple_design_unstrat$md_unstrat,
      se_simple_design = res_simple_design_unstrat$se_unstrat,
      p_simple_design = res_simple_design_unstrat$p_unstrat,
      
      # SMD对比
      smd_strat = strata_balance_smd_strat$mean_smd,
      smd_simple = strata_balance_smd_unstrat$mean_smd,
      
      # 治疗组不平衡
      imbalance_strat = res_strat_design$imbalance,
      imbalance_simple = res_simple_design_unstrat$imbalance,
      
      # 设计参数
      STRATA_LEVELS = STRATA_LEVELS,
      TARGET_N = TARGET_N,
      total_enrolled = total_enrolled,
      
      stringsAsFactors = FALSE
    ),
    
    # 详细结果
    stratified_results = res_strat_design,
    simple_results = res_simple_design_unstrat,
    
    raw_data = data.frame(
      Subject_ID = 1:total_enrolled,
      Strata_A = enrolled_strata_strat,
      Treatment_strat = enrolled_trt_strat,
      Treatment_simple = enrolled_trt_simple,
      Outcome_strat = outcomes_strat,
      Outcome_simple = outcomes_unstrat
    )
  )
}
# ------------------------------------------------------------------------------
# 7. 批量模拟函数：单次参数组合的多次模拟
# ------------------------------------------------------------------------------
run_batch_simulation <- function(
  n_iter = 10000,
  batch_id = 1,
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  SD_COMMON = 15,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEAN_MATRIX = NULL,
   IS_STRATIFIED = TRUE 
) {
  set.seed(123 + batch_id * 1000) 
  
  results <- replicate(n_iter, {
    run_single_trial(
      N_POOL = N_POOL,
      TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE,
      SD_COMMON = SD_COMMON,
      STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO,
      MEAN_MATRIX = MEAN_MATRIX,
      IS_STRATIFIED = IS_STRATIFIED
    )
  }, simplify = FALSE)
  
  results <- results[!sapply(results, is.null)]
  
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

# ------------------------------------------------------------------------------
# 8. 生成汇总统计（用于 Excel 输出）
# ------------------------------------------------------------------------------
# ==============================================================================
# 生成 Excel 输出（完整版）
# 包含：汇总统计、参数设置、参数说明、参数组合摘要
# ==============================================================================

generate_summary_stats <- function(
  simulation_data, 
  param_id = 1,
  # ========== 参数信息（可选，如果提供则包含在输出中）==========
  N_POOL = NULL,
  TARGET_N = NULL,
  BLOCK_SIZE = NULL,
  SD_COMMON = NULL,
  STRATA_LEVELS = NULL,
  STRATA_PROPORTIONS = NULL,
  TREATMENT_RATIO = NULL,
  TREATMENT_RATIO_NAME = NULL,
  MEAN_SCENARIO = NULL,
  MEAN_MATRIX_STR = NULL
) {
  
  # ============================================================================
  # 1. 分层随机化设计（分层随机化 + ANCOVA分析）
  # ============================================================================
  
  # 治疗组不平衡（分层设计通常更平衡）
  imb_median_strat <- median(simulation_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat <- quantile(simulation_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat <- quantile(simulation_data$imbalance_strat, 0.75, na.rm = TRUE)
  arm_imbalance_strat <- sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat)
  
  # Power（ANCOVA分析）
  power_strat_design <- mean(simulation_data$p_strat_design < 0.05, na.rm = TRUE)
  
  # 效应估计
  pe_mean_strat <- mean(simulation_data$md_strat_design, na.rm = TRUE)
  pe_sd_strat <- sd(simulation_data$md_strat_design, na.rm = TRUE)
  
  # 标准误（关键指标）
  se_mean_strat <- mean(simulation_data$se_strat_design, na.rm = TRUE)
  se_sd_strat <- sd(simulation_data$se_strat_design, na.rm = TRUE)
  
  # ============================================================================
  # 2. 分层随机化设计（分层Z检验）
  # ============================================================================
  
  # Power（分层Z检验）
  power_strat_z <- mean(simulation_data$p_strat_z < 0.05, na.rm = TRUE)
  
  # 效应估计
  pe_mean_strat_z <- mean(simulation_data$md_strat_z, na.rm = TRUE)
  pe_sd_strat_z <- sd(simulation_data$md_strat_z, na.rm = TRUE)
  
  # 标准误
  se_mean_strat_z <- mean(simulation_data$se_strat_z, na.rm = TRUE)
  se_sd_strat_z <- sd(simulation_data$se_strat_z, na.rm = TRUE)
  
  # ============================================================================
  # 3. 分层随机化设计（未分层分析）
  # ============================================================================
  
  # Power（未分层分析）
  power_strat_unstrat <- mean(simulation_data$p_strat_unstrat < 0.05, na.rm = TRUE)
  
  # 效应估计
  pe_mean_strat_unstrat <- mean(simulation_data$md_strat_unstrat, na.rm = TRUE)
  pe_sd_strat_unstrat <- sd(simulation_data$md_strat_unstrat, na.rm = TRUE)
  
  # 标准误
  se_mean_strat_unstrat <- mean(simulation_data$se_strat_unstrat, na.rm = TRUE)
  se_sd_strat_unstrat <- sd(simulation_data$se_strat_unstrat, na.rm = TRUE)
  
  # ============================================================================
  # 4. 简单随机化设计（简单随机化 + 未分层分析）
  # ============================================================================
  
  # 治疗组不平衡
  imb_median_simple <- median(simulation_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple <- quantile(simulation_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple <- quantile(simulation_data$imbalance_simple, 0.75, na.rm = TRUE)
  arm_imbalance_simple <- sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple)
  
  # Power（未分层分析）
  power_simple_design <- mean(simulation_data$p_simple_design < 0.05, na.rm = TRUE)
  
  # 效应估计
  pe_mean_simple <- mean(simulation_data$md_simple_design, na.rm = TRUE)
  pe_sd_simple <- sd(simulation_data$md_simple_design, na.rm = TRUE)
  
  # 标准误
  se_mean_simple <- mean(simulation_data$se_simple_design, na.rm = TRUE)
  se_sd_simple <- sd(simulation_data$se_simple_design, na.rm = TRUE)
  
  # ============================================================================
  # 5. 对比指标（核心发现）
  # ============================================================================
  
  # Power差异（分层ANCOVA - 简单设计）
  power_diff <- power_strat_design - power_simple_design
  power_diff_pct <- sprintf("%+.1f%%", power_diff * 100)
  
  # Power差异（分层Z检验 - 简单设计）
  power_diff_z <- power_strat_z - power_simple_design
  power_diff_z_pct <- sprintf("%+.1f%%", power_diff_z * 100)
  
  # Power差异（分层未分层 - 简单设计）
  power_diff_unstrat <- power_strat_unstrat - power_simple_design
  power_diff_unstrat_pct <- sprintf("%+.1f%%", power_diff_unstrat * 100)
  
  # 标准误比率（分层ANCOVA / 简单设计，<1表示分层更精确）
  se_ratio <- se_mean_strat / se_mean_simple
  se_ratio_pct <- sprintf("%.3f", se_ratio)
  
  # 标准误比率（分层Z检验 / 简单设计）
  se_ratio_z <- se_mean_strat_z / se_mean_simple
  se_ratio_z_pct <- sprintf("%.3f", se_ratio_z)
  
  # 标准误比率（分层未分层 / 简单设计）
  se_ratio_unstrat <- se_mean_strat_unstrat / se_mean_simple
  se_ratio_unstrat_pct <- sprintf("%.3f", se_ratio_unstrat)
  
  # 效率增益（方差缩减比例，分层ANCOVA vs 简单设计）
  efficiency_gain <- (se_mean_simple^2 - se_mean_strat^2) / se_mean_simple^2
  efficiency_gain_pct <- sprintf("%.1f%%", efficiency_gain * 100)
  
  # 效率增益（分层Z检验 vs 简单设计）
  efficiency_gain_z <- (se_mean_simple^2 - se_mean_strat_z^2) / se_mean_simple^2
  efficiency_gain_z_pct <- sprintf("%.1f%%", efficiency_gain_z * 100)
  
  # 效率增益（分层未分层 vs 简单设计）
  efficiency_gain_unstrat <- (se_mean_simple^2 - se_mean_strat_unstrat^2) / se_mean_simple^2
  efficiency_gain_unstrat_pct <- sprintf("%.1f%%", efficiency_gain_unstrat * 100)
  
  # ============================================================================
  # 6. SMD对比（平衡性指标）
  # ============================================================================
  
  smd_mean_strat <- mean(simulation_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(simulation_data$smd_simple, na.rm = TRUE)
  smd_summary <- sprintf("%.3f / %.3f", smd_mean_strat, smd_mean_simple)
 
  
  # SMD > 0.15 比例（B因素不平衡率）
  prop_smd_gt_015_strat <- mean(simulation_data$smd_strat > 0.15, na.rm = TRUE)
  prop_smd_gt_015_simple <- mean(simulation_data$smd_simple > 0.15, na.rm = TRUE)
  smd_imbalance_prop <- sprintf("%.1f%% / %.1f%%", 
                                   prop_smd_gt_015_strat * 100,
                                   prop_smd_gt_015_simple * 100)

  # ============================================================================
  # 5. 组装结果
  # ============================================================================
  
  result <- data.frame(
    Simulation_ID = param_id,
    
    # 样本量和设计参数
    TARGET_N = ifelse(is.null(TARGET_N), NA, TARGET_N),
    BLOCK_SIZE = ifelse(is.null(BLOCK_SIZE), NA, BLOCK_SIZE),
    STRATA_LEVELS = ifelse(is.null(STRATA_LEVELS), NA, STRATA_LEVELS),

    
    # 治疗组不平衡对比
    Arm_imbalance_strat = arm_imbalance_strat,
    Arm_imbalance_simple = arm_imbalance_simple,
    
    # Power对比（核心，4套）
    Power_strat_design = sprintf("%.3f", power_strat_design),
    Power_strat_z = sprintf("%.3f", power_strat_z),
    Power_strat_unstrat = sprintf("%.3f", power_strat_unstrat),
    Power_simple_design = sprintf("%.3f", power_simple_design),
    Power_difference = power_diff_pct,
    Power_diff_strat_z_vs_simple = power_diff_z_pct,
    Power_diff_strat_unstrat_vs_simple = power_diff_unstrat_pct,
    
    # 标准误对比（效率）
    SE_mean_strat = sprintf("%.3f", se_mean_strat),
    SE_mean_strat_z = sprintf("%.3f", se_mean_strat_z),
    SE_mean_strat_unstrat = sprintf("%.3f", se_mean_strat_unstrat),
    SE_mean_simple = sprintf("%.3f", se_mean_simple),
    SE_ratio_strat_to_simple = se_ratio_pct,
    SE_ratio_strat_z_to_simple = se_ratio_z_pct,
    SE_ratio_strat_unstrat_to_simple = se_ratio_unstrat_pct,
    Efficiency_gain = efficiency_gain_pct,
    Efficiency_gain_strat_z = efficiency_gain_z_pct,
    Efficiency_gain_unstrat = efficiency_gain_unstrat_pct,
    
    # 效应估计一致性（4套）
    Mean_diff_strat = sprintf("%.2f (%.2f)", pe_mean_strat, pe_sd_strat),
    Mean_diff_strat_z = sprintf("%.2f (%.2f)", pe_mean_strat_z, pe_sd_strat_z),
    Mean_diff_strat_unstrat = sprintf("%.2f (%.2f)", pe_mean_strat_unstrat, pe_sd_strat_unstrat),
    Mean_diff_simple = sprintf("%.2f (%.2f)", pe_mean_simple, pe_sd_simple),
    
    # SMD对比 
    SMD_strat_vs_simple = smd_summary,
    
    Prop_SMD_gt_015 = smd_imbalance_prop,
    
    stringsAsFactors = FALSE
  )
  
  # 添加可选参数信息
  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(SD_COMMON)) result$SD_COMMON <- SD_COMMON
  if (!is.null(STRATA_PROPORTIONS)) result$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ":")
  if (!is.null(TREATMENT_RATIO)) result$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  if (!is.null(TREATMENT_RATIO_NAME)) result$TREATMENT_RATIO_NAME <- TREATMENT_RATIO_NAME
  if (!is.null(MEAN_SCENARIO)) result$MEAN_SCENARIO <- MEAN_SCENARIO
  if (!is.null(MEAN_MATRIX_STR)) result$MEAN_MATRIX_STR <- MEAN_MATRIX_STR
  
  return(result)
}

# ------------------------------------------------------------------------------
# 8.5. 生成Excel输出（含4套结果）
# ------------------------------------------------------------------------------
generate_excel_output <- function(summary_data, output_prefix = "simulation_results") {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(output_prefix, "_", timestamp, ".xlsx")
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("          生成临床试验模拟报告（4套结果对比）\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("需要 openxlsx 包: install.packages('openxlsx')")
  }
  
  wb <- openxlsx::createWorkbook()
  
  # ============================================================================
  # 工作表1: Combined（参数+结果）
  # ============================================================================
  cat("[1/3] 创建工作表: Combined (参数 + 4套结果对比)\n")
  
  desired_order <- c(
    # 标识列
    "Simulation_ID", "Param_ID", "Batch_ID",
    
    # 设计参数（浅蓝背景）
    "N_POOL", "TARGET_N", "BLOCK_SIZE", "SD_COMMON",
    "STRATA_LEVELS", "STRATA_PROPORTIONS",
    "TREATMENT_RATIO", "TREATMENT_RATIO_NAME",
    "MEAN_SCENARIO", "MEAN_MATRIX_STR", "EFFECT_SIZE",
    
    # 分层随机化设计结果（分层分析 ANCOVA，浅绿背景）
    "Arm_imbalance_strat", "Power_strat_design", 
    "SE_mean_strat", "Mean_diff_strat",
    
    # 分层随机化设计结果（分层Z检验，浅绿背景）
    "Power_strat_z", "SE_mean_strat_z", "Mean_diff_strat_z",
    
    # 分层随机化设计结果（未分层分析，浅绿背景）
    "Power_strat_unstrat", "SE_mean_strat_unstrat", "Mean_diff_strat_unstrat",
    
    # 简单随机化设计结果（浅黄背景）
    "Arm_imbalance_simple", "Power_simple_design",
    "SE_mean_simple", "Mean_diff_simple",
    
    # SMD对比
    "SMD_strat_vs_simple",
    
    # 对比指标（浅红背景，突出显示）
    "Power_difference", "Power_diff_strat_z_vs_simple", "Power_diff_strat_unstrat_vs_simple",
    "SE_ratio_strat_to_simple", "SE_ratio_strat_z_to_simple", "SE_ratio_strat_unstrat_to_simple",
    "Efficiency_gain", "Efficiency_gain_strat_z", "Efficiency_gain_unstrat"
  )
  
  # 只保留存在的列
  combined_cols <- intersect(desired_order, colnames(summary_data))
  combined_df <- summary_data[, combined_cols, drop = FALSE]
  
  openxlsx::addWorksheet(wb, "Combined")
  openxlsx::writeData(wb, "Combined", combined_df, startRow = 1, startCol = 1)
  
  # 格式化样式
  header_style <- openxlsx::createStyle(
    fontColour = "white", 
    fgFill = "#2E86C1", 
    fontSize = 11, 
    textDecoration = "bold"
  )
  
  param_style <- openxlsx::createStyle(
    fgFill = "#D6EAF8",  # 浅蓝
    textDecoration = "bold"
  )
  
  strat_style <- openxlsx::createStyle(
    fgFill = "#D5F5E3",  # 浅绿
    textDecoration = "bold"
  )
  
  simple_style <- openxlsx::createStyle(
    fgFill = "#FCF3CF",  # 浅黄
    textDecoration = "bold"
  )
  
  diff_style <- openxlsx::createStyle(
    fgFill = "#F5B7B1",  # 浅红
    textDecoration = "bold"
  )
  
  # 计算各区域列数
  id_cols <- 3  # Simulation_ID, Param_ID, Batch_ID
  param_cols <- sum(grepl("^(N_|TARGET_|BLOCK_|SD_|STRATA_|TREATMENT_|MEAN_|EFFECT_SIZE)", combined_cols))
  strat_result_cols <- sum(grepl("_strat$|_strat_", combined_cols) & !grepl("_vs_", combined_cols))
  simple_result_cols <- sum(grepl("_simple$|_simple_", combined_cols) & !grepl("_vs_", combined_cols))
  diff_cols <- sum(grepl("^(Power_difference|Power_diff_strat_z_vs_simple|Power_diff_strat_unstrat_vs_simple|SE_ratio_strat|SE_ratio_strat_z_to_simple|SE_ratio_strat_unstrat_to_simple|Efficiency_gain|Efficiency_gain_strat_z|Efficiency_gain_unstrat|SMD_strat_vs_simple)", combined_cols))
  
  # 应用样式
  openxlsx::addStyle(wb, "Combined", header_style, rows = 1, cols = 1:ncol(combined_df), gridExpand = TRUE)
  
  if (param_cols > 0) {
    openxlsx::addStyle(wb, "Combined", param_style, rows = 1, 
                       cols = (id_cols + 1):(id_cols + param_cols), gridExpand = TRUE)
  }
  
  strat_start <- id_cols + param_cols + 1
  strat_end <- strat_start + strat_result_cols - 1
  if (strat_result_cols > 0) {
    openxlsx::addStyle(wb, "Combined", strat_style, rows = 1, 
                       cols = strat_start:strat_end, gridExpand = TRUE)
  }
  
  simple_start <- strat_end + 1
  simple_end <- simple_start + simple_result_cols - 1
  if (simple_result_cols > 0) {
    openxlsx::addStyle(wb, "Combined", simple_style, rows = 1, 
                       cols = simple_start:simple_end, gridExpand = TRUE)
  }
  
  if (diff_cols > 0) {
    openxlsx::addStyle(wb, "Combined", diff_style, rows = 1, 
                       cols = (simple_end + 1):ncol(combined_df), gridExpand = TRUE)
  }
  
  openxlsx::setColWidths(wb, "Combined", cols = 1:ncol(combined_df), widths = "auto")
  openxlsx::freezePane(wb, "Combined", firstRow = TRUE, firstCol = TRUE)
  
  cat(sprintf("  ✓ Combined 工作表已创建 (%d 行 x %d 列)\n", 
              nrow(combined_df), ncol(combined_df)))
  
  # ============================================================================
  # 工作表2: Column_Description
  # ============================================================================
  cat("[2/3] 创建工作表: Column_Description\n")
  
  col_descriptions <- data.frame(
    Column_Name = c(
      # 标识
      "Simulation_ID", "Param_ID", "Batch_ID",
      
      # 参数
      "N_POOL", "TARGET_N", "BLOCK_SIZE", "SD_COMMON",
      "STRATA_LEVELS", "STRATA_PROPORTIONS",
      "TREATMENT_RATIO", "TREATMENT_RATIO_NAME",
      "MEAN_SCENARIO", "MEAN_MATRIX_STR", "EFFECT_SIZE",
      
      # 分层随机化设计（分层分析 ANCOVA）
      "Arm_imbalance_strat", "Power_strat_design", 
      "SE_mean_strat", "Mean_diff_strat",
      
      # 分层随机化设计（分层Z检验）
      "Power_strat_z", "SE_mean_strat_z", "Mean_diff_strat_z",
      
      # 分层随机化设计（未分层分析）
      "Power_strat_unstrat", "SE_mean_strat_unstrat", "Mean_diff_strat_unstrat",
      
      # SMD对比
      "SMD_strat_vs_simple",
      
      # 简单随机化设计
      "Arm_imbalance_simple", "Power_simple_design",
      "SE_mean_simple", "Mean_diff_simple",
      
      # 对比指标
      "Power_difference", "Power_diff_strat_z_vs_simple", "Power_diff_strat_unstrat_vs_simple",
      "SE_ratio_strat_to_simple", "SE_ratio_strat_z_to_simple", "SE_ratio_strat_unstrat_to_simple",
      "Efficiency_gain", "Efficiency_gain_strat_z", "Efficiency_gain_unstrat"
    ),
    
    Description = c(
      # 标识
      "模拟组合编号", "参数组合ID", "批次ID (1-10, Overall=总体)",
      
      # 参数
      "初始受试者池大小", "目标入组样本量", "区组大小", "结局指标标准差",
      "分层数量", "各层比例（逗号分隔）",
      "治疗组分配比例（逗号分隔）", "治疗组比例名称",
      "均值场景名称", "均值矩阵字符串表示", "效应量",
      
      # 分层随机化（分层分析 ANCOVA）
      "分层设计组间不平衡度：中位数 (Q1-Q3)", 
      "分层随机化+ANCOVA分析的把握度",
      "分层设计的标准误均值", "分层设计的效应估计均值 (SD)",
      
      # 分层随机化（分层Z检验）
      "分层随机化+分层Z检验的把握度",
      "分层设计分层Z检验的标准误均值", 
      "分层设计分层Z检验的效应估计均值 (SD)",
      
      # 分层随机化（未分层分析）
      "分层随机化+未分层分析的把握度",
      "分层设计未分层分析的标准误均值", 
      "分层设计未分层分析的效应估计均值 (SD)",
      
      # SMD对比
      "SMD对比：分层设计 / 简单设计",
      
      # 简单随机化
      "简单设计组间不平衡度：中位数 (Q1-Q3)",
      "简单随机化+未分层分析的把握度",
      "简单设计的标准误均值", "简单设计的效应估计均值 (SD)",
      
      # 对比
      "把握度差异（分层ANCOVA - 简单），如+3.8%",
      "把握度差异（分层Z检验 - 简单），如+2.5%",
      "把握度差异（分层未分层 - 简单），如+1.2%",
      "标准误比率（分层ANCOVA / 简单），<1表示分层更精确",
      "标准误比率（分层Z检验 / 简单）",
      "标准误比率（分层未分层 / 简单）",
      "效率增益百分比（ANCOVA vs 简单），如8.6%",
      "效率增益百分比（分层Z检验 vs 简单），如5.4%",
      "效率增益百分比（未分层 vs 简单），如3.2%"
    ),
    
    Example = c(
      # 标识
      "1, 2, 3", "1", "1,2,...,10,Overall",
      
      # 参数
      "1000", "182", "4, 6", "15",
      "2, 4", "0.5,0.5 或 0.3,0.7",
      "1,1 或 2,1", "1:1, 2:1",
      "Custom", "10,15,10,15", "5",
      
      # 分层随机化（分层分析 ANCOVA）
      "0.0 (0.0-1.0)", "0.823", "2.45", "5.01 (1.42)",
      
      # 分层随机化（分层Z检验）
      "0.819", "2.50", "5.00 (1.42)",
      
      # 分层随机化（未分层分析）
      "0.812", "2.55", "5.00 (1.43)",
      
      # SMD对比
      "0.021 / 0.152",
      
      # 简单随机化
      "2.0 (0.0-4.0)", "0.785", "2.68", "5.02 (1.45)",
      
      # 对比
      "+3.8%", "+2.5%", "+1.5%", "0.914", "0.933", "0.952", "8.6%", "5.4%", "3.2%"
    ),
    
    stringsAsFactors = FALSE
  )
  
  # 只保留存在的列
  col_descriptions <- col_descriptions[col_descriptions$Column_Name %in% colnames(combined_df), ]
  
  openxlsx::addWorksheet(wb, "Column_Description")
  openxlsx::writeData(wb, "Column_Description", col_descriptions, startRow = 1, startCol = 1)
  openxlsx::setColWidths(wb, "Column_Description", cols = 1:3, widths = c(30, 70, 20))
  openxlsx::addStyle(wb, "Column_Description", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  cat(sprintf("  ✓ Column_Description 工作表已创建 (%d 行)\n", nrow(col_descriptions)))
  
  # ============================================================================
  # 工作表3: Summary_Overview
  # ============================================================================
  if (nrow(summary_data) > 0) {
    cat("[3/3] 创建工作表: Summary_Overview\n")
    
    # 提取关键参数的唯一个数
    overview_data <- data.frame(
      Category = c(
        "总模拟组合数", 
        "批次范围",
        "样本量设置", 
        "区组大小", 
        "层数", 
        "治疗组比例",
        "分层设计平均Power(ANCOVA)",
        "分层设计平均Power(Z检验)",
        "分层设计平均Power(未分层)",
        "简单设计平均Power",
        "平均Power差异(ANCOVA-简单)",
        "平均Power差异(Z检验-简单)",
        "平均Power差异(未分层-简单)",
        "平均效率增益(ANCOVA)",
        "平均效率增益(Z检验)",
        "平均效率增益(未分层)"
      ),
      Values = c(
        as.character(nrow(summary_data)),
        paste(range(summary_data$Batch_ID), collapse = " - "),
        paste(unique(summary_data$TARGET_N), collapse = ", "),
        paste(unique(summary_data$BLOCK_SIZE), collapse = ", "),
        paste(unique(summary_data$STRATA_LEVELS), collapse = ", "),
        paste(unique(summary_data$TREATMENT_RATIO_NAME), collapse = ", "),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_design), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_z), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_unstrat), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_design) - 
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_z) - 
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_unstrat) - 
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%.1f%%", mean(as.numeric(gsub("%", "", summary_data$Efficiency_gain)), na.rm = TRUE)),
        sprintf("%.1f%%", mean(as.numeric(gsub("%", "", summary_data$Efficiency_gain_strat_z)), na.rm = TRUE)),
        sprintf("%.1f%%", mean(as.numeric(gsub("%", "", summary_data$Efficiency_gain_unstrat)), na.rm = TRUE))
      ),
      stringsAsFactors = FALSE
    )
    
    openxlsx::addWorksheet(wb, "Summary_Overview")
    openxlsx::writeData(wb, "Summary_Overview", overview_data, startRow = 1, startCol = 1)
    openxlsx::setColWidths(wb, "Summary_Overview", cols = 1:2, widths = c(35, 50))
    openxlsx::addStyle(wb, "Summary_Overview", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
    
    cat(sprintf("  ✓ Summary_Overview 工作表已创建 (%d 行)\n", nrow(overview_data)))
  } else {
    cat("[3/3] 跳过: Summary_Overview (无数据)\n")
  }
  
  # ============================================================================
  # 保存文件
  # ============================================================================
  tryCatch({
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("✓ Excel报告已成功保存: %s\n", output_file))
    cat(strrep("=", 70), "\n", sep = "")
    cat("工作表说明:\n")
    cat("  • Combined: 参数设置 + 4套结果对比\n")
    cat("    - 浅蓝色: 设计参数\n")
    cat("    - 浅绿色: 分层随机化设计结果（ANCOVA/Z检验/未分层）\n")
    cat("    - 浅黄色: 简单随机化设计结果\n")
    cat("    - 浅红色: 关键对比指标（Power差异、效率增益）\n")
    cat("  • Column_Description: 所有列的详细说明\n")
    cat("  • Summary_Overview: 参数分布摘要 + 平均Power对比\n")
    cat(strrep("=", 70), "\n\n")
    
    return(output_file)
    
  }, error = function(e) {
    warning(sprintf("保存Excel文件时出错: %s", e$message))
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# 9. 主函数：运行完整模拟（单次参数组合）
# ------------------------------------------------------------------------------
run_simulation <- function(
  n_iter = 10000,
  n_batch = 1,
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  SD_COMMON = 15,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEAN_MATRIX = NULL,
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("开始模拟: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("参数: N_POOL=%d, TARGET_N=%d, BLOCK_SIZE=%d\n", 
                N_POOL, TARGET_N, BLOCK_SIZE))
    cat(sprintf("      SD=%.1f, STRATA_LEVELS=%d\n", SD_COMMON, STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    
    if (!is.null(MEAN_MATRIX)) {
      cat("      MEAN_MATRIX:\n")
      print(MEAN_MATRIX)
    }
  }
  
  batch_results <- lapply(1:n_batch, function(i) {
    if (verbose && n_batch > 1) {
      cat(sprintf("\r完成批次 %d / %d", i, n_batch))
    }
    run_batch_simulation(
      n_iter = n_iter,
      batch_id = i,
      N_POOL = N_POOL,
      TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE,
      SD_COMMON = SD_COMMON,
      STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO,
      MEAN_MATRIX = MEAN_MATRIX
    )
  })
  
  if (verbose && n_batch > 1) cat("\n")
  
  final_data <- do.call(rbind, batch_results)
  
  # 添加参数信息
  final_data$N_POOL <- N_POOL
  final_data$TARGET_N <- TARGET_N
  final_data$BLOCK_SIZE <- BLOCK_SIZE
  final_data$SD_COMMON <- SD_COMMON
  final_data$STRATA_LEVELS <- STRATA_LEVELS
  final_data$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ",")
  final_data$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  
  if (!is.null(MEAN_MATRIX)) {
    final_data$MEAN_MATRIX <- matrix_to_string(MEAN_MATRIX)
  }
  
  return(final_data)
}
# ------------------------------------------------------------------------------
# 9.5. 主函数：运行完整模拟（每个 batch 单独汇总）
# ------------------------------------------------------------------------------
run_simulation_per_batch <- function(
  n_iter = 1000,
  n_batch = 10,
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  SD_COMMON = 15,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEAN_MATRIX = NULL,
  IS_STRATIFIED = TRUE,
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("开始模拟: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("参数: N_POOL=%d, TARGET_N=%d, BLOCK_SIZE=%d\n", 
                N_POOL, TARGET_N, BLOCK_SIZE))
    cat(sprintf("      SD=%.1f, STRATA_LEVELS=%d\n", SD_COMMON, STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    
    if (!is.null(MEAN_MATRIX)) {
      cat("      MEAN_MATRIX:\n")
      print(MEAN_MATRIX)
    }
  }
  
  # 存储每个 batch 的详细结果和汇总
  batch_results_list <- list()
  batch_summaries_list <- list()
  
  for (batch_id in 1:n_batch) {
    if (verbose) {
      cat(sprintf("\r处理批次 %d / %d", batch_id, n_batch))
    }
    
    # 运行单个 batch
    batch_result <- run_batch_simulation(
      n_iter = n_iter,
      batch_id = batch_id,
      N_POOL = N_POOL,
      TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE,
      SD_COMMON = SD_COMMON,
      STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO,
      MEAN_MATRIX = MEAN_MATRIX,
      IS_STRATIFIED = IS_STRATIFIED 
    )
    
    # 为该 batch 生成汇总统计
    batch_summary <- generate_summary_stats(
      simulation_data = batch_result,
      param_id = batch_id,
      N_POOL = N_POOL,
      TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE,
      SD_COMMON = SD_COMMON,
      STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
      TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
      TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
      MEAN_SCENARIO = if (!is.null(MEAN_MATRIX)) "Custom" else "Default",
      MEAN_MATRIX_STR = if (!is.null(MEAN_MATRIX)) matrix_to_string(MEAN_MATRIX) else NA
    )
    
    # 添加 batch ID
    batch_result$Batch_ID <- batch_id
    batch_summary$Batch_ID <- batch_id
    
    batch_results_list[[batch_id]] <- batch_result
    batch_summaries_list[[batch_id]] <- batch_summary
  }
  
  if (verbose) cat("\n")
  
  # 合并所有 batch 的详细结果
  all_detailed_results <- do.call(rbind, batch_results_list)
  
  # 合并所有 batch 的汇总结果（每个 batch 一行）
  all_batch_summaries <- do.call(rbind, batch_summaries_list)
  
  # 添加总体汇总（所有 batch 合并）
  overall_summary <- generate_summary_stats(
    simulation_data = all_detailed_results,
    param_id = "Overall",
    N_POOL = N_POOL,
    TARGET_N = TARGET_N,
    BLOCK_SIZE = BLOCK_SIZE,
    SD_COMMON = SD_COMMON,
    STRATA_LEVELS = STRATA_LEVELS,
    STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
    TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
    TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
    MEAN_SCENARIO = if (!is.null(MEAN_MATRIX)) "Custom" else "Default",
    MEAN_MATRIX_STR = if (!is.null(MEAN_MATRIX)) matrix_to_string(MEAN_MATRIX) else NA
  )
  overall_summary$Batch_ID <- "Overall"
  
  # 将总体汇总添加到最后
  all_batch_summaries <- rbind(all_batch_summaries, overall_summary)
  
  if (verbose) {
    cat(sprintf("\n✓ 模拟完成！共 %d 个批次，每个批次 %d 次模拟，总计 %d 次\n", 
                n_batch, n_iter, n_batch * n_iter))
    cat(sprintf("  - 详细结果: %d 行\n", nrow(all_detailed_results)))
    cat(sprintf("  - 汇总结果: %d 行（%d 个批次 + 1 行总体）\n", 
                nrow(all_batch_summaries), n_batch))
  }
  
  return(list(
    detailed_results = all_detailed_results,
    batch_summaries = all_batch_summaries,
    overall_summary = overall_summary
  ))
}
# ------------------------------------------------------------------------------
# 10. 批量测试函数：测试多组参数组合
# ------------------------------------------------------------------------------
run_parameter_grid <- function(
  param_grid,
  n_iter = 10000,
  n_batch = 1,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "simulation_results"
) {
  results_list <- list()
  summary_list <- list()
  
  for (i in 1:nrow(param_grid)) {
    if (verbose) {
      cat(sprintf("\n========== 测试参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    }
    
    # 提取当前行的参数
    params <- param_grid[i, , drop = FALSE]
    
    # 解析均值矩阵
    mean_matrix <- NULL
    if ("MEAN_MATRIX_STR" %in% colnames(params)) {
      mean_matrix <- string_to_matrix(
        as.character(params$MEAN_MATRIX_STR), 
        n_strata = params$STRATA_LEVELS
      )
    }
    
    # 运行模拟
    sim_result <- run_simulation(
      n_iter = n_iter,
      n_batch = n_batch,
      N_POOL = params$N_POOL,
      TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE,
      SD_COMMON = params$SD_COMMON,
      STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ","))),
      TREATMENT_RATIO = if ("TREATMENT_RATIO" %in% colnames(params)) {
        as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
      } else {
        c(1, 1)
      },
      MEAN_MATRIX = mean_matrix,
      verbose = verbose
    )
    
    # 添加参数组合编号
    sim_result$Param_ID <- i
    
    results_list[[i]] <- sim_result
    
    # 生成该参数组合的汇总统计
    summary_stats <- generate_summary_stats(
  simulation_data = sim_result,
  param_id = i,
  N_POOL = params$N_POOL,
  TARGET_N = params$TARGET_N,
  BLOCK_SIZE = params$BLOCK_SIZE,
  SD_COMMON = params$SD_COMMON,
  STRATA_LEVELS = params$STRATA_LEVELS,
  STRATA_PROPORTIONS = as.character(params$STRATA_PROPORTIONS),
  TREATMENT_RATIO = as.character(params$TREATMENT_RATIO),
  TREATMENT_RATIO_NAME = as.character(params$TREATMENT_RATIO_NAME),
  MEAN_SCENARIO = as.character(params$MEAN_SCENARIO),
  MEAN_MATRIX_STR = as.character(params$MEAN_MATRIX_STR)
)
    summary_stats$Param_ID <- i
    summary_list[[i]] <- summary_stats
  }
  
  # 合并所有结果
  final_results <- do.call(rbind, results_list)
  summary_results <- do.call(rbind, summary_list)
  
  # 添加参数信息到汇总表
  summary_results <- merge(summary_results, 
                          param_grid[, c("Param_ID", names(param_grid))], 
                          by = "Param_ID", 
                          all.x = TRUE)
  
  if (verbose) {
    cat("\n========== 所有参数组合测试完成 ==========\n")
  }
  
  # 生成 Excel 输出
  if (generate_excel) {
    generate_excel_output(summary_results, output_prefix)
  }
  
  return(list(
    detailed_results = final_results,
    summary_results = summary_results
  ))
}

# ------------------------------------------------------------------------------
# 10.5. 批量测试函数：测试多组参数组合（每个 batch 单独汇总）
# ------------------------------------------------------------------------------
run_parameter_grid_per_batch <- function(
  param_grid,
  n_iter = 1000,
  n_batch = 10,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "simulation_results_per_batch"
) {
  all_batch_summaries_list <- list()
  current_row <- 1
  
  for (i in 1:nrow(param_grid)) {
    if (verbose) {
      cat(sprintf("\n========== 测试参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    }
    
    # 提取当前行的参数
    params <- param_grid[i, , drop = FALSE]

  # ✅ 关键：从 param_grid 读取 IS_STRATIFIED（不是函数参数！）
  is_stratified <- if ("IS_STRATIFIED" %in% colnames(params)) {
    as.logical(params$IS_STRATIFIED)  # 确保是逻辑值
  } else {
    TRUE  # 默认分层
  }
    
    # 解析均值矩阵
    mean_matrix <- NULL
    if ("MEAN_MATRIX_STR" %in% colnames(params)) {
      mean_matrix <- string_to_matrix(
        as.character(params$MEAN_MATRIX_STR), 
        n_strata = params$STRATA_LEVELS
      )
    }
    
    # 运行模拟（每个 batch 单独汇总）
    sim_results <- run_simulation_per_batch(
      n_iter = n_iter,
      n_batch = n_batch,
      N_POOL = params$N_POOL,
      TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE,
      SD_COMMON = params$SD_COMMON,
      STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ","))),
      TREATMENT_RATIO = if ("TREATMENT_RATIO" %in% colnames(params)) {
        as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
      } else {
        c(1, 1)
      },
      MEAN_MATRIX = mean_matrix,
       IS_STRATIFIED = is_stratified, 
      verbose = verbose && !verbose  # 避免嵌套进度条
    )
    
    # 为每个 batch 汇总行添加 Param_ID 和参数网格中的其他列
    batch_summaries <- sim_results$batch_summaries
    batch_summaries$Param_ID <- i
    
    # 添加参数网格中的其他列（确保与 param_grid 一致）
    param_cols_to_add <- setdiff(colnames(param_grid), "Param_ID")
    for (col in param_cols_to_add) {
      batch_summaries[[col]] <- params[[col]]
    }
    
    all_batch_summaries_list[[current_row]] <- batch_summaries
    current_row <- current_row + 1
  }
  
  # 合并所有参数组合的所有 batch 汇总
  all_batch_summaries <- do.call(rbind, all_batch_summaries_list)
  
  # 重置行名
  rownames(all_batch_summaries) <- NULL
  
  if (verbose) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("✓ 所有参数组合测试完成！\n"))
    cat(sprintf("  总参数组合数: %d\n", nrow(param_grid)))
    cat(sprintf("  每组合批次数: %d\n", n_batch))
    cat(sprintf("  总汇总行数: %d (%d 组合 × %d 批次 + %d Overall 行)\n", 
                nrow(all_batch_summaries), nrow(param_grid), n_batch, nrow(param_grid)))
    cat(strrep("=", 70), "\n\n")
  }
  
  # 生成 Excel 输出
  if (generate_excel) {
    output_file <- generate_excel_output(
      summary_data = all_batch_summaries,
      output_prefix = output_prefix
    )
    
    if (verbose) {
      cat(sprintf("\n✓ Excel 报告已保存: %s\n", output_file))
      cat(sprintf("  工作表 'Combined' 包含 %d 行（每个 batch 一行）\n", nrow(all_batch_summaries)))
    }
  }
  
  return(list(
    batch_summaries_all = all_batch_summaries,
    param_grid = param_grid
  ))
}