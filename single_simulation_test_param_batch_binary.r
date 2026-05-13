# ==============================================================================
# 临床研究二分类数据模拟流程 (完整版)
# 功能：
#   - 参数化模拟系统
#   - 支持任意层数和自定义比例
#   - 支持自定义治疗组分配比例 (1:1, 2:1, 3:1等)
#   - 支持每层每组的事件概率矩阵定义
#   - 自动计算随机列表大小
#   - 计算分层因素的 Standardized Mean Difference (SMD)
#   - 支持从 Excel 读取参数网格
#   - 未分层分析：Fisher Exact Test
#   - 分层分析：Cochran-Mantel-Haenszel (CMH) Test
#   - Excel 输出汇总统计
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 加载必要的库
# ------------------------------------------------------------------------------
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("pbapply", quietly = TRUE)) install.packages("pbapply")
library(openxlsx)
library(pbapply)
library(dplyr)
# ------------------------------------------------------------------------------
# 1. 辅助函数：动态生成所有可能的区组排列
# ------------------------------------------------------------------------------
generate_all_blocks <- function(block_size, ratio = c(1, 1)) {
  if (length(ratio) != 2) stop("ratio 必须是长度为 2 的向量")
  ratio_sum <- sum(ratio)
  if (block_size %% ratio_sum != 0) stop(sprintf("区组大小 %d 必须能被比例总和 %d 整除", block_size, ratio_sum))
  
  n_group1 <- as.integer((ratio[1] / ratio_sum) * block_size)
  
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
  
  if (n1 == 0 || n2 == 0) return(NA)
  
  n_strata <- length(unique(enrolled_strata))
  smd_values <- numeric(n_strata)
  
  for (s in 1:n_strata) {
    prop_trt1 <- mean(enrolled_strata[enrolled_trt == 1] == s)
    prop_trt2 <- mean(enrolled_strata[enrolled_trt == 2] == s)
    p_pool <- (prop_trt1 + prop_trt2) / 2
    
    if (p_pool > 0 && p_pool < 1) {
      smd_values[s] <- abs(prop_trt1 - prop_trt2) / sqrt(p_pool * (1 - p_pool))
    } else {
      smd_values[s] <- 0
    }
  }
  
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
                nrow = n_strata, ncol = 2, byrow = TRUE)
  return(mat)
}

# ------------------------------------------------------------------------------
# 5.5 从 Excel 读取参数网格
# ------------------------------------------------------------------------------

gen_prob_str <- function(ctrl_start, n_strata, delta, trt_lift, digits = 4) {
  ctrl <- ctrl_start + (0:(n_strata - 1)) * delta
  trt  <- ctrl + trt_lift
  
  # 关键：先 round 消除浮点噪音，再拼接
  ctrl <- round(ctrl, digits)
  trt  <- round(trt, digits)
  
  paste(paste(ctrl, trt, sep = ","), collapse = ";")
}

read_param_grid_from_excel <- function(excel_path, sheet = 1) {
  if (!file.exists(excel_path)) {
    stop(sprintf("Excel 文件不存在: %s", excel_path))
  }
  
  param_grid <- openxlsx::read.xlsx(excel_path, sheet = sheet)
  
  required_cols <- c("TARGET_N", "STRATA_LEVELS", "STRATA_PROPORTIONS", 
                     "TREATMENT_RATIO", "ctrl_start", "delta", "trt_lift")

  missing_cols <- setdiff(required_cols, colnames(param_grid))

  if (length(missing_cols) > 0) {
    stop(sprintf("Excel 参数表缺少必要列: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (!"Param_ID" %in% colnames(param_grid)) {
    param_grid$Param_ID <- 1:nrow(param_grid)
  }
  


# 批量转换关键列为数值型
param_grid <- transform(param_grid,
  ctrl_start = as.numeric(ctrl_start),
  delta      = as.numeric(delta),
  trt_lift   = as.numeric(trt_lift)
)

# 然后再生成 prob
param_grid$PROB_MATRIX_STR<- mapply(
  gen_prob_str,
  ctrl_start = param_grid$ctrl_start,
  n_strata   = param_grid$STRATA_LEVELS,
  delta      = param_grid$delta,
  trt_lift   = param_grid$trt_lift
)
  # 默认值
  if (!"N_POOL" %in% colnames(param_grid)) param_grid$N_POOL <- 1000
  if (!"BLOCK_SIZE" %in% colnames(param_grid)) param_grid$BLOCK_SIZE <- 4
  if (!"MEAN_SCENARIO" %in% colnames(param_grid)) param_grid$MEAN_SCENARIO <- "Custom"
  if (!"TREATMENT_RATIO_NAME" %in% colnames(param_grid)) {
    param_grid$TREATMENT_RATIO_NAME <- param_grid$TREATMENT_RATIO
  }
  
  cat(sprintf("✓ 已从 Excel 读取 %d 行参数组合: %s\n", nrow(param_grid), excel_path))
  return(param_grid)
}


# ------------------------------------------------------------------------------
# 6. 核心函数：执行单次试验（二分类结局，Fisher + CMH）
# ------------------------------------------------------------------------------
run_single_trial <- function(
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  PROB_MATRIX = NULL,
  IS_STRATIFIED = TRUE
) {
  # 验证参数
  if (abs(sum(STRATA_PROPORTIONS) - 1) > 1e-6) {
    stop("STRATA_PROPORTIONS 的和必须等于 1")
  }
  if (length(STRATA_PROPORTIONS) != STRATA_LEVELS) {
    stop("STRATA_PROPORTIONS 的长度必须等于 STRATA_LEVELS")
  }
  
  # 验证或创建 PROB_MATRIX
  if (is.null(PROB_MATRIX)) {
    PROB_MATRIX <- matrix(c(0.3, 0.5), nrow = STRATA_LEVELS, ncol = 2, byrow = TRUE)
  }
  
  if (nrow(PROB_MATRIX) != STRATA_LEVELS) {
    stop(sprintf("PROB_MATRIX 的行数 (%d) 必须等于 STRATA_LEVELS (%d)", 
                 nrow(PROB_MATRIX), STRATA_LEVELS))
  }
  if (ncol(PROB_MATRIX) != 2) {
    stop("PROB_MATRIX 必须有 2 列（对应 2 个治疗组）")
  }
  if (any(PROB_MATRIX < 0 | PROB_MATRIX > 1)) {
    stop("PROB_MATRIX 的所有值必须在 [0, 1] 范围内")
  }
  
  # 自动计算随机列表大小
  RAND_LIST_SIZE_PER_STRATA <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  RAND_LIST_SIZE_TOTAL <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  
  # Step 1: 生成入组人群
  pool_strata <- sample(1:STRATA_LEVELS, size = N_POOL, 
                        replace = TRUE, prob = STRATA_PROPORTIONS)
  
  # Step 2: 生成两套随机表
  rand_lists <- generate_stratified_rand_lists(
    n_strata = STRATA_LEVELS, 
    n_per_strata = RAND_LIST_SIZE_PER_STRATA,
    block_size = BLOCK_SIZE, ratio = TREATMENT_RATIO
  )
  ptr_list <- rep(0, STRATA_LEVELS)
  
  rand_list_global <- generate_rand_list(
    n_per_strata = RAND_LIST_SIZE_TOTAL,
    block_size = BLOCK_SIZE, ratio = TREATMENT_RATIO
  )
  ptr_global <- 0
  
  enrolled_trt_strat <- numeric(TARGET_N)
  enrolled_trt_simple <- numeric(TARGET_N)
  enrolled_strata <- numeric(TARGET_N)
  total_enrolled <- 0
  
  # Step 3: 模拟入组流程
  for (i in 1:N_POOL) {
    if (total_enrolled >= TARGET_N) break
    s <- pool_strata[i]
    if (ptr_list[s] >= length(rand_lists[[s]])) next
    if (ptr_global >= length(rand_list_global)) break
    
    total_enrolled <- total_enrolled + 1
    ptr_list[s] <- ptr_list[s] + 1
    enrolled_trt_strat[total_enrolled] <- rand_lists[[s]][ptr_list[s]]
    ptr_global <- ptr_global + 1
    enrolled_trt_simple[total_enrolled] <- rand_list_global[ptr_global]
    enrolled_strata[total_enrolled] <- s
  }
  
  enrolled_trt_strat <- enrolled_trt_strat[1:total_enrolled]
  enrolled_trt_simple <- enrolled_trt_simple[1:total_enrolled]
  enrolled_strata <- enrolled_strata[1:total_enrolled]
  enrolled_strata_strat <- enrolled_strata
  enrolled_strata_simple <- enrolled_strata
  
  # Step 4: 计算分层因素的 SMD
  strata_balance_smd_strat <- calculate_strata_smd(enrolled_trt_strat, enrolled_strata_strat)
  strata_balance_smd_unstrat <- calculate_strata_smd(enrolled_trt_simple, enrolled_strata_simple)
  
  # Step 5: 模拟二分类结局
  outcomes_strat <- numeric(total_enrolled)
  outcomes_unstrat <- numeric(total_enrolled)
  
  for (i in 1:total_enrolled) {
    p_strat <- PROB_MATRIX[enrolled_strata_strat[i], enrolled_trt_strat[i]]
    outcomes_strat[i] <- rbinom(1, size = 1, prob = p_strat)
    p_unstrat <- PROB_MATRIX[enrolled_strata_simple[i], enrolled_trt_simple[i]]
    outcomes_unstrat[i] <- rbinom(1, size = 1, prob = p_unstrat)
  }
  
  # Step 6: 统计分析（二分类）
  analyze_dataset_binary <- function(outcomes, enrolled_trt, enrolled_strata_A, 
                                     is_stratified_design = TRUE) {
    n1 <- sum(enrolled_trt == 1)
    n2 <- sum(enrolled_trt == 2)
    imbalance <- abs(n1/n2)
    
    res <- list(
      n1 = n1, n2 = n2, imbalance = imbalance,
      p_unstrat = NA, or_unstrat = NA, or_ci_lower_unstrat = NA, or_ci_upper_unstrat = NA,
      rd_unstrat = NA, rr_unstrat = NA, resp_rate_trt1_unstrat = NA, resp_rate_trt2_unstrat = NA,
      p_strat = NA, or_strat = NA, or_ci_lower_strat = NA, or_ci_upper_strat = NA,
      rd_strat = NA, resp_rate_trt1_strat = NA, resp_rate_trt2_strat = NA,
      method_strat = NA, method_unstrat = NA,
      chisq_unstrat = NA, chisq_strat = NA, vrf_actual = NA
    )
    
    # ========== 未分层分析：Fisher Exact Test ==========
    if (n1 >= 1 && n2 >= 1) {
      tbl_2x2 <- table(factor(enrolled_trt, levels = c(1, 2)),
                       factor(outcomes, levels = c(1, 0)))
      
      if (nrow(tbl_2x2) == 2 && ncol(tbl_2x2) == 2 && sum(tbl_2x2) > 0) {
        # 响应率
        res$resp_rate_trt1_unstrat <- tbl_2x2[1, 1] / sum(tbl_2x2[1, ])
        res$resp_rate_trt2_unstrat <- tbl_2x2[2, 1] / sum(tbl_2x2[2, ])
        # 率差
        res$rd_unstrat <- res$resp_rate_trt2_unstrat - res$resp_rate_trt1_unstrat
        # 率比
        res$rr_unstrat <- ifelse(res$resp_rate_trt1_unstrat > 0, 
                                  res$resp_rate_trt2_unstrat / res$resp_rate_trt1_unstrat, NA)

        ft <- tryCatch(fisher.test(tbl_2x2[, c(2, 1)]), error = function(e) NULL)
        if (!is.null(ft)) {
          res$p_unstrat <- ft$p.value
          res$or_unstrat <- as.numeric(ft$estimate)
          res$or_ci_lower_unstrat <- ft$conf.int[1]
          res$or_ci_upper_unstrat <- ft$conf.int[2]
          res$method_unstrat <- "Fisher Exact"
        }
      }
    }
      
      # Logistic regression (unstratified) for VRF
      df_glm <- data.frame(y = outcomes, trt = enrolled_trt, stringsAsFactors = FALSE)
      fit_unstrat <- tryCatch(glm(y ~ trt, family = binomial, data = df_glm), error = function(e) NULL)
      if (!is.null(fit_unstrat) && "trt" %in% rownames(summary(fit_unstrat)$coefficients)) {
        z_unstrat <- summary(fit_unstrat)$coefficients["trt", "z.value"]
        res$chisq_unstrat <- as.numeric(z_unstrat)^2
      }
    }
    
    # ========== 分层分析：CMH Test ==========
    if (is_stratified_design) {
      if (length(unique(enrolled_strata_A)) >= 2) {
        n_strata <- length(unique(enrolled_strata_A))
        strata_vals <- sort(unique(enrolled_strata_A))
        
        arrays <- array(0, dim = c(2, 2, n_strata))
        valid_strata <- rep(FALSE, n_strata)
        
        for (idx_s in 1:n_strata) {
          s <- strata_vals[idx_s]
          sel <- enrolled_strata_A == s
          if (sum(sel) >= 2) {
            tbl_s <- table(factor(enrolled_trt[sel], levels = c(1, 2)),
                           factor(outcomes[sel], levels = c(1, 0)))
            if (nrow(tbl_s) == 2 && ncol(tbl_s) == 2 && all(tbl_s >= 0)) {
              arrays[,,idx_s] <- as.matrix(tbl_s)
              valid_strata[idx_s] <- TRUE
            }
          }
        }
        
        if (sum(valid_strata) >= 1) {
          arrays_valid <- arrays[,,valid_strata, drop = FALSE]
          
          mh <- tryCatch(mantelhaen.test(arrays_valid[, c(2, 1), ]), error = function(e) NULL)
          
          if (!is.null(mh)) {
            res$p_strat <- mh$p.value
            res$or_strat <- as.numeric(mh$estimate)
            if (!is.null(mh$conf.int)) {
              res$or_ci_lower_strat <- mh$conf.int[1]
              res$or_ci_upper_strat <- mh$conf.int[2]
            }
            res$method_strat <- "CMH"
            
            # 分层率差（按样本量加权）
            rd_s <- numeric()
            w_s <- numeric()
            rr1_s <- numeric()
            rr2_s <- numeric()
            for (idx_s in which(valid_strata)) {
              tbl_s <- arrays[,,idx_s]
              n_total_s <- sum(tbl_s)
              p1s <- tbl_s[1, 1] / sum(tbl_s[1, ])
              p2s <- tbl_s[2, 1] / sum(tbl_s[2, ])
              rd_s <- c(rd_s, p2s - p1s)
              w_s <- c(w_s, n_total_s)
              rr1_s <- c(rr1_s, p1s)
              rr2_s <- c(rr2_s, p2s)
            }
            if (sum(w_s) > 0) {
              res$rd_strat <- sum(w_s * rd_s) / sum(w_s)
              res$resp_rate_trt1_strat <- sum(w_s * rr1_s) / sum(w_s)
              res$resp_rate_trt2_strat <- sum(w_s * rr2_s) / sum(w_s)
            }
          }
        } else {
          res$method_strat <- "Insufficient strata data"
        }
      } else {
        res$method_strat <- "Insufficient strata levels"
      }
    } else {
      res$method_strat <- "Unstratified only"
    }
    
    # Logistic regression (stratified) for VRF
    if (is_stratified_design && length(unique(enrolled_strata_A)) >= 2) {
      df_glm2 <- data.frame(y = outcomes, trt = enrolled_trt, strata = factor(enrolled_strata_A), stringsAsFactors = FALSE)
      fit_strat <- tryCatch(glm(y ~ trt + strata, family = binomial, data = df_glm2), error = function(e) NULL)
      if (!is.null(fit_strat) && "trt" %in% rownames(summary(fit_strat)$coefficients)) {
        z_strat <- summary(fit_strat)$coefficients["trt", "z.value"]
        res$chisq_strat <- as.numeric(z_strat)^2
      }
    }
    
    # VRF actual
    if (!is.na(res$chisq_strat) && !is.na(res$chisq_unstrat) && res$chisq_unstrat > 0) {
      res$vrf_actual <- res$chisq_strat / res$chisq_unstrat
    }
    
    return(res)
  }
  
  # 调用分析
  res_strat_design <- analyze_dataset_binary(
    outcomes = outcomes_strat, enrolled_trt = enrolled_trt_strat,
    enrolled_strata_A = enrolled_strata_strat, is_stratified_design = TRUE
  )
  
  res_simple_design <- analyze_dataset_binary(
    outcomes = outcomes_unstrat, enrolled_trt = enrolled_trt_simple,
    enrolled_strata_A = enrolled_strata_simple, is_stratified_design = FALSE
  )
  
  # VRF expected: variance decomposition based on PROB_MATRIX
  p_strata <- (PROB_MATRIX[, 1] + PROB_MATRIX[, 2]) / 2
  var_within <- sum(STRATA_PROPORTIONS * p_strata * (1 - p_strata))
  p_mean <- sum(STRATA_PROPORTIONS * p_strata)
  var_between <- sum(STRATA_PROPORTIONS * (p_strata - p_mean)^2)
  vrf_expected <- if (var_within > 0) 1 + var_between / var_within else 1
  
  # 返回结果
  list(
    summary = data.frame(
      # 分层随机化 + CMH 分析
      p_strat_design = res_strat_design$p_strat,
      or_strat_design = res_strat_design$or_strat,
      rd_strat_design = res_strat_design$rd_strat,
      resp_rate_trt1_strat = res_strat_design$resp_rate_trt1_strat,
      resp_rate_trt2_strat = res_strat_design$resp_rate_trt2_strat,
      
      # 分层随机化 + 未分层 Fisher 分析
      p_strat_unstrat = res_strat_design$p_unstrat,
      or_strat_unstrat = res_strat_design$or_unstrat,
      rd_strat_unstrat = res_strat_design$rd_unstrat,
      resp_rate_trt1_strat_unstrat = res_strat_design$resp_rate_trt1_unstrat,
      resp_rate_trt2_strat_unstrat = res_strat_design$resp_rate_trt2_unstrat,
      
      # 简单随机化 + 未分层 Fisher 分析
      p_simple_design = res_simple_design$p_unstrat,
      or_simple_design = res_simple_design$or_unstrat,
      rd_simple_design = res_simple_design$rd_unstrat,
      resp_rate_trt1_simple = res_simple_design$resp_rate_trt1_unstrat,
      resp_rate_trt2_simple = res_simple_design$resp_rate_trt2_unstrat,
      
      # SMD 对比
      smd_strat = strata_balance_smd_strat$mean_smd,
      smd_simple = strata_balance_smd_unstrat$mean_smd,
      
      # 治疗组不平衡
      imbalance_strat = res_strat_design$imbalance,
      imbalance_simple = res_simple_design$imbalance,
      
      # VRF
      vrf_actual = res_strat_design$vrf_actual,
      vrf_expected = vrf_expected,
      
      # 设计参数
      STRATA_LEVELS = STRATA_LEVELS,
      TARGET_N = TARGET_N,
      total_enrolled = total_enrolled,
      
      stringsAsFactors = FALSE
    ),
    stratified_results = res_strat_design,
    simple_results = res_simple_design,
    raw_data = data.frame(
      Subject_ID = 1:total_enrolled,
      Strata_A = enrolled_strata_strat,
      Treatment_strat = enrolled_trt_strat,
      Treatment_simple = enrolled_trt_simple,
      Outcome_strat = outcomes_strat,
      Outcome_simple = outcomes_unstrat,
      stringsAsFactors = FALSE
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
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  PROB_MATRIX = NULL,
  IS_STRATIFIED = TRUE 
) {
  set.seed(123 + batch_id * 1000) 
  
  results <- replicate(n_iter, {
    run_single_trial(
      N_POOL = N_POOL, TARGET_N = TARGET_N, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO, PROB_MATRIX = PROB_MATRIX,
      IS_STRATIFIED = IS_STRATIFIED
    )
  }, simplify = FALSE)
  
  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}


# ------------------------------------------------------------------------------
# 8. 生成汇总统计（二分类版，用于 Excel 输出）
# ------------------------------------------------------------------------------
generate_summary_stats <- function(
  simulation_data, 
  param_id = 1,
  N_POOL = NULL,
  TARGET_N = NULL,
  BLOCK_SIZE = NULL,
  STRATA_LEVELS = NULL,
  STRATA_PROPORTIONS = NULL,
  TREATMENT_RATIO = NULL,
  TREATMENT_RATIO_NAME = NULL,
  MEAN_SCENARIO = NULL,
  PROB_MATRIX_STR = NULL
) {
  
  # 1. 治疗组不平衡
  imb_median_strat <- median(simulation_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat <- quantile(simulation_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat <- quantile(simulation_data$imbalance_strat, 0.75, na.rm = TRUE)
  arm_imbalance_strat <- sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat)
  
  imb_median_simple <- median(simulation_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple <- quantile(simulation_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple <- quantile(simulation_data$imbalance_simple, 0.75, na.rm = TRUE)
  arm_imbalance_simple <- sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple)
  
  # 2. Power
  power_strat_design <- mean(simulation_data$p_strat_design < 0.05, na.rm = TRUE)
  power_strat_unstrat <- mean(simulation_data$p_strat_unstrat < 0.05, na.rm = TRUE)
  power_simple_design <- mean(simulation_data$p_simple_design < 0.05, na.rm = TRUE)
  
  # 3. 效应估计：OR
  or_mean_strat <- mean(simulation_data$or_strat_design, na.rm = TRUE)
  or_sd_strat <- sd(simulation_data$or_strat_design, na.rm = TRUE)
  or_mean_strat_unstrat <- mean(simulation_data$or_strat_unstrat, na.rm = TRUE)
  or_sd_strat_unstrat <- sd(simulation_data$or_strat_unstrat, na.rm = TRUE)
  or_mean_simple <- mean(simulation_data$or_simple_design, na.rm = TRUE)
  or_sd_simple <- sd(simulation_data$or_simple_design, na.rm = TRUE)
  
  # 4. 效应估计：RD
  rd_mean_strat <- mean(simulation_data$rd_strat_design, na.rm = TRUE)
  rd_sd_strat <- sd(simulation_data$rd_strat_design, na.rm = TRUE)
  rd_mean_strat_unstrat <- mean(simulation_data$rd_strat_unstrat, na.rm = TRUE)
  rd_sd_strat_unstrat <- sd(simulation_data$rd_strat_unstrat, na.rm = TRUE)
  rd_mean_simple <- mean(simulation_data$rd_simple_design, na.rm = TRUE)
  rd_sd_simple <- sd(simulation_data$rd_simple_design, na.rm = TRUE)
  
  # 5. 响应率
  rr1_mean_strat <- mean(simulation_data$resp_rate_trt1_strat, na.rm = TRUE)
  rr2_mean_strat <- mean(simulation_data$resp_rate_trt2_strat, na.rm = TRUE)
  rr1_mean_strat_unstrat <- mean(simulation_data$resp_rate_trt1_strat_unstrat, na.rm = TRUE)
  rr2_mean_strat_unstrat <- mean(simulation_data$resp_rate_trt2_strat_unstrat, na.rm = TRUE)
  rr1_mean_simple <- mean(simulation_data$resp_rate_trt1_simple, na.rm = TRUE)
  rr2_mean_simple <- mean(simulation_data$resp_rate_trt2_simple, na.rm = TRUE)
  
  # 6. 对比指标
  power_diff <- power_strat_design - power_simple_design
  power_diff_pct <- sprintf("%+.1f%%", power_diff * 100)
  power_diff_unstrat <- power_strat_unstrat - power_simple_design
  power_diff_unstrat_pct <- sprintf("%+.1f%%", power_diff_unstrat * 100)
  
  # 7. SMD
  smd_mean_strat <- mean(simulation_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(simulation_data$smd_simple, na.rm = TRUE)
  smd_summary <- sprintf("%.3f / %.3f", smd_mean_strat, smd_mean_simple)
  prop_smd_gt_015_strat <- mean(simulation_data$smd_strat > 0.15, na.rm = TRUE)
  prop_smd_gt_015_simple <- mean(simulation_data$smd_simple > 0.15, na.rm = TRUE)
  smd_imbalance_prop <- sprintf("%.1f%% / %.1f%%", 
                                 prop_smd_gt_015_strat * 100,
                                 prop_smd_gt_015_simple * 100)
  
  # 组装结果
  result <- data.frame(
    Simulation_ID = param_id,
    TARGET_N = ifelse(is.null(TARGET_N), NA, TARGET_N),
    BLOCK_SIZE = ifelse(is.null(BLOCK_SIZE), NA, BLOCK_SIZE),
    STRATA_LEVELS = ifelse(is.null(STRATA_LEVELS), NA, STRATA_LEVELS),
    
    Arm_imbalance_strat = arm_imbalance_strat,
    Arm_imbalance_simple = arm_imbalance_simple,
    
    Power_strat_design = sprintf("%.3f", power_strat_design),
    Power_strat_unstrat = sprintf("%.3f", power_strat_unstrat),
    Power_simple_design = sprintf("%.3f", power_simple_design),
    Power_difference = power_diff_pct,
    Power_diff_strat_unstrat_vs_simple = power_diff_unstrat_pct,
    
    OR_mean_strat = sprintf("%.2f (%.2f)", or_mean_strat, or_sd_strat),
    OR_mean_strat_unstrat = sprintf("%.2f (%.2f)", or_mean_strat_unstrat, or_sd_strat_unstrat),
    OR_mean_simple = sprintf("%.2f (%.2f)", or_mean_simple, or_sd_simple),
    
    RD_mean_strat = sprintf("%.3f (%.3f)", rd_mean_strat, rd_sd_strat),
    RD_mean_strat_unstrat = sprintf("%.3f (%.3f)", rd_mean_strat_unstrat, rd_sd_strat_unstrat),
    RD_mean_simple = sprintf("%.3f (%.3f)", rd_mean_simple, rd_sd_simple),
    
    Resp_rate_trt1_strat = sprintf("%.1f%%", rr1_mean_strat * 100),
    Resp_rate_trt2_strat = sprintf("%.1f%%", rr2_mean_strat * 100),
    Resp_rate_trt1_strat_unstrat = sprintf("%.1f%%", rr1_mean_strat_unstrat * 100),
    Resp_rate_trt2_strat_unstrat = sprintf("%.1f%%", rr2_mean_strat_unstrat * 100),
    Resp_rate_trt1_simple = sprintf("%.1f%%", rr1_mean_simple * 100),
    Resp_rate_trt2_simple = sprintf("%.1f%%", rr2_mean_simple * 100),
    
    SMD_strat_vs_simple = smd_summary,
    Prop_SMD_gt_015 = smd_imbalance_prop,
    VRF_actual = sprintf("%.3f", mean(simulation_data$vrf_actual, na.rm = TRUE)),
    VRF_expected = sprintf("%.3f", mean(simulation_data$vrf_expected, na.rm = TRUE)),
    
    stringsAsFactors = FALSE
  )
  
  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(STRATA_PROPORTIONS)) result$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ":")
  if (!is.null(TREATMENT_RATIO)) result$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  if (!is.null(TREATMENT_RATIO_NAME)) result$TREATMENT_RATIO_NAME <- TREATMENT_RATIO_NAME
  if (!is.null(MEAN_SCENARIO)) result$MEAN_SCENARIO <- MEAN_SCENARIO
  if (!is.null(PROB_MATRIX_STR)) result$PROB_MATRIX_STR <- PROB_MATRIX_STR
  
  return(result)
}

# ------------------------------------------------------------------------------
# 8.5. 生成 Excel 输出（二分类版）
# ------------------------------------------------------------------------------
generate_excel_output <- function(summary_data, output_prefix = "simulation_binary_results") {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(output_prefix, "_", timestamp, ".xlsx")
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("          生成临床试验二分类模拟报告\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  wb <- openxlsx::createWorkbook()
  
  # 工作表1: Combined
  cat("[1/3] 创建工作表: Combined\n")
  
  desired_order <- c(
    "Simulation_ID", "Param_ID", "Batch_ID",
    "N_POOL", "TARGET_N", "BLOCK_SIZE",
    "STRATA_LEVELS", "STRATA_PROPORTIONS",
    "TREATMENT_RATIO", "TREATMENT_RATIO_NAME",
    "MEAN_SCENARIO", "PROB_MATRIX_STR",
    "Arm_imbalance_strat", "Power_strat_design", 
    "OR_mean_strat", "RD_mean_strat", "Resp_rate_trt1_strat", "Resp_rate_trt2_strat",
    "Power_strat_unstrat", "OR_mean_strat_unstrat", "RD_mean_strat_unstrat",
    "Resp_rate_trt1_strat_unstrat", "Resp_rate_trt2_strat_unstrat",
    "Arm_imbalance_simple", "Power_simple_design",
    "OR_mean_simple", "RD_mean_simple", "Resp_rate_trt1_simple", "Resp_rate_trt2_simple",
    "SMD_strat_vs_simple", "Prop_SMD_gt_015", "VRF_actual", "VRF_expected",
    "Power_difference", "Power_diff_strat_unstrat_vs_simple"
  )
  
  combined_cols <- intersect(desired_order, colnames(summary_data))
  combined_df <- summary_data[, combined_cols, drop = FALSE]
  
  openxlsx::addWorksheet(wb, "Combined")
  openxlsx::writeData(wb, "Combined", combined_df, startRow = 1, startCol = 1)
  
  header_style <- openxlsx::createStyle(
    fontColour = "white", fgFill = "#2E86C1", fontSize = 11, textDecoration = "bold"
  )
  param_style <- openxlsx::createStyle(fgFill = "#D6EAF8", textDecoration = "bold")
  strat_style <- openxlsx::createStyle(fgFill = "#D5F5E3", textDecoration = "bold")
  simple_style <- openxlsx::createStyle(fgFill = "#FCF3CF", textDecoration = "bold")
  diff_style <- openxlsx::createStyle(fgFill = "#F5B7B1", textDecoration = "bold")
  
  id_cols <- 3
  param_cols <- sum(grepl("^(N_|TARGET_|BLOCK_|STRATA_|TREATMENT_|MEAN_|PROB_)", combined_cols))
  strat_result_cols <- sum(grepl("_strat$|_strat_", combined_cols) & !grepl("_vs_", combined_cols))
  simple_result_cols <- sum(grepl("_simple$|_simple_", combined_cols) & !grepl("_vs_", combined_cols))
  diff_cols <- sum(grepl("^(Power_difference|Power_diff|SMD_strat_vs_simple)", combined_cols))
  
  openxlsx::addStyle(wb, "Combined", header_style, rows = 1, cols = 1:ncol(combined_df), gridExpand = TRUE)
  if (param_cols > 0) {
    openxlsx::addStyle(wb, "Combined", param_style, rows = 1, 
                       cols = (id_cols + 1):(id_cols + param_cols), gridExpand = TRUE)
  }
  strat_start <- id_cols + param_cols + 1
  strat_end <- strat_start + strat_result_cols - 1
  if (strat_result_cols > 0) {
    openxlsx::addStyle(wb, "Combined", strat_style, rows = 1, cols = strat_start:strat_end, gridExpand = TRUE)
  }
  simple_start <- strat_end + 1
  simple_end <- simple_start + simple_result_cols - 1
  if (simple_result_cols > 0) {
    openxlsx::addStyle(wb, "Combined", simple_style, rows = 1, cols = simple_start:simple_end, gridExpand = TRUE)
  }
  if (diff_cols > 0) {
    openxlsx::addStyle(wb, "Combined", diff_style, rows = 1, cols = (simple_end + 1):ncol(combined_df), gridExpand = TRUE)
  }
  
  openxlsx::setColWidths(wb, "Combined", cols = 1:ncol(combined_df), widths = "auto")
  openxlsx::freezePane(wb, "Combined", firstRow = TRUE, firstCol = TRUE)
  
  cat(sprintf("  ✓ Combined 工作表已创建 (%d 行 x %d 列)\n", nrow(combined_df), ncol(combined_df)))
  
  # 工作表2: Column_Description
  cat("[2/3] 创建工作表: Column_Description\n")
  
  col_descriptions <- data.frame(
    Column_Name = c(
      "Simulation_ID", "Param_ID", "Batch_ID",
      "N_POOL", "TARGET_N", "BLOCK_SIZE", "STRATA_LEVELS", "STRATA_PROPORTIONS",
      "TREATMENT_RATIO", "TREATMENT_RATIO_NAME", "MEAN_SCENARIO", "PROB_MATRIX_STR",
      "Arm_imbalance_strat", "Power_strat_design", 
      "OR_mean_strat", "RD_mean_strat", "Resp_rate_trt1_strat", "Resp_rate_trt2_strat",
      "Power_strat_unstrat", "OR_mean_strat_unstrat", "RD_mean_strat_unstrat",
      "Resp_rate_trt1_strat_unstrat", "Resp_rate_trt2_strat_unstrat",
      "Arm_imbalance_simple", "Power_simple_design",
      "OR_mean_simple", "RD_mean_simple", "Resp_rate_trt1_simple", "Resp_rate_trt2_simple",
      "SMD_strat_vs_simple", "Prop_SMD_gt_015", "VRF_actual", "VRF_expected",
      "Power_difference", "Power_diff_strat_unstrat_vs_simple"
    ),
    Description = c(
      "模拟组合编号", "参数组合ID", "批次ID",
      "初始受试者池大小", "目标入组样本量", "区组大小", "分层数量", "各层比例",
      "治疗组分配比例", "治疗组比例名称", "场景名称", "概率矩阵字符串",
      "分层设计组间不平衡度：中位数 (Q1-Q3)", 
      "分层随机化+CMH分析的把握度",
      "分层设计CMH的OR均值 (SD)", "分层设计CMH的RD均值 (SD)", 
      "分层设计CMH组1响应率", "分层设计CMH组2响应率",
      "分层随机化+未分层Fisher的把握度",
      "分层设计未分层Fisher的OR均值 (SD)", "分层设计未分层Fisher的RD均值 (SD)",
      "分层设计未分层组1响应率", "分层设计未分层组2响应率",
      "简单设计组间不平衡度：中位数 (Q1-Q3)",
      "简单随机化+Fisher的把握度",
      "简单设计的OR均值 (SD)", "简单设计的RD均值 (SD)",
      "简单设计组1响应率", "简单设计组2响应率",
      "SMD对比：分层设计 / 简单设计",
      "SMD>0.15的比例：分层 / 简单",
      "VRF实际值：分层Logistic Wald / 未分层Logistic Wald",
      "VRF预期值：基于PROB_MATRIX方差分解",
      "把握度差异（CMH - 简单）",
      "把握度差异（分层未分层 - 简单）"
    ),
    Example = c(
      "1", "1", "1,2,...,10,Overall",
      "1000", "182", "4", "2", "0.5,0.5",
      "1,1", "1:1", "Custom", "0.3,0.5;0.3,0.5",
      "0.0 (0.0-1.0)", "0.823", "2.15 (0.85)", "0.150 (0.052)", "30.1%", "45.2%",
      "0.812", "2.10 (0.90)", "0.145 (0.055)", "29.8%", "44.5%",
      "0.123 / 0.145", "12.3% / 15.6%", "1.234", "1.180",
      "2.0 (0.0-4.0)", "0.785", "2.05 (0.95)", "0.140 (0.058)", "30.5%", "44.0%",
      "0.021 / 0.152", "+3.8%", "+1.5%"
    ),
    stringsAsFactors = FALSE
  )
  
  col_descriptions <- col_descriptions[col_descriptions$Column_Name %in% colnames(combined_df), ]
  openxlsx::addWorksheet(wb, "Column_Description")
  openxlsx::writeData(wb, "Column_Description", col_descriptions, startRow = 1, startCol = 1)
  openxlsx::setColWidths(wb, "Column_Description", cols = 1:3, widths = c(30, 70, 20))
  openxlsx::addStyle(wb, "Column_Description", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  cat(sprintf("  ✓ Column_Description 工作表已创建 (%d 行)\n", nrow(col_descriptions)))
  
  # 工作表3: Summary_Overview
  if (nrow(summary_data) > 0) {
    cat("[3/3] 创建工作表: Summary_Overview\n")
    
    overview_data <- data.frame(
      Category = c(
        "总模拟组合数", "批次范围", "样本量设置", "区组大小", "层数", "治疗组比例",
        "分层设计平均Power(CMH)", "分层设计平均Power(未分层)", "简单设计平均Power",
        "平均Power差异(CMH-简单)", "平均Power差异(未分层-简单)"
      ),
      Values = c(
        as.character(nrow(summary_data)),
        paste(range(summary_data$Batch_ID), collapse = " - "),
        paste(unique(summary_data$TARGET_N), collapse = ", "),
        paste(unique(summary_data$BLOCK_SIZE), collapse = ", "),
        paste(unique(summary_data$STRATA_LEVELS), collapse = ", "),
        paste(unique(summary_data$TREATMENT_RATIO_NAME), collapse = ", "),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_design), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_unstrat), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_design) - 
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_unstrat) - 
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE))
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
  
  tryCatch({
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("✓ Excel报告已成功保存: %s\n", output_file))
    cat(strrep("=", 70), "\n\n", sep = "")
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
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  PROB_MATRIX = NULL,
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("开始模拟: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("参数: N_POOL=%d, TARGET_N=%d, BLOCK_SIZE=%d\n", N_POOL, TARGET_N, BLOCK_SIZE))
    cat(sprintf("      STRATA_LEVELS=%d\n", STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    if (!is.null(PROB_MATRIX)) {
      cat("      PROB_MATRIX:\n")
      print(PROB_MATRIX)
    }
  }
  
  batch_results <- lapply(1:n_batch, function(i) {
    if (verbose && n_batch > 1) cat(sprintf("\r完成批次 %d / %d", i, n_batch))
    run_batch_simulation(
      n_iter = n_iter, batch_id = i, N_POOL = N_POOL, TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
      PROB_MATRIX = PROB_MATRIX
    )
  })
  
  if (verbose && n_batch > 1) cat("\n")
  
  final_data <- do.call(rbind, batch_results)
  
  final_data$N_POOL <- N_POOL
  final_data$TARGET_N <- TARGET_N
  final_data$BLOCK_SIZE <- BLOCK_SIZE
  final_data$STRATA_LEVELS <- STRATA_LEVELS
  final_data$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ",")
  final_data$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  
  if (!is.null(PROB_MATRIX)) {
    final_data$PROB_MATRIX <- matrix_to_string(PROB_MATRIX)
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
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  PROB_MATRIX = NULL,
  IS_STRATIFIED = TRUE,
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("开始模拟: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("参数: N_POOL=%d, TARGET_N=%d, BLOCK_SIZE=%d\n", N_POOL, TARGET_N, BLOCK_SIZE))
    cat(sprintf("      STRATA_LEVELS=%d\n", STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    if (!is.null(PROB_MATRIX)) { cat("      PROB_MATRIX:\n"); print(PROB_MATRIX) }
  }
  
  batch_results_list <- list()
  batch_summaries_list <- list()
  
  for (batch_id in 1:n_batch) {
    if (verbose) cat(sprintf("\r处理批次 %d / %d", batch_id, n_batch))
    
    batch_result <- run_batch_simulation(
      n_iter = n_iter, batch_id = batch_id, N_POOL = N_POOL, TARGET_N = TARGET_N,
      BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
      PROB_MATRIX = PROB_MATRIX, IS_STRATIFIED = IS_STRATIFIED
    )
    
    batch_summary <- generate_summary_stats(
      simulation_data = batch_result, param_id = batch_id,
      N_POOL = N_POOL, TARGET_N = TARGET_N, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
      TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
      TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
      MEAN_SCENARIO = if (!is.null(PROB_MATRIX)) "Custom" else "Default",
      PROB_MATRIX_STR = if (!is.null(PROB_MATRIX)) matrix_to_string(PROB_MATRIX) else NA
    )
    
    batch_result$Batch_ID <- batch_id
    batch_summary$Batch_ID <- batch_id
    
    batch_results_list[[batch_id]] <- batch_result
    batch_summaries_list[[batch_id]] <- batch_summary
  }
  
  if (verbose) cat("\n")
  
  all_detailed_results <- do.call(rbind, batch_results_list)
  all_batch_summaries <- do.call(rbind, batch_summaries_list)
  
  overall_summary <- generate_summary_stats(
    simulation_data = all_detailed_results, param_id = "Overall",
    N_POOL = N_POOL, TARGET_N = TARGET_N, BLOCK_SIZE = BLOCK_SIZE,
    STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
    TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
    TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
    MEAN_SCENARIO = if (!is.null(PROB_MATRIX)) "Custom" else "Default",
    PROB_MATRIX_STR = if (!is.null(PROB_MATRIX)) matrix_to_string(PROB_MATRIX) else NA
  )
  overall_summary$Batch_ID <- "Overall"
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
  output_prefix = "simulation_binary_results"
) {
  results_list <- list()
  summary_list <- list()
  
  for (i in 1:nrow(param_grid)) {
    if (verbose) cat(sprintf("\n========== 测试参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    
    params <- param_grid[i, , drop = FALSE]
    
    # 解析概率矩阵
    prob_matrix <- NULL
    if ("PROB_MATRIX_STR" %in% colnames(params)) {
      prob_matrix <- string_to_matrix(
        as.character(params$PROB_MATRIX_STR), 
        n_strata = params$STRATA_LEVELS
      )
    }
    
    sim_result <- run_simulation(
      n_iter = n_iter, n_batch = n_batch,
      N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE, STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ","))),
      TREATMENT_RATIO = if ("TREATMENT_RATIO" %in% colnames(params)) {
        as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
      } else { c(1, 1) },
      PROB_MATRIX = prob_matrix, verbose = verbose
    )
    
    sim_result$Param_ID <- i
    results_list[[i]] <- sim_result
    
    summary_stats <- generate_summary_stats(
      simulation_data = sim_result, param_id = i,
      N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE, STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.character(params$STRATA_PROPORTIONS),
      TREATMENT_RATIO = as.character(params$TREATMENT_RATIO),
      TREATMENT_RATIO_NAME = as.character(params$TREATMENT_RATIO_NAME),
      MEAN_SCENARIO = as.character(params$MEAN_SCENARIO),
      PROB_MATRIX_STR = as.character(params$PROB_MATRIX_STR)
    )
    summary_stats$Param_ID <- i
    summary_list[[i]] <- summary_stats
  }
  
  final_results <- do.call(rbind, results_list)
  summary_results <- do.call(rbind, summary_list)
  
  summary_results <- merge(summary_results, 
                          param_grid[, c("Param_ID", names(param_grid))], 
                          by = "Param_ID", all.x = TRUE)
  
  if (verbose) cat("\n========== 所有参数组合测试完成 ==========\n")
  
  if (generate_excel) generate_excel_output(summary_results, output_prefix)
  
  return(list(detailed_results = final_results, summary_results = summary_results))
}

# ------------------------------------------------------------------------------
# 10.5. 批量测试函数（每个 batch 单独汇总）——支持 Excel 参数
# ------------------------------------------------------------------------------
run_parameter_grid_per_batch <- function(
  param_grid,
  n_iter = 1000,
  n_batch = 10,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "simulation_binary_results_per_batch"
) {
  all_batch_summaries_list <- list()
  current_row <- 1
  
  for (i in 1:nrow(param_grid)) {
    if (verbose) cat(sprintf("\n========== 测试参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    
    params <- param_grid[i, , drop = FALSE]
    
    is_stratified <- if ("IS_STRATIFIED" %in% colnames(params)) {
      as.logical(params$IS_STRATIFIED)
    } else { TRUE }
    
    # 解析概率矩阵
    prob_matrix <- NULL
    if ("PROB_MATRIX_STR" %in% colnames(params)) {
      prob_matrix <- string_to_matrix(
        as.character(params$PROB_MATRIX_STR), 
        n_strata = params$STRATA_LEVELS
      )
    }
    
    # 从 Excel 读取的 n_iter / n_batch 优先级高于函数参数
    iter <- if ("N_ITER" %in% colnames(params) && !is.na(params$N_ITER)) as.integer(params$N_ITER) else n_iter
    batch <- if ("N_BATCH" %in% colnames(params) && !is.na(params$N_BATCH)) as.integer(params$N_BATCH) else n_batch
    
    sim_results <- run_simulation_per_batch(
      n_iter = iter, n_batch = batch,
      N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE, STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ","))),
      TREATMENT_RATIO = if ("TREATMENT_RATIO" %in% colnames(params)) {
        as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
      } else { c(1, 1) },
      PROB_MATRIX = prob_matrix, IS_STRATIFIED = is_stratified,
      verbose = verbose && !verbose
    )
    
    batch_summaries <- sim_results$batch_summaries
    batch_summaries$Param_ID <- i
    
    param_cols_to_add <- setdiff(colnames(param_grid), "Param_ID")
    for (col in param_cols_to_add) {
      batch_summaries[[col]] <- params[[col]]
    }
    
    all_batch_summaries_list[[current_row]] <- batch_summaries
    current_row <- current_row + 1
  }
  
  all_batch_summaries <- do.call(rbind, all_batch_summaries_list)
  rownames(all_batch_summaries) <- NULL
  
  if (verbose) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("✓ 所有参数组合测试完成！\n"))
    cat(sprintf("  总参数组合数: %d\n", nrow(param_grid)))
    cat(sprintf("  每组合批次数: %d\n", n_batch))
    cat(sprintf("  总汇总行数: %d\n", nrow(all_batch_summaries)))
    cat(strrep("=", 70), "\n\n", sep = "")
  }
  
  if (generate_excel) {
    output_file <- generate_excel_output(
      summary_data = all_batch_summaries, output_prefix = output_prefix
    )
    if (verbose && !is.null(output_file)) {
      cat(sprintf("\n✓ Excel 报告已保存: %s\n", output_file))
    }
  }
  
  return(list(batch_summaries_all = all_batch_summaries, param_grid = param_grid))
}

# ==============================================================================
# 使用示例（Excel 参数输入）
# ==============================================================================
# 
# 1. 准备 Excel 参数表（例如 param_grid_binary.xlsx）：
#    必须列：TARGET_N, STRATA_LEVELS, STRATA_PROPORTIONS, TREATMENT_RATIO, PROB_MATRIX_STR
#    可选列：Param_ID, N_POOL, BLOCK_SIZE, TREATMENT_RATIO_NAME, MEAN_SCENARIO, N_ITER, N_BATCH
#
# 2. 在 R 中运行：
#    param_grid <- read_param_grid_from_excel("param_grid_binary.xlsx")
#    results <- run_parameter_grid_per_batch(
#      param_grid = param_grid,
#      n_iter = 1000,      # 可被 Excel 中 N_ITER 列覆盖
#      n_batch = 10,       # 可被 Excel 中 N_BATCH 列覆盖
#      verbose = TRUE,
#      generate_excel = TRUE,
#      output_prefix = "binary_simulation"
#    )
#
# PROB_MATRIX_STR 格式示例（2层，组1概率0.3，组2概率0.5）：
#    0.3,0.5;0.3,0.5
# ==============================================================================
