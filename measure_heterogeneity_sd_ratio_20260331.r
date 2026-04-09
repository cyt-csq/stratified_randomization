# ==============================================================================
# 从 Excel 读取参数并执行批量模拟（最终版 - 保留所有原始参数列）
# ==============================================================================

# 0. 加载必要的库
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("pbapply", quietly = TRUE)) install.packages("pbapply")

library(openxlsx)
library(pbapply)
library(dplyr)

# ==============================================================================
# 1. 读取 Excel 参数文件（保留所有原始列）
# ==============================================================================
read_simulation_params <- function(file_path) {
  # 读取 Excel 文件
  params <- read.xlsx(file_path, sheet = 1)
  
  # 数据清洗和转换
  params <- params %>%
    dplyr::mutate(
      # 解析治疗组比例（支持中文和英文冒号）
      TREATMENT_RATIO = sapply(tretment_ratio, function(x) {
        x_clean <- gsub(":", ":", as.character(x))
        ratio_vals <- as.numeric(unlist(strsplit(x_clean, ":")))
        paste(ratio_vals, collapse = ",")
      }),
      
      # 解析层权重
      STRATA_PROPORTIONS = weights,
      
      # 解析均值矩阵字符串
      MEAN_MATRIX_STR = mean_matrix_str,
      
      # 计算层数
      STRATA_LEVELS = n_layers,
      
      # 样本量
      TARGET_N = sample_size,
      
      # 区组大小
      BLOCK_SIZE = block_size,
      
      # 使用 sd_common 列
      SD_COMMON = sd_common,
      
      # 配置 ID 作为场景名称
      MEAN_SCENARIO = config_id,
      
      # 治疗组比例名称
      TREATMENT_RATIO_NAME = tretment_ratio
    )
  
  # 添加 Param_ID
  params$Param_ID <- 1:nrow(params)
  
  return(params)
}

# ==============================================================================
# 2-4. 辅助函数（保持不变）
# ==============================================================================
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

generate_rand_list <- function(n_per_strata, block_size = 4, ratio = c(1, 1)) {
  blocks <- generate_all_blocks(block_size, ratio)
  n_blocks_needed <- ceiling(n_per_strata / block_size)
  selected_indices <- sample(length(blocks), n_blocks_needed, replace = TRUE)
  selected_blocks <- blocks[selected_indices]
  rand_vec <- unlist(selected_blocks)
  return(rand_vec[1:n_per_strata])
}

generate_stratified_rand_lists <- function(n_strata, n_per_strata, block_size = 4, ratio = c(1, 1)) {
  rand_lists <- vector("list", n_strata)
  for (s in 1:n_strata) {
    rand_lists[[s]] <- generate_rand_list(n_per_strata, block_size, ratio)
  }
  return(rand_lists)
}

calculate_strata_smd <- function(enrolled_trt, enrolled_strata) {
  n1 <- sum(enrolled_trt == 1)
  n2 <- sum(enrolled_trt == 2)
  
  if (n1 == 0 || n2 == 0) {
    return(list(smd_per_strata = NA, mean_smd = NA, max_smd = NA, median_smd = NA))
  }
  
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
  
  return(list(
    smd_per_strata = smd_values,
    mean_smd = mean(smd_values, na.rm = TRUE),
    max_smd = max(smd_values, na.rm = TRUE),
    median_smd = median(smd_values, na.rm = TRUE)
  ))
}

matrix_to_string <- function(mat) {
  if (is.null(mat)) return(NA)
  paste(apply(mat, 1, function(row) paste(row, collapse = ",")), collapse = ";")
}

string_to_matrix <- function(str, n_strata) {
  if (is.na(str) || str == "") return(NULL)
  rows <- strsplit(str, ";")[[1]]
  mat <- matrix(as.numeric(unlist(strsplit(rows, ","))), 
                nrow = n_strata, 
                ncol = 2, 
                byrow = TRUE)
  return(mat)
}

# ==============================================================================
# 5. 核心模拟函数
# ==============================================================================
run_single_trial <- function(
  N_POOL = 1000,
  TARGET_N = 182,
  BLOCK_SIZE = 4,
  SD_COMMON = 15,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEAN_MATRIX = NULL
) {
  # 参数验证
  if (abs(sum(STRATA_PROPORTIONS) - 1) > 1e-6) stop("STRATA_PROPORTIONS 的和必须等于 1")
  if (length(STRATA_PROPORTIONS) != STRATA_LEVELS) stop("STRATA_PROPORTIONS 的长度必须等于 STRATA_LEVELS")
  
  if (is.null(MEAN_MATRIX)) {
    MEAN_MATRIX <- matrix(c(10, 15), nrow = STRATA_LEVELS, ncol = 2, byrow = TRUE)
  }
  if (nrow(MEAN_MATRIX) != STRATA_LEVELS) stop("MEAN_MATRIX 行数不匹配")
  
  # 随机列表大小计算
  RAND_LIST_SIZE_PER_STRATA <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  RAND_LIST_SIZE_TOTAL <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  
  # Step 1: 生成入组人群
  pool_strata <- sample(1:STRATA_LEVELS, size = N_POOL, replace = TRUE, prob = STRATA_PROPORTIONS)
  
  # Step 2: 生成两种随机表
  rand_lists_strat <- generate_stratified_rand_lists(STRATA_LEVELS, RAND_LIST_SIZE_PER_STRATA, BLOCK_SIZE, TREATMENT_RATIO)
  ptr_list_strat <- rep(0, STRATA_LEVELS)
  
  rand_list_unstrat <- generate_rand_list(RAND_LIST_SIZE_TOTAL, BLOCK_SIZE, TREATMENT_RATIO)
  ptr_global_unstrat <- 0
  
  # Step 3: 模拟入组
  enrolled_strata <- numeric(TARGET_N)
  enrolled_trt_strat <- numeric(TARGET_N)
  enrolled_trt_unstrat <- numeric(TARGET_N)
  total_enrolled <- 0
  
  for (i in 1:N_POOL) {
    if (total_enrolled >= TARGET_N) break
    s <- pool_strata[i]
    enrolled_strata[total_enrolled + 1] <- s
    
    # 分层随机化
    if (ptr_list_strat[s] < length(rand_lists_strat[[s]])) {
      ptr_list_strat[s] <- ptr_list_strat[s] + 1
      enrolled_trt_strat[total_enrolled + 1] <- rand_lists_strat[[s]][ptr_list_strat[s]]
    }
    
    # 非分层随机化
    if (ptr_global_unstrat < length(rand_list_unstrat)) {
      ptr_global_unstrat <- ptr_global_unstrat + 1
      enrolled_trt_unstrat[total_enrolled + 1] <- rand_list_unstrat[ptr_global_unstrat]
    }
    
    total_enrolled <- total_enrolled + 1
  }
  
  enrolled_strata <- enrolled_strata[1:total_enrolled]
  enrolled_trt_strat <- enrolled_trt_strat[1:total_enrolled]
  enrolled_trt_unstrat <- enrolled_trt_unstrat[1:total_enrolled]
  
  # Step 4: 随机化平衡性检查
  strata_balance_smd_strat <- calculate_strata_smd(enrolled_trt_strat, enrolled_strata)
  strata_balance_smd_unstrat <- calculate_strata_smd(enrolled_trt_unstrat, enrolled_strata)
  
  # Step 5: 模拟临床结果（两套独立结局）
  outcomes_strat <- numeric(total_enrolled)
  outcomes_unstrat <- numeric(total_enrolled)
  
  for (i in 1:total_enrolled) {
    mu_strat <- MEAN_MATRIX[enrolled_strata[i], enrolled_trt_strat[i]]
    outcomes_strat[i] <- rnorm(1, mean = mu_strat, sd = SD_COMMON)
    
    mu_unstrat <- MEAN_MATRIX[enrolled_strata[i], enrolled_trt_unstrat[i]]
    outcomes_unstrat[i] <- rnorm(1, mean = mu_unstrat, sd = SD_COMMON)
  }
  
  # Step 6: 统计分析
  n1_unstrat <- sum(enrolled_trt_unstrat == 1)
  n2_unstrat <- sum(enrolled_trt_unstrat == 2)
  
  outcomes_trt1_unstrat <- outcomes_unstrat[enrolled_trt_unstrat == 1]
  outcomes_trt2_unstrat <- outcomes_unstrat[enrolled_trt_unstrat == 2]
  
  mean1_unstrat <- mean(outcomes_trt1_unstrat)
  mean2_unstrat <- mean(outcomes_trt2_unstrat)
  md_unstrat <- mean2_unstrat - mean1_unstrat
  
  sd1_unstrat <- sd(outcomes_trt1_unstrat)
  sd2_unstrat <- sd(outcomes_trt2_unstrat)
  
  if (n1_unstrat > 1 & n2_unstrat > 1) {
    pooled_sd_unstrat <- sqrt(((n1_unstrat - 1) * sd1_unstrat^2 + (n2_unstrat - 1) * sd2_unstrat^2) / (n1_unstrat + n2_unstrat - 2))
    se_unstrat <- pooled_sd_unstrat * sqrt(1/n1_unstrat + 1/n2_unstrat)
    var_unstrat <- se_unstrat^2
    z_unstrat <- md_unstrat / se_unstrat
    p_unstrat <- 2 * pnorm(-abs(z_unstrat))
  } else {
    pooled_sd_unstrat <- NA
    se_unstrat <- NA
    var_unstrat <- NA
    p_unstrat <- NA
  }
  
  # 分层分析
  n1_strat <- sum(enrolled_trt_strat == 1)
  n2_strat <- sum(enrolled_trt_strat == 2)
  
  outcomes_trt1_strat <- outcomes_strat[enrolled_trt_strat == 1]
  outcomes_trt2_strat <- outcomes_strat[enrolled_trt_strat == 2]
  
  stratum_stats <- data.frame(
    k = 1:STRATA_LEVELS,
    n_trt1 = numeric(STRATA_LEVELS),
    n_trt2 = numeric(STRATA_LEVELS),
    mean_trt1 = numeric(STRATA_LEVELS),
    mean_trt2 = numeric(STRATA_LEVELS),
    var_trt1 = numeric(STRATA_LEVELS),
    var_trt2 = numeric(STRATA_LEVELS)
  )
  
  for (k in 1:STRATA_LEVELS) {
    idx_k <- which(enrolled_strata == k)
    if (length(idx_k) > 0) {
      trt_k <- enrolled_trt_strat[idx_k]
      idx_trt1 <- idx_k[trt_k == 1]
      idx_trt2 <- idx_k[trt_k == 2]
      
      stratum_stats$n_trt1[k] <- length(idx_trt1)
      stratum_stats$n_trt2[k] <- length(idx_trt2)
      
      if (length(idx_trt1) >= 2) {
        stratum_stats$mean_trt1[k] <- mean(outcomes_strat[idx_trt1])
        stratum_stats$var_trt1[k] <- var(outcomes_strat[idx_trt1])
      } else {
        stratum_stats$mean_trt1[k] <- NA
        stratum_stats$var_trt1[k] <- NA
      }
      
      if (length(idx_trt2) >= 2) {
        stratum_stats$mean_trt2[k] <- mean(outcomes_strat[idx_trt2])
        stratum_stats$var_trt2[k] <- var(outcomes_strat[idx_trt2])
      } else {
        stratum_stats$mean_trt2[k] <- NA
        stratum_stats$var_trt2[k] <- NA
      }
    }
  }
  
  valid_strata <- !is.na(stratum_stats$var_trt1) & !is.na(stratum_stats$var_trt2) &
                  stratum_stats$n_trt1 > 1 & stratum_stats$n_trt2 > 1
  
  if (sum(valid_strata) > 0) {
    weights <- numeric(STRATA_LEVELS)
    diffs <- numeric(STRATA_LEVELS)
    
    for (k in 1:STRATA_LEVELS) {
      if (valid_strata[k]) {
        var_diff <- stratum_stats$var_trt1[k] / stratum_stats$n_trt1[k] + 
                    stratum_stats$var_trt2[k] / stratum_stats$n_trt2[k]
        if (var_diff > 0) {
          weights[k] <- 1 / var_diff
          diffs[k] <- stratum_stats$mean_trt2[k] - stratum_stats$mean_trt1[k]
        }
      }
    }
    
    if (sum(weights) > 0) {
      md_strat <- sum(weights * diffs, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    } else {
      md_strat <- NA
    }
  } else {
    md_strat <- NA
  }
  
  # 分层 Z 检验
  p_strat <- NA
  se_strat <- NA
  var_strat <- NA
  
  if (n1_strat > 1 & n2_strat > 1) {
    z_strat_num <- 0
    z_strat_denom <- 0
    for (k in 1:STRATA_LEVELS) {
      idx_k <- which(enrolled_strata == k)
      if (length(idx_k) > 0) {
        trt_k <- enrolled_trt_strat[idx_k]
        out_k <- outcomes_strat[idx_k]
        n1k <- sum(trt_k == 1)
        n2k <- sum(trt_k == 2)
        if (n1k > 1 & n2k > 1) {
          out1k <- out_k[trt_k == 1]
          out2k <- out_k[trt_k == 2]
          d_k <- mean(out2k) - mean(out1k)
          sd1k <- sd(out1k)
          sd2k <- sd(out2k)
          pooled_sd_k <- sqrt(((n1k - 1) * sd1k^2 + (n2k - 1) * sd2k^2) / (n1k + n2k - 2))
          se_k <- pooled_sd_k * sqrt(1/n1k + 1/n2k)
          w_k <- 1 / (se_k^2)
          z_strat_num <- z_strat_num + w_k * d_k
          z_strat_denom <- z_strat_denom + w_k
        }
      }
    }
    if (z_strat_denom > 0) {
      var_strat <- 1 / z_strat_denom
      se_strat <- sqrt(var_strat)
      z_stat_strat <- md_strat / se_strat
      p_strat <- 2 * pnorm(-abs(z_stat_strat))
    }
  }
  
  # 计算 R²
  r_squared <- NA
  tryCatch({
    df_model <- data.frame(
      outcome = outcomes_strat,
      treatment = factor(enrolled_trt_strat),
      strata = factor(enrolled_strata)
    )
    model <- lm(outcome ~ treatment + strata, data = df_model)
    r_squared <- summary(model)$r.squared
  }, error = function(e) {
    r_squared <<- NA
  })
  
  # 变异度分解
  var_between_trt1 <- NA
  var_between_trt2 <- NA
  var_within_trt1 <- NA
  var_within_trt2 <- NA
  
  valid_trt1 <- valid_strata & !is.na(stratum_stats$mean_trt1) & stratum_stats$n_trt1 > 0
  valid_trt2 <- valid_strata & !is.na(stratum_stats$mean_trt2) & stratum_stats$n_trt2 > 0
  
  if (sum(valid_trt1) >= 2) {
    overall_mean_trt1 <- weighted.mean(stratum_stats$mean_trt1[valid_trt1], stratum_stats$n_trt1[valid_trt1])
    var_between_trt1 <- weighted.mean((stratum_stats$mean_trt1[valid_trt1] - overall_mean_trt1)^2, stratum_stats$n_trt1[valid_trt1])
  }
  
  if (sum(valid_trt2) >= 2) {
    overall_mean_trt2 <- weighted.mean(stratum_stats$mean_trt2[valid_trt2], stratum_stats$n_trt2[valid_trt2])
    var_between_trt2 <- weighted.mean((stratum_stats$mean_trt2[valid_trt2] - overall_mean_trt2)^2, stratum_stats$n_trt2[valid_trt2])
  }
  
  valid_var_trt1 <- valid_strata & !is.na(stratum_stats$var_trt1)
  valid_var_trt2 <- valid_strata & !is.na(stratum_stats$var_trt2)
  
  if (sum(valid_var_trt1) >= 1 && sum(stratum_stats$n_trt1[valid_var_trt1] - 1) > 0) {
    var_within_trt1 <- sum((stratum_stats$n_trt1[valid_var_trt1] - 1) * stratum_stats$var_trt1[valid_var_trt1]) / 
                       sum(stratum_stats$n_trt1[valid_var_trt1] - 1)
  }
  
  if (sum(valid_var_trt2) >= 1 && sum(stratum_stats$n_trt2[valid_var_trt2] - 1) > 0) {
    var_within_trt2 <- sum((stratum_stats$n_trt2[valid_var_trt2] - 1) * stratum_stats$var_trt2[valid_var_trt2]) / 
                       sum(stratum_stats$n_trt2[valid_var_trt2] - 1)
  }
  
  # 返回结果
  return(list(
    summary = data.frame(
      n1_strat = n1_strat,
      n2_strat = n2_strat,
      n1_unstrat = n1_unstrat,
      n2_unstrat = n2_unstrat,
      mean_diff_unstrat = md_unstrat,
      pooled_sd_unstrat = pooled_sd_unstrat,
      se_unstrat = se_unstrat,
      var_unstrat = var_unstrat,
      p_unstrat = p_unstrat,
      mean_diff_strat = md_strat,
      se_strat = se_strat,
      var_strat = var_strat,
      p_strat = p_strat,
      between_sd_trt1 = if (!is.na(var_between_trt1)) sqrt(var_between_trt1) else NA,
      between_sd_trt2 = if (!is.na(var_between_trt2)) sqrt(var_between_trt2) else NA,
      within_pooled_sd_trt1 = if (!is.na(var_within_trt1)) sqrt(var_within_trt1) else NA,
      within_pooled_sd_trt2 = if (!is.na(var_within_trt2)) sqrt(var_within_trt2) else NA,
      strata_balance_smd_mean_strat = strata_balance_smd_strat$mean_smd,
      strata_balance_smd_max_strat = strata_balance_smd_strat$max_smd,
      strata_balance_smd_mean_unstrat = strata_balance_smd_unstrat$mean_smd,
      strata_balance_smd_max_unstrat = strata_balance_smd_unstrat$max_smd,
      r_squared = r_squared,
      stringsAsFactors = FALSE
    )
  ))
}

# ==============================================================================
# 6. 批量模拟（保留所有原始参数列）
# ==============================================================================
run_batch_from_excel <- function(
  param_grid,
  n_iter = 1000,
  n_batch = 10,
  output_prefix = "simulation_results"
) {
  # ===== 自动生成带时间戳和随机数的唯一文件名 =====
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- sample(1000:9999, 1)
  output_file <- sprintf("%s_%s_%d.xlsx", output_prefix, timestamp, random_suffix)
  # ===============================================
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("开始批量模拟\n")
  cat(sprintf("参数组合数：%d\n", nrow(param_grid)))
  cat(sprintf("每次模拟迭代数：%d\n", n_iter))
  cat(sprintf("批次数：%d\n", n_batch))
  cat(sprintf("输出文件：%s\n", output_file))
  cat(strrep("=", 70), "\n\n", sep = "")
  
  all_results <- list()
  row_counter <- 1
  
  for (i in 1:nrow(param_grid)) {
    cat(sprintf("\n========== 处理参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    
    params <- param_grid[i, ]
    
    # 解析参数
    mean_matrix <- string_to_matrix(as.character(params$mean_matrix_str), 
                                    n_strata = params$n_layers)
    
    strata_props <- as.numeric(unlist(strsplit(as.character(params$weights), ",")))
    treat_ratio <- as.numeric(unlist(strsplit(as.character(params$tretment_ratio), ":")))
    treat_ratio <- as.numeric(unlist(strsplit(gsub(":", ":", as.character(params$tretment_ratio)), ":")))
    
    # 存储所有 batch 的原始数据（用于 overall 汇总）
    all_batch_data <- list()
    
    # 运行多个 batch
    for (b in 1:n_batch) {
      cat(sprintf("\r  处理批次 %d / %d", b, n_batch))
      
      # 每个 batch 使用不同的种子
      set.seed(123 + i * 1000 + b * 100)
      
      batch_results <- replicate(n_iter, {
        run_single_trial(
          N_POOL = 1000,
          TARGET_N = params$sample_size,
          BLOCK_SIZE = params$block_size,
          SD_COMMON = params$sd_common,
          STRATA_LEVELS = params$n_layers,
          STRATA_PROPORTIONS = strata_props,
          TREATMENT_RATIO = treat_ratio,
          MEAN_MATRIX = mean_matrix
        )
      }, simplify = FALSE)
      
      # 汇总统计
      summaries <- lapply(batch_results, function(x) x$summary)
      sim_data <- do.call(rbind, summaries)
      
      # 存储原始数据（用于 overall）
      all_batch_data[[b]] <- sim_data
      
      # 计算汇总指标
      power_unstrat <- mean(sim_data$p_unstrat < 0.05, na.rm = TRUE)
      power_strat <- mean(sim_data$p_strat < 0.05, na.rm = TRUE)
      
      # 格式化函数
      format_stats <- function(vec) {
        m <- mean(vec, na.rm = TRUE)
        s <- sd(vec, na.rm = TRUE)
        med <- median(vec, na.rm = TRUE)
        q1 <- quantile(vec, 0.25, na.rm = TRUE)
        q3 <- quantile(vec, 0.75, na.rm = TRUE)
        sprintf("%.3f (%.3f) / %.3f (%.3f-%.3f)", m, s, med, q1, q3)
      }
      
      # VRF 计算
      vrf <- sim_data$var_unstrat / sim_data$var_strat
      vrf_str <- format_stats(vrf)
      
      sample_size_gain <- (vrf - 1) * 100
      ss_gain_str <- format_stats(sample_size_gain)
      
      # R²
      r_sq_str <- format_stats(sim_data$r_squared)
      
      # 平衡性 SMD
      balance_smd_strat <- sprintf("%.3f / %.3f", 
                                    mean(sim_data$strata_balance_smd_mean_strat, na.rm = TRUE),
                                    mean(sim_data$strata_balance_smd_max_strat, na.rm = TRUE))
      balance_smd_unstrat <- sprintf("%.3f / %.3f", 
                                      mean(sim_data$strata_balance_smd_mean_unstrat, na.rm = TRUE),
                                      mean(sim_data$strata_balance_smd_max_unstrat, na.rm = TRUE))
      
      prop_imb_strat <- sprintf("%.1f%%", mean(sim_data$strata_balance_smd_max_strat > 0.15, na.rm = TRUE) * 100)
      prop_imb_unstrat <- sprintf("%.1f%%", mean(sim_data$strata_balance_smd_max_unstrat > 0.15, na.rm = TRUE) * 100)
      
      # 点估计
      pe_unstrat_str <- format_stats(sim_data$mean_diff_unstrat)
      pe_strat_str <- format_stats(sim_data$mean_diff_strat)
      
      # 变异度分解
      n1 <- sim_data$n1_strat
      n2 <- sim_data$n2_strat
      
      var_between_trt1 <- sim_data$between_sd_trt1^2
      var_between_trt2 <- sim_data$between_sd_trt2^2
      var_within_trt1 <- sim_data$within_pooled_sd_trt1^2
      var_within_trt2 <- sim_data$within_pooled_sd_trt2^2
      
      var_between_weighted <- ((n1 - 1) * var_between_trt1 + (n2 - 1) * var_between_trt2) / (n1 + n2 - 2)
      var_within_weighted <- ((n1 - 1) * var_within_trt1 + (n2 - 1) * var_within_trt2) / (n1 + n2 - 2)
      
      between_sd_str <- format_stats(sqrt(var_between_weighted))
      within_sd_str <- format_stats(sqrt(var_within_weighted))
      sd_ratio_str <- format_stats(sqrt(var_between_weighted) / sqrt(var_within_weighted))
      
      # 存储结果（每个 batch 一行）- 保留所有原始 Excel 列
      result_row <- data.frame(
        # 原始参数列（从 Excel 保留）
        Param_ID = i,
        sample_size = params$sample_size,
        tretment_ratio = params$tretment_ratio,
        block_size = params$block_size,
        config_id = params$config_id,
        n_layers = params$n_layers,
        weights = params$weights,
        ratio = params$ratio,
        target_between_sd = params$target_between_sd,
        effect_size = params$effect_size,
        sd_common = params$sd_common,
        mean_matrix_str = params$mean_matrix_str,
        
        # 批次 ID
        Batch_ID = b,
        
        # 模拟结果
        Power_Unstrat = power_unstrat,
        Power_Strat = power_strat,
        Delta_Power = power_strat - power_unstrat,
        PE_Unstrat = pe_unstrat_str,
        PE_Strat = pe_strat_str,
        Strata_Balance_SMD_Unstrat = balance_smd_unstrat,
        Strata_Balance_SMD_Strat = balance_smd_strat,
        Prop_Imbalance_Unstrat = prop_imb_unstrat,
        Prop_Imbalance_Strat = prop_imb_strat,
        VRF = vrf_str,
        Sample_Size_Gain = ss_gain_str,
        Between_SD = between_sd_str,
        Within_SD = within_sd_str,
        SD_Ratio = sd_ratio_str,
        R_Squared = r_sq_str,
        stringsAsFactors = FALSE
      )
      
      all_results[[row_counter]] <- result_row
      row_counter <- row_counter + 1
    }
    
    # ========== 添加 Overall 汇总行（所有 batch 合并）==========
    cat(sprintf("\n  生成 Overall 汇总行..."))
    
    # 合并所有 batch 的原始数据
    overall_sim_data <- do.call(rbind, all_batch_data)
    
    # 计算 overall 汇总指标
    power_unstrat_overall <- mean(overall_sim_data$p_unstrat < 0.05, na.rm = TRUE)
    power_strat_overall <- mean(overall_sim_data$p_strat < 0.05, na.rm = TRUE)
    
    # VRF 计算
    vrf_overall <- overall_sim_data$var_unstrat / overall_sim_data$var_strat
    vrf_overall_str <- format_stats(vrf_overall)
    
    sample_size_gain_overall <- (vrf_overall - 1) * 100
    ss_gain_overall_str <- format_stats(sample_size_gain_overall)
    
    # R²
    r_sq_overall_str <- format_stats(overall_sim_data$r_squared)
    
    # 平衡性 SMD
    balance_smd_strat_overall <- sprintf("%.3f / %.3f", 
                                          mean(overall_sim_data$strata_balance_smd_mean_strat, na.rm = TRUE),
                                          mean(overall_sim_data$strata_balance_smd_max_strat, na.rm = TRUE))
    balance_smd_unstrat_overall <- sprintf("%.3f / %.3f", 
                                            mean(overall_sim_data$strata_balance_smd_mean_unstrat, na.rm = TRUE),
                                            mean(overall_sim_data$strata_balance_smd_max_unstrat, na.rm = TRUE))
    
    prop_imb_strat_overall <- sprintf("%.1f%%", mean(overall_sim_data$strata_balance_smd_max_strat > 0.15, na.rm = TRUE) * 100)
    prop_imb_unstrat_overall <- sprintf("%.1f%%", mean(overall_sim_data$strata_balance_smd_max_unstrat > 0.15, na.rm = TRUE) * 100)
    
    # 点估计
    pe_unstrat_overall_str <- format_stats(overall_sim_data$mean_diff_unstrat)
    pe_strat_overall_str <- format_stats(overall_sim_data$mean_diff_strat)
    
    # 变异度分解
    n1_overall <- overall_sim_data$n1_strat
    n2_overall <- overall_sim_data$n2_strat
    
    var_between_trt1_overall <- overall_sim_data$between_sd_trt1^2
    var_between_trt2_overall <- overall_sim_data$between_sd_trt2^2
    var_within_trt1_overall <- overall_sim_data$within_pooled_sd_trt1^2
    var_within_trt2_overall <- overall_sim_data$within_pooled_sd_trt2^2
    
    var_between_weighted_overall <- ((n1_overall - 1) * var_between_trt1_overall + (n2_overall - 1) * var_between_trt2_overall) / (n1_overall + n2_overall - 2)
    var_within_weighted_overall <- ((n1_overall - 1) * var_within_trt1_overall + (n2_overall - 1) * var_within_trt2_overall) / (n1_overall + n2_overall - 2)
    
    between_sd_overall_str <- format_stats(sqrt(var_between_weighted_overall))
    within_sd_overall_str <- format_stats(sqrt(var_within_weighted_overall))
    sd_ratio_overall_str <- format_stats(sqrt(var_between_weighted_overall) / sqrt(var_within_weighted_overall))
    
    # 存储 Overall 结果行
    overall_row <- data.frame(
      # 原始参数列
      Param_ID = i,
      sample_size = params$sample_size,
      tretment_ratio = params$tretment_ratio,
      block_size = params$block_size,
      config_id = params$config_id,
      n_layers = params$n_layers,
      weights = params$weights,
      ratio = params$ratio,
      target_between_sd = params$target_between_sd,
      effect_size = params$effect_size,
      sd_common = params$sd_common,
      mean_matrix_str = params$mean_matrix_str,
      
      # 批次 ID
      Batch_ID = "Overall",
      
      # 模拟结果
      Power_Unstrat = power_unstrat_overall,
      Power_Strat = power_strat_overall,
      Delta_Power = power_strat_overall - power_unstrat_overall,
      PE_Unstrat = pe_unstrat_overall_str,
      PE_Strat = pe_strat_overall_str,
      Strata_Balance_SMD_Unstrat = balance_smd_unstrat_overall,
      Strata_Balance_SMD_Strat = balance_smd_strat_overall,
      Prop_Imbalance_Unstrat = prop_imb_unstrat_overall,
      Prop_Imbalance_Strat = prop_imb_strat_overall,
      VRF = vrf_overall_str,
      Sample_Size_Gain = ss_gain_overall_str,
      Between_SD = between_sd_overall_str,
      Within_SD = within_sd_overall_str,
      SD_Ratio = sd_ratio_overall_str,
      R_Squared = r_sq_overall_str,
      stringsAsFactors = FALSE
    )
    
    all_results[[row_counter]] <- overall_row
    row_counter <- row_counter + 1
    
    cat(sprintf("\n  ✓ 完成：Power(Strat)=%.3f, VRF=%.3f\n", 
                power_strat_overall, mean(vrf_overall, na.rm = TRUE)))
  }
  
  # 合并所有结果
  final_results <- do.call(rbind, all_results)
  
  # 保存 Excel
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("保存结果...\n")
  
  wb <- createWorkbook()
  addWorksheet(wb, "Results")
  writeData(wb, "Results", final_results)
  
  # 添加描述
  addWorksheet(wb, "Description")
  desc <- data.frame(
    Column = colnames(final_results),
    Description = c(
      "参数组合 ID", "样本量", "治疗组比例", "区组大小", "配置名称", "分层数", "分层比例",
      "ratio", "目标层间 SD", "效应量", "SD_Common", "均值矩阵字符串",
      "批次 ID",
      "非分层分析把握度", "分层分析把握度", "功效增益",
      "非分层点估计", "分层点估计",
      "非分层平衡性 SMD", "分层平衡性 SMD",
      "非分层不平衡比例", "分层不平衡比例",
      "方差减少因子", "等效样本量增益 (%)",
      "层间 SD", "层内 SD", "层间/层内比",
      "模型 R²"
    )
  )
  writeData(wb, "Description", desc)
  
  setColWidths(wb, "Results", cols = 1:ncol(final_results), widths = "auto")
  setColWidths(wb, "Description", cols = 1:2, widths = c(25, 50))
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat(sprintf("✓ 结果已保存至：%s\n", output_file))
  cat(sprintf("✓ 总行数：%d (参数组合 %d × (批次 %d + Overall 1))\n", 
              nrow(final_results), nrow(param_grid), n_batch))
  cat(strrep("=", 70), "\n\n", sep = "")
  
  # 打印预览（包含 Overall 行）
  cat("========== 结果预览（前 22 行，含 Overall） ==========\n")
  print(head(final_results[, c("Param_ID", "Batch_ID", "config_id", "sd_common", 
                                "Power_Unstrat", "Power_Strat", "Delta_Power", "VRF")], 22))
  
  # 返回结果和文件名
  return(list(
    results = final_results,
    output_file = output_file
  ))
}

# ==============================================================================
# 7. 主程序
# ==============================================================================
# 读取 Excel 参数
param_grid <- read_simulation_params("C:/Yuting/stratified randomization/simulation plan/sd_ratio_setting_ss.xlsx")

cat("\n========== 参数概览 ==========\n")
print(summary(param_grid))
cat(sprintf("\n总参数组合数：%d\n", nrow(param_grid)))

# 运行批量模拟
results <- run_batch_from_excel(
  param_grid = param_grid,
  n_iter = 10,
  n_batch = 5
)

cat("\n========== 模拟完成！ ==========\n")