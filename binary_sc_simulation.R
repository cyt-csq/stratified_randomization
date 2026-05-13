# ==============================================================================
# 分层随机化 vs 完全随机化模拟（二分类结局，含 Latent Variable）
# ==============================================================================
# 说明：
#   1. 基于 sc_binary_template.r 的交叉层概率结构
#   2. Pool 中每个人独立赋予 分层因素(Actual) 和 Latent因素 的值
#   3. 结局概率由 (Actual, Latent) 交叉层决定（cross_detail）
#   4. 分层随机化：仅按 Actual 因素分层随机；分析时 CMH 也仅按 Actual 分层
#   5. 完全随机化：不分层随机；分析时 Fisher Exact Test
#   6. 同时输出：分层随机化+未分层 Fisher，用于对比
# ==============================================================================

library(openxlsx)
library(pbapply)

# ==============================================================================
# 1. 辅助函数（从 sc_binary_template.r 复用）
# ==============================================================================

parse_prop_vec <- function(str) {
  if (is.na(str) || str == "") return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(as.character(str), ",")[[1]]))
  if (any(is.na(v))) stop(sprintf("无法解析比例字符串: %s", str))
  return(v)
}

prob_vec_to_str <- function(ctrl_vec, trt_vec, digits = 4) {
  paste(
    paste(round(ctrl_vec, digits), round(trt_vec, digits), sep = ","),
    collapse = ";"
  )
}

#' 生成交叉层参数（含校验）
gen_cross_params <- function(param_row) {
  as_num <- function(x) as.numeric(as.character(x))
  
  n_s    <- as_num(param_row$STRATA_LEVELS)
  p_s    <- parse_prop_vec(param_row$STRATA_PROPORTIONS)
  ctrl0  <- as_num(param_row$ctrl_start)
  delta  <- as_num(param_row$delta)
  
  n_sL   <- as_num(param_row$STRATA_LEVELS_L)
  p_sL   <- parse_prop_vec(param_row$STRATA_PROPORTIONS_L)
  ctrl0L <- as_num(param_row$ctrl_start_l)
  deltaL <- as_num(param_row$delta_l)
  
  lift   <- as_num(param_row$trt_lift)
  
  if (length(p_s) != n_s) {
    stop(sprintf("STRATA_PROPORTIONS 元素个数(%d)与 STRATA_LEVELS(%d)不符", length(p_s), n_s))
  }
  if (length(p_sL) != n_sL) {
    stop(sprintf("STRATA_PROPORTIONS_L 元素个数(%d)与 STRATA_LEVELS_L(%d)不符", length(p_sL), n_sL))
  }
  if (abs(sum(p_s) - 1) > 1e-6) {
    stop(sprintf("STRATA_PROPORTIONS 之和必须等于1，当前为 %.4f", sum(p_s)))
  }
  if (abs(sum(p_sL) - 1) > 1e-6) {
    stop(sprintf("STRATA_PROPORTIONS_L 之和必须等于1，当前为 %.4f", sum(p_sL)))
  }
  
  ctrl_s  <- ctrl0  + (0:(n_s  - 1)) * delta
  trt_s   <- ctrl_s + lift
  p_mean  <- sum(ctrl_s * p_s)
  
  ctrl_sL <- ctrl0L + (0:(n_sL - 1)) * deltaL
  trt_sL  <- ctrl_sL + lift
  p_meanL <- sum(ctrl_sL * p_sL)
  
  if (abs(p_mean - p_meanL) > 1e-6) {
    warning(sprintf(
      "Param_ID=%s: 实际因素均值(%.4f)与 latent 因素均值(%.4f)不一致，差异 %.4f",
      param_row$Param_ID, p_mean, p_meanL, p_meanL - p_mean
    ))
  }
  
  ctrl_cross <- outer(ctrl_s, ctrl_sL, FUN = function(a, l) a + l - p_mean)
  trt_cross  <- ctrl_cross + lift
  
  if (any(ctrl_cross < 0 | ctrl_cross > 1) || any(trt_cross < 0 | trt_cross > 1)) {
    stop(sprintf("Param_ID=%s: 交叉层概率超出[0,1]范围", param_row$Param_ID))
  }
  
  prop_cross <- outer(p_s, p_sL, FUN = "*")
  
  ctrl_strat <- rowSums(ctrl_cross * prop_cross) / p_s
  trt_strat  <- rowSums(trt_cross * prop_cross) / p_s
  
  ctrl_overall <- sum(ctrl_cross * prop_cross)
  trt_overall  <- sum(trt_cross * prop_cross)
  
  cross_df <- expand.grid(
    Actual = 1:n_s,
    Latent = 1:n_sL,
    stringsAsFactors = FALSE
  )
  cross_df$Prop      <- as.vector(prop_cross)
  cross_df$Ctrl_Rate <- as.vector(ctrl_cross)
  cross_df$Trt_Rate  <- as.vector(trt_cross)
  
  list(
    actual_ctrl    = ctrl_s,
    actual_trt     = trt_s,
    latent_ctrl    = ctrl_sL,
    latent_trt     = trt_sL,
    cross_ctrl     = ctrl_cross,
    cross_trt      = trt_cross,
    cross_prop     = prop_cross,
    PROB_STRAT_STR = prob_vec_to_str(ctrl_strat, trt_strat),
    PROB_CR_STR    = prob_vec_to_str(ctrl_overall, trt_overall),
    cross_detail   = cross_df
  )
}

#' 从 Excel 读取参数网格
read_param_grid_from_excel <- function(excel_path, sheet = 1) {
  if (!file.exists(excel_path)) {
    stop(sprintf("Excel 文件不存在: %s", excel_path))
  }
  
  param_grid <- openxlsx::read.xlsx(excel_path, sheet = sheet)
  
  required_cols <- c(
    "TARGET_N", "STRATA_LEVELS", "STRATA_PROPORTIONS",
    "TREATMENT_RATIO", "ctrl_start", "delta",
    "STRATA_LEVELS_L", "STRATA_PROPORTIONS_L", "ctrl_start_l", "delta_l",
    "trt_lift"
  )
  
  missing_cols <- setdiff(required_cols, colnames(param_grid))
  if (length(missing_cols) > 0) {
    stop(sprintf("Excel 参数表缺少必要列: %s", paste(missing_cols, collapse = ", ")))
  }
  
  if (!"Param_ID" %in% colnames(param_grid)) {
    param_grid$Param_ID <- 1:nrow(param_grid)
  }
  
  num_cols <- c(
    "N_POOL", "TARGET_N", "BLOCK_SIZE", "STRATA_LEVELS", "STRATA_LEVELS_L",
    "ctrl_start", "delta", "ctrl_start_l", "delta_l", "trt_lift",
    "N_ITER", "N_BATCH"
  )
  for (col in num_cols) {
    if (col %in% colnames(param_grid)) {
      param_grid[[col]] <- as.numeric(as.character(param_grid[[col]]))
    }
  }
  
  na_check <- sapply(param_grid[intersect(num_cols, colnames(param_grid))], function(x) any(is.na(x)))
  if (any(na_check)) {
    stop(sprintf("以下列含非数值内容: %s", paste(names(na_check)[na_check], collapse = ", ")))
  }
  
  if (!"N_POOL" %in% colnames(param_grid))          param_grid$N_POOL <- 1000
  if (!"BLOCK_SIZE" %in% colnames(param_grid))     param_grid$BLOCK_SIZE <- 4
  if (!"MEAN_SCENARIO" %in% colnames(param_grid))  param_grid$MEAN_SCENARIO <- "Custom"
  if (!"MEAN_SCENARIO_L" %in% colnames(param_grid)) param_grid$MEAN_SCENARIO_L <- "Custom_L"
  if (!"TREATMENT_RATIO_NAME" %in% colnames(param_grid)) {
    param_grid$TREATMENT_RATIO_NAME <- as.character(param_grid$TREATMENT_RATIO)
  }
  
  cross_results <- lapply(1:nrow(param_grid), function(i) {
    gen_cross_params(param_grid[i, ])
  })
  
  param_grid$PROB_STRAT_STR <- sapply(cross_results, `[[`, "PROB_STRAT_STR")
  param_grid$PROB_CR_STR    <- sapply(cross_results, `[[`, "PROB_CR_STR")
  param_grid$cross_detail   <- I(lapply(cross_results, `[[`, "cross_detail"))
  param_grid$cross_ctrl_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_ctrl), 4), collapse = ";")
  })
  param_grid$cross_trt_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_trt), 4), collapse = ";")
  })
  param_grid$cross_prop_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_prop), 4), collapse = ";")
  })
  
  cat(sprintf("已从 Excel 读取 %d 行参数组合: %s\n", nrow(param_grid), excel_path))
  return(param_grid)
}

# ==============================================================================
# 2. 随机列表生成辅助函数
# ==============================================================================

generate_all_blocks <- function(block_size, ratio = c(1, 1)) {
  if (length(ratio) != 2) stop("ratio 必须是长度为 2 的向量")
  ratio_sum <- sum(ratio)
  if (block_size %% ratio_sum != 0) {
    stop(sprintf("区组大小 %d 必须能被比例总和 %d 整除", block_size, ratio_sum))
  }
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

generate_rand_list <- function(n_per_strata, block_size = 4, ratio = c(1, 1)) {
  blocks <- generate_all_blocks(block_size, ratio)
  n_blocks_needed <- ceiling(n_per_strata / block_size)
  selected_indices <- sample(length(blocks), n_blocks_needed, replace = TRUE)
  selected_blocks <- blocks[selected_indices]
  rand_vec <- unlist(selected_blocks)
  return(rand_vec[1:n_per_strata])
}

# ==============================================================================
# 3. SMD 计算
# ==============================================================================

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
  list(
    smd_per_strata = smd_values,
    mean_smd = mean(smd_values),
    max_smd = max(smd_values),
    median_smd = median(smd_values)
  )
}

# ==============================================================================
# 4. 核心：单次试验
# ==============================================================================

run_single_trial_sc <- function(
    N_POOL = 1000,
    TARGET_N = 182,
    BLOCK_SIZE = 4,
    TREATMENT_RATIO = c(1, 1),
    cross_detail = NULL,
    STRATA_LEVELS = 2,
    STRATA_LEVELS_L = 2,
    STRATA_PROPORTIONS = c(0.5, 0.5),
    STRATA_PROPORTIONS_L = c(0.5, 0.5)
) {
  if (is.null(cross_detail)) stop("cross_detail 不能为空")
  
  RAND_LIST_SIZE_PER_STRATA <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  RAND_LIST_SIZE_TOTAL      <- ceiling(TARGET_N / BLOCK_SIZE) * BLOCK_SIZE
  
  # Step 1: Pool 中独立赋值 Actual 和 Latent
  pool_actual <- sample(1:STRATA_LEVELS, size = N_POOL,
                        replace = TRUE, prob = STRATA_PROPORTIONS)
  pool_latent <- sample(1:STRATA_LEVELS_L, size = N_POOL,
                        replace = TRUE, prob = STRATA_PROPORTIONS_L)
  
  # Step 2: 生成两套随机列表
  rand_lists_strat <- lapply(1:STRATA_LEVELS, function(s) {
    generate_rand_list(RAND_LIST_SIZE_PER_STRATA, BLOCK_SIZE, TREATMENT_RATIO)
  })
  ptr_strat <- rep(0, STRATA_LEVELS)
  
  rand_list_simple <- generate_rand_list(RAND_LIST_SIZE_TOTAL, BLOCK_SIZE, TREATMENT_RATIO)
  ptr_simple <- 0
  
  # Step 3: 模拟入组
  enrolled_actual    <- integer(TARGET_N)
  enrolled_latent    <- integer(TARGET_N)
  enrolled_trt_strat <- integer(TARGET_N)
  enrolled_trt_simple<- integer(TARGET_N)
  
  n_enrolled <- 0
  for (i in 1:N_POOL) {
    if (n_enrolled >= TARGET_N) break
    s <- pool_actual[i]
    if (ptr_strat[s] >= length(rand_lists_strat[[s]])) next
    if (ptr_simple >= length(rand_list_simple)) break
    
    n_enrolled <- n_enrolled + 1
    ptr_strat[s] <- ptr_strat[s] + 1
    enrolled_trt_strat[n_enrolled]  <- rand_lists_strat[[s]][ptr_strat[s]]
    ptr_simple <- ptr_simple + 1
    enrolled_trt_simple[n_enrolled] <- rand_list_simple[ptr_simple]
    enrolled_actual[n_enrolled]     <- s
    enrolled_latent[n_enrolled]     <- pool_latent[i]
  }
  
  enrolled_actual     <- enrolled_actual[1:n_enrolled]
  enrolled_latent     <- enrolled_latent[1:n_enrolled]
  enrolled_trt_strat  <- enrolled_trt_strat[1:n_enrolled]
  enrolled_trt_simple <- enrolled_trt_simple[1:n_enrolled]
  
  # Step 4: SMD（仅按 Actual 因素计算）
  smd_strat  <- calculate_strata_smd(enrolled_trt_strat, enrolled_actual)
  smd_simple <- calculate_strata_smd(enrolled_trt_simple, enrolled_actual)
  
  # Step 5: 结局模拟（基于交叉层真实概率）
  cross_lookup <- matrix(NA, nrow = STRATA_LEVELS, ncol = STRATA_LEVELS_L)
  for (idx in 1:nrow(cross_detail)) {
    cross_lookup[cross_detail$Actual[idx], cross_detail$Latent[idx]] <- idx
  }
  
  outcomes_strat  <- numeric(n_enrolled)
  outcomes_simple <- numeric(n_enrolled)
  probs_strat     <- numeric(n_enrolled)
  probs_simple    <- numeric(n_enrolled)
  cross_idx       <- integer(n_enrolled)
  
  for (i in 1:n_enrolled) {
    idx <- cross_lookup[enrolled_actual[i], enrolled_latent[i]]
    cross_idx[i] <- idx
    p_ctrl <- cross_detail$Ctrl_Rate[idx]
    p_trt  <- cross_detail$Trt_Rate[idx]
    probs_strat[i]  <- ifelse(enrolled_trt_strat[i]  == 1, p_ctrl, p_trt)
    probs_simple[i] <- ifelse(enrolled_trt_simple[i] == 1, p_ctrl, p_trt)
    outcomes_strat[i]  <- rbinom(1, 1, probs_strat[i])
    outcomes_simple[i] <- rbinom(1, 1, probs_simple[i])
  }
  
  # Step 6: 统计分析
  analyze_dataset_sc <- function(outcomes, trt, strata_actual, do_stratified = TRUE) {
    n1 <- sum(trt == 1)
    n2 <- sum(trt == 2)
    imbalance <- abs(n1 / n2)
    
    res <- list(
      n1 = n1, n2 = n2, imbalance = imbalance,
      p_unstrat = NA, or_unstrat = NA, or_ci_lower_unstrat = NA, or_ci_upper_unstrat = NA,
      rd_unstrat = NA, rr_unstrat = NA, resp_rate_trt1_unstrat = NA, resp_rate_trt2_unstrat = NA,
      p_strat = NA, or_strat = NA, or_ci_lower_strat = NA, or_ci_upper_strat = NA,
      rd_strat = NA, resp_rate_trt1_strat = NA, resp_rate_trt2_strat = NA,
      method_strat = NA, method_unstrat = NA
    )
    
    # --- 未分层 Fisher Exact ---
    if (n1 >= 1 && n2 >= 1) {
      tbl_2x2 <- table(factor(trt, levels = c(1, 2)),
                       factor(outcomes, levels = c(1, 0)))
      if (nrow(tbl_2x2) == 2 && ncol(tbl_2x2) == 2 && sum(tbl_2x2) > 0) {
        res$resp_rate_trt1_unstrat <- tbl_2x2[1, 1] / sum(tbl_2x2[1, ])
        res$resp_rate_trt2_unstrat <- tbl_2x2[2, 1] / sum(tbl_2x2[2, ])
        res$rd_unstrat <- res$resp_rate_trt2_unstrat - res$resp_rate_trt1_unstrat
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
    
    # --- 分层 CMH（仅按 Actual 因素分层，Latent 不进入分析） ---
    if (do_stratified && length(unique(strata_actual)) >= 2) {
      n_strata <- length(unique(strata_actual))
      strata_vals <- sort(unique(strata_actual))
      arrays <- array(0, dim = c(2, 2, n_strata))
      valid_strata <- rep(FALSE, n_strata)
      
      for (idx_s in 1:n_strata) {
        s <- strata_vals[idx_s]
        sel <- strata_actual == s
        if (sum(sel) >= 2) {
          tbl_s <- table(factor(trt[sel], levels = c(1, 2)),
                         factor(outcomes[sel], levels = c(1, 0)))
          if (nrow(tbl_s) == 2 && ncol(tbl_s) == 2 && all(tbl_s >= 0)) {
            arrays[,, idx_s] <- as.matrix(tbl_s)
            valid_strata[idx_s] <- TRUE
          }
        }
      }
      
      if (sum(valid_strata) >= 1) {
        arrays_valid <- arrays[,, valid_strata, drop = FALSE]
        mh <- tryCatch(mantelhaen.test(arrays_valid[, c(2, 1), ]), error = function(e) NULL)
        if (!is.null(mh)) {
          res$p_strat <- mh$p.value
          res$or_strat <- as.numeric(mh$estimate)
          if (!is.null(mh$conf.int)) {
            res$or_ci_lower_strat <- mh$conf.int[1]
            res$or_ci_upper_strat <- mh$conf.int[2]
          }
          res$method_strat <- "CMH"
          
          rd_s <- numeric()
          w_s  <- numeric()
          rr1_s <- numeric()
          rr2_s <- numeric()
          for (idx_s in which(valid_strata)) {
            tbl_s <- arrays[,, idx_s]
            n_total_s <- sum(tbl_s)
            p1s <- tbl_s[1, 1] / sum(tbl_s[1, ])
            p2s <- tbl_s[2, 1] / sum(tbl_s[2, ])
            rd_s  <- c(rd_s, p2s - p1s)
            w_s   <- c(w_s, n_total_s)
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
    } else if (!do_stratified) {
      res$method_strat <- "Unstratified only"
    } else {
      res$method_strat <- "Insufficient strata levels"
    }
    
    return(res)
  }
  
  res_strat_design  <- analyze_dataset_sc(outcomes_strat,  enrolled_trt_strat,  enrolled_actual, do_stratified = TRUE)
  res_simple_design <- analyze_dataset_sc(outcomes_simple, enrolled_trt_simple, enrolled_actual, do_stratified = FALSE)
  
  list(
    summary = data.frame(
      p_strat_design   = res_strat_design$p_strat,
      or_strat_design  = res_strat_design$or_strat,
      rd_strat_design  = res_strat_design$rd_strat,
      resp_rate_trt1_strat = res_strat_design$resp_rate_trt1_strat,
      resp_rate_trt2_strat = res_strat_design$resp_rate_trt2_strat,
      
      p_strat_unstrat   = res_strat_design$p_unstrat,
      or_strat_unstrat  = res_strat_design$or_unstrat,
      rd_strat_unstrat  = res_strat_design$rd_unstrat,
      resp_rate_trt1_strat_unstrat = res_strat_design$resp_rate_trt1_unstrat,
      resp_rate_trt2_strat_unstrat = res_strat_design$resp_rate_trt2_unstrat,
      
      p_simple_design   = res_simple_design$p_unstrat,
      or_simple_design  = res_simple_design$or_unstrat,
      rd_simple_design  = res_simple_design$rd_unstrat,
      resp_rate_trt1_simple = res_simple_design$resp_rate_trt1_unstrat,
      resp_rate_trt2_simple = res_simple_design$resp_rate_trt2_unstrat,
      
      smd_strat  = smd_strat$mean_smd,
      smd_simple = smd_simple$mean_smd,
      imbalance_strat  = res_strat_design$imbalance,
      imbalance_simple = res_simple_design$imbalance,
      STRATA_LEVELS = STRATA_LEVELS,
      TARGET_N = TARGET_N,
      total_enrolled = n_enrolled,
      stringsAsFactors = FALSE
    ),
    stratified_results = res_strat_design,
    simple_results = res_simple_design,
    cross_lookup = cross_lookup,
    cross_detail_used = cross_detail,
    raw_data = data.frame(
      Subject_ID = 1:n_enrolled,
      Strata_Actual = enrolled_actual,
      Strata_Latent = enrolled_latent,
      Cross_IDX = cross_idx,
      Treatment_strat = enrolled_trt_strat,
      Treatment_simple = enrolled_trt_simple,
      Prob_strat = probs_strat,
      Prob_simple = probs_simple,
      Outcome_strat = outcomes_strat,
      Outcome_simple = outcomes_simple,
      stringsAsFactors = FALSE
    )
  )
}

# ==============================================================================
# 5. 批量模拟
# ==============================================================================

run_batch_simulation_sc <- function(
    n_iter = 1000,
    batch_id = 1,
    N_POOL = 1000,
    TARGET_N = 182,
    BLOCK_SIZE = 4,
    TREATMENT_RATIO = c(1, 1),
    cross_detail = NULL,
    STRATA_LEVELS = 2,
    STRATA_LEVELS_L = 2,
    STRATA_PROPORTIONS = c(0.5, 0.5),
    STRATA_PROPORTIONS_L = c(0.5, 0.5)
) {
  set.seed(123 + batch_id * 1000)
  
  results <- replicate(n_iter, {
    run_single_trial_sc(
      N_POOL = N_POOL, TARGET_N = TARGET_N, BLOCK_SIZE = BLOCK_SIZE,
      TREATMENT_RATIO = TREATMENT_RATIO, cross_detail = cross_detail,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_LEVELS_L = STRATA_LEVELS_L,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS, STRATA_PROPORTIONS_L = STRATA_PROPORTIONS_L
    )
  }, simplify = FALSE)
  
  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

# ==============================================================================
# 6. 汇总统计
# ==============================================================================

generate_summary_stats_sc <- function(
    simulation_data,
    param_id = 1,
    N_POOL = NULL,
    TARGET_N = NULL,
    BLOCK_SIZE = NULL,
    STRATA_LEVELS = NULL,
    STRATA_PROPORTIONS = NULL,
    STRATA_LEVELS_L = NULL,
    STRATA_PROPORTIONS_L = NULL,
    TREATMENT_RATIO = NULL,
    TREATMENT_RATIO_NAME = NULL,
    MEAN_SCENARIO = NULL,
    MEAN_SCENARIO_L = NULL,
    PROB_STRAT_STR = NULL,
    PROB_CR_STR = NULL
) {
  # 1. 治疗组不平衡
  imb_median_strat  <- median(simulation_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat      <- quantile(simulation_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat      <- quantile(simulation_data$imbalance_strat, 0.75, na.rm = TRUE)
  arm_imbalance_strat <- sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat)
  
  imb_median_simple <- median(simulation_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple     <- quantile(simulation_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple     <- quantile(simulation_data$imbalance_simple, 0.75, na.rm = TRUE)
  arm_imbalance_simple <- sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple)
  
  # 2. Power
  power_strat_design  <- mean(simulation_data$p_strat_design < 0.05, na.rm = TRUE)
  power_strat_unstrat <- mean(simulation_data$p_strat_unstrat < 0.05, na.rm = TRUE)
  power_simple_design <- mean(simulation_data$p_simple_design < 0.05, na.rm = TRUE)
  
  # 3. OR
  or_mean_strat  <- mean(simulation_data$or_strat_design, na.rm = TRUE)
  or_sd_strat    <- sd(simulation_data$or_strat_design, na.rm = TRUE)
  or_mean_strat_unstrat <- mean(simulation_data$or_strat_unstrat, na.rm = TRUE)
  or_sd_strat_unstrat   <- sd(simulation_data$or_strat_unstrat, na.rm = TRUE)
  or_mean_simple <- mean(simulation_data$or_simple_design, na.rm = TRUE)
  or_sd_simple   <- sd(simulation_data$or_simple_design, na.rm = TRUE)
  
  # 4. RD
  rd_mean_strat  <- mean(simulation_data$rd_strat_design, na.rm = TRUE)
  rd_sd_strat    <- sd(simulation_data$rd_strat_design, na.rm = TRUE)
  rd_mean_strat_unstrat <- mean(simulation_data$rd_strat_unstrat, na.rm = TRUE)
  rd_sd_strat_unstrat   <- sd(simulation_data$rd_strat_unstrat, na.rm = TRUE)
  rd_mean_simple <- mean(simulation_data$rd_simple_design, na.rm = TRUE)
  rd_sd_simple   <- sd(simulation_data$rd_simple_design, na.rm = TRUE)
  
  # 5. 响应率
  rr1_mean_strat  <- mean(simulation_data$resp_rate_trt1_strat, na.rm = TRUE)
  rr2_mean_strat  <- mean(simulation_data$resp_rate_trt2_strat, na.rm = TRUE)
  rr1_mean_strat_unstrat <- mean(simulation_data$resp_rate_trt1_strat_unstrat, na.rm = TRUE)
  rr2_mean_strat_unstrat <- mean(simulation_data$resp_rate_trt2_strat_unstrat, na.rm = TRUE)
  rr1_mean_simple <- mean(simulation_data$resp_rate_trt1_simple, na.rm = TRUE)
  rr2_mean_simple <- mean(simulation_data$resp_rate_trt2_simple, na.rm = TRUE)
  
  # 6. Power 差异
  power_diff <- power_strat_design - power_simple_design
  power_diff_pct <- sprintf("%+.1f%%", power_diff * 100)
  power_diff_unstrat <- power_strat_unstrat - power_simple_design
  power_diff_unstrat_pct <- sprintf("%+.1f%%", power_diff_unstrat * 100)
  
  # 7. SMD
  smd_mean_strat  <- mean(simulation_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(simulation_data$smd_simple, na.rm = TRUE)
  smd_summary <- sprintf("%.3f / %.3f", smd_mean_strat, smd_mean_simple)
  prop_smd_gt_015_strat  <- mean(simulation_data$smd_strat > 0.15, na.rm = TRUE)
  prop_smd_gt_015_simple <- mean(simulation_data$smd_simple > 0.15, na.rm = TRUE)
  smd_imbalance_prop <- sprintf("%.1f%% / %.1f%%",
                                 prop_smd_gt_015_strat * 100,
                                 prop_smd_gt_015_simple * 100)
  
  result <- data.frame(
    Simulation_ID = param_id,
    TARGET_N = ifelse(is.null(TARGET_N), NA, TARGET_N),
    BLOCK_SIZE = ifelse(is.null(BLOCK_SIZE), NA, BLOCK_SIZE),
    STRATA_LEVELS = ifelse(is.null(STRATA_LEVELS), NA, STRATA_LEVELS),
    STRATA_LEVELS_L = ifelse(is.null(STRATA_LEVELS_L), NA, STRATA_LEVELS_L),
    
    Arm_imbalance_strat = arm_imbalance_strat,
    Arm_imbalance_simple = arm_imbalance_simple,
    
    Power_strat_design  = sprintf("%.3f", power_strat_design),
    Power_strat_unstrat = sprintf("%.3f", power_strat_unstrat),
    Power_simple_design = sprintf("%.3f", power_simple_design),
    Power_difference = power_diff_pct,
    Power_diff_strat_unstrat_vs_simple = power_diff_unstrat_pct,
    
    OR_mean_strat  = sprintf("%.2f (%.2f)", or_mean_strat, or_sd_strat),
    OR_mean_strat_unstrat = sprintf("%.2f (%.2f)", or_mean_strat_unstrat, or_sd_strat_unstrat),
    OR_mean_simple = sprintf("%.2f (%.2f)", or_mean_simple, or_sd_simple),
    
    RD_mean_strat  = sprintf("%.3f (%.3f)", rd_mean_strat, rd_sd_strat),
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
    
    stringsAsFactors = FALSE
  )
  
  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(STRATA_PROPORTIONS)) result$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ":")
  if (!is.null(STRATA_PROPORTIONS_L)) result$STRATA_PROPORTIONS_L <- paste(STRATA_PROPORTIONS_L, collapse = ":")
  if (!is.null(TREATMENT_RATIO)) result$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  if (!is.null(TREATMENT_RATIO_NAME)) result$TREATMENT_RATIO_NAME <- TREATMENT_RATIO_NAME
  if (!is.null(MEAN_SCENARIO)) result$MEAN_SCENARIO <- MEAN_SCENARIO
  if (!is.null(MEAN_SCENARIO_L)) result$MEAN_SCENARIO_L <- MEAN_SCENARIO_L
  if (!is.null(PROB_STRAT_STR)) result$PROB_STRAT_STR <- PROB_STRAT_STR
  if (!is.null(PROB_CR_STR)) result$PROB_CR_STR <- PROB_CR_STR
  
  return(result)
}

# ==============================================================================
# 7. Excel 输出
# ==============================================================================

generate_excel_output_sc <- function(summary_data, output_prefix = "simulation_sc_binary_results", cross_detail_data = NULL) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(output_prefix, "_", timestamp, ".xlsx")
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("          生成临床试验二分类模拟报告 (SC 模板版)\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  wb <- openxlsx::createWorkbook()
  
  # 工作表1: Combined
  cat("[1/3] 创建工作表: Combined\n")
  
  desired_order <- c(
    "Simulation_ID", "Param_ID", "Batch_ID",
    "N_POOL", "TARGET_N", "BLOCK_SIZE",
    "STRATA_LEVELS", "STRATA_PROPORTIONS",
    "STRATA_LEVELS_L", "STRATA_PROPORTIONS_L",
    "TREATMENT_RATIO", "TREATMENT_RATIO_NAME",
    "MEAN_SCENARIO", "MEAN_SCENARIO_L",
    "PROB_STRAT_STR", "PROB_CR_STR",
    "Arm_imbalance_strat", "Power_strat_design",
    "OR_mean_strat", "RD_mean_strat", "Resp_rate_trt1_strat", "Resp_rate_trt2_strat",
    "Power_strat_unstrat", "OR_mean_strat_unstrat", "RD_mean_strat_unstrat",
    "Resp_rate_trt1_strat_unstrat", "Resp_rate_trt2_strat_unstrat",
    "Arm_imbalance_simple", "Power_simple_design",
    "OR_mean_simple", "RD_mean_simple", "Resp_rate_trt1_simple", "Resp_rate_trt2_simple",
    "SMD_strat_vs_simple",
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
  cat(sprintf("  Combined 工作表已创建 (%d 行 x %d 列)\n", nrow(combined_df), ncol(combined_df)))
  
  # 工作表2: Column_Description
  cat("[2/3] 创建工作表: Column_Description\n")
  
  col_descriptions <- data.frame(
    Column_Name = c(
      "Simulation_ID", "Param_ID", "Batch_ID",
      "N_POOL", "TARGET_N", "BLOCK_SIZE", "STRATA_LEVELS", "STRATA_PROPORTIONS",
      "STRATA_LEVELS_L", "STRATA_PROPORTIONS_L",
      "TREATMENT_RATIO", "TREATMENT_RATIO_NAME", "MEAN_SCENARIO", "MEAN_SCENARIO_L",
      "PROB_STRAT_STR", "PROB_CR_STR",
      "Arm_imbalance_strat", "Power_strat_design",
      "OR_mean_strat", "RD_mean_strat", "Resp_rate_trt1_strat", "Resp_rate_trt2_strat",
      "Power_strat_unstrat", "OR_mean_strat_unstrat", "RD_mean_strat_unstrat",
      "Resp_rate_trt1_strat_unstrat", "Resp_rate_trt2_strat_unstrat",
      "Arm_imbalance_simple", "Power_simple_design",
      "OR_mean_simple", "RD_mean_simple", "Resp_rate_trt1_simple", "Resp_rate_trt2_simple",
      "SMD_strat_vs_simple",
      "Power_difference", "Power_diff_strat_unstrat_vs_simple"
    ),
    Description = c(
      "模拟组合编号", "参数组合ID", "批次ID",
      "初始受试者池大小", "目标入组样本量", "区组大小", "分层因素层数", "分层因素各层比例",
      "Latent因素层数", "Latent因素各层比例",
      "治疗组分配比例", "治疗组比例名称", "场景名称", "Latent场景名称",
      "分层随机化概率字符串", "完全随机化概率字符串",
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
      "把握度差异（CMH - 简单）",
      "把握度差异（分层未分层 - 简单）"
    ),
    Example = c(
      "1", "1", "1,2,...,10,Overall",
      "1000", "182", "4", "2", "0.5,0.5",
      "2", "0.5,0.5",
      "1,1", "1:1", "Custom", "Custom_L",
      "0.3,0.5;0.3,0.5", "0.35,0.55",
      "0.0 (0.0-1.0)", "0.823", "2.15 (0.85)", "0.150 (0.052)", "30.1%", "45.2%",
      "0.812", "2.10 (0.90)", "0.145 (0.055)", "29.8%", "44.5%",
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
  cat(sprintf("  Column_Description 工作表已创建 (%d 行)\n", nrow(col_descriptions)))
  
  # 工作表3: Summary_Overview
  if (nrow(summary_data) > 0) {
    cat("[3/3] 创建工作表: Summary_Overview\n")
    
    overview_data <- data.frame(
      Category = c(
        "总模拟组合数", "批次范围", "样本量设置", "区组大小",
        "分层因素层数", "Latent因素层数", "治疗组比例",
        "分层设计平均Power(CMH)", "分层设计平均Power(未分层)", "简单设计平均Power",
        "平均Power差异(CMH-简单)", "平均Power差异(未分层-简单)"
      ),
      Values = c(
        as.character(nrow(summary_data)),
        paste(range(summary_data$Batch_ID), collapse = " - "),
        paste(unique(summary_data$TARGET_N), collapse = ", "),
        paste(unique(summary_data$BLOCK_SIZE), collapse = ", "),
        paste(unique(summary_data$STRATA_LEVELS), collapse = ", "),
        paste(unique(summary_data$STRATA_LEVELS_L), collapse = ", "),
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
    cat(sprintf("  Summary_Overview 工作表已创建 (%d 行)\n", nrow(overview_data)))
  } else {
    cat("[3/3] 跳过: Summary_Overview (无数据)\n")
  }
  
  # 工作表4: Cross_Detail（QC用）
  if (!is.null(cross_detail_data) && nrow(cross_detail_data) > 0) {
    cat("[4/4] 创建工作表: Cross_Detail\n")
    openxlsx::addWorksheet(wb, "Cross_Detail")
    openxlsx::writeData(wb, "Cross_Detail", cross_detail_data, startRow = 1, startCol = 1)
    openxlsx::setColWidths(wb, "Cross_Detail", cols = 1:ncol(cross_detail_data), widths = "auto")
    openxlsx::addStyle(wb, "Cross_Detail", header_style, rows = 1, cols = 1:ncol(cross_detail_data), gridExpand = TRUE)
    cat(sprintf("  Cross_Detail 工作表已创建 (%d 行 x %d 列)\n", nrow(cross_detail_data), ncol(cross_detail_data)))
  } else {
    cat("[4/4] 跳过: Cross_Detail (无数据)\n")
  }
  
  tryCatch({
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("Excel报告已成功保存: %s\n", output_file))
    cat(strrep("=", 70), "\n\n", sep = "")
    return(output_file)
  }, error = function(e) {
    warning(sprintf("保存Excel文件时出错: %s", e$message))
    return(NULL)
  })
}

# ==============================================================================
# 8. 主函数（per-batch 汇总）
# ==============================================================================

run_simulation_per_batch_sc <- function(
    param_grid,
    n_iter = 1000,
    n_batch = 10,
    verbose = TRUE,
    generate_excel = TRUE,
    output_prefix = "simulation_sc_binary_results"
) {
  all_batch_summaries_list <- list()
  current_row <- 1
  
  for (i in 1:nrow(param_grid)) {
    if (verbose) cat(sprintf("\n========== 测试参数组合 %d / %d ==========\n", i, nrow(param_grid)))
    
    params <- param_grid[i, , drop = FALSE]
    
    iter <- if ("N_ITER" %in% colnames(params) && !is.na(params$N_ITER)) as.integer(params$N_ITER) else n_iter
    batch <- if ("N_BATCH" %in% colnames(params) && !is.na(params$N_BATCH)) as.integer(params$N_BATCH) else n_batch
    
    p_s   <- parse_prop_vec(params$STRATA_PROPORTIONS)
    p_sL  <- parse_prop_vec(params$STRATA_PROPORTIONS_L)
    ratio <- as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
    
    cross_detail <- params$cross_detail[[1]]
    
    batch_results_list <- list()
    batch_summaries_list <- list()
    
    for (batch_id in 1:batch) {
      if (verbose) cat(sprintf("\r  处理批次 %d / %d", batch_id, batch))
      
      batch_result <- run_batch_simulation_sc(
        n_iter = iter, batch_id = batch_id,
        N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
        BLOCK_SIZE = params$BLOCK_SIZE, TREATMENT_RATIO = ratio,
        cross_detail = cross_detail,
        STRATA_LEVELS = params$STRATA_LEVELS,
        STRATA_LEVELS_L = params$STRATA_LEVELS_L,
        STRATA_PROPORTIONS = p_s,
        STRATA_PROPORTIONS_L = p_sL
      )
      
      batch_summary <- generate_summary_stats_sc(
        simulation_data = batch_result, param_id = batch_id,
        N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
        BLOCK_SIZE = params$BLOCK_SIZE,
        STRATA_LEVELS = params$STRATA_LEVELS,
        STRATA_PROPORTIONS = p_s,
        STRATA_LEVELS_L = params$STRATA_LEVELS_L,
        STRATA_PROPORTIONS_L = p_sL,
        TREATMENT_RATIO = paste(ratio, collapse = ","),
        TREATMENT_RATIO_NAME = as.character(params$TREATMENT_RATIO_NAME),
        MEAN_SCENARIO = as.character(params$MEAN_SCENARIO),
        MEAN_SCENARIO_L = as.character(params$MEAN_SCENARIO_L),
        PROB_STRAT_STR = as.character(params$PROB_STRAT_STR),
        PROB_CR_STR = as.character(params$PROB_CR_STR)
      )
      
      batch_result$Batch_ID <- batch_id
      batch_summary$Batch_ID <- batch_id
      
      batch_results_list[[batch_id]] <- batch_result
      batch_summaries_list[[batch_id]] <- batch_summary
    }
    if (verbose) cat("\n")
    
    all_detailed <- do.call(rbind, batch_results_list)
    all_batch_summaries <- do.call(rbind, batch_summaries_list)
    
    overall_summary <- generate_summary_stats_sc(
      simulation_data = all_detailed, param_id = "Overall",
      N_POOL = params$N_POOL, TARGET_N = params$TARGET_N,
      BLOCK_SIZE = params$BLOCK_SIZE,
      STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = p_s,
      STRATA_LEVELS_L = params$STRATA_LEVELS_L,
      STRATA_PROPORTIONS_L = p_sL,
      TREATMENT_RATIO = paste(ratio, collapse = ","),
      TREATMENT_RATIO_NAME = as.character(params$TREATMENT_RATIO_NAME),
      MEAN_SCENARIO = as.character(params$MEAN_SCENARIO),
      MEAN_SCENARIO_L = as.character(params$MEAN_SCENARIO_L),
      PROB_STRAT_STR = as.character(params$PROB_STRAT_STR),
      PROB_CR_STR = as.character(params$PROB_CR_STR)
    )
    overall_summary$Batch_ID <- "Overall"
    all_batch_summaries <- rbind(all_batch_summaries, overall_summary)
    
    # 附加 param_grid 中的其他列
    param_cols_to_add <- setdiff(colnames(param_grid), c("Param_ID", "cross_detail"))
    for (col in param_cols_to_add) {
      all_batch_summaries[[col]] <- params[[col]]
    }
    all_batch_summaries$Param_ID <- i
    
    all_batch_summaries_list[[current_row]] <- all_batch_summaries
    current_row <- current_row + 1
    
    if (verbose) {
      cat(sprintf("  完成: %d 批次 x %d 次 = %d 次模拟\n", batch, iter, batch * iter))
    }
  }
  
  all_batch_summaries_final <- do.call(rbind, all_batch_summaries_list)
  rownames(all_batch_summaries_final) <- NULL
  
  # 收集所有参数组合的 cross_detail（用于QC）
  cross_detail_list <- list()
  for (i in 1:nrow(param_grid)) {
    cd <- param_grid$cross_detail[[i]]
    cd$Param_ID <- i
    # 也把对应的 PROB_STRAT_STR / PROB_CR_STR 挂上来方便对照
    cd$PROB_STRAT_STR <- param_grid$PROB_STRAT_STR[i]
    cd$PROB_CR_STR    <- param_grid$PROB_CR_STR[i]
    cross_detail_list[[i]] <- cd
  }
  cross_detail_all <- do.call(rbind, cross_detail_list)
  
  if (verbose) {
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("所有参数组合测试完成！\n"))
    cat(sprintf("  总参数组合数: %d\n", nrow(param_grid)))
    cat(sprintf("  总汇总行数: %d\n", nrow(all_batch_summaries_final)))
    cat(strrep("=", 70), "\n\n", sep = "")
  }
  
  if (generate_excel) {
    output_file <- generate_excel_output_sc(
      summary_data = all_batch_summaries_final,
      output_prefix = output_prefix,
      cross_detail_data = cross_detail_all
    )
    if (verbose && !is.null(output_file)) {
      cat(sprintf("\nExcel 报告已保存: %s\n", output_file))
    }
  }
  
  return(list(
    batch_summaries_all = all_batch_summaries_final,
    param_grid = param_grid,
    cross_detail_all = cross_detail_all
  ))
}

# ==============================================================================
# 9. 使用示例
# ==============================================================================
# 
 param_grid <- read_param_grid_from_excel("C:/Yuting/AI/kimi/param_grid_binary_template_sc.xlsx")

results <- run_simulation_per_batch_sc(
  param_grid = param_grid,
  n_iter = 1000,      # 可被 Excel 中 N_ITER 列覆盖
  n_batch = 10,       # 可被 Excel 中 N_BATCH 列覆盖
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "sc_binary_simulation"
)
# ==============================================================================
