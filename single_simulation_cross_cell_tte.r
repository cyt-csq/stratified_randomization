# ==============================================================================
# Cross-Cell Latent TTE Simulation (精简版)
# 只比对两种设计：
#   1. Stratified: 按A分层随机化 + ~trt+strata(A) Cox/Logrank
#   2. Simple:     简单随机化      + ~trt Cox/Logrank
# ==============================================================================

library(openxlsx)
library(pbapply)
library(survival)

# ------------------------------------------------------------------------------
# 1. 生成 cross-cell 精确中位矩阵
# ------------------------------------------------------------------------------
generate_exact_median <- function(
  overall_median_ctrl,
  overall_hr,
  base_factors_a,
  base_factors_b,
  prop_a,
  prop_b
) {
  if (length(base_factors_a) != length(prop_a))
    stop("base_factors_a 与 prop_a 长度必须相同")
  if (length(base_factors_b) != length(prop_b))
    stop("base_factors_b 与 prop_b 长度必须相同")
  if (abs(sum(prop_a) - 1) > 1e-6 || abs(sum(prop_b) - 1) > 1e-6)
    stop("prop_a 和 prop_b 必须各自求和为 1")

  overall_median_trt <- overall_median_ctrl / overall_hr

  grid <- expand.grid(A = seq_along(base_factors_a), B = seq_along(base_factors_b),
                      stringsAsFactors = FALSE)
  grid$prop   <- prop_a[grid$A] * prop_b[grid$B]
  grid$factor <- base_factors_a[grid$A] * base_factors_b[grid$B]

  target_fn <- function(lambda) {
    sum(grid$prop * exp(-log(2) * grid$factor * lambda)) - 0.5
  }
  lambda <- uniroot(target_fn, interval = c(1e-10, 1e6))$root

  grid$median_ctrl <- overall_median_ctrl / (grid$factor * lambda)
  grid$median_trt  <- grid$median_ctrl / overall_hr

  list(lambda = lambda, joint_grid = grid,
       overall_median_ctrl = overall_median_ctrl,
       overall_median_trt  = overall_median_trt)
}

# ------------------------------------------------------------------------------
# 2. 设计阶段预期 VRF (基于 A 层间/层内方差分解)
# ------------------------------------------------------------------------------
calculate_expected_vrf_cross_cell <- function(joint_grid, prop_a, prop_b) {
  ln2 <- log(2)
  n_a <- length(prop_a)
  grid <- joint_grid
  grid$mean_cell <- 0.5 * (grid$median_ctrl / ln2) + 0.5 * (grid$median_trt / ln2)
  grid$var_cell  <- 0.5 * (grid$median_ctrl / ln2)^2 + 0.5 * (grid$median_trt / ln2)^2

  var_within_a <- numeric(n_a)
  mean_a       <- numeric(n_a)
  for (a in 1:n_a) {
    sub <- grid[grid$A == a, ]
    w   <- sub$prop / sum(sub$prop)
    mean_a[a]       <- sum(w * sub$mean_cell)
    var_within_a[a] <- sum(w * sub$var_cell) + sum(w * sub$mean_cell^2) - mean_a[a]^2
  }

  overall_mean <- sum(prop_a * mean_a)
  overall_var  <- sum(prop_a * var_within_a) + sum(prop_a * mean_a^2) - overall_mean^2

  overall_var / sum(prop_a * var_within_a)
}

# ------------------------------------------------------------------------------
# 3. 随机化辅助函数
# ------------------------------------------------------------------------------
generate_rand_list <- function(n_per_strata, block_size = 4, ratio = c(1, 1)) {
  ratio_sum <- sum(ratio)
  if (block_size %% ratio_sum != 0)
    stop(sprintf("block_size %d must be divisible by ratio sum %d", block_size, ratio_sum))
  n_group1 <- as.integer((ratio[1] / ratio_sum) * block_size)
  base_block <- c(rep(1, n_group1), rep(2, block_size - n_group1))
  n_blocks_needed <- ceiling(n_per_strata / block_size)
  rand_vec <- unlist(lapply(seq_len(n_blocks_needed), function(i) sample(base_block)))
  rand_vec[seq_len(n_per_strata)]
}

generate_stratified_rand_lists <- function(n_strata, n_per_strata, block_size = 4, ratio = c(1, 1)) {
  lapply(seq_len(n_strata), function(s) generate_rand_list(n_per_strata, block_size, ratio))
}

# ------------------------------------------------------------------------------
# 4. SMD 计算
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
  list(mean_smd = mean(smd_values), max_smd = max(smd_values))
}

# ------------------------------------------------------------------------------
# 5. 单次试验
# ------------------------------------------------------------------------------
run_single_trial_cross_cell <- function(
  N_POOL = 5000,
  TARGET_EVENTS = 66,
  BLOCK_SIZE = 4,
  N_STRATA_A = 2,
  PROP_A = c(0.5, 0.5),
  BASE_FACTORS_A = c(1, 3),
  N_STRATA_B = 3,
  PROP_B = c(0.2, 0.3, 0.5),
  BASE_FACTORS_B = c(1, 2, 4),
  OVERALL_MEDIAN_CTRL = 15,
  OVERALL_HR = 0.5,
  TREATMENT_RATIO = c(1, 1),
  ENROLL_PERIOD = 12,
  STUDY_DURATION = 36
) {
  # 5.1 生成 cross-cell 中位数
  cross <- generate_exact_median(
    overall_median_ctrl = OVERALL_MEDIAN_CTRL,
    overall_hr = OVERALL_HR,
    base_factors_a = BASE_FACTORS_A,
    base_factors_b = BASE_FACTORS_B,
    prop_a = PROP_A,
    prop_b = PROP_B
  )
  grid <- cross$joint_grid
  n_cross <- nrow(grid)

  median_lookup_ctrl <- grid$median_ctrl
  median_lookup_trt  <- grid$median_trt

  # 5.2 生成池
  pool_a <- sample(1:N_STRATA_A, size = N_POOL, replace = TRUE, prob = PROP_A)
  pool_b <- sample(1:N_STRATA_B, size = N_POOL, replace = TRUE, prob = PROP_B)
  pool_cross_id <- (pool_a - 1) * N_STRATA_B + pool_b

  # 5.3 随机化（只按 A 分层）
  RAND_LIST_SIZE <- ceiling(N_POOL / BLOCK_SIZE) * BLOCK_SIZE
  rand_lists <- generate_stratified_rand_lists(
    n_strata = N_STRATA_A, n_per_strata = RAND_LIST_SIZE,
    block_size = BLOCK_SIZE, ratio = TREATMENT_RATIO)
  ptr_list <- rep(0, N_STRATA_A)

  rand_list_global <- generate_rand_list(RAND_LIST_SIZE, BLOCK_SIZE, TREATMENT_RATIO)
  ptr_global <- 0

  enrolled_trt_strat <- numeric(N_POOL)
  enrolled_trt_simple <- numeric(N_POOL)
  enrolled_a <- numeric(N_POOL)
  enrolled_cross_id <- numeric(N_POOL)
  total_enrolled <- 0

  for (i in 1:N_POOL) {
    a <- pool_a[i]
    if (ptr_list[a] >= length(rand_lists[[a]])) next
    if (ptr_global >= length(rand_list_global)) break

    total_enrolled <- total_enrolled + 1
    ptr_list[a] <- ptr_list[a] + 1
    enrolled_trt_strat[total_enrolled] <- rand_lists[[a]][ptr_list[a]]
    ptr_global <- ptr_global + 1
    enrolled_trt_simple[total_enrolled] <- rand_list_global[ptr_global]
    enrolled_a[total_enrolled] <- a
    enrolled_cross_id[total_enrolled] <- pool_cross_id[i]
  }

  enrolled_trt_strat   <- enrolled_trt_strat[1:total_enrolled]
  enrolled_trt_simple  <- enrolled_trt_simple[1:total_enrolled]
  enrolled_a           <- enrolled_a[1:total_enrolled]
  enrolled_cross_id    <- enrolled_cross_id[1:total_enrolled]

  # 5.4 SMD
  smd_strat  <- calculate_strata_smd(enrolled_trt_strat, enrolled_a)
  smd_simple <- calculate_strata_smd(enrolled_trt_simple, enrolled_a)

  # 5.5 生成 TTE 数据
  generate_tte <- function(enrolled_trt, enrolled_cross_id) {
    n <- length(enrolled_trt)
    enroll_time <- runif(n, 0, ENROLL_PERIOD)
    base_median <- ifelse(enrolled_trt == 1,
                          median_lookup_ctrl[enrolled_cross_id],
                          median_lookup_trt[enrolled_cross_id])
    rate_vec <- log(2) / base_median
    survival_time <- rexp(n, rate = rate_vec)
    censor_time <- STUDY_DURATION - enroll_time
    event_time <- pmin(survival_time, censor_time)
    event_status <- as.numeric(survival_time <= censor_time)
    data.frame(enroll_time = enroll_time, event_time = event_time,
               event_status = event_status, stringsAsFactors = FALSE)
  }

  tte_strat  <- generate_tte(enrolled_trt_strat, enrolled_cross_id)
  tte_simple <- generate_tte(enrolled_trt_simple, enrolled_cross_id)

  # 5.6 事件驱动截断
  truncate_tte <- function(tte_data, enrolled_trt, enrolled_a) {
    ord <- order(tte_data$enroll_time)
    cum_events <- cumsum(tte_data$event_status[ord])
    if (max(cum_events) < TARGET_EVENTS) {
      stop(sprintf("N_POOL (%d) insufficient for TARGET_EVENTS (%d)", N_POOL, TARGET_EVENTS))
    }
    idx <- min(which(cum_events >= TARGET_EVENTS))
    sel <- ord[1:idx]
    list(tte_data = tte_data[sel, ], enrolled_trt = enrolled_trt[sel],
         enrolled_a = enrolled_a[sel])
  }

  trunc_strat  <- truncate_tte(tte_strat, enrolled_trt_strat, enrolled_a)
  trunc_simple <- truncate_tte(tte_simple, enrolled_trt_simple, enrolled_a)

  # 5.7 统计分析
  analyze <- function(tte_data, enrolled_trt, enrolled_a, is_stratified_design = TRUE) {
    n1 <- sum(enrolled_trt == 1)
    n2 <- sum(enrolled_trt == 2)
    total_events <- sum(tte_data$event_status)
    df <- data.frame(time = tte_data$event_time, status = tte_data$event_status,
                     trt = enrolled_trt, strata = enrolled_a, stringsAsFactors = FALSE)

    res <- list(
      n1 = n1, n2 = n2, imbalance = abs(n1 - n2),
      total_events = total_events,
      hr = NA, hr_ci_lower = NA, hr_ci_upper = NA, p_cox = NA,
      p_logrank = NA, chisq_logrank = NA
    )

    if (n1 >= 1 && n2 >= 1 && total_events >= 3) {
      fmla_cox <- if (is_stratified_design)
        Surv(time, status) ~ trt + strata(strata)
      else
        Surv(time, status) ~ trt
      cox_fit <- tryCatch(coxph(fmla_cox, data = df), error = function(e) NULL)
      if (!is.null(cox_fit)) {
        sm <- summary(cox_fit)
        res$hr <- as.numeric(sm$conf.int[1, 1])
        res$hr_ci_lower <- as.numeric(sm$conf.int[1, 3])
        res$hr_ci_upper <- as.numeric(sm$conf.int[1, 4])
        res$p_cox <- sm$coefficients[1, 5]
      }
      fmla_lr <- if (is_stratified_design)
        Surv(time, status) ~ trt + strata(strata)
      else
        Surv(time, status) ~ trt
      lr_fit <- tryCatch(survdiff(fmla_lr, data = df), error = function(e) NULL)
      if (!is.null(lr_fit)) {
        res$p_logrank <- 1 - pchisq(lr_fit$chisq, df = length(lr_fit$n) - 1)
        res$chisq_logrank <- lr_fit$chisq
      }
    }
    res
  }

  res_strat  <- analyze(trunc_strat$tte_data, trunc_strat$enrolled_trt, trunc_strat$enrolled_a, TRUE)
  res_simple <- analyze(trunc_simple$tte_data, trunc_simple$enrolled_trt, trunc_simple$enrolled_a, FALSE)

  # 5.8 预期 VRF
  vrf_expected <- calculate_expected_vrf_cross_cell(grid, PROP_A, PROP_B)

  vrf_actual <- NA
  if (!is.na(res_strat$chisq_logrank) && !is.na(res_simple$chisq_logrank) &&
      res_simple$chisq_logrank > 0) {
    vrf_actual <- res_strat$chisq_logrank / res_simple$chisq_logrank
  }

  list(
    summary = data.frame(
      p_strat_design = res_strat$p_logrank,
      p_simple_design = res_simple$p_logrank,
      hr_strat = res_strat$hr,
      hr_simple = res_simple$hr,
      events_strat = res_strat$total_events,
      events_simple = res_simple$total_events,
      n_strat = length(trunc_strat$enrolled_trt),
      n_simple = length(trunc_simple$enrolled_trt),
      imbalance_strat = res_strat$imbalance,
      imbalance_simple = res_simple$imbalance,
      smd_strat = smd_strat$mean_smd,
      smd_simple = smd_simple$mean_smd,
      vrf_actual = vrf_actual,
      vrf_expected = vrf_expected,
      stringsAsFactors = FALSE
    ),
    cross_cell_grid = grid
  )
}

# ------------------------------------------------------------------------------
# 6. Batch simulation
# ------------------------------------------------------------------------------
run_batch_simulation_cross_cell <- function(
  n_iter = 1000, batch_id = 1,
  N_POOL = 5000, TARGET_EVENTS = 66, BLOCK_SIZE = 4,
  N_STRATA_A = 2, PROP_A = c(0.5, 0.5), BASE_FACTORS_A = c(1, 3),
  N_STRATA_B = 3, PROP_B = c(0.2, 0.3, 0.5), BASE_FACTORS_B = c(1, 2, 4),
  OVERALL_MEDIAN_CTRL = 15, OVERALL_HR = 0.5,
  TREATMENT_RATIO = c(1, 1), ENROLL_PERIOD = 12, STUDY_DURATION = 36
) {
  set.seed(123 + batch_id * 1000)
  results <- replicate(n_iter, {
    run_single_trial_cross_cell(
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      N_STRATA_A = N_STRATA_A, PROP_A = PROP_A, BASE_FACTORS_A = BASE_FACTORS_A,
      N_STRATA_B = N_STRATA_B, PROP_B = PROP_B, BASE_FACTORS_B = BASE_FACTORS_B,
      OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR,
      TREATMENT_RATIO = TREATMENT_RATIO, ENROLL_PERIOD = ENROLL_PERIOD,
      STUDY_DURATION = STUDY_DURATION)
  }, simplify = FALSE)
  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

# ------------------------------------------------------------------------------
# 7. Parallel batch simulation
# ------------------------------------------------------------------------------
run_batch_simulation_parallel_cross_cell <- function(
  n_iter = 1000, batch_id = 1,
  N_POOL = 5000, TARGET_EVENTS = 66, BLOCK_SIZE = 4,
  N_STRATA_A = 2, PROP_A = c(0.5, 0.5), BASE_FACTORS_A = c(1, 3),
  N_STRATA_B = 3, PROP_B = c(0.2, 0.3, 0.5), BASE_FACTORS_B = c(1, 2, 4),
  OVERALL_MEDIAN_CTRL = 15, OVERALL_HR = 0.5,
  TREATMENT_RATIO = c(1, 1), ENROLL_PERIOD = 12, STUDY_DURATION = 36,
  n_cores = max(1, parallel::detectCores() - 1)
) {
  if (n_cores <= 1) {
    return(run_batch_simulation_cross_cell(
      n_iter = n_iter, batch_id = batch_id,
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      N_STRATA_A = N_STRATA_A, PROP_A = PROP_A, BASE_FACTORS_A = BASE_FACTORS_A,
      N_STRATA_B = N_STRATA_B, PROP_B = PROP_B, BASE_FACTORS_B = BASE_FACTORS_B,
      OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR,
      TREATMENT_RATIO = TREATMENT_RATIO, ENROLL_PERIOD = ENROLL_PERIOD,
      STUDY_DURATION = STUDY_DURATION))
  }

  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(cl, varlist = c(
    "run_single_trial_cross_cell", "generate_exact_median",
    "calculate_expected_vrf_cross_cell",
    "generate_rand_list", "generate_stratified_rand_lists",
    "calculate_strata_smd"
  ), envir = environment())
  parallel::clusterEvalQ(cl, library(survival))

  set.seed(123 + batch_id * 1000)
  seeds <- sample.int(.Machine$integer.max, n_iter)

  results <- parallel::parLapply(cl, seeds, function(seed) {
    set.seed(seed)
    run_single_trial_cross_cell(
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      N_STRATA_A = N_STRATA_A, PROP_A = PROP_A, BASE_FACTORS_A = BASE_FACTORS_A,
      N_STRATA_B = N_STRATA_B, PROP_B = PROP_B, BASE_FACTORS_B = BASE_FACTORS_B,
      OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR,
      TREATMENT_RATIO = TREATMENT_RATIO, ENROLL_PERIOD = ENROLL_PERIOD,
      STUDY_DURATION = STUDY_DURATION)
  })

  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

# ------------------------------------------------------------------------------
# 8. Summary statistics
# ------------------------------------------------------------------------------
generate_summary_stats_cross_cell <- function(
  sim_data, param_id = 1, N_POOL = NULL, TARGET_EVENTS = NULL,
  N_STRATA_A = NULL, PROP_A = NULL, N_STRATA_B = NULL, PROP_B = NULL,
  OVERALL_MEDIAN_CTRL = NULL, OVERALL_HR = NULL
) {
  power_strat  <- mean(sim_data$p_strat_design < 0.05, na.rm = TRUE)
  power_simple <- mean(sim_data$p_simple_design < 0.05, na.rm = TRUE)

  hr_mean_strat  <- mean(sim_data$hr_strat, na.rm = TRUE)
  hr_sd_strat    <- sd(sim_data$hr_strat, na.rm = TRUE)
  hr_mean_simple <- mean(sim_data$hr_simple, na.rm = TRUE)
  hr_sd_simple   <- sd(sim_data$hr_simple, na.rm = TRUE)

  n_strat_mean  <- mean(sim_data$n_strat, na.rm = TRUE)
  n_simple_mean <- mean(sim_data$n_simple, na.rm = TRUE)
  events_strat  <- mean(sim_data$events_strat, na.rm = TRUE)
  events_simple <- mean(sim_data$events_simple, na.rm = TRUE)

  imb_median_strat  <- median(sim_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat      <- quantile(sim_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat      <- quantile(sim_data$imbalance_strat, 0.75, na.rm = TRUE)
  imb_median_simple <- median(sim_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple     <- quantile(sim_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple     <- quantile(sim_data$imbalance_simple, 0.75, na.rm = TRUE)

  smd_mean_strat  <- mean(sim_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(sim_data$smd_simple, na.rm = TRUE)

  vrf_actual_mean   <- mean(sim_data$vrf_actual, na.rm = TRUE)
  vrf_expected_mean <- mean(sim_data$vrf_expected, na.rm = TRUE)

  result <- data.frame(
    Simulation_ID = param_id,
    Power_strat_design  = sprintf("%.3f", power_strat),
    Power_simple_design = sprintf("%.3f", power_simple),
    Power_difference    = sprintf("%+.1f%%", (power_strat - power_simple) * 100),
    HR_mean_strat       = sprintf("%.3f (%.3f)", hr_mean_strat, hr_sd_strat),
    HR_mean_simple      = sprintf("%.3f (%.3f)", hr_mean_simple, hr_sd_simple),
    N_strat_mean        = sprintf("%.1f", n_strat_mean),
    N_simple_mean       = sprintf("%.1f", n_simple_mean),
    Events_mean_strat   = sprintf("%.1f", events_strat),
    Events_mean_simple  = sprintf("%.1f", events_simple),
    Arm_imbalance_strat  = sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat),
    Arm_imbalance_simple = sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple),
    SMD_strat_mean      = sprintf("%.3f", smd_mean_strat),
    SMD_simple_mean     = sprintf("%.3f", smd_mean_simple),
    VRF_actual          = sprintf("%.3f", vrf_actual_mean),
    VRF_expected        = sprintf("%.3f", vrf_expected_mean),
    stringsAsFactors = FALSE
  )

  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(TARGET_EVENTS)) result$TARGET_EVENTS <- TARGET_EVENTS
  if (!is.null(N_STRATA_A)) result$N_STRATA_A <- N_STRATA_A
  if (!is.null(PROP_A)) result$PROP_A <- paste(PROP_A, collapse = ",")
  if (!is.null(N_STRATA_B)) result$N_STRATA_B <- N_STRATA_B
  if (!is.null(PROP_B)) result$PROP_B <- paste(PROP_B, collapse = ",")
  if (!is.null(OVERALL_MEDIAN_CTRL)) result$OVERALL_MEDIAN_CTRL <- OVERALL_MEDIAN_CTRL
  if (!is.null(OVERALL_HR)) result$OVERALL_HR <- OVERALL_HR
  result
}

# ------------------------------------------------------------------------------
# 9. Excel output
# ------------------------------------------------------------------------------
generate_excel_output_cross_cell <- function(summary_data, output_prefix = "cross_cell_tte_results") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(output_prefix, "_", timestamp, ".xlsx")

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Combined")
  openxlsx::writeData(wb, "Combined", summary_data, startRow = 1, startCol = 1)

  header_style <- openxlsx::createStyle(
    fontColour = "white", fgFill = "#2E86C1", fontSize = 11, textDecoration = "bold"
  )
  openxlsx::addStyle(wb, "Combined", header_style, rows = 1, cols = 1:ncol(summary_data), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Combined", cols = 1:ncol(summary_data), widths = "auto")
  openxlsx::freezePane(wb, "Combined", firstRow = TRUE, firstCol = TRUE)

  if (nrow(summary_data) > 0) {
    overview <- data.frame(
      Category = c(
        "Total parameter combinations", "Avg Power stratified", "Avg Power simple",
        "Avg Power diff", "Avg VRF actual", "Avg VRF expected"
      ),
      Value = c(
        as.character(nrow(summary_data)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_strat_design), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%+.3f", mean(as.numeric(summary_data$Power_strat_design) -
                                as.numeric(summary_data$Power_simple_design), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$VRF_actual), na.rm = TRUE)),
        sprintf("%.3f", mean(as.numeric(summary_data$VRF_expected), na.rm = TRUE))
      ),
      stringsAsFactors = FALSE
    )
    openxlsx::addWorksheet(wb, "Overview")
    openxlsx::writeData(wb, "Overview", overview, startRow = 1, startCol = 1)
    openxlsx::setColWidths(wb, "Overview", cols = 1:2, widths = c(35, 50))
    openxlsx::addStyle(wb, "Overview", header_style, rows = 1, cols = 1:2, gridExpand = TRUE)
  }

  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  cat(sprintf("Excel saved: %s\n", output_file))
  output_file
}

# ------------------------------------------------------------------------------
# 10. Read parameter grid from Excel
# ------------------------------------------------------------------------------
read_param_grid_cross_cell <- function(excel_path, sheet = 1) {
  if (!file.exists(excel_path)) stop(sprintf("Excel not found: %s", excel_path))
  pg <- openxlsx::read.xlsx(excel_path, sheet = sheet)
  required <- c("TARGET_EVENTS", "N_STRATA_A", "PROP_A", "N_STRATA_B", "PROP_B",
                "OVERALL_MEDIAN_CTRL", "OVERALL_HR")
  missing <- setdiff(required, colnames(pg))
  if (length(missing) > 0) stop(sprintf("Missing columns: %s", paste(missing, collapse = ", ")))
  if (!"Param_ID" %in% colnames(pg)) pg$Param_ID <- 1:nrow(pg)
  if (!"N_POOL" %in% colnames(pg)) pg$N_POOL <- 5000
  if (!"BLOCK_SIZE" %in% colnames(pg)) pg$BLOCK_SIZE <- 4
  if (!"TREATMENT_RATIO" %in% colnames(pg)) pg$TREATMENT_RATIO <- "1,1"
  if (!"ENROLL_PERIOD" %in% colnames(pg)) pg$ENROLL_PERIOD <- 12
  if (!"STUDY_DURATION" %in% colnames(pg)) pg$STUDY_DURATION <- 36
  if (!"N_ITER" %in% colnames(pg)) pg$N_ITER <- 1000
  if (!"N_BATCH" %in% colnames(pg)) pg$N_BATCH <- 10
  cat(sprintf("Loaded %d parameter combinations from %s\n", nrow(pg), excel_path))
  pg
}

# ------------------------------------------------------------------------------
# 11. Main entry: run simulation per batch for a single parameter combination
# ------------------------------------------------------------------------------
run_simulation_per_batch_cross_cell <- function(
  n_iter = 1000, n_batch = 10,
  N_POOL = 5000, TARGET_EVENTS = 66, BLOCK_SIZE = 4,
  N_STRATA_A = 2, PROP_A = c(0.5, 0.5), BASE_FACTORS_A = c(1, 3),
  N_STRATA_B = 3, PROP_B = c(0.2, 0.3, 0.5), BASE_FACTORS_B = c(1, 2, 4),
  OVERALL_MEDIAN_CTRL = 15, OVERALL_HR = 0.5,
  TREATMENT_RATIO = c(1, 1), ENROLL_PERIOD = 12, STUDY_DURATION = 36,
  use_parallel = FALSE, n_cores = max(1, parallel::detectCores() - 1),
  verbose = TRUE
) {
  batch_results <- list()
  batch_summaries <- list()

  for (bid in 1:n_batch) {
    if (verbose) cat(sprintf("\rProcessing batch %d / %d", bid, n_batch))
    res <- if (use_parallel) {
      run_batch_simulation_parallel_cross_cell(
        n_iter = n_iter, batch_id = bid,
        N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
        N_STRATA_A = N_STRATA_A, PROP_A = PROP_A, BASE_FACTORS_A = BASE_FACTORS_A,
        N_STRATA_B = N_STRATA_B, PROP_B = PROP_B, BASE_FACTORS_B = BASE_FACTORS_B,
        OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR,
        TREATMENT_RATIO = TREATMENT_RATIO, ENROLL_PERIOD = ENROLL_PERIOD,
        STUDY_DURATION = STUDY_DURATION, n_cores = n_cores)
    } else {
      run_batch_simulation_cross_cell(
        n_iter = n_iter, batch_id = bid,
        N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
        N_STRATA_A = N_STRATA_A, PROP_A = PROP_A, BASE_FACTORS_A = BASE_FACTORS_A,
        N_STRATA_B = N_STRATA_B, PROP_B = PROP_B, BASE_FACTORS_B = BASE_FACTORS_B,
        OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR,
        TREATMENT_RATIO = TREATMENT_RATIO, ENROLL_PERIOD = ENROLL_PERIOD,
        STUDY_DURATION = STUDY_DURATION)
    }
    summ <- generate_summary_stats_cross_cell(
      sim_data = res, param_id = bid,
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
      N_STRATA_A = N_STRATA_A, PROP_A = PROP_A,
      N_STRATA_B = N_STRATA_B, PROP_B = PROP_B,
      OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR)
    res$Batch_ID <- bid
    summ$Batch_ID <- bid
    batch_results[[bid]] <- res
    batch_summaries[[bid]] <- summ
  }
  if (verbose) cat("\n")

  all_detailed <- do.call(rbind, batch_results)
  all_summary  <- do.call(rbind, batch_summaries)

  overall <- generate_summary_stats_cross_cell(
    sim_data = all_detailed, param_id = "Overall",
    N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
    N_STRATA_A = N_STRATA_A, PROP_A = PROP_A,
    N_STRATA_B = N_STRATA_B, PROP_B = PROP_B,
    OVERALL_MEDIAN_CTRL = OVERALL_MEDIAN_CTRL, OVERALL_HR = OVERALL_HR)
  overall$Batch_ID <- "Overall"
  all_summary <- rbind(all_summary, overall)

  list(detailed_results = all_detailed, batch_summaries = all_summary, overall_summary = overall)
}

# ------------------------------------------------------------------------------
# 12. Parameter grid runner
# ------------------------------------------------------------------------------
run_parameter_grid_per_batch_cross_cell <- function(
  param_grid, n_iter = 1000, n_batch = 10,
  verbose = TRUE, generate_excel = TRUE,
  output_prefix = "cross_cell_tte_results",
  use_parallel = FALSE,
  n_cores = max(1, parallel::detectCores() - 1)
) {
  all_summaries <- list()

  for (i in 1:nrow(param_grid)) {
    if (verbose) cat(sprintf("\n========== Parameter combination %d / %d ==========\n", i, nrow(param_grid)))
    p <- param_grid[i, , drop = FALSE]

    iter <- if ("N_ITER" %in% names(p) && !is.na(p$N_ITER)) as.integer(p$N_ITER) else n_iter
    batch <- if ("N_BATCH" %in% names(p) && !is.na(p$N_BATCH)) as.integer(p$N_BATCH) else n_batch

    sim <- run_simulation_per_batch_cross_cell(
      n_iter = iter, n_batch = batch,
      N_POOL = p$N_POOL, TARGET_EVENTS = p$TARGET_EVENTS, BLOCK_SIZE = p$BLOCK_SIZE,
      N_STRATA_A = p$N_STRATA_A,
      PROP_A = as.numeric(unlist(strsplit(as.character(p$PROP_A), ","))),
      BASE_FACTORS_A = as.numeric(unlist(strsplit(as.character(p$BASE_FACTORS_A), ","))),
      N_STRATA_B = p$N_STRATA_B,
      PROP_B = as.numeric(unlist(strsplit(as.character(p$PROP_B), ","))),
      BASE_FACTORS_B = as.numeric(unlist(strsplit(as.character(p$BASE_FACTORS_B), ","))),
      OVERALL_MEDIAN_CTRL = p$OVERALL_MEDIAN_CTRL, OVERALL_HR = p$OVERALL_HR,
      TREATMENT_RATIO = as.numeric(unlist(strsplit(as.character(p$TREATMENT_RATIO), ","))),
      ENROLL_PERIOD = p$ENROLL_PERIOD, STUDY_DURATION = p$STUDY_DURATION,
      use_parallel = use_parallel, n_cores = n_cores,
      verbose = verbose && !verbose
    )

    summ <- sim$batch_summaries
    summ$Param_ID <- i
    for (col in setdiff(names(param_grid), "Param_ID")) {
      summ[[col]] <- p[[col]]
    }
    all_summaries[[i]] <- summ
  }

  all_summaries <- do.call(rbind, all_summaries)
  rownames(all_summaries) <- NULL

  if (verbose) {
    cat(sprintf("\nAll %d combinations complete! Total summary rows: %d\n", nrow(param_grid), nrow(all_summaries)))
  }

  if (generate_excel) {
    out <- generate_excel_output_cross_cell(all_summaries, output_prefix)
  }

  list(batch_summaries_all = all_summaries, param_grid = param_grid)
}
