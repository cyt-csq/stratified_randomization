# ==============================================================================
# Clinical Study Time-to-Event (TTE) Simulation Framework (Complete)
# ==============================================================================

# 0. Load required libraries
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("pbapply", quietly = TRUE)) install.packages("pbapply")
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
library(openxlsx)
library(pbapply)
library(survival)

# 1. Helper: Generate single stratum random list (fast, no combn)
generate_rand_list <- function(n_per_strata, block_size = 4, ratio = c(1, 1)) {
  ratio_sum <- sum(ratio)
  if (block_size %% ratio_sum != 0) stop(sprintf("block_size %d must be divisible by ratio sum %d", block_size, ratio_sum))
  n_group1 <- as.integer((ratio[1] / ratio_sum) * block_size)
  base_block <- c(rep(1, n_group1), rep(2, block_size - n_group1))
  n_blocks_needed <- ceiling(n_per_strata / block_size)
  rand_vec <- unlist(lapply(seq_len(n_blocks_needed), function(i) sample(base_block)))
  return(rand_vec[seq_len(n_per_strata)])
}

# 3. Helper: Generate stratified random lists
generate_stratified_rand_lists <- function(n_strata, n_per_strata, block_size = 4, ratio = c(1, 1)) {
  rand_lists <- vector("list", n_strata)
  for (s in 1:n_strata) {
    rand_lists[[s]] <- generate_rand_list(n_per_strata, block_size, ratio)
  }
  return(rand_lists)
}

# 4. Helper: Calculate SMD for stratification balance
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

# 5. Helper: Matrix to string conversion
matrix_to_string <- function(mat) {
  paste(apply(mat, 1, function(row) paste(row, collapse = ",")), collapse = ";")
}

string_to_matrix <- function(str, n_strata) {
  rows <- strsplit(str, ";")[[1]]
  mat <- matrix(as.numeric(unlist(strsplit(rows, ","))),
                nrow = n_strata, ncol = 2, byrow = TRUE)
  return(mat)
}

# 5.1 Helper: Generate MEDIAN_MATRIX from parameters (when MEDIAN_MATRIX_STR is missing)
generate_median_matrix_from_params <- function(n_strata, hr, trt_median, prognostic_hr, strata_props) {
  base_trt  <- trt_median
  base_ctrl <- base_trt * hr
  
  if (abs(prognostic_hr - 1) < 1e-6) {
    row_str <- sprintf("%.4f,%.4f", base_ctrl, base_trt)
    return(paste(rep(row_str, n_strata), collapse = ";"))
  }
  
  base_factors <- seq(from = 1, to = prognostic_hr, length.out = n_strata)
  
  # Exact numerical solution for lambda: find lambda such that mixture median = base
  target_fn <- function(lambda) {
    sum(strata_props * exp(-log(2) * base_factors * lambda)) - 0.5
  }
  lambda <- uniroot(target_fn, interval = c(0.01, 100))$root
  
  strata_factors <- base_factors * lambda
  
  ctrl_vals <- base_ctrl / strata_factors
  trt_vals  <- base_trt / strata_factors
  
  rows <- sprintf("%.4f,%.4f", ctrl_vals, trt_vals)
  paste(rows, collapse = ";")
}

# 5.5 Helper: Calculate expected VRF based on exponential distribution variance
calculate_expected_vrf <- function(MEDIAN_MATRIX, STRATA_PROPORTIONS) {
  medians_ctrl <- MEDIAN_MATRIX[, 1]
  rates <- log(2) / medians_ctrl
  means <- 1 / rates
  variances <- 1 / (rates^2)
  w <- STRATA_PROPORTIONS / sum(STRATA_PROPORTIONS)
  pooled_within_var <- sum(w * variances)
  if (pooled_within_var <= 0) return(1)
  overall_mean <- sum(w * means)
  between_var <- sum(w * (means - overall_mean)^2)
  return((pooled_within_var + between_var) / pooled_within_var)
}

# 5.6 Read parameter grid from Excel
read_param_grid_from_excel <- function(excel_path, sheet = 1) {
  if (!file.exists(excel_path)) {
    stop(sprintf("Excel file does not exist: %s", excel_path))
  }
  param_grid <- openxlsx::read.xlsx(excel_path, sheet = sheet)
  required_cols <- c("TARGET_EVENTS", "STRATA_LEVELS", "STRATA_PROPORTIONS",
                     "TREATMENT_RATIO", "MEDIAN_MATRIX_STR")
  missing_cols <- setdiff(required_cols, colnames(param_grid))
  if (length(missing_cols) > 0) {
    stop(sprintf("Excel parameter table missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  if (!"Param_ID" %in% colnames(param_grid)) {
    param_grid$Param_ID <- 1:nrow(param_grid)
  }
  if (!"N_POOL" %in% colnames(param_grid)) param_grid$N_POOL <- 5000
  if (!"BLOCK_SIZE" %in% colnames(param_grid)) param_grid$BLOCK_SIZE <- 4
  if (!"ENROLL_PERIOD" %in% colnames(param_grid)) param_grid$ENROLL_PERIOD <- 12
  if (!"STUDY_DURATION" %in% colnames(param_grid)) param_grid$STUDY_DURATION <- 36
  if (!"MEAN_SCENARIO" %in% colnames(param_grid)) param_grid$MEAN_SCENARIO <- "Custom"
  if (!"TREATMENT_RATIO_NAME" %in% colnames(param_grid)) {
    param_grid$TREATMENT_RATIO_NAME <- param_grid$TREATMENT_RATIO
  }
  cat(sprintf("Loaded %d parameter combinations from Excel: %s\n", nrow(param_grid), excel_path))
  return(param_grid)
}

# 6. Core function: Execute single trial (TTE endpoint, Unstratified Cox + Stratified Cox)
run_single_trial <- function(
  N_POOL = 5000,
  TARGET_EVENTS = 66,
  BLOCK_SIZE = 4,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12,
  STUDY_DURATION = 36,
  HAS_PROGNOSTIC = TRUE,
  PROGNOSTIC_HR = 1,
  LATENT_LEVELS = 1,
  LATENT_PROPORTIONS = c(1),
  LATENT_HR = 1,
  HR = 0.5,
  TRT_MEDIAN = 30
) {
  # Validate parameters
  if (abs(sum(STRATA_PROPORTIONS) - 1) > 1e-6) {
    stop("STRATA_PROPORTIONS must sum to 1")
  }
  if (length(STRATA_PROPORTIONS) != STRATA_LEVELS) {
    stop("Length of STRATA_PROPORTIONS must equal STRATA_LEVELS")
  }
  
  # Validate or create MEDIAN_MATRIX
  if (is.null(MEDIAN_MATRIX) || any(is.na(MEDIAN_MATRIX))) {
    if (!is.na(PROGNOSTIC_HR) && PROGNOSTIC_HR > 1) {
      # Dynamic calculation using Scheme B (max-ratio + lambda scaling)
      base_trt  <- TRT_MEDIAN
      base_ctrl <- base_trt * HR
      base_factors <- seq(from = 1, to = PROGNOSTIC_HR, length.out = STRATA_LEVELS)
      lambda <- sum(STRATA_PROPORTIONS / base_factors)
      strata_factors <- base_factors * lambda
      MEDIAN_MATRIX <- matrix(
        c(base_ctrl / strata_factors, base_trt / strata_factors),
        nrow = STRATA_LEVELS, ncol = 2, byrow = FALSE
      )
    } else {
      base_trt  <- TRT_MEDIAN
      base_ctrl <- base_trt * HR
      MEDIAN_MATRIX <- matrix(c(base_ctrl, base_trt), nrow = STRATA_LEVELS, ncol = 2, byrow = TRUE)
    }
  }
  if (nrow(MEDIAN_MATRIX) != STRATA_LEVELS) {
    stop(sprintf("MEDIAN_MATRIX rows (%d) must equal STRATA_LEVELS (%d)",
                 nrow(MEDIAN_MATRIX), STRATA_LEVELS))
  }
  if (ncol(MEDIAN_MATRIX) != 2) {
    stop("MEDIAN_MATRIX must have 2 columns (corresponding to 2 treatment groups)")
  }
  if (any(is.na(MEDIAN_MATRIX)) || any(MEDIAN_MATRIX <= 0)) {
    stop("All values in MEDIAN_MATRIX must be positive and non-missing")
  }
  
  # Validate latent parameters
  if (!is.na(LATENT_LEVELS) && LATENT_LEVELS > 1) {
    if (any(is.na(LATENT_PROPORTIONS)) || abs(sum(LATENT_PROPORTIONS) - 1) > 1e-6) {
      stop("LATENT_PROPORTIONS must sum to 1")
    }
    if (length(LATENT_PROPORTIONS) != LATENT_LEVELS) {
      stop("Length of LATENT_PROPORTIONS must equal LATENT_LEVELS")
    }
    if (is.na(LATENT_HR) || LATENT_HR <= 0) {
      stop("LATENT_HR must be positive")
    }
  }
  
  # Random list size (large enough for event-driven)
  RAND_LIST_SIZE_PER_STRATA <- ceiling(N_POOL / BLOCK_SIZE) * BLOCK_SIZE
  RAND_LIST_SIZE_TOTAL <- ceiling(N_POOL / BLOCK_SIZE) * BLOCK_SIZE
  
  # Step 1: Generate pool with prognostic factor and latent factor
  pool_strata <- sample(1:STRATA_LEVELS, size = N_POOL,
                        replace = TRUE, prob = STRATA_PROPORTIONS)
  pool_latent <- if (!is.na(LATENT_LEVELS) && LATENT_LEVELS > 1) {
    sample(1:LATENT_LEVELS, size = N_POOL, replace = TRUE, prob = LATENT_PROPORTIONS)
  } else {
    rep(1, N_POOL)
  }
  
  # Step 2: Generate randomization lists
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
  
  enrolled_trt_strat <- numeric(N_POOL)
  enrolled_trt_simple <- numeric(N_POOL)
  enrolled_strata <- numeric(N_POOL)
  total_enrolled <- 0
  
  # Step 3: Simulate enrollment
  for (i in 1:N_POOL) {
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
  
  # Step 4: Calculate SMD for stratification balance
  strata_balance_smd_strat <- calculate_strata_smd(enrolled_trt_strat, enrolled_strata_strat)
  strata_balance_smd_unstrat <- calculate_strata_smd(enrolled_trt_simple, enrolled_strata_simple)
  
  # Step 5: Generate survival data (vectorized, with latent factor adjustment)
  enrolled_latent <- pool_latent[1:total_enrolled]
  
  generate_tte_data <- function(enrolled_trt, enrolled_strata, enrolled_latent) {
    n <- length(enrolled_trt)
    enroll_time <- runif(n, 0, ENROLL_PERIOD)
    # Base median from MEDIAN_MATRIX
    base_median <- MEDIAN_MATRIX[cbind(enrolled_strata, enrolled_trt)]
    # Latent adjustment: arithmetic progression (same logic as prognostic HR)
    # LHR=1 -> step=0 -> no effect; LHR=2 -> step=1; LHR=3 -> step=3; LHR=5 -> step=5
    if (!is.na(LATENT_LEVELS) && LATENT_LEVELS > 1 && !is.na(LATENT_HR) && LATENT_HR > 1) {
      step <- if (LATENT_HR == 2) 1 else LATENT_HR
      latent_factor <- 1 + (enrolled_latent - 1) * step
      adjusted_median <- base_median / latent_factor
    } else {
      adjusted_median <- base_median
    }
    rate_vec <- log(2) / adjusted_median
    survival_time <- rexp(n, rate = rate_vec)
    censor_time <- STUDY_DURATION - enroll_time
    event_time <- pmin(survival_time, censor_time)
    event_status <- as.numeric(survival_time <= censor_time)
    data.frame(
      enroll_time = enroll_time,
      survival_time = survival_time,
      censor_time = censor_time,
      event_time = event_time,
      event_status = event_status,
      stringsAsFactors = FALSE
    )
  }
  tte_data_strat <- generate_tte_data(enrolled_trt_strat, enrolled_strata_strat, enrolled_latent)
  tte_data_simple <- generate_tte_data(enrolled_trt_simple, enrolled_strata_simple, enrolled_latent)
  
  # Step 6: Event-driven truncation (each design cuts off independently at TARGET_EVENTS)
  # Stratified design
  ord_strat <- order(tte_data_strat$enroll_time)
  cum_events_strat <- cumsum(tte_data_strat$event_status[ord_strat])
  if (max(cum_events_strat) < TARGET_EVENTS) {
    stop(sprintf("N_POOL (%d) insufficient for TARGET_EVENTS (%d) in stratified design", N_POOL, TARGET_EVENTS))
  }
  idx_strat <- min(which(cum_events_strat >= TARGET_EVENTS))
  sel_strat <- ord_strat[1:idx_strat]
  n_final <- length(sel_strat)

  # Simple design
  ord_simple <- order(tte_data_simple$enroll_time)
  cum_events_simple <- cumsum(tte_data_simple$event_status[ord_simple])
  if (max(cum_events_simple) < TARGET_EVENTS) {
    stop(sprintf("N_POOL (%d) insufficient for TARGET_EVENTS (%d) in simple design", N_POOL, TARGET_EVENTS))
  }
  idx_simple <- min(which(cum_events_simple >= TARGET_EVENTS))
  sel_simple <- ord_simple[1:idx_simple]

  trunc_strat <- list(
    tte_data = tte_data_strat[sel_strat, ],
    enrolled_trt = enrolled_trt_strat[sel_strat],
    enrolled_strata = enrolled_strata_strat[sel_strat])
  trunc_simple <- list(
    tte_data = tte_data_simple[sel_simple, ],
    enrolled_trt = enrolled_trt_simple[sel_simple],
    enrolled_strata = enrolled_strata_simple[sel_simple])

  # Step 7: Statistical analysis (TTE - Cox PH)
  analyze_dataset_tte <- function(tte_data, enrolled_trt, enrolled_strata_A, is_stratified_design = TRUE) {
    n1 <- sum(enrolled_trt == 1)
    n2 <- sum(enrolled_trt == 2)
    imbalance <- abs(n1/n2)
    total_events <- sum(tte_data$event_status)
    total_censored <- sum(1 - tte_data$event_status)
    censor_rate <- mean(1 - tte_data$event_status)
    median_trt1 <- median(tte_data$event_time[enrolled_trt == 1])
    median_trt2 <- median(tte_data$event_time[enrolled_trt == 2])

    # Count strata by event numbers
    events_per_strata <- tapply(tte_data$event_status, enrolled_strata_A, sum)
    n_strata_zero_events <- sum(events_per_strata == 0, na.rm = TRUE)
    n_strata_few_events <- sum(events_per_strata > 0 & events_per_strata < 5, na.rm = TRUE)

    res <- list(
      n1 = n1, n2 = n2, imbalance = imbalance,
      total_events = total_events, total_censored = total_censored,
      censor_rate = censor_rate,
      median_survival_trt1 = median_trt1, median_survival_trt2 = median_trt2,
      n_strata_zero_events = n_strata_zero_events,
      n_strata_few_events = n_strata_few_events,
      hr_unstrat = NA, hr_ci_lower_unstrat = NA, hr_ci_upper_unstrat = NA,
      p_unstrat = NA, se_unstrat = NA,
      hr_strat = NA, hr_ci_lower_strat = NA, hr_ci_upper_strat = NA,
      p_strat = NA, se_strat = NA,
      p_logrank_unstrat = NA,
      p_logrank_strat = NA,
      chisq_logrank_unstrat = NA,
      chisq_logrank_strat = NA,
      vrf_actual = NA)

    df <- data.frame(
      time = tte_data$event_time, status = tte_data$event_status,
      trt = enrolled_trt, strata = enrolled_strata_A,
      stringsAsFactors = FALSE)

    if (n1 >= 1 && n2 >= 1 && total_events >= 3) {
      cox_unstrat <- tryCatch(coxph(Surv(time, status) ~ trt, data = df), error = function(e) NULL)
      if (!is.null(cox_unstrat)) {
        sm <- summary(cox_unstrat)
        res$hr_unstrat <- as.numeric(sm$conf.int[1, 1])
        res$hr_ci_lower_unstrat <- as.numeric(sm$conf.int[1, 3])
        res$hr_ci_upper_unstrat <- as.numeric(sm$conf.int[1, 4])
        res$p_unstrat <- sm$coefficients[1, 5]
        res$se_unstrat <- sm$coefficients[1, 3]
      }
      lr_unstrat <- tryCatch(survdiff(Surv(time, status) ~ trt, data = df), error = function(e) NULL)
      if (!is.null(lr_unstrat)) {
        res$p_logrank_unstrat <- 1 - pchisq(lr_unstrat$chisq, df = length(lr_unstrat$n) - 1)
        res$chisq_logrank_unstrat <- lr_unstrat$chisq
      }
    }

    if (is_stratified_design && length(unique(enrolled_strata_A)) >= 2 && total_events >= 3) {
      cox_strat <- tryCatch(coxph(Surv(time, status) ~ trt + strata(strata), data = df), error = function(e) NULL)
      if (!is.null(cox_strat)) {
        sm <- summary(cox_strat)
        res$hr_strat <- as.numeric(sm$conf.int[1, 1])
        res$hr_ci_lower_strat <- as.numeric(sm$conf.int[1, 3])
        res$hr_ci_upper_strat <- as.numeric(sm$conf.int[1, 4])
        res$p_strat <- sm$coefficients[1, 5]
        res$se_strat <- sm$coefficients[1, 3]
      }
      lr_strat <- tryCatch(survdiff(Surv(time, status) ~ trt + strata(strata), data = df), error = function(e) NULL)
      if (!is.null(lr_strat)) {
        res$p_logrank_strat <- 1 - pchisq(lr_strat$chisq, df = length(lr_strat$n) - 1)
        res$chisq_logrank_strat <- lr_strat$chisq
      }
    }
    # Actual VRF: stratified chisq / unstratified chisq
    if (!is.na(res$chisq_logrank_strat) && !is.na(res$chisq_logrank_unstrat) && res$chisq_logrank_unstrat > 0) {
      res$vrf_actual <- res$chisq_logrank_strat / res$chisq_logrank_unstrat
    }
    return(res)
  }

  res_strat_design <- analyze_dataset_tte(trunc_strat$tte_data, trunc_strat$enrolled_trt, trunc_strat$enrolled_strata, TRUE)
  res_simple_design <- analyze_dataset_tte(trunc_simple$tte_data, trunc_simple$enrolled_trt, trunc_simple$enrolled_strata, FALSE)

  vrf_expected <- calculate_expected_vrf(MEDIAN_MATRIX, STRATA_PROPORTIONS)

  strata_median_str <- paste(
    sprintf("%.4f,%.4f", MEDIAN_MATRIX[, 1], MEDIAN_MATRIX[, 2]),
    collapse = ";"
  )

  list(
    summary = data.frame(
      p_strat_design = res_strat_design$p_logrank_strat,
      vrf_actual = res_strat_design$vrf_actual,
      vrf_expected = vrf_expected,
      hr_strat_design = res_strat_design$hr_strat,
      se_strat_design = res_strat_design$se_strat,
      median_trt1_strat = res_strat_design$median_survival_trt1,
      median_trt2_strat = res_strat_design$median_survival_trt2,
      events_total_strat = res_strat_design$total_events,
      events_trt1_strat = sum(trunc_strat$tte_data$event_status[trunc_strat$enrolled_trt == 1]),
      events_trt2_strat = sum(trunc_strat$tte_data$event_status[trunc_strat$enrolled_trt == 2]),
      n_strata_zero_events_strat = res_strat_design$n_strata_zero_events,
      n_strata_few_events_strat = res_strat_design$n_strata_few_events,
      p_strat_unstrat = res_strat_design$p_logrank_unstrat,
      hr_strat_unstrat = res_strat_design$hr_unstrat,
      se_strat_unstrat = res_strat_design$se_unstrat,
      median_trt1_strat_unstrat = res_strat_design$median_survival_trt1,
      median_trt2_strat_unstrat = res_strat_design$median_survival_trt2,
      p_simple_design = res_simple_design$p_logrank_unstrat,
      hr_simple_design = res_simple_design$hr_unstrat,
      se_simple_design = res_simple_design$se_unstrat,
      median_trt1_simple = res_simple_design$median_survival_trt1,
      median_trt2_simple = res_simple_design$median_survival_trt2,
      events_total_simple = res_simple_design$total_events,
      events_trt1_simple = sum(trunc_simple$tte_data$event_status[trunc_simple$enrolled_trt == 1]),
      events_trt2_simple = sum(trunc_simple$tte_data$event_status[trunc_simple$enrolled_trt == 2]),
      n_strata_zero_events_simple = res_simple_design$n_strata_zero_events,
      n_strata_few_events_simple = res_simple_design$n_strata_few_events,
      censor_rate_strat = mean(1 - trunc_strat$tte_data$event_status),
      censor_rate_simple = mean(1 - trunc_simple$tte_data$event_status),
      smd_strat = strata_balance_smd_strat$mean_smd,
      smd_simple = strata_balance_smd_unstrat$mean_smd,
      n_strat = length(sel_strat),
      n_simple = length(sel_simple),
      imbalance_strat = res_strat_design$imbalance,
      imbalance_simple = res_simple_design$imbalance,
      strata_median_str = strata_median_str,
      STRATA_LEVELS = STRATA_LEVELS, TARGET_EVENTS = TARGET_EVENTS,
      stringsAsFactors = FALSE),
    stratified_results = res_strat_design,
    simple_results = res_simple_design)
}

# 7. Batch simulation function: multiple iterations for a single parameter combination
run_batch_simulation <- function(
  n_iter = 10,
  batch_id = 10,
  N_POOL = 5000,
  TARGET_EVENTS = 66,
  BLOCK_SIZE = 4,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12,
  STUDY_DURATION = 36,
  HAS_PROGNOSTIC = TRUE,
  LATENT_LEVELS = 1,
  LATENT_PROPORTIONS = c(1),
  LATENT_HR = 1,
  HR = 0.5,
  TRT_MEDIAN = 30
) {
  set.seed(123 + batch_id * 1000)
  
  results <- replicate(n_iter, {
    run_single_trial(
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO, MEDIAN_MATRIX = MEDIAN_MATRIX,
      ENROLL_PERIOD = ENROLL_PERIOD, STUDY_DURATION = STUDY_DURATION,
      HAS_PROGNOSTIC = HAS_PROGNOSTIC,
      LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
      HR = HR, TRT_MEDIAN = TRT_MEDIAN
    )
  }, simplify = FALSE)
  
  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

# 8. Generate summary statistics (TTE version, for Excel output)
generate_summary_stats_old <- function(
  simulation_data,
  param_id = 1,
  N_POOL = NULL,
  TARGET_EVENTS = NULL,
  BLOCK_SIZE = NULL,
  STRATA_LEVELS = NULL,
  STRATA_PROPORTIONS = NULL,
  TREATMENT_RATIO = NULL,
  TREATMENT_RATIO_NAME = NULL,
  MEAN_SCENARIO = NULL,
  MEDIAN_MATRIX_STR = NULL
) {
  
  # 1. Treatment group imbalance
  imb_median_strat <- median(simulation_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat <- quantile(simulation_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat <- quantile(simulation_data$imbalance_strat, 0.75, na.rm = TRUE)
  arm_imbalance_strat <- sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat)
  
  imb_median_simple <- median(simulation_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple <- quantile(simulation_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple <- quantile(simulation_data$imbalance_simple, 0.75, na.rm = TRUE)
  arm_imbalance_simple <- sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple)
  
  # 2. Power (Cox PH)
  power_strat_design <- mean(simulation_data$p_strat_design < 0.05, na.rm = TRUE)
  power_strat_unstrat <- mean(simulation_data$p_strat_unstrat < 0.05, na.rm = TRUE)
  power_simple_design <- mean(simulation_data$p_simple_design < 0.05, na.rm = TRUE)
  # 2b. Power (Logrank)
  power_logrank_strat_design <- mean(simulation_data$p_logrank_strat_design < 0.05, na.rm = TRUE)
  power_logrank_strat_unstrat <- mean(simulation_data$p_logrank_strat_unstrat < 0.05, na.rm = TRUE)
  power_logrank_simple_design <- mean(simulation_data$p_logrank_simple_design < 0.05, na.rm = TRUE)
  
  # 3. Effect estimate: HR
  hr_mean_strat <- mean(simulation_data$hr_strat_design, na.rm = TRUE)
  hr_sd_strat <- sd(simulation_data$hr_strat_design, na.rm = TRUE)
  hr_mean_strat_unstrat <- mean(simulation_data$hr_strat_unstrat, na.rm = TRUE)
  hr_sd_strat_unstrat <- sd(simulation_data$hr_strat_unstrat, na.rm = TRUE)
  hr_mean_simple <- mean(simulation_data$hr_simple_design, na.rm = TRUE)
  hr_sd_simple <- sd(simulation_data$hr_simple_design, na.rm = TRUE)
  
  # 4. SE
  se_mean_strat <- mean(simulation_data$se_strat_design, na.rm = TRUE)
  se_sd_strat <- sd(simulation_data$se_strat_design, na.rm = TRUE)
  se_mean_strat_unstrat <- mean(simulation_data$se_strat_unstrat, na.rm = TRUE)
  se_sd_strat_unstrat <- sd(simulation_data$se_strat_unstrat, na.rm = TRUE)
  se_mean_simple <- mean(simulation_data$se_simple_design, na.rm = TRUE)
  se_sd_simple <- sd(simulation_data$se_simple_design, na.rm = TRUE)
  
  # 5. CI coverage (HR < 1, check if CI covers true HR; assume true HR from MEDIAN_MATRIX)
  # Note: true HR is not directly available here, so we compute proportion of CIs excluding 1
  # This is handled as a proxy metric
  
  # 6. Median survival
  med1_mean_strat <- mean(simulation_data$median_trt1_strat, na.rm = TRUE)
  med2_mean_strat <- mean(simulation_data$median_trt2_strat, na.rm = TRUE)
  med1_mean_strat_unstrat <- mean(simulation_data$median_trt1_strat_unstrat, na.rm = TRUE)
  med2_mean_strat_unstrat <- mean(simulation_data$median_trt2_strat_unstrat, na.rm = TRUE)
  med1_mean_simple <- mean(simulation_data$median_trt1_simple, na.rm = TRUE)
  med2_mean_simple <- mean(simulation_data$median_trt2_simple, na.rm = TRUE)
  
  # 7. Event counts and censor rates
  events_mean_strat <- mean(simulation_data$events_total_strat, na.rm = TRUE)
  censor_mean_strat <- mean(simulation_data$censor_rate_strat, na.rm = TRUE)
  events_mean_simple <- mean(simulation_data$events_total_simple, na.rm = TRUE)
  censor_mean_simple <- mean(simulation_data$censor_rate_simple, na.rm = TRUE)
  
  # 8. Comparison metrics
  power_diff <- power_strat_design - power_simple_design
  power_diff_pct <- sprintf("%+.1f%%", power_diff * 100)
  power_diff_unstrat <- power_strat_unstrat - power_simple_design
  power_diff_unstrat_pct <- sprintf("%+.1f%%", power_diff_unstrat * 100)
  
  # 9. SMD
  smd_mean_strat <- mean(simulation_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(simulation_data$smd_simple, na.rm = TRUE)
  smd_summary <- sprintf("%.3f / %.3f", smd_mean_strat, smd_mean_simple)
  prop_smd_gt_015_strat <- mean(simulation_data$smd_strat > 0.15, na.rm = TRUE)
  prop_smd_gt_015_simple <- mean(simulation_data$smd_simple > 0.15, na.rm = TRUE)
  smd_imbalance_prop <- sprintf("%.1f%% / %.1f%%",
                                 prop_smd_gt_015_strat * 100,
                                 prop_smd_gt_015_simple * 100)
  
  # Assemble result
  result <- data.frame(
    Simulation_ID = param_id,
    TARGET_EVENTS = ifelse(is.null(TARGET_EVENTS), NA, TARGET_EVENTS),
    BLOCK_SIZE = ifelse(is.null(BLOCK_SIZE), NA, BLOCK_SIZE),
    STRATA_LEVELS = ifelse(is.null(STRATA_LEVELS), NA, STRATA_LEVELS),
    
    Arm_imbalance_strat = arm_imbalance_strat,
    Arm_imbalance_simple = arm_imbalance_simple,
    
    Power_Cox_strat_design = sprintf("%.3f", power_strat_design),
    Power_Cox_strat_unstrat = sprintf("%.3f", power_strat_unstrat),
    Power_Cox_simple_design = sprintf("%.3f", power_simple_design),
    Power_Logrank_strat_design = sprintf("%.3f", power_logrank_strat_design),
    Power_Logrank_strat_unstrat = sprintf("%.3f", power_logrank_strat_unstrat),
    Power_Logrank_simple_design = sprintf("%.3f", power_logrank_simple_design),
    Power_difference_Cox = power_diff_pct,
    Power_diff_strat_unstrat_vs_simple_Cox = power_diff_unstrat_pct,
    
    HR_mean_strat = sprintf("%.2f (%.2f)", hr_mean_strat, hr_sd_strat),
    HR_mean_strat_unstrat = sprintf("%.2f (%.2f)", hr_mean_strat_unstrat, hr_sd_strat_unstrat),
    HR_mean_simple = sprintf("%.2f (%.2f)", hr_mean_simple, hr_sd_simple),
    
    SE_mean_strat = sprintf("%.3f (%.3f)", se_mean_strat, se_sd_strat),
    SE_mean_strat_unstrat = sprintf("%.3f (%.3f)", se_mean_strat_unstrat, se_sd_strat_unstrat),
    SE_mean_simple = sprintf("%.3f (%.3f)", se_mean_simple, se_sd_simple),
    
    Median_survival_trt1_strat = sprintf("%.1f", med1_mean_strat),
    Median_survival_trt2_strat = sprintf("%.1f", med2_mean_strat),
    Median_survival_trt1_strat_unstrat = sprintf("%.1f", med1_mean_strat_unstrat),
    Median_survival_trt2_strat_unstrat = sprintf("%.1f", med2_mean_strat_unstrat),
    Median_survival_trt1_simple = sprintf("%.1f", med1_mean_simple),
    Median_survival_trt2_simple = sprintf("%.1f", med2_mean_simple),
    
    Events_mean_strat = sprintf("%.1f", events_mean_strat),
    Censor_rate_strat = sprintf("%.1f%%", censor_mean_strat * 100),
    Events_mean_simple = sprintf("%.1f", events_mean_simple),
    Censor_rate_simple = sprintf("%.1f%%", censor_mean_simple * 100),
    
    SMD_strat_vs_simple = smd_summary,
    Prop_SMD_gt_015 = smd_imbalance_prop,
    
    stringsAsFactors = FALSE
  )
  
  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(STRATA_PROPORTIONS)) result$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ":")
  if (!is.null(TREATMENT_RATIO)) result$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  if (!is.null(TREATMENT_RATIO_NAME)) result$TREATMENT_RATIO_NAME <- TREATMENT_RATIO_NAME
  if (!is.null(MEAN_SCENARIO)) result$MEAN_SCENARIO <- MEAN_SCENARIO
  if (!is.null(MEDIAN_MATRIX_STR)) result$MEDIAN_MATRIX_STR <- MEDIAN_MATRIX_STR
  
  return(result)
}

# 8.5 Generate Excel output (TTE version)
generate_excel_output_tte <- function(summary_data, output_prefix = "simulation_tte_results") {
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(output_prefix, "_", timestamp, ".xlsx")
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("          Clinical Trial TTE Simulation Report\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  wb <- openxlsx::createWorkbook()
  
  # Sheet 1: Combined
  cat("[1/3] Creating worksheet: Combined\n")
  
  desired_order <- c(
    "Simulation_ID", "Param_ID", "Batch_ID",
    "N_POOL", "TARGET_EVENTS", "BLOCK_SIZE",
    "STRATA_LEVELS", "STRATA_PROPORTIONS",
    "TREATMENT_RATIO", "TREATMENT_RATIO_NAME",
    "MEAN_SCENARIO", "MEDIAN_MATRIX_STR", "PROGNOSTIC_HR", "Strata_Medians", "TRUE_HR",
    "Arm_imbalance_strat", "Power_strat_design",
    "HR_mean_strat", "SE_mean_strat", "Median_survival_trt1_strat", "Median_survival_trt2_strat",
    "Events_mean_strat", "Censor_rate_strat",
    "N_strata_zero_strat", "N_strata_few_strat",
    "Power_strat_unstrat",
    "HR_mean_strat_unstrat", "SE_mean_strat_unstrat",
    "Median_survival_trt1_strat_unstrat", "Median_survival_trt2_strat_unstrat",
    "Events_mean_strat_unstrat", "Censor_rate_strat_unstrat",
    "Arm_imbalance_simple", "Power_simple_design",
    "HR_mean_simple", "SE_mean_simple", "Median_survival_trt1_simple", "Median_survival_trt2_simple",
    "Events_mean_simple", "Censor_rate_simple",
    "N_strata_zero_simple", "N_strata_few_simple",
    "N_strat_mean", "N_simple_mean",
    "VRF_actual", "VRF_expected",
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
  param_cols <- sum(grepl("^(N_|TARGET_|BLOCK_|STRATA_|TREATMENT_|MEAN_|MEDIAN_)", combined_cols))
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
  
  cat(sprintf("  Combined worksheet created (%d rows x %d cols)\n", nrow(combined_df), ncol(combined_df)))
  
  # Sheet 2: Column_Description
  cat("[2/3] Creating worksheet: Column_Description\n")
  
  col_descriptions <- data.frame(
    Column_Name = c(
      "Simulation_ID", "Param_ID", "Batch_ID",
      "N_POOL", "TARGET_EVENTS", "BLOCK_SIZE", "STRATA_LEVELS", "STRATA_PROPORTIONS",
      "TREATMENT_RATIO", "TREATMENT_RATIO_NAME", "MEAN_SCENARIO", "MEDIAN_MATRIX_STR",
      "Arm_imbalance_strat", "Power_strat_design",
      "HR_mean_strat", "SE_mean_strat", "Median_survival_trt1_strat", "Median_survival_trt2_strat",
      "Events_mean_strat", "Censor_rate_strat",
      "N_strata_zero_strat", "N_strata_few_strat",
      "Power_strat_unstrat",
      "HR_mean_strat_unstrat", "SE_mean_strat_unstrat",
      "Median_survival_trt1_strat_unstrat", "Median_survival_trt2_strat_unstrat",
      "Events_mean_strat_unstrat", "Censor_rate_strat_unstrat",
      "Arm_imbalance_simple", "Power_simple_design",
      "HR_mean_simple", "SE_mean_simple", "Median_survival_trt1_simple", "Median_survival_trt2_simple",
      "Events_mean_simple", "Censor_rate_simple",
      "N_strata_zero_simple", "N_strata_few_simple",
      "N_strat_mean", "N_simple_mean",
      "VRF_actual", "VRF_expected",
      "SMD_strat_vs_simple",
      "Power_difference", "Power_diff_strat_unstrat_vs_simple"
    ),
    Description = c(
      "Simulation combination ID", "Parameter combination ID", "Batch ID",
      "Initial subject pool size", "Target number of events", "Block size", "Number of strata", "Strata proportions",
      "Treatment group allocation ratio", "Treatment ratio name", "Scenario name", "Median matrix string",
      "Stratified design arm imbalance: median (Q1-Q3)",
      "Power (stratified randomization + stratified Logrank)",
      "Stratified design HR mean (SD)", "Stratified design SE mean (SD)",
      "Stratified design median survival trt1", "Stratified design median survival trt2",
      "Stratified design mean event count", "Stratified design censor rate",
      "Mean number of strata with 0 events (stratified design)",
      "Mean number of strata with 1-4 events (stratified design)",
      "Power (stratified randomization + unstratified Logrank)",
      "Stratified design unstratified HR mean (SD)", "Stratified design unstratified SE mean (SD)",
      "Stratified unstratified median survival trt1", "Stratified unstratified median survival trt2",
      "Stratified unstratified mean event count", "Stratified unstratified censor rate",
      "Simple design arm imbalance: median (Q1-Q3)",
      "Power (simple randomization + unstratified Logrank)",
      "Simple design HR mean (SD)", "Simple design SE mean (SD)",
      "Simple design median survival trt1", "Simple design median survival trt2",
      "Simple design mean event count", "Simple design censor rate",
      "Mean number of strata with 0 events (simple design)",
      "Mean number of strata with 1-4 events (simple design)",
      "Mean enrolled sample size (stratified design)",
      "Mean enrolled sample size (simple design)",
      "Actual VRF: stratified chisq / unstratified chisq (mean)",
      "Expected VRF: (pooled within-var + between-var) / pooled within-var",
      "SMD comparison: stratified / simple",
      "Power difference (stratified - simple)",
      "Power difference (stratified unstratified - simple)"
    ),
    stringsAsFactors = FALSE
  )
  
  col_descriptions <- col_descriptions[col_descriptions$Column_Name %in% colnames(combined_df), ]
  openxlsx::addWorksheet(wb, "Column_Description")
  openxlsx::writeData(wb, "Column_Description", col_descriptions, startRow = 1, startCol = 1)
  openxlsx::setColWidths(wb, "Column_Description", cols = 1:3, widths = c(30, 70, 20))
  openxlsx::addStyle(wb, "Column_Description", header_style, rows = 1, cols = 1:3, gridExpand = TRUE)
  
  cat(sprintf("  Column_Description worksheet created (%d rows)\n", nrow(col_descriptions)))
  
  # Sheet 3: Summary_Overview
  if (nrow(summary_data) > 0) {
    cat("[3/3] Creating worksheet: Summary_Overview\n")
    
    overview_data <- data.frame(
      Category = c(
        "Total parameter combinations", "Batch range", "Event target", "Block size", "Strata levels", "Treatment ratio",
        "Avg Power stratified Cox", "Avg Power stratified unstratified", "Avg Power simple",
        "Avg Power diff (stratified Cox - simple)", "Avg Power diff (unstratified - simple)"
      ),
      Values = c(
        as.character(nrow(summary_data)),
        paste(range(summary_data$Batch_ID), collapse = " - "),
        paste(unique(summary_data$TARGET_EVENTS), collapse = ", "),
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
    
    cat(sprintf("  Summary_Overview worksheet created (%d rows)\n", nrow(overview_data)))
  } else {
    cat("[3/3] Skipped: Summary_Overview (no data)\n")
  }
  
  tryCatch({
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat(sprintf("Excel report saved successfully: %s\n", output_file))
    cat(strrep("=", 70), "\n\n", sep = "")
    return(output_file)
  }, error = function(e) {
    warning(sprintf("Error saving Excel file: %s", e$message))
    return(NULL)
  })
}

run_batch_simulation_tte <- function(
  n_iter = 1000, batch_id = 1, N_POOL = 5000, TARGET_EVENTS = 66,
  BLOCK_SIZE = 4, STRATA_LEVELS = 2, STRATA_PROPORTIONS = rep(1 / STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1), MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12, STUDY_DURATION = 36, HAS_PROGNOSTIC = TRUE,
  LATENT_LEVELS = 1, LATENT_PROPORTIONS = c(1), LATENT_HR = 1, HR = 0.5, TRT_MEDIAN = 30
) {
  set.seed(123 + batch_id * 1000)
  results <- replicate(n_iter, {
    run_single_trial(
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO, MEDIAN_MATRIX = MEDIAN_MATRIX,
      ENROLL_PERIOD = ENROLL_PERIOD, STUDY_DURATION = STUDY_DURATION,
      HAS_PROGNOSTIC = HAS_PROGNOSTIC,
      LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
      HR = HR, TRT_MEDIAN = TRT_MEDIAN)
  }, simplify = FALSE)
  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}

generate_summary_stats <- function(
  simulation_data, param_id = 1, N_POOL = NULL, TARGET_EVENTS = NULL,
  BLOCK_SIZE = NULL, STRATA_LEVELS = NULL, STRATA_PROPORTIONS = NULL,
  TREATMENT_RATIO = NULL, TREATMENT_RATIO_NAME = NULL, MEAN_SCENARIO = NULL,
  MEDIAN_MATRIX_STR = NULL, TRUE_HR = NULL, PROGNOSTIC_HR = NULL
) {
  imb_median_strat <- median(simulation_data$imbalance_strat, na.rm = TRUE)
  imb_q1_strat <- quantile(simulation_data$imbalance_strat, 0.25, na.rm = TRUE)
  imb_q3_strat <- quantile(simulation_data$imbalance_strat, 0.75, na.rm = TRUE)
  arm_imbalance_strat <- sprintf("%.1f (%.1f-%.1f)", imb_median_strat, imb_q1_strat, imb_q3_strat)

  imb_median_simple <- median(simulation_data$imbalance_simple, na.rm = TRUE)
  imb_q1_simple <- quantile(simulation_data$imbalance_simple, 0.25, na.rm = TRUE)
  imb_q3_simple <- quantile(simulation_data$imbalance_simple, 0.75, na.rm = TRUE)
  arm_imbalance_simple <- sprintf("%.1f (%.1f-%.1f)", imb_median_simple, imb_q1_simple, imb_q3_simple)

  power_strat_design <- mean(simulation_data$p_strat_design < 0.05, na.rm = TRUE)
  power_strat_unstrat <- mean(simulation_data$p_strat_unstrat < 0.05, na.rm = TRUE)
  power_simple_design <- mean(simulation_data$p_simple_design < 0.05, na.rm = TRUE)

  hr_mean_strat <- mean(simulation_data$hr_strat_design, na.rm = TRUE)
  hr_sd_strat <- sd(simulation_data$hr_strat_design, na.rm = TRUE)
  hr_mean_strat_unstrat <- mean(simulation_data$hr_strat_unstrat, na.rm = TRUE)
  hr_sd_strat_unstrat <- sd(simulation_data$hr_strat_unstrat, na.rm = TRUE)
  hr_mean_simple <- mean(simulation_data$hr_simple_design, na.rm = TRUE)
  hr_sd_simple <- sd(simulation_data$hr_simple_design, na.rm = TRUE)

  hr_bias_strat <- ifelse(is.null(TRUE_HR), NA, hr_mean_strat - TRUE_HR)
  hr_bias_strat_unstrat <- ifelse(is.null(TRUE_HR), NA, hr_mean_strat_unstrat - TRUE_HR)
  hr_bias_simple <- ifelse(is.null(TRUE_HR), NA, hr_mean_simple - TRUE_HR)

  hr_mse_strat <- ifelse(is.null(TRUE_HR), NA, mean((simulation_data$hr_strat_design - TRUE_HR)^2, na.rm = TRUE))
  hr_mse_strat_unstrat <- ifelse(is.null(TRUE_HR), NA, mean((simulation_data$hr_strat_unstrat - TRUE_HR)^2, na.rm = TRUE))
  hr_mse_simple <- ifelse(is.null(TRUE_HR), NA, mean((simulation_data$hr_simple_design - TRUE_HR)^2, na.rm = TRUE))

  if (!is.null(TRUE_HR)) {
    ci_cover_strat <- mean(simulation_data$hr_ci_lower_strat <= TRUE_HR & simulation_data$hr_ci_upper_strat >= TRUE_HR, na.rm = TRUE)
    ci_cover_strat_unstrat <- mean(simulation_data$hr_ci_lower_strat_unstrat <= TRUE_HR & simulation_data$hr_ci_upper_strat_unstrat >= TRUE_HR, na.rm = TRUE)
    ci_cover_simple <- mean(simulation_data$hr_ci_lower_simple <= TRUE_HR & simulation_data$hr_ci_upper_simple >= TRUE_HR, na.rm = TRUE)
  } else {
    ci_cover_strat <- NA; ci_cover_strat_unstrat <- NA; ci_cover_simple <- NA
  }

  n_strat_mean <- mean(simulation_data$n_strat, na.rm = TRUE)
  n_simple_mean <- mean(simulation_data$n_simple, na.rm = TRUE)
  events_mean_strat <- mean(simulation_data$events_total_strat, na.rm = TRUE)
  events_mean_simple <- mean(simulation_data$events_total_simple, na.rm = TRUE)
  censor_mean_strat <- mean(1 - simulation_data$events_total_strat / simulation_data$n_strat, na.rm = TRUE)
  censor_mean_simple <- mean(1 - simulation_data$events_total_simple / simulation_data$n_simple, na.rm = TRUE)
  n_strata_zero_mean_strat <- mean(simulation_data$n_strata_zero_events_strat, na.rm = TRUE)
  n_strata_few_mean_strat <- mean(simulation_data$n_strata_few_events_strat, na.rm = TRUE)
  n_strata_zero_mean_simple <- mean(simulation_data$n_strata_zero_events_simple, na.rm = TRUE)
  n_strata_few_mean_simple <- mean(simulation_data$n_strata_few_events_simple, na.rm = TRUE)

  vrf_actual_mean <- mean(simulation_data$vrf_actual, na.rm = TRUE)
  vrf_expected_val <- mean(simulation_data$vrf_expected, na.rm = TRUE)

  smd_mean_strat <- mean(simulation_data$smd_strat, na.rm = TRUE)
  smd_mean_simple <- mean(simulation_data$smd_simple, na.rm = TRUE)
  smd_summary <- sprintf("%.3f / %.3f", smd_mean_strat, smd_mean_simple)

  power_diff <- power_strat_design - power_simple_design
  power_diff_pct <- sprintf("%+.1f%%", power_diff * 100)
  power_diff_unstrat <- power_strat_unstrat - power_simple_design
  power_diff_unstrat_pct <- sprintf("%+.1f%%", power_diff_unstrat * 100)

  result <- data.frame(
    Simulation_ID = param_id,
    TARGET_EVENTS = ifelse(is.null(TARGET_EVENTS), NA, TARGET_EVENTS),
    BLOCK_SIZE = ifelse(is.null(BLOCK_SIZE), NA, BLOCK_SIZE),
    STRATA_LEVELS = ifelse(is.null(STRATA_LEVELS), NA, STRATA_LEVELS),
    Arm_imbalance_strat = arm_imbalance_strat,
    Arm_imbalance_simple = arm_imbalance_simple,
    Power_strat_design = sprintf("%.3f", power_strat_design),
    Power_strat_unstrat = sprintf("%.3f", power_strat_unstrat),
    Power_simple_design = sprintf("%.3f", power_simple_design),
    Power_difference = power_diff_pct,
    Power_diff_strat_unstrat_vs_simple = power_diff_unstrat_pct,
    HR_mean_strat = sprintf("%.3f (%.3f)", hr_mean_strat, hr_sd_strat),
    HR_mean_strat_unstrat = sprintf("%.3f (%.3f)", hr_mean_strat_unstrat, hr_sd_strat_unstrat),
    HR_mean_simple = sprintf("%.3f (%.3f)", hr_mean_simple, hr_sd_simple),
    HR_bias_strat = ifelse(is.na(hr_bias_strat), NA, sprintf("%.4f", hr_bias_strat)),
    HR_bias_strat_unstrat = ifelse(is.na(hr_bias_strat_unstrat), NA, sprintf("%.4f", hr_bias_strat_unstrat)),
    HR_bias_simple = ifelse(is.na(hr_bias_simple), NA, sprintf("%.4f", hr_bias_simple)),
    HR_MSE_strat = ifelse(is.na(hr_mse_strat), NA, sprintf("%.4f", hr_mse_strat)),
    HR_MSE_strat_unstrat = ifelse(is.na(hr_mse_strat_unstrat), NA, sprintf("%.4f", hr_mse_strat_unstrat)),
    HR_MSE_simple = ifelse(is.na(hr_mse_simple), NA, sprintf("%.4f", hr_mse_simple)),
    CI_coverage_strat = ifelse(is.na(ci_cover_strat), NA, sprintf("%.3f", ci_cover_strat)),
    CI_coverage_strat_unstrat = ifelse(is.na(ci_cover_strat_unstrat), NA, sprintf("%.3f", ci_cover_strat_unstrat)),
    CI_coverage_simple = ifelse(is.na(ci_cover_simple), NA, sprintf("%.3f", ci_cover_simple)),
    N_strat_mean = sprintf("%.1f", n_strat_mean),
    N_simple_mean = sprintf("%.1f", n_simple_mean),
    Events_mean_strat = sprintf("%.1f", events_mean_strat),
    Events_mean_simple = sprintf("%.1f", events_mean_simple),
    Censor_rate_strat = sprintf("%.1f%%", censor_mean_strat * 100),
    Censor_rate_simple = sprintf("%.1f%%", censor_mean_simple * 100),
    N_strata_zero_strat = sprintf("%.1f", n_strata_zero_mean_strat),
    N_strata_few_strat = sprintf("%.1f", n_strata_few_mean_strat),
    N_strata_zero_simple = sprintf("%.1f", n_strata_zero_mean_simple),
    N_strata_few_simple = sprintf("%.1f", n_strata_few_mean_simple),
    VRF_actual = sprintf("%.3f", vrf_actual_mean),
    VRF_expected = sprintf("%.3f", vrf_expected_val),
    SMD_strat_vs_simple = smd_summary,
    PROGNOSTIC_HR = ifelse(is.null(PROGNOSTIC_HR), NA, PROGNOSTIC_HR),
    Strata_Medians = {
      s <- simulation_data$strata_median_str
      s[!is.na(s)][1]
    },
    stringsAsFactors = FALSE)

  if (!is.null(N_POOL)) result$N_POOL <- N_POOL
  if (!is.null(STRATA_PROPORTIONS)) result$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ":")
  if (!is.null(TREATMENT_RATIO)) result$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  if (!is.null(TREATMENT_RATIO_NAME)) result$TREATMENT_RATIO_NAME <- TREATMENT_RATIO_NAME
  if (!is.null(MEAN_SCENARIO)) result$MEAN_SCENARIO <- MEAN_SCENARIO
  if (!is.null(MEDIAN_MATRIX_STR)) result$MEDIAN_MATRIX_STR <- MEDIAN_MATRIX_STR
  if (!is.null(PROGNOSTIC_HR)) result$PROGNOSTIC_HR <- PROGNOSTIC_HR
  if (!is.null(TRUE_HR)) result$TRUE_HR <- TRUE_HR
  return(result)
}

# 9. Main function: Run full simulation (single parameter combination)
run_simulation <- function(
  n_iter = 10000,
  n_batch = 1,
  N_POOL = 5000,
  TARGET_EVENTS = 66,
  BLOCK_SIZE = 4,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12,
  STUDY_DURATION = 36,
  HAS_PROGNOSTIC = TRUE,
  LATENT_LEVELS = 1,
  LATENT_PROPORTIONS = c(1),
  LATENT_HR = 1,
  HR = 0.5,
  PROGNOSTIC_HR = 1,
  TRT_MEDIAN = 30,
  use_parallel = FALSE,
  n_cores = max(1, parallel::detectCores() - 1),
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("Starting simulation: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("Parameters: N_POOL=%d, TARGET_EVENTS=%d, BLOCK_SIZE=%d\n", N_POOL, TARGET_EVENTS, BLOCK_SIZE))
    cat(sprintf("      STRATA_LEVELS=%d\n", STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    cat(sprintf("      ENROLL_PERIOD=%d, STUDY_DURATION=%d\n", ENROLL_PERIOD, STUDY_DURATION))
    if (!is.null(MEDIAN_MATRIX)) {
      cat("      MEDIAN_MATRIX:\n")
      print(MEDIAN_MATRIX)
    }
  }
  
  batch_results <- lapply(1:n_batch, function(i) {
    if (verbose && n_batch > 1) cat(sprintf("\rCompleted batch %d / %d", i, n_batch))
    run_batch_simulation(
      n_iter = n_iter, batch_id = i, N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
      BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
      MEDIAN_MATRIX = MEDIAN_MATRIX, ENROLL_PERIOD = ENROLL_PERIOD,
      STUDY_DURATION = STUDY_DURATION, HAS_PROGNOSTIC = HAS_PROGNOSTIC,
      LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
      HR = HR, TRT_MEDIAN = TRT_MEDIAN
    )
  })
  
  if (verbose && n_batch > 1) cat("\n")
  
  final_data <- do.call(rbind, batch_results)
  
  final_data$N_POOL <- N_POOL
  final_data$TARGET_EVENTS <- TARGET_EVENTS
  final_data$BLOCK_SIZE <- BLOCK_SIZE
  final_data$STRATA_LEVELS <- STRATA_LEVELS
  final_data$STRATA_PROPORTIONS <- paste(STRATA_PROPORTIONS, collapse = ",")
  final_data$TREATMENT_RATIO <- paste(TREATMENT_RATIO, collapse = ":")
  
  if (!is.null(MEDIAN_MATRIX)) {
    final_data$MEDIAN_MATRIX <- matrix_to_string(MEDIAN_MATRIX)
  }
  
  return(final_data)
}

# 9.5 Main function: Run full simulation (per batch summary)
run_simulation_per_batch <- function(
  n_iter = 1000,
  n_batch = 10,
  N_POOL = 5000,
  TARGET_EVENTS = 66,
  BLOCK_SIZE = 4,
  STRATA_LEVELS = 2,
  STRATA_PROPORTIONS = rep(1/STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1),
  MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12,
  STUDY_DURATION = 36,
  HAS_PROGNOSTIC = TRUE,
  LATENT_LEVELS = 1,
  LATENT_PROPORTIONS = c(1),
  LATENT_HR = 1,
  HR = 0.5,
  PROGNOSTIC_HR = 1,
  TRT_MEDIAN = 30,
  use_parallel = FALSE,
  n_cores = max(1, parallel::detectCores() - 1),
  verbose = TRUE
) {
  if (verbose) {
    cat(sprintf("Starting simulation: n_iter=%d, n_batch=%d\n", n_iter, n_batch))
    cat(sprintf("Parameters: N_POOL=%d, TARGET_EVENTS=%d, BLOCK_SIZE=%d\n", N_POOL, TARGET_EVENTS, BLOCK_SIZE))
    cat(sprintf("      STRATA_LEVELS=%d\n", STRATA_LEVELS))
    cat(sprintf("      PROPORTIONS=%s\n", paste(round(STRATA_PROPORTIONS, 3), collapse=", ")))
    cat(sprintf("      TREATMENT_RATIO=%s\n", paste(TREATMENT_RATIO, collapse=":")))
    cat(sprintf("      ENROLL_PERIOD=%d, STUDY_DURATION=%d\n", ENROLL_PERIOD, STUDY_DURATION))
    if (!is.null(MEDIAN_MATRIX)) { cat("      MEDIAN_MATRIX:\n"); print(MEDIAN_MATRIX) }
  }
  
  batch_results_list <- list()
  batch_summaries_list <- list()
  
  for (batch_id in 1:n_batch) {
    if (verbose) cat(sprintf("\rProcessing batch %d / %d", batch_id, n_batch))
    
    batch_result <- if (use_parallel) {
      run_batch_simulation_parallel(
        n_iter = n_iter, batch_id = batch_id, N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
        BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
        STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
        MEDIAN_MATRIX = MEDIAN_MATRIX, ENROLL_PERIOD = ENROLL_PERIOD,
        STUDY_DURATION = STUDY_DURATION, HAS_PROGNOSTIC = HAS_PROGNOSTIC,
        LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
        HR = HR, PROGNOSTIC_HR = PROGNOSTIC_HR, TRT_MEDIAN = TRT_MEDIAN, n_cores = n_cores)
    } else {
      run_batch_simulation(
        n_iter = n_iter, batch_id = batch_id, N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
        BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
        STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
        MEDIAN_MATRIX = MEDIAN_MATRIX, ENROLL_PERIOD = ENROLL_PERIOD,
        STUDY_DURATION = STUDY_DURATION, HAS_PROGNOSTIC = HAS_PROGNOSTIC,
        LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
        HR = HR, PROGNOSTIC_HR = PROGNOSTIC_HR, TRT_MEDIAN = TRT_MEDIAN)
    }
    
    batch_summary <- generate_summary_stats(
      simulation_data = batch_result, param_id = batch_id,
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
      TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
      TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
      MEAN_SCENARIO = if (!is.null(MEDIAN_MATRIX)) "Custom" else "Default",
      MEDIAN_MATRIX_STR = if (!is.null(MEDIAN_MATRIX)) matrix_to_string(MEDIAN_MATRIX) else NA,
      PROGNOSTIC_HR = PROGNOSTIC_HR
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
    N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
    STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = paste(STRATA_PROPORTIONS, collapse = ","),
    TREATMENT_RATIO = paste(TREATMENT_RATIO, collapse = ","),
    TREATMENT_RATIO_NAME = paste(TREATMENT_RATIO, collapse = ":"),
    MEAN_SCENARIO = if (!is.null(MEDIAN_MATRIX)) "Custom" else "Default",
    MEDIAN_MATRIX_STR = if (!is.null(MEDIAN_MATRIX)) matrix_to_string(MEDIAN_MATRIX) else NA
  )
  overall_summary$Batch_ID <- "Overall"
  all_batch_summaries <- rbind(all_batch_summaries, overall_summary)
  
  if (verbose) {
    cat(sprintf("\nSimulation complete! Total %d batches, %d iterations each, %d total\n",
                n_batch, n_iter, n_batch * n_iter))
    cat(sprintf("  - Detailed results: %d rows\n", nrow(all_detailed_results)))
    cat(sprintf("  - Summary results: %d rows (%d batches + 1 overall)\n",
                nrow(all_batch_summaries), n_batch))
  }
  
  return(list(
    detailed_results = all_detailed_results,
    batch_summaries = all_batch_summaries,
    overall_summary = overall_summary
  ))
}

run_parameter_grid_per_batch <- function(
  param_grid,
  n_iter = 1000,
  n_batch = 10,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "simulation_tte_results_per_batch",
  use_parallel = FALSE,
  n_cores = max(1, parallel::detectCores() - 1)
) {
  all_batch_summaries_list <- list()
  current_row <- 1

  for (i in 1:nrow(param_grid)) {
    if (verbose) cat(sprintf("\n========== Parameter combination %d / %d ==========\n", i, nrow(param_grid)))

    params <- param_grid[i, , drop = FALSE]

    median_matrix <- NULL
    median_matrix_str <- NA
    if ("MEDIAN_MATRIX_STR" %in% colnames(params)) {
      median_matrix_str <- as.character(params$MEDIAN_MATRIX_STR)
    }
    if (is.na(median_matrix_str) || median_matrix_str == "NA") {
      hr_val <- if ("HR" %in% colnames(params)) as.numeric(params$HR) else 0.5
      median_matrix_str <- generate_median_matrix_from_params(
        n_strata = params$STRATA_LEVELS,
        hr = hr_val,
        trt_median = if ("TRT_MEDIAN" %in% colnames(params)) as.numeric(params$TRT_MEDIAN) else 30,
        prognostic_hr = if ("PROGNOSTIC_HR" %in% colnames(params)) as.numeric(params$PROGNOSTIC_HR) else 1,
        strata_props = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ",")))
      )
    }
    median_matrix <- string_to_matrix(median_matrix_str, n_strata = params$STRATA_LEVELS)

    iter <- if ("N_ITER" %in% colnames(params) && !is.na(params$N_ITER)) as.integer(params$N_ITER) else n_iter
    batch <- if ("N_BATCH" %in% colnames(params) && !is.na(params$N_BATCH)) as.integer(params$N_BATCH) else n_batch

    sim_results <- run_simulation_per_batch(
      n_iter = iter, n_batch = batch,
      N_POOL = params$N_POOL, TARGET_EVENTS = params$TARGET_EVENTS,
      BLOCK_SIZE = params$BLOCK_SIZE, STRATA_LEVELS = params$STRATA_LEVELS,
      STRATA_PROPORTIONS = as.numeric(unlist(strsplit(as.character(params$STRATA_PROPORTIONS), ","))),
      TREATMENT_RATIO = if ("TREATMENT_RATIO" %in% colnames(params)) {
        as.numeric(unlist(strsplit(as.character(params$TREATMENT_RATIO), ",")))
      } else { c(1, 1) },
      MEDIAN_MATRIX = median_matrix,
      HR = if ("HR" %in% colnames(params)) as.numeric(params$HR) else 0.5,
      ENROLL_PERIOD = if ("ENROLL_PERIOD" %in% colnames(params)) params$ENROLL_PERIOD else 12,
      STUDY_DURATION = if ("STUDY_DURATION" %in% colnames(params)) params$STUDY_DURATION else 36,
      HAS_PROGNOSTIC = if ("HAS_PROGNOSTIC" %in% colnames(params)) { v <- as.logical(params$HAS_PROGNOSTIC); ifelse(is.na(v), TRUE, v) } else TRUE,
      LATENT_LEVELS = if ("LATENT_LEVELS" %in% colnames(params)) { v <- as.integer(params$LATENT_LEVELS); ifelse(is.na(v), 1, v) } else 1,
      LATENT_PROPORTIONS = if ("LATENT_PROPORTIONS" %in% colnames(params)) { v <- as.numeric(unlist(strsplit(as.character(params$LATENT_PROPORTIONS), ","))); if (any(is.na(v))) c(1) else v } else c(1),
      LATENT_HR = if ("LATENT_HR" %in% colnames(params)) { v <- as.numeric(params$LATENT_HR); ifelse(is.na(v), 1, v) } else 1,
      PROGNOSTIC_HR = if ("PROGNOSTIC_HR" %in% colnames(params)) as.numeric(params$PROGNOSTIC_HR) else 1,
      TRT_MEDIAN = if ("TRT_MEDIAN" %in% colnames(params)) as.numeric(params$TRT_MEDIAN) else 30,
      use_parallel = use_parallel, n_cores = n_cores,
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
    cat(sprintf("All parameter combinations complete!\n"))
    cat(sprintf("  Total combinations: %d\n", nrow(param_grid)))
    cat(sprintf("  Batches per combination: %d\n", n_batch))
    cat(sprintf("  Total summary rows: %d\n", nrow(all_batch_summaries)))
    cat(strrep("=", 70), "\n\n", sep = "")
  }

  if (generate_excel) {
    output_file <- generate_excel_output_tte(
      summary_data = all_batch_summaries, output_prefix = output_prefix)
    if (verbose && !is.null(output_file)) {
      cat(sprintf("\nExcel report saved: %s\n", output_file))
    }
  }

  return(list(batch_summaries_all = all_batch_summaries, param_grid = param_grid))
}

# ------------------------------------------------------------------------------
# Parallel batch simulation (Windows compatible)
# ------------------------------------------------------------------------------
run_batch_simulation_parallel <- function(
  n_iter = 1000, batch_id = 1, N_POOL = 5000, TARGET_EVENTS = 66,
  BLOCK_SIZE = 4, STRATA_LEVELS = 2, STRATA_PROPORTIONS = rep(1 / STRATA_LEVELS, STRATA_LEVELS),
  TREATMENT_RATIO = c(1, 1), MEDIAN_MATRIX = NULL,
  ENROLL_PERIOD = 12, STUDY_DURATION = 36, HAS_PROGNOSTIC = TRUE,
  LATENT_LEVELS = 1, LATENT_PROPORTIONS = c(1), LATENT_HR = 1,
  HR = 0.5, PROGNOSTIC_HR = 1, TRT_MEDIAN = 30,
  n_cores = max(1, parallel::detectCores() - 1)
) {
  if (n_cores <= 1) {
    return(run_batch_simulation(
      n_iter = n_iter, batch_id = batch_id, N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS,
      BLOCK_SIZE = BLOCK_SIZE, STRATA_LEVELS = STRATA_LEVELS,
      STRATA_PROPORTIONS = STRATA_PROPORTIONS, TREATMENT_RATIO = TREATMENT_RATIO,
      MEDIAN_MATRIX = MEDIAN_MATRIX, ENROLL_PERIOD = ENROLL_PERIOD,
      STUDY_DURATION = STUDY_DURATION, HAS_PROGNOSTIC = HAS_PROGNOSTIC,
      LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
      HR = HR, TRT_MEDIAN = TRT_MEDIAN))
  }

  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export necessary objects and load survival package on workers
  parallel::clusterExport(cl, varlist = c(
    "run_single_trial", "generate_rand_list", "generate_stratified_rand_lists",
    "calculate_strata_smd", "calculate_expected_vrf", "matrix_to_string", "string_to_matrix"
  ), envir = environment())
  parallel::clusterEvalQ(cl, library(survival))

  set.seed(123 + batch_id * 1000)
  seeds <- sample.int(.Machine$integer.max, n_iter)

  results <- parallel::parLapply(cl, seeds, function(seed) {
    set.seed(seed)
    run_single_trial(
      N_POOL = N_POOL, TARGET_EVENTS = TARGET_EVENTS, BLOCK_SIZE = BLOCK_SIZE,
      STRATA_LEVELS = STRATA_LEVELS, STRATA_PROPORTIONS = STRATA_PROPORTIONS,
      TREATMENT_RATIO = TREATMENT_RATIO, MEDIAN_MATRIX = MEDIAN_MATRIX,
      ENROLL_PERIOD = ENROLL_PERIOD, STUDY_DURATION = STUDY_DURATION,
      HAS_PROGNOSTIC = HAS_PROGNOSTIC,
      LATENT_LEVELS = LATENT_LEVELS, LATENT_PROPORTIONS = LATENT_PROPORTIONS, LATENT_HR = LATENT_HR,
      HR = HR, TRT_MEDIAN = TRT_MEDIAN)
  })

  results <- results[!sapply(results, is.null)]
  summaries <- lapply(results, function(x) x$summary)
  do.call(rbind, summaries)
}
