# ==============================================================================
# TTE simulation example run script
# ==============================================================================

source("C:\\Yuting\\AI\\kimi\\single_simulation_test_param_batch_tte.r")
library(dplyr)

# ------------------------------------------------------------------------------
# Option 1: Read parameters from Excel (recommended for full analysis)
# ------------------------------------------------------------------------------
# First run create_tte_param_template.R to generate template, then modify:
  param_grid <- read_param_grid_from_excel("C:\\Yuting\\AI\\kimi\\tte\\sa\\param_grid_tte_template_sa.xlsx")

# ------------------------------------------------------------------------------
# Option 2: Define parameter grid directly in R (quick test)
# ------------------------------------------------------------------------------
#param_grid <- data.frame(
#   Param_ID = 1:2,
#   N_POOL = 5000,
#   TARGET_EVENTS = c(66, 170),
#   BLOCK_SIZE = 4,
#   STRATA_LEVELS = 2,
#   STRATA_PROPORTIONS = "0.5,0.5",
#   TREATMENT_RATIO = "1,1",
#   TREATMENT_RATIO_NAME = "1:1",
#   HR = c(0.5, 0.65),
#   TRT_MEDIAN = 30,
#   HAS_PROGNOSTIC = FALSE,
#   PROGNOSTIC_HR = NA,
#   LATENT_LEVELS = 2,
#   LATENT_PROPORTIONS = "0.5,0.5",
#   LATENT_HR = 2,
#   ENROLL_PERIOD = 12,
#   STUDY_DURATION = 36,
#   MEAN_SCENARIO = c("HR0.5_test", "HR0.65_test"),
#   MEDIAN_MATRIX_STR = c("15,30;15,30", "19.5,30;19.5,30"),
#   stringsAsFactors = FALSE
# )

# ------------------------------------------------------------------------------
# Run simulation (per batch summary)
# -------------------------------------------------------------------------
results <- run_parameter_grid_per_batch(
  param_grid = param_grid,
  n_iter = 10,
  n_batch = 1,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "tte_simulation_example",
  use_parallel = TRUE,      # Enable parallel processing for speedup
  n_cores = 4               # Number of CPU cores (default: detectCores() - 1)
)

# ------------------------------------------------------------------------------
# View results
# ------------------------------------------------------------------------------
cat("\n========== Overall Summary ==========\n")
print(results$batch_summaries_all %>%
  filter(Batch_ID == "Overall") %>%
  select(Param_ID, TARGET_EVENTS, HR, MEAN_SCENARIO,
         Power_strat_design, Power_simple_design,
         HR_mean_strat, HR_mean_simple,
         N_strat_mean, N_simple_mean,
         Events_mean_strat, Events_mean_simple,
         N_strata_zero_strat, N_strata_few_strat,
         VRF_actual, VRF_expected,
         SMD_strat_vs_simple))
