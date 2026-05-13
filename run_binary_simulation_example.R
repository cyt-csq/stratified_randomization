# ==============================================================================
# 二分类模拟示例运行脚本
# 功能：从 Excel 读取参数并运行模拟
# ==============================================================================

# 1. 加载核心模拟函数
source("C:\\Yuting\\AI\\kimi\\single_simulation_test_param_batch_binary.r")

# ------------------------------------------------------------------------------
# 方式一：从 Excel 读取参数（推荐）
# ------------------------------------------------------------------------------
# 先运行 create_binary_param_template.R 生成模板，修改后使用：
param_grid <- read_param_grid_from_excel("C:\\Yuting\\AI\\kimi\\binary\\sb\\param_grid_binary_template_sa.xlsx")

# ------------------------------------------------------------------------------
# 方式二：直接在 R 中定义参数网格（快速测试）
# ------------------------------------------------------------------------------
# param_grid <- data.frame(
#   Param_ID = 1:2,
#   N_POOL = 1000,
#   TARGET_N = c(46, 182),
#   BLOCK_SIZE = 4,
#   STRATA_LEVELS = 2,
#   STRATA_PROPORTIONS = "0.5,0.5",
#   TREATMENT_RATIO = "1,1",
#   TREATMENT_RATIO_NAME = "1:1",
#   MEAN_SCENARIO = c("2L_N46", "2L_N182"),
#   PROB_MATRIX_STR = "0.3,0.5;0.3,0.5",  # 2层，每组概率0.3 vs 0.5
#   stringsAsFactors = FALSE
# )

# ------------------------------------------------------------------------------
# 运行模拟（每个 batch 单独汇总）
# ------------------------------------------------------------------------------
results <- run_parameter_grid_per_batch(
  param_grid = param_grid,
  n_iter = 10 ,        # 每批次模拟次数（可被 Excel 中 N_ITER 列覆盖）
  n_batch = 1 ,         # 批次数（可被 Excel 中 N_BATCH 列覆盖）
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "binary_simulation_example"
)

# ------------------------------------------------------------------------------
# 查看结果
# ------------------------------------------------------------------------------
cat("\n========== 总体汇总 ==========\n")
print(results$batch_summaries_all %>% 
  filter(Batch_ID == "Overall") %>%
  select(Param_ID, TARGET_N, Power_strat_design, Power_simple_design, 
         OR_mean_strat, RD_mean_strat, SMD_strat_vs_simple))
