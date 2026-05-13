# ==============================================================================
# 双潜在分层因素模拟系统（完整版）
# 功能：
#   - A因素：用于分层随机化和分层分析（固定2层，50:50比例）
#   - B因素：仅用于结局模拟和SMD评估（2/4/8层，多种比例）
#   - 支持层间差异级别：low/medium/high
#   - 自动生成PDF报告（分组可视化）+ Excel汇总
# ==============================================================================

# 0. 加载必要包
required_packages <- c("ggplot2", "scales", "dplyr", "pdftools", "openxlsx")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(scales)
library(dplyr)
library(pdftools)
library(openxlsx)

# 加载核心模拟函数
source("C:\\Yuting\\AI\\kimi\\sc\\ancova计算-scenarioc 20260410.r")

# ------------------------------------------------------------------------------
# 1. 辅助函数：矩阵与字符串转换
# ------------------------------------------------------------------------------
matrix_to_string <- function(mat) {
  paste(apply(mat, 1, function(row) paste(row, collapse = ",")), collapse = ";")
}

# ------------------------------------------------------------------------------
# 2. 辅助函数：创建交叉层均值矩阵（支持多种维度和层间差异）
# ------------------------------------------------------------------------------
create_cross_strata_matrix_scenarios <- function(scenario = "low", 
                                                  dim_a = 2, 
                                                  dim_b = 2,
                                                  STRATA_PROPORTIONS_B = "0.5,0.5") {
  # 1. 清理和标准化输入 (保留向量结构)
  scenario <- trimws(as.character(scenario))
  STRATA_PROPORTIONS <- trimws(as.character(STRATA_PROPORTIONS_B))
  dim_a <- as.integer(dim_a)
  dim_b <- as.integer(dim_b)
  
  # 2. 定义内部核心函数：仅处理"标量"情况
  .get_single_matrix <- function(scen, da, db, strata_prop) {
    
    # ========== LOW SCENARIO, dim_b = 2 ==========
    if (da == 2 && db == 2 && scen == "low" && strata_prop == "0.5,0.5") {
      matrix(c(5,15, 7,17, 13,23, 15,25), nrow=4, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 2 && scen == "low" && strata_prop == "0.4,0.6") {
      matrix(c(4.8,14.8, 6.8,16.8, 12.8,22.8, 14.8,24.8), nrow=4, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 2 && scen == "low" && strata_prop == "0.3,0.7") {
      matrix(c(4.6,14.6, 6.6,16.6, 12.6,22.6, 14.6,24.6), nrow=4, ncol=2, byrow=TRUE)
    
    # ========== LOW SCENARIO, dim_b = 4 ==========
    } else if (da == 2 && db == 4 && scen == "low" && strata_prop == "0.25,0.25,0.25,0.25") {
      matrix(c(7,17, 8,18, 9,19, 10,20, 10,20, 11,21, 12,22, 13,23), 
             nrow=8, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 4 && scen == "low" && strata_prop == "0.15,0.15,0.35,0.35") {
      matrix(c(6.6,16.6, 7.6,17.6, 8.6,18.6, 9.6,19.6, 9.6,19.6, 10.6,20.6, 11.6,21.6, 12.6,22.6), 
             nrow=8, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 4 && scen == "low" && strata_prop == "0.12,0.18,0.28,0.42") {
      matrix(c(6.5,16.5, 7.5,17.5, 8.5,18.5, 9.5,19.5, 9.5,19.5, 10.5,20.5, 11.5,21.5, 12.5,22.5), 
             nrow=8, ncol=2, byrow=TRUE)
    
    # ========== LOW SCENARIO, dim_b = 8 ==========
    } else if (da == 2 && db == 8 && scen == "low" && strata_prop == "0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125") {
      matrix(c(5.5,15.5, 6.5,16.5,7.5,17.5,8.5,18.5,9.5,19.5,10.5,20.5,11.5,21.5,12.5,22.5,7.5,17.5,8.5,18.5, 9.5,19.5,10.5,20.5,
      11.5,21.5, 12.5,22.5, 13.5,23.5,14.5,24.5
), 
             nrow=16, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 8 && scen == "low" && strata_prop == "0.06,0.06,0.09,0.09,0.14,0.14,0.21,0.21") {
      matrix(c(4.5,14.5,5.5,15.5,6.5,16.5,7.5,17.5, 8.5,18.5, 9.5,19.5, 10.5,20.5, 11.5,21.5, 6.5,16.5, 7.5,17.5, 8.5,18.5, 9.5
,19.5, 10.5,20.5, 11.5,21.5, 12.5,22.5, 13.5,23.5
), 
             nrow=16, ncol=2, byrow=TRUE)
    
    # ========== HIGH SCENARIO, dim_b = 2 ==========
    } else if (da == 2 && db == 2 && scen == "high" && strata_prop == "0.5,0.5") {
      matrix(c(-2,8, 14,24, 6,16, 22,32), nrow=4, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 2 && scen == "high" && strata_prop == "0.4,0.6") {
      matrix(c(-3.6,6.4, 12.4,22.4, 4.4,14.4, 20.4,30.4), nrow=4, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 2 && scen == "high" && strata_prop == "0.3,0.7") {
      matrix(c(17.2,27.2, 1.2,11.2, 25.2,35.2, 9.2,19.2), nrow=4, ncol=2, byrow=TRUE)
    
    # ========== HIGH SCENARIO, dim_b = 4 ==========
    } else if (da == 2 && db == 4 && scen == "high" && strata_prop == "0.25,0.25,0.25,0.25") {
      matrix(c(1,11, 6,16, 11,21, 16,26, 4,14, 9,19, 14,24, 19,29), 
             nrow=8, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 4 && scen == "high" && strata_prop == "0.15,0.15,0.35,0.35") {
      matrix(c(-1,9, 4,14, 9,19, 14,24, 2,12, 7,17, 12,22, 17,27), 
             nrow=8, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 4 && scen == "high" && strata_prop == "0.12,0.18,0.28,0.42") {
      matrix(c(-1.5,8.5,  3.5,13.5,  8.5,18.5,  13.5,23.5, 1.5,11.5,  6.5,16.5, 11.5,21.5,  16.5,26.5), 
             nrow=8, ncol=2, byrow=TRUE)
    
    # ========== HIGH SCENARIO, dim_b = 8 ==========
    } else if (da == 2 && db == 8 && scen == "high" && strata_prop == "0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125") {
      matrix(c(-5,5, -1,9, 3,13, 7,17, 11,21, 15,25, 19,29, 23,33, -3,7, 1,11, 5,15, 9,19, 13,23
,17,27,21,31,25,35
), 
             nrow=16, ncol=2, byrow=TRUE)
    } else if (da == 2 && db == 8 && scen == "high" && strata_prop == "0.06,0.06,0.09,0.09,0.14,0.14,0.21,0.21") {
      matrix(c(27,37,  23,33, 19,29, 15,25, 11,21, 7,17, 3,13, -1,9, 29,39, 25,35, 21,31, 17
,27, 13,23, 9,19, 5,15, 1,11
), 
             nrow=16, ncol=2, byrow=TRUE)
    
    } else {
      stop(paste0("未知场景或维度组合：'", scen, "' (", da, "×", db, ") ", 
                  "STRATA_PROPORTIONS='", strata_prop, "'"))
    }
  }
  
  # 3. 处理向量化输入 (兼容标量和向量)
  len_scen <- length(scenario)
  len_strata <- length(STRATA_PROPORTIONS)
  len_a <- length(dim_a)
  len_b <- length(dim_b)
  n <- max(len_scen, len_strata, len_a, len_b)
  
  # 标量回收 (Recycling)
  if (len_scen == 1) scenario <- rep(scenario, n)
  if (len_strata == 1) STRATA_PROPORTIONS <- rep(STRATA_PROPORTIONS, n)
  if (len_a == 1) dim_a <- rep(dim_a, n)
  if (len_b == 1) dim_b <- rep(dim_b, n)
  
  # 4. 执行逻辑
  if (n == 1) {
    # 情况 A：所有输入都是单个值 -> 直接返回矩阵
    return(.get_single_matrix(scenario, dim_a, dim_b, STRATA_PROPORTIONS))
  } else {
    # 情况 B：输入包含向量 -> 返回命名列表
    result_list <- vector("list", n)
    for (i in 1:n) {
      result_list[[i]] <- .get_single_matrix(scenario[i], dim_a[i], dim_b[i], STRATA_PROPORTIONS[i])
    }
    # 给列表元素命名，方便识别（比例字符串较长，做简化处理）
    strata_short <- gsub(",", "_", STRATA_PROPORTIONS)
    names(result_list) <- paste0("Scen_", scenario, "_Strata_", strata_short, 
                                 "_A", dim_a, "_B", dim_b)
    return(result_list)
  }
}
# ------------------------------------------------------------------------------
# 3. 创建比例映射表（B因素）
# ------------------------------------------------------------------------------
proportion_map <- data.frame(
  STRATA_LEVELS_B = c(
    rep(2, 3),      # 2层：3种比例
    rep(4, 3),      # 4层：3种比例
    rep(8, 2)       # 8层：2种比例
  ),
  PROPORTION_TYPE = c(
    "equal_50_50", "unequal_40_60", "unequal_30_70",
    "equal_25", "unequal_15_35", "unequal_12_42",
    "equal_125", "unequal_06_21"
  ),
  STRATA_PROPORTIONS_B = c(
    "0.5,0.5", "0.4,0.6", "0.3,0.7",
    "0.25,0.25,0.25,0.25", "0.15,0.15,0.35,0.35", "0.12,0.18,0.28,0.42",
    "0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125",
    "0.06,0.06,0.09,0.09,0.14,0.14,0.21,0.21"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 4. 基础参数网格（删除IS_STRATIFIED_BY_A和ANALYZE_BY_A）
# ------------------------------------------------------------------------------

param_grid_base <- data.frame(
  TARGET_N = rep(c(46), each = 16),
  STRATA_LEVELS_A = rep(2, 16),
  STRATA_LEVELS_B = rep(c(2,2,2,4,4,4,8,8), 2),
  # 删除：IS_STRATIFIED_BY_A = rep(c(FALSE), each = 16)
  # 删除：ANALYZE_BY_A = rep(FALSE, 16)
  SCENARIO_TYPE = rep(c("low", "high"), each = 8),
  STRATA_PROPORTIONS_B = rep(c(
    "0.5,0.5", "0.4,0.6", "0.3,0.7",
    "0.25,0.25,0.25,0.25", "0.15,0.15,0.35,0.35", "0.12,0.18,0.28,0.42",
    "0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125",
    "0.06,0.06,0.09,0.09,0.14,0.14,0.21,0.21"
  ), 2),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 5. 合并比例映射表，生成完整参数网格
# ------------------------------------------------------------------------------

param_grid_expanded <- merge(
  param_grid_base,
  proportion_map,
  by = c("STRATA_LEVELS_B", "STRATA_PROPORTIONS_B"),
  all.x = TRUE
)

# ------------------------------------------------------------------------------
# 6. 为每行生成均值矩阵和场景描述（修订版）
# ------------------------------------------------------------------------------

param_grid <- param_grid_expanded %>%
  rowwise() %>%
  mutate(
    # 生成均值矩阵字符串
    MEAN_MATRIX_STR = matrix_to_string(create_cross_strata_matrix_scenarios(
      scenario = SCENARIO_TYPE,
      dim_a = STRATA_LEVELS_A,
      dim_b = STRATA_LEVELS_B,
      STRATA_PROPORTIONS_B = STRATA_PROPORTIONS_B
    )),
    
    # 场景描述
    SCENARIO_DESC = case_when(
      SCENARIO_TYPE == "low" ~ "strata B层间差异比A小",
      SCENARIO_TYPE == "high" ~ "strata B层间差异比A大"
    ),
    
    # 场景名称（包含维度、差异级别、比例类型）
    # 更新：反映双设计对比，不再指定分层策略
    MEAN_SCENARIO = sprintf("A%dB%d_%s_%s_dual_design", 
                           STRATA_LEVELS_A, STRATA_LEVELS_B, 
                           SCENARIO_TYPE, PROPORTION_TYPE),
    
    # 固定参数
    N_POOL = 1000,
    BLOCK_SIZE = 4,
    SD_COMMON = 12,
    STRATA_PROPORTIONS_A = "0.5,0.5",  # A因素固定50:50
    TREATMENT_RATIO = "1,1",
    TREATMENT_RATIO_NAME = "1:1",
    
    # 新增：标记这是双设计对比（可选，用于文档）
    DESIGN_COMPARISON = "stratified_vs_simple"
  ) %>%
  ungroup() %>%
  mutate(Param_ID = 1:n()) %>%
  select(
    Param_ID, TARGET_N, STRATA_LEVELS_A, STRATA_LEVELS_B, PROPORTION_TYPE,
    # 删除：IS_STRATIFIED_BY_A, ANALYZE_BY_A
    SCENARIO_TYPE, SCENARIO_DESC, 
    MEAN_SCENARIO, MEAN_MATRIX_STR, N_POOL, BLOCK_SIZE, SD_COMMON,
    STRATA_PROPORTIONS_A, STRATA_PROPORTIONS_B,
    TREATMENT_RATIO, TREATMENT_RATIO_NAME,
    DESIGN_COMPARISON  # 可选，用于说明
  )

# ------------------------------------------------------------------------------
# 7. 运行参数网格模拟（修订版调用）
# ------------------------------------------------------------------------------

results <- run_parameter_grid_per_batch(
  param_grid = param_grid,
  n_iter = 1000,
  n_batch = 10,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "simulation_dual_design_AB_factors"  # 更新前缀
)
# ==============================================================================
# 双PDF报告生成：完整版（PDF1 + PDF2 均含分层/不分层双线）
# ==============================================================================

# 检查必要包
required_packages <- c("ggplot2", "scales", "dplyr", "tidyr", "pdftools")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(pdftools)


# ------------------------------------------------------------------------------
# 1. 准备完整数据（含PROPORTION_TYPE用于排序）
# ------------------------------------------------------------------------------
cat("\n========== 准备数据用于PDF生成 ==========\n")

if (!exists("results") || !"batch_summaries_all" %in% names(results)) {
  stop("错误: results$batch_summaries_all 不存在，请先运行模拟")
}

# 创建完整数据框（包含所有PDF需要的列 + PROPORTION_TYPE用于排序）
power_by_batch_full <- results$batch_summaries_all %>%
  filter(Batch_ID != "Overall") %>%
  mutate(
    # 基础转换
    Batch_ID_num = as.numeric(Batch_ID),
    
    # 格式化B因素层占比（用于显示）
    Proportion_Formatted = sapply(strsplit(as.character(STRATA_PROPORTIONS_B), ","), function(x) {
      paste(round(as.numeric(x) * 100, 0), collapse = ":")
    }),
    
    # 修复：完整映射Strata_Effect_Level（含Median）
    Strata_Effect_Level = case_when(
      SCENARIO_TYPE == "low" ~ "Low",
      SCENARIO_TYPE == "medium" ~ "Median",
      SCENARIO_TYPE == "high" ~ "High",
      TRUE ~ NA_character_
    ),
    
    # 创建PDF1分组键：样本量-层间差异-层数
    Group_Key_PDF1 = sprintf("%d-%s-%dL", TARGET_N, Strata_Effect_Level, STRATA_LEVELS_B),
    
    # 创建PDF2分组键：样本量-层占比-层数
    Group_Key_PDF2 = sprintf("%d-%s-%dL", TARGET_N, Proportion_Formatted, STRATA_LEVELS_B),
    
    # 提取不分层和分层Power
    Power_Unstrat = as.numeric(Power_simple_design),
    Power_Strat = as.numeric(Power_strat_design),
    
    # 🔑 新增：创建排序用的PROPORTION_TYPE（用于均衡→不均衡排序）
    PROPORTION_TYPE_SORT = case_when(
      STRATA_PROPORTIONS_B == "0.5,0.5" ~ "A_equal_50_50",
      STRATA_PROPORTIONS_B == "0.4,0.6" ~ "B_unequal_40_60",
      STRATA_PROPORTIONS_B == "0.3,0.7" ~ "C_unequal_30_70",
      STRATA_PROPORTIONS_B == "0.25,0.25,0.25,0.25" ~ "A_equal_25",
      STRATA_PROPORTIONS_B == "0.15,0.15,0.35,0.35" ~ "B_unequal_15_35",
      STRATA_PROPORTIONS_B == "0.12,0.18,0.28,0.42" ~ "C_unequal_12_42",
      STRATA_PROPORTIONS_B == "0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125" ~ "A_equal_125",
      STRATA_PROPORTIONS_B == "0.06,0.06,0.09,0.09,0.14,0.14,0.21,0.21" ~ "B_unequal_06_21",
      TRUE ~ "Z_unknown"
    )
  ) %>%
  select(
    TARGET_N, STRATA_LEVELS_B, Strata_Effect_Level, Proportion_Formatted,
    Group_Key_PDF1, Group_Key_PDF2, PROPORTION_TYPE_SORT,
    Batch_ID, Batch_ID_num, Power_Unstrat, Power_Strat
  ) %>%
  filter(!is.na(Strata_Effect_Level))

cat(sprintf("✓ 数据准备完成: %d 行\n", nrow(power_by_batch_full)))
cat(sprintf("✓ 唯一PDF1分组键: %d\n", n_distinct(power_by_batch_full$Group_Key_PDF1)))
cat(sprintf("✓ 唯一PDF2分组键: %d\n", n_distinct(power_by_batch_full$Group_Key_PDF2)))

# 设置输出目录
output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
 
# ==============================================================================
# PDF 1: 严格按 2层→4层→8层 + Low→High 顺序输出（共6页）- 无警告版
# ==============================================================================
cat("\n========== 生成 PDF 1: 2层/4层/8层 × Low/High（共6页）==========\n")

# 仅保留样本量=46 + 层间差异=Low/High 的数据
power_by_batch_pdf1 <- power_by_batch_full %>%
  filter(TARGET_N == 46, Strata_Effect_Level %in% c("Low", "High"))

# 🔑 关键修复：创建分组数据框（直接使用列值，避免字符串解析）
group_df_pdf1 <- power_by_batch_pdf1 %>%
  distinct(STRATA_LEVELS_B, Strata_Effect_Level) %>%
  arrange(STRATA_LEVELS_B, factor(Strata_Effect_Level, levels = c("Low", "High")))

pdf_file_pdf1 <- file.path(output_dir, "power_batch_PDF1_6pages_2L4L8L_LowHigh.pdf")
cairo_pdf(pdf_file_pdf1, width = 12, height = 7.5, family = "SimSun", onefile = TRUE)

plot_count_pdf1 <- 0
for (i in 1:nrow(group_df_pdf1)) {
  # 🔑 直接从分组数据框提取（无字符串解析！）
  strata_level <- group_df_pdf1$STRATA_LEVELS_B[i]
  effect_level <- group_df_pdf1$Strata_Effect_Level[i]
  
  # 直接过滤（无需解析group_key）
  group_data <- power_by_batch_pdf1 %>%
    filter(STRATA_LEVELS_B == strata_level, Strata_Effect_Level == effect_level)
  
  if (nrow(group_data) == 0) next
  
  target_n <- 46
  strata_levels <- strata_level
  strata_effect <- effect_level
  
  # 转换为长格式（修复：正确使用列名）
  plot_data <- group_data %>%
    select(Batch_ID_num, Proportion_Formatted, Power_Unstrat, Power_Strat) %>%
    pivot_longer(
      cols = c(Power_Unstrat, Power_Strat),
      names_to = "Analysis_Type",
      values_to = "Power_Value"
    ) %>%
    mutate(
      Analysis_Type = case_when(
        Analysis_Type == "Power_Unstrat" ~ "不分层分析",
        Analysis_Type == "Power_Strat" ~ "分层分析",
        TRUE ~ Analysis_Type
      )
    )
  
  batch_count_per_level <- as.integer(nrow(group_data) / (length(unique(group_data$Proportion_Formatted)) * 2))
  
  # 创建图形
  p <- ggplot(plot_data, aes(x = Batch_ID_num, y = Power_Value, 
                           color = Proportion_Formatted, 
                           linetype = Analysis_Type,
                           group = interaction(Proportion_Formatted, Analysis_Type))) +
    geom_line(data = filter(plot_data, Analysis_Type == "不分层分析"), size = 1, alpha = 0.9) +
    geom_point(data = filter(plot_data, Analysis_Type == "不分层分析"), size = 1, alpha = 0.85) +
    geom_line(data = filter(plot_data, Analysis_Type == "分层分析"), size = 1, alpha = 0.9, linetype = "dashed") +
    geom_point(data = filter(plot_data, Analysis_Type == "分层分析"), size = 1, alpha = 0.85, shape = 17) +
    geom_hline(yintercept = 0.8, linetype = "dotted", color = "black", size = 0.6) +
    labs(
      title = sprintf("%d 层(B因素) | 样本量 = %d | 层间差异 = %s", strata_levels, target_n, strata_effect),
      subtitle = sprintf("层占比对比（颜色） + 分析方法对比（线型）", batch_count_per_level),
      x = "批次 ID", y = "把握度 (Power)", color = "层占比", linetype = "分析方法",
      caption = "黑色虚线: 80% Power 目标线 | 实线=不分层, 虚线=分层 | 三角形=分层分析点"
    ) +
    scale_y_continuous(labels = percent, limits = c(0.50, 0.95), breaks = seq(0.50, 0.95, by = 0.05)) +
    scale_x_continuous(breaks = seq(1, max(plot_data$Batch_ID_num), by = 1)) +
    scale_color_brewer(palette = "Set1", name = "层占比") +
    scale_linetype_manual(
      values = c("不分层分析" = "solid", "分层分析" = "dashed"),
      labels = c("不分层分析 (实线)", "分层分析 (虚线)")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", size = 0.45),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  print(p)
  plot_count_pdf1 <- plot_count_pdf1 + 1
  
  props_in_group <- sort(unique(group_data$Proportion_Formatted))
  cat(sprintf("✓ [%d/6] %d层 | 层间差异=%s | 层占比: %s\n", 
              plot_count_pdf1, strata_levels, strata_effect,
              paste(props_in_group, collapse = ", ")))
}
dev.off()
cat(sprintf("✓ PDF 1 已生成: %s (6 页) | 顺序: 2层(Low/High) → 4层(Low/High) → 8层(Low/High)\n", pdf_file_pdf1))


# ==============================================================================
# PDF 2: 相同样本量+同样占比+层数 → 不同层间差异用颜色区分（分层/不分层双线）
# 排序：2层 → 4层 → 8层 | 均衡 → 不均衡
# ==============================================================================
cat("\n========== 生成 PDF 2: 2层→4层→8层 + 均衡→不均衡（层间差异用颜色区分，分层/不分层双线）==========\n")

# 🔑 修复排序：先按层数(2,4,8)，再按层占比均衡度(自定义排序)，再按样本量
unique_groups_pdf2 <- power_by_batch_full %>%
  distinct(Group_Key_PDF2, TARGET_N, Proportion_Formatted, STRATA_LEVELS_B, PROPORTION_TYPE_SORT) %>%
  arrange(
    STRATA_LEVELS_B,  # 2层 → 4层 → 8层
    PROPORTION_TYPE_SORT,  # A_equal → B_unequal → C_unequal (均衡→不均衡)
    TARGET_N  # 小样本 → 大样本
  ) %>%
  pull(Group_Key_PDF2)

pdf_file_pdf2 <- file.path(output_dir, "power_batch_PDF2_2L_4L_8L_order.pdf")
cairo_pdf(pdf_file_pdf2, width = 12, height = 7.5, family = "SimSun", onefile = TRUE)

plot_count_pdf2 <- 0
for (group_key in unique_groups_pdf2) {
  group_data <- power_by_batch_full %>% filter(Group_Key_PDF2 == group_key)
  if (nrow(group_data) == 0) next
  
  target_n <- unique(group_data$TARGET_N)[1]
  prop_formatted <- unique(group_data$Proportion_Formatted)[1]
  strata_levels <- unique(group_data$STRATA_LEVELS_B)[1]
  
  # 转换为长格式
  plot_data <- group_data %>%
    select(Batch_ID_num, Strata_Effect_Level, Power_Unstrat, Power_Strat) %>%
    pivot_longer(
      cols = c(Power_Unstrat, Power_Strat),
      names_to = "Analysis_Type",
      values_to = "Power_Value"
    ) %>%
    mutate(
      Analysis_Type = case_when(
        Analysis_Type == "Power_Unstrat" ~ "不分层分析",
        Analysis_Type == "Power_Strat" ~ "分层分析",
        TRUE ~ Analysis_Type
      )
    )
  
  batch_count_per_level <- as.integer(nrow(group_data) / (length(unique(group_data$Strata_Effect_Level)) * 2))
  
  p <- ggplot(plot_data, aes(x = Batch_ID_num, y = Power_Value, 
                           color = Strata_Effect_Level, 
                           linetype = Analysis_Type,
                           group = interaction(Strata_Effect_Level, Analysis_Type))) +
    geom_line(data = filter(plot_data, Analysis_Type == "不分层分析"), size = 1, alpha = 0.9) +
    geom_point(data = filter(plot_data, Analysis_Type == "不分层分析"), size = 1, alpha = 0.85) +
    geom_line(data = filter(plot_data, Analysis_Type == "分层分析"), size = 1, alpha = 0.9, linetype = "dashed") +
    geom_point(data = filter(plot_data, Analysis_Type == "分层分析"), size = 1, alpha = 0.85, shape = 17) +
    geom_hline(yintercept = 0.8, linetype = "dotted", color = "black", size = 0.6) +
    labs(
      title = sprintf("%d 层(B因素) | 样本量 = %d | 层占比 = %s", strata_levels, target_n, prop_formatted),
      subtitle = sprintf("层间差异对比（颜色） + 分析方法对比（线型） ", batch_count_per_level),
      x = "批次 ID", y = "把握度 (Power)", color = "层间差异级别", linetype = "分析方法",
      caption = "黑色虚线: 80% Power 目标线 | 实线=不分层, 虚线=分层 | 三角形=分层分析点"
    ) +
    scale_y_continuous(labels = percent, limits = c(0.50, 0.95), breaks = seq(0.50, 0.95, by = 0.05)) +
    scale_x_continuous(breaks = seq(1, max(plot_data$Batch_ID_num), by = 1)) +
    scale_color_manual(
      values = c("Low" = "#3498DB", "Median" = "#27AE60", "High" = "#E74C3C"),
      labels = c("Low" = "小 (Low)", "Median" = "中等 (Median)", "High" = "大 (High)")
    ) +
    scale_linetype_manual(
      values = c("不分层分析" = "solid", "分层分析" = "dashed"),
      labels = c("不分层分析 (实线)", "分层分析 (虚线)")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", size = 0.45),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  print(p)
  plot_count_pdf2 <- plot_count_pdf2 + 1
  
  levels_in_group <- sort(unique(group_data$Strata_Effect_Level))
  cat(sprintf("✓ [%d/%d] %d层 | 样本量=%d | 层占比=%s | 层间差异: %s\n", 
              plot_count_pdf2, length(unique_groups_pdf2), strata_levels, target_n, prop_formatted,
              paste(levels_in_group, collapse = ", ")))
}
dev.off()
cat(sprintf("✓ PDF 2 已生成: %s (%d 页) | 顺序: 2层→4层→8层 + 均衡→不均衡\n", pdf_file_pdf2, plot_count_pdf2))

# ==============================================================================
# 最终提示
# ==============================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ 双PDF报告生成完成（严格按 2层→4层→8层 + 均衡→不均衡 顺序）！\n")
cat(strrep("=", 70), "\n")
cat(sprintf("• PDF 1: %s (%d 页)\n", pdf_file_pdf1, plot_count_pdf1))
cat(sprintf("• PDF 2: %s (%d 页)\n", pdf_file_pdf2, plot_count_pdf2))
cat("\n排序逻辑:\n")
cat("  1. 层数升序: 2层 → 4层 → 8层（所有2层图先出现，然后4层，最后8层）\n")
cat("  2. 层占比均衡度: \n")
cat("     • 2层: 50:50 → 40:60 → 30:70\n")
cat("     • 4层: 25:25:25:25 → 15:15:35:35 → 12:18:28:42\n")
cat("     • 8层: 12.5:...:12.5 → 6:6:9:9:14:14:21:21\n")
cat("  3. 样本量升序: 小样本(46) → 大样本(182)\n")
cat("\n关键改进:\n")
cat("  • PDF1: 按 层数 → 层间差异 → 样本量 排序\n")
cat("  • PDF2: 按 层数 → 层占比均衡度(自定义A/B/C排序) → 样本量 排序\n")
cat("  • 所有图形同时展示分层/不分层Power（双线+三角形点）\n")
cat("  • 控制台输出清晰标注每页的层数、样本量、层占比/层间差异\n")
cat("\n提示: 在 Adobe Acrobat 中打开 PDF，按生成顺序直接阅读（无需书签）。\n")
cat(strrep("=", 70), "\n")
# ==============================================================================
# 最终提示
# ==============================================================================
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ 双PDF报告生成完成（均含分层/不分层双线）！\n")
cat(strrep("=", 70), "\n")
cat(sprintf("• PDF 1 (相同样本量+层间差异+层数): %s (%d 页)\n", pdf_file_pdf1, plot_count_pdf1))
cat(sprintf("• PDF 2 (相同样本量+同样占比+层数): %s (%d 页)\n", pdf_file_pdf2, plot_count_pdf2))
cat("\nPDF 1 特点:\n")
cat("  • 按 样本量-层间差异-层数 分组（如 182-Low-8L）\n")
cat("  • 颜色 = 层占比（50:50, 40:60, 30:70等）\n")
cat("  • 线型 = 分析方法（实线=不分层, 虚线=分层）\n")
cat("  • 点形状 = 分层分析用三角形（便于区分）\n")
cat("\nPDF 2 特点:\n")
cat("  • 按 样本量-层占比-层数 分组（如 182-50:50-8L）\n")
cat("  • 颜色 = 层间差异级别（蓝=小, 绿=中, 红=大）\n")
cat("  • 线型 = 分析方法（实线=不分层, 虚线=分层）\n")
cat("  • 点形状 = 分层分析用三角形（便于区分）\n")
cat("\n关键修复:\n")
cat("  • PDF1 和 PDF2 均同时展示分层/不分层Power（双线对比）\n")
cat("  • 修复Strata_Effect_Level映射：补充'Median'级别（原代码缺失）\n")
cat("  • 修复pivot_longer后列名：用Analysis_Type而非不存在的name列\n")
cat("  • 批次数计算：使用as.integer确保sprintf中使用整数格式（%d）\n")
cat("  • 统一数据准备：所有分组键在power_by_batch_full中一次性创建，避免变量冲突\n")
cat("\n提示: 在 Adobe Acrobat 中打开 PDF，点击左侧'书签'面板快速导航。\n")
cat(strrep("=", 70), "\n")
# ------------------------------------------------------------------------------
# 9. 生成汇总表格（含层间差异与对照组均值）
# ------------------------------------------------------------------------------
if (exists("results") && "batch_summaries_all" %in% names(results)) {
  power_summary <- results$batch_summaries_all %>%
    filter(Batch_ID == "Overall") %>%
    select(
      TARGET_N, STRATA_LEVELS_B, PROPORTION_TYPE, 
      SCENARIO_TYPE, Power_strat_design, Power_strat_z, Power_strat_unstrat, Power_simple_design, SMD_B_strat_vs_simple
    ) %>%
    mutate(
      Power_Strat = as.numeric(Power_strat_design),
      Power_Strat_Z = as.numeric(Power_strat_z),
      Power_Strat_Unstrat = as.numeric(Power_strat_unstrat),
      Power_Simple = as.numeric(Power_simple_design),
      SMD_B_Mean = as.numeric(sapply(strsplit(SMD_B_strat_vs_simple, " / "), `[`, 2))
    ) %>%
    group_by(TARGET_N, STRATA_LEVELS_B, PROPORTION_TYPE, SCENARIO_TYPE) %>%
    summarise(
      Power_Strat = mean(Power_Strat, na.rm = TRUE),
      Power_Strat_Z = mean(Power_Strat_Z, na.rm = TRUE),
      Power_Strat_Unstrat = mean(Power_Strat_Unstrat, na.rm = TRUE),
      Power_Simple = mean(Power_Simple, na.rm = TRUE),
      SMD_B_Mean = mean(SMD_B_Mean, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(TARGET_N, STRATA_LEVELS_B, PROPORTION_TYPE, SCENARIO_TYPE) %>%
    mutate(
      Power_Strat = sprintf("%.1f%%", Power_Strat * 100),
      Power_Strat_Z = sprintf("%.1f%%", Power_Strat_Z * 100),
      Power_Strat_Unstrat = sprintf("%.1f%%", Power_Strat_Unstrat * 100),
      Power_Simple = sprintf("%.1f%%", Power_Simple * 100),
      SMD_B_Mean = sprintf("%.3f", SMD_B_Mean)
    )
  
  summary_csv <- file.path(output_dir, "power_summary_strata_effect_grouping.csv")
  write.csv(power_summary, summary_csv, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("✓ 汇总表格已保存: %s\n", summary_csv))
  print(power_summary)
}

# ------------------------------------------------------------------------------
# 10. 最终提示
# ------------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("✓ 双潜在分层因素模拟完成！\n")
cat(strrep("=", 70), "\n")
cat("• 样本量: 182 (大样本) / 46 (小样本)\n")
cat("• A因素: 固定2层，50:50比例（用于分层随机化和分析）\n")
cat("• B因素: 2/4/8层，多种比例配置（仅用于结局模拟和SMD评估）\n")
cat("• 层间差异: low/medium/high 三级\n")
cat("• 输出:\n")
cat("  - Excel报告: 包含所有场景的Power、SMD、不平衡指标\n")
cat("  - PDF报告: 按样本量-层间差异-层数分组，层占比用颜色区分\n")
cat("\n关键价值:\n")
cat("  • 量化B因素未分层随机化的风险（SMD_B）\n")
cat("  • 识别高风险组合：小样本+高层间差异+不均衡比例\n")
cat("  • 为分层策略提供数据支持（何时必须分层，何时可简化）\n")
cat("\n提示: 完整运行请将 param_grid[1:12, ] 改为 param_grid（约96行）\n")
cat(strrep("=", 70), "\n")