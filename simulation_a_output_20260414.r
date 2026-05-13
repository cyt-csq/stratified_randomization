

library(ggplot2)
library(scales)
library(dplyr)

# 加载核心模拟函数
source("C:\\Yuting\\AI\\kimi\\single_simulation_test_param_batch_20260413.r")

# 定义均值矩阵场景（每个场景绑定专属分层比例）
mean_scenarios <- list(
  # ========== 2 层场景 ==========
  list(
    name = "Global_Effect_2L_1", 
    matrix = matrix(c(10, 15, 10, 15), nrow = 2, ncol = 2, byrow = TRUE),
    proportions = list(c(0.5, 0.5), c(0.4, 0.6), c(0.3, 0.7)),  # 仅等比例
    effect_size = 5  # 标注效应量
  ),
   # ========== 2 层场景 ==========
  list(
    name = "Global_Effect_2L_2", 
    matrix = matrix(c(10, 20, 10, 20), nrow = 2, ncol = 2, byrow = TRUE),
    proportions = list(c(0.5, 0.5), c(0.4, 0.6), c(0.3, 0.7)),  # 仅等比例
    effect_size = 10  # 标注效应量
  ),
 
    # ========== 2 层场景 ====N=20======
  list(
    name = "Global_Effect_2L_3", 
    matrix = matrix(c(10, 25, 10, 25), nrow = 2, ncol = 2, byrow = TRUE),
    proportions = list(c(0.5, 0.5), c(0.4, 0.6), c(0.3, 0.7)),  # 仅等比例
    effect_size = 15  # 标注效应量
  ),


  # ========== 4 层场景 ==========
  list(
    name = "Global_Effect_4L_1", 
    matrix = matrix(rep(c(10, 15), 4), nrow = 4, ncol = 2, byrow = TRUE),
       proportions = list(
      c(0.25, 0.25, 0.25, 0.25),      # 等比例
      c(0.15, 0.15, 0.35, 0.35),      # 轻度不平衡
      c(0.12, 0.18, 0.28, 0.42)       # 中度不平衡
    ),
    effect_size = 5
  ),

    list(
    name = "Global_Effect_4L_2", 
    matrix = matrix(rep(c(10, 20), 4), nrow = 4, ncol = 2, byrow = TRUE),
       proportions = list(
      c(0.25, 0.25, 0.25, 0.25),      # 等比例
      c(0.15, 0.15, 0.35, 0.35),      # 轻度不平衡
      c(0.12, 0.18, 0.28, 0.42)       # 中度不平衡
    ),
    effect_size = 10
  ),

    list(
    name = "Global_Effect_4L_3", 
    matrix = matrix(rep(c(10, 25), 4), nrow = 4, ncol = 2, byrow = TRUE),
       proportions = list(
      c(0.25, 0.25, 0.25, 0.25),      # 等比例
      c(0.15, 0.15, 0.35, 0.35),      # 轻度不平衡
      c(0.12, 0.18, 0.28, 0.42)       # 中度不平衡
    ),
    effect_size = 15
  ),


  # ========== 8 层场景 ==========
  list(
    name = "Global_Effect_8L_1", 
    matrix = matrix(rep(c(10, 15), 8), nrow = 8, ncol = 2, byrow = TRUE),
         proportions = list(
      rep(0.125, 8),  # 等比例
      c(0.06, 0.06, 0.09, 0.09, 0.14, 0.14, 0.21, 0.21)  # 不等比例
    ),
    effect_size = 5
  ),

    list(
    name = "Global_Effect_8L_2", 
    matrix = matrix(rep(c(10, 20), 8), nrow = 8, ncol = 2, byrow = TRUE),
        proportions = list(
      rep(0.125, 8),  # 等比例
      c(0.06, 0.06, 0.09, 0.09, 0.14, 0.14, 0.21, 0.21)  # 不等比例
    ),
    effect_size = 10
  ),

   list(
    name = "Global_Effect_8L_3", 
    matrix = matrix(rep(c(10, 25), 8), nrow = 8, ncol = 2, byrow = TRUE),
        proportions = list(
      rep(0.125, 8),  # 等比例
      c(0.06, 0.06, 0.09, 0.09, 0.14, 0.14, 0.21, 0.21)  # 不等比例
    ),
    effect_size = 15
  )
  
  
)

# ========== 样本量 → 场景 → 比例 的三层绑定 ==========
sample_size_config <- list(
  "182b2" = list(
    ratio = c(1, 1),
    block_size = 2,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_1", "Global_Effect_4L_1","Global_Effect_8L_1")  # 182 样本量只测试效应量=5 的场景
  ),

"182b4" = list(
    ratio = c(1, 1),
    block_size = 4,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_1", "Global_Effect_4L_1","Global_Effect_8L_1")  # 182 样本量只测试效应量=5 的场景
  ),

"182b6" = list(
    ratio = c(1, 1),
    block_size = 6,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_1", "Global_Effect_4L_1","Global_Effect_8L_1")  # 182 样本量只测试效应量=5 的场景
  ),

  "204b3" = list(
    ratio = c(2, 1),
    block_size = 3,
    ratio_name = "2:1",
    scenarios = c("Global_Effect_2L_1", "Global_Effect_4L_1","Global_Effect_8L_1")  # 204 样本量测试效应量=5  
  ),

  "204b6" = list(
    ratio = c(2, 1),
    block_size = 6,
    ratio_name = "2:1",
    scenarios = c("Global_Effect_2L_1", "Global_Effect_4L_1","Global_Effect_8L_1")  # 204 样本量测试效应量=5  
  ),


   "46b2" = list(
    ratio = c(1, 1),
    block_size = 2,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_2", "Global_Effect_4L_2","Global_Effect_8L_2")  # 46 样本量只测试效应量=10 的场景
  ),

   "46b4" = list(
    ratio = c(1, 1),
    block_size = 4,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_2", "Global_Effect_4L_2","Global_Effect_8L_2")  # 46 样本量只测试效应量=10 的场景
  ),

   "46b6" = list(
    ratio = c(1, 1),
    block_size = 6,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_2", "Global_Effect_4L_2","Global_Effect_8L_2")  # 46 样本量只测试效应量=10 的场景
  ),


  "51b3" = list(
    ratio = c(2, 1),
    block_size = 3,
    ratio_name = "2:1",
    scenarios = c("Global_Effect_2L_2", "Global_Effect_4L_2","Global_Effect_8L_2")  # 51 样本量测试效应量=10 层场景
  ),

  "51b6" = list(
    ratio = c(2, 1),
    block_size = 6,
    ratio_name = "2:1",
    scenarios = c("Global_Effect_2L_2", "Global_Effect_4L_2","Global_Effect_8L_2")  # 51 样本量测试效应量=10 层场景
  ),

  
   "20b2" = list(
    ratio = c(1, 1),
    block_size = 2,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_3", "Global_Effect_4L_3","Global_Effect_8L_3")  # 46 样本量只测试效应量=10 的场景
  ),

   "20b4" = list(
    ratio = c(1, 1),
    block_size = 4,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_3", "Global_Effect_4L_3","Global_Effect_8L_3")  # 46 样本量只测试效应量=10 的场景
  ),

   "20b6" = list(
    ratio = c(1, 1),
    block_size = 6,
    ratio_name = "1:1",
    scenarios = c("Global_Effect_2L_3", "Global_Effect_4L_3","Global_Effect_8L_3")  # 46 样本量只测试效应量=10 的场景
  )
)

# 将 mean_scenarios 转换为以 name 为键的列表（便于查找）
scenarios_by_name <- setNames(mean_scenarios, sapply(mean_scenarios, `[[`, "name"))

# 创建参数网格（修复：正确解析样本量）
param_rows <- list()
row_idx <- 1

for (target_n in names(sample_size_config)) {
  config <- sample_size_config[[target_n]]
  
  # 🔑 修复1: 从键名中提取纯数字样本量（如 "182b2" → 182）
  target_n_parts <- strsplit(target_n, "_")[[1]]
  target_n_clean <- target_n_parts[1]  # 取下划线前的部分
    # 如果没有下划线（如 "182b2"），则尝试提取开头的数字
  if (length(target_n_parts) == 1 && !grepl("^[0-9]+$", target_n_clean)) {
    # 提取开头连续数字（如 "182b2" → "182"）
    target_n_clean <- regmatches(target_n, regexpr("^[0-9]+", target_n))[[1]]
  }
  
  target_n_int <- as.integer(target_n_clean)
  
  if (is.na(target_n_int)) {
    warning(sprintf("无法解析样本量: '%s'，跳过", target_n))
    next
  }
  
  # 验证配置
  if (is.null(config$scenarios) || length(config$scenarios) == 0) {
    warning(sprintf("样本量 %s 未指定场景，跳过", target_n))
    next
  }
  
  for (scenario_name in config$scenarios) {
    if (!scenario_name %in% names(scenarios_by_name)) {
      stop(sprintf("错误: 场景 '%s' 未在 mean_scenarios 中定义", scenario_name))
    }
    
    scenario <- scenarios_by_name[[scenario_name]]
    n_strata <- nrow(scenario$matrix)
    
    # 验证比例
    for (prop in scenario$proportions) {
      if (length(prop) != n_strata || abs(sum(prop) - 1) > 1e-6) {
        stop(sprintf("场景 %s: 比例无效", scenario_name))
      }
    }
    
    # 创建参数行（修复：使用 target_n_int）
    for (prop in scenario$proportions) {
      new_row <- data.frame(
        N_POOL = 1000,
        TARGET_N = target_n_int,   
        BLOCK_SIZE = config$block_size,
        SD_COMMON = 12,
        STRATA_LEVELS = n_strata,
        STRATA_PROPORTIONS = paste(prop, collapse = ","),
        TREATMENT_RATIO = paste(config$ratio, collapse = ","),
        MEAN_SCENARIO = scenario_name,
        MEAN_MATRIX_STR = matrix_to_string(scenario$matrix),
        EFFECT_SIZE = scenario$effect_size,
        stringsAsFactors = FALSE
      )
      param_rows[[row_idx]] <- new_row
      row_idx <- row_idx + 1
    }
  }
}

# 合并为数据框
param_grid <- do.call(rbind, param_rows)
rownames(param_grid) <- NULL
param_grid$Param_ID <- 1:nrow(param_grid)

# 验证输出
cat("参数组合数量:", nrow(param_grid), "\n\n")
cat("========== 样本量与场景绑定验证 ==========\n")
print(xtabs(~ TARGET_N + MEAN_SCENARIO, data = param_grid))

cat("\n========== 按样本量统计 ==========\n")
print(table(param_grid$TARGET_N))

cat("\n========== 前12行预览（验证精准绑定） ==========\n")
print(head(param_grid[, c("Param_ID", "TARGET_N", "MEAN_SCENARIO", 
                         "STRATA_LEVELS", "STRATA_PROPORTIONS", 
                         "TREATMENT_RATIO", "EFFECT_SIZE")], 12))

# 运行参数网格测试（仅测试绑定的组合）
results <- run_parameter_grid_per_batch(
  param_grid = param_grid,
  n_iter = 1000,
  n_batch = 50,
  verbose = TRUE,
  generate_excel = TRUE,
  output_prefix = "sample_size_scenario_binding"
)

# 分析：不同样本量在对应场景下的 Power
library(dplyr)
power_analysis <- results$batch_summaries_all %>%
  filter(Batch_ID == "Overall") %>%
  select(TARGET_N, MEAN_SCENARIO, EFFECT_SIZE, Power_simple_design, Power_strat_design) %>%
  mutate(
    Power_Unstrat = as.numeric(Power_simple_design),
    Power_Strat = as.numeric(Power_strat_design)
  )

cat("\n========== 样本量-场景绑定的 Power 分析 ==========\n")
print(power_analysis)

# ==============================================================================
# Power 批次分析（按样本量+层数+Block Size分组）- 修订版
# ==============================================================================

# 检查必要包
required_packages <- c("ggplot2", "scales", "dplyr", "pdftools")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
library(ggplot2)
library(scales)
library(dplyr)
library(pdftools)

# 1. 准备数据：提取每个 batch 的 Power（排除 Overall）
power_by_batch <- results$batch_summaries_all %>%
  filter(Batch_ID != "Overall") %>%
  mutate(
    Batch_ID_num = as.numeric(Batch_ID),
    # 格式化比例（如 "0.5,0.5" → "50:50"）
    Proportion_Formatted = sapply(strsplit(as.character(STRATA_PROPORTIONS), ","), function(x) {
      paste(round(as.numeric(x) * 100, 0), collapse = ":")
    }),
    # 🔑 修订1: 创建分组键：样本量-层数-Block Size（用于分组）
    Group_Key = sprintf("%d-%dL-block%d", TARGET_N, STRATA_LEVELS, BLOCK_SIZE),
    # 提取 Power
    Power_Unstrat = as.numeric(Power_simple_design),
    Power_Strat = as.numeric(Power_strat_design)
  ) %>%
  select(
    MEAN_SCENARIO, TARGET_N, EFFECT_SIZE, STRATA_LEVELS, STRATA_PROPORTIONS, 
    BLOCK_SIZE,  # ← 添加 BLOCK_SIZE 列
    Proportion_Formatted, Group_Key, Batch_ID, Batch_ID_num, 
    Power_Unstrat, Power_Strat
  )

# 2. 获取唯一分组列表（按样本量+层数+Block Size）
unique_groups <- unique(power_by_batch$Group_Key)
cat(sprintf("========== Power 批次分析（按样本量+层数+Block Size分组）==========\n"))
cat(sprintf("检测到 %d 个唯一分组（样本量+层数+Block Size）\n\n", length(unique_groups)))

# 获取当前日期
current_date <- format(Sys.Date(), "%Y%m%d")  # 格式：YYYYMMDD

# 构建输出路径（使用当天日期）
output_dir <- file.path("C:/Yuting/AI/kimi/sc", current_date)

# 创建目录（如果不存在）
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(sprintf("✓ 已创建输出目录: %s\n\n", output_dir))
} else {
  cat(sprintf("✓ 使用输出目录: %s\n\n", output_dir))
}


# 4. 创建 PDF 文件（cairo_pdf 支持中文）
pdf_file <- file.path(output_dir, "power_batch_analysis_grouped_by_blocksize.pdf")

# 关键：使用 cairo_pdf（Windows 系统宋体）
cairo_pdf(
  pdf_file, 
  width = 10, 
  height = 6.5, 
  family = "SimSun",  # Windows 系统宋体
  onefile = TRUE      # 多页 PDF
)

# 5. 为每个分组生成图形（比例用颜色，分析方法用线型）
plot_count <- 0
bookmark_titles <- character()

for (i in seq_along(unique_groups)) {
  group_key <- unique_groups[i]
  group_data <- power_by_batch %>% filter(Group_Key == group_key)
  
  if (nrow(group_data) == 0) next
  
  # 🔑 修订2: 获取分组元信息（新增block_size）
  target_n <- unique(group_data$TARGET_N)[1]
  strata_levels <- unique(group_data$STRATA_LEVELS)[1]
  block_size <- unique(group_data$BLOCK_SIZE)[1]  # ← 新增
  unique_props <- sort(unique(group_data$Proportion_Formatted))
  batch_count <- length(unique(group_data$Batch_ID))
  
  # 创建图形
  p <- ggplot(group_data, aes(x = Batch_ID_num)) +
    # 未分层分析（实线，固定黑色）
    geom_line(aes(y = Power_Unstrat, group = Proportion_Formatted, linetype = "不分层分析"), 
              color = "black", size = 1.1) +
    geom_point(aes(y = Power_Unstrat, group = Proportion_Formatted, shape = "不分层分析"), 
               color = "black", size = 2.8, alpha = 0.85) +
    
    # 分层分析（虚线，按比例着色，仅当存在时）
    geom_line(aes(y = Power_Strat, color = Proportion_Formatted, linetype = "分层分析"), 
              size = 1.1, na.rm = TRUE) +
    geom_point(aes(y = Power_Strat, color = Proportion_Formatted, shape = "分层分析"), 
               size = 2.8, na.rm = TRUE) +
    
    # 80% 参考线
    geom_hline(yintercept = 0.8, linetype = "dotted", color = "black", size = 0.6) +
    
    # 70% 辅助线
    geom_hline(yintercept = 0.7, linetype = "dotted", color = "gray70", size = 0.6) +
    
    # 🔑 修订3: 标题中添加Block Size
    labs(
      title = sprintf("%d 层分层 | 样本量=%d | Block Size=%d", 
                     strata_levels, target_n, block_size),
      subtitle = sprintf("不同比例对比（颜色） + 分析方法对比（线型） | 批次数=%d", batch_count),
      x = "批次 ID",
      y = "把握度 (Power)",
      color = "分层比例",
      linetype = "分析方法",
      shape = "分析方法",
      caption = "黑色虚线: 80% Power 目标线 | 灰色虚线: 70% 辅助线"
    ) +
    # 坐标轴（Y轴60%-100%）
    scale_y_continuous(
      labels = percent, 
      limits = c(0.60, 1.00),
      breaks = seq(0.60, 1.00, by = 0.05),
      expand = expansion(mult = c(0.05, 0.03))
    ) +
    scale_x_continuous(breaks = seq(1, max(group_data$Batch_ID_num), by = 1)) +
    # 线型映射
    scale_linetype_manual(
      values = c("不分层分析" = "solid", "分层分析" = "dashed"),
      labels = c("不分层分析 (实线)", "分层分析 (虚线)")
    ) +
    # 形状映射
    scale_shape_manual(
      values = c("不分层分析" = 16, "分层分析" = 21),
      labels = c("不分层分析", "分层分析")
    ) +
    # 颜色映射
    scale_color_brewer(palette = "Set1", name = "分层比例") +
    # 主题
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray92", size = 0.45),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  # 绘制到PDF（自动创建新页面）
  print(p)
  
  # 记录书签标题（包含block size）
  bookmark_title <- sprintf("%d层-%d样本-block%d", strata_levels, target_n, block_size)
  bookmark_titles <- c(bookmark_titles, bookmark_title)
  plot_count <- plot_count + 1
  
  cat(sprintf("✓ [%d/%d] %s | 比例: %s | Block Size=%d | 批次数=%d\n",
              i, length(unique_groups), group_key,
              paste(unique_props, collapse = ", "),
              block_size, batch_count))
}

# 6. 关闭PDF设备
dev.off()

# 7. 添加书签（使用 pdftools）
cat("\n正在添加PDF书签...\n")
tryCatch({
  # 创建书签结构
  bookmarks <- list()
  for (i in 1:plot_count) {
    bookmarks[[i]] <- list(
      title = bookmark_titles[i],
      page = i,
      level = 1
    )
  }
  
  # 保存带书签的PDF
  pdf_file_with_bookmarks <- gsub("\\.pdf$", "_with_bookmarks.pdf", pdf_file)
  pdf_subset(pdf_file, pages = 1:plot_count, bookmarks = bookmarks, output = pdf_file_with_bookmarks)
  
  cat(sprintf("✓ PDF书签已添加: %s (共 %d 页)\n", pdf_file_with_bookmarks, plot_count))
  
}, error = function(e) {
  warning(sprintf("⚠ 书签添加失败（%s），但PDF已生成: %s", e$message, pdf_file))
  cat("提示: 可手动打开PDF查看所有图形（每页一张）\n")
})

# 8. 生成汇总表格（按分组）
cat("\n========== Power 批次分析汇总（按样本量+层数+Block Size分组）==========\n")
power_summary <- power_by_batch %>%
  group_by(Group_Key, TARGET_N, STRATA_LEVELS, BLOCK_SIZE, Proportion_Formatted) %>%
  summarise(
    Scenario_Count = n_distinct(MEAN_SCENARIO),
    Batch_Count = n_distinct(Batch_ID),
    Power_Unstrat_Mean = mean(Power_Unstrat, na.rm = TRUE),
    Power_Unstrat_SD = sd(Power_Unstrat, na.rm = TRUE),
    Power_Strat_Mean = mean(Power_Strat, na.rm = TRUE),
    Power_Strat_SD = sd(Power_Strat, na.rm = TRUE),
    Power_Strat_Valid = sum(!is.na(Power_Strat)),
    .groups = "drop"
  ) %>%
  mutate(
    Power_Unstrat_CV = Power_Unstrat_SD / Power_Unstrat_Mean * 100,
    Power_Strat_CV = ifelse(Power_Strat_Valid > 1, 
                           Power_Strat_SD / Power_Strat_Mean * 100, NA),
    Status_Unstrat = ifelse(Power_Unstrat_Mean >= 0.8, "✓ 达标", 
                           ifelse(Power_Unstrat_Mean >= 0.7, "⚠ 边缘", "✗ 不足")),
    Status_Strat = ifelse(Power_Strat_Mean >= 0.8, "✓ 达标", 
                         ifelse(Power_Strat_Mean >= 0.7, "⚠ 边缘", "✗ 不足"))
  ) %>%
  select(
    Group_Key, TARGET_N, STRATA_LEVELS, BLOCK_SIZE, Proportion_Formatted,
    Scenario_Count, Batch_Count,
    Status_Unstrat, Power_Unstrat_Mean, Power_Unstrat_SD, Power_Unstrat_CV,
    Status_Strat, Power_Strat_Mean, Power_Strat_SD, Power_Strat_CV
  ) %>%
  arrange(TARGET_N, STRATA_LEVELS, BLOCK_SIZE, Proportion_Formatted)

# 格式化输出
power_summary_formatted <- power_summary %>%
  mutate(
    Power_Unstrat_Mean = sprintf("%.1f%%", Power_Unstrat_Mean * 100),
    Power_Unstrat_SD = sprintf("%.1f%%", Power_Unstrat_SD * 100),
    Power_Unstrat_CV = sprintf("%.1f%%", Power_Unstrat_CV),
    Power_Strat_Mean = ifelse(is.na(Power_Strat_Mean), "NA", 
                             sprintf("%.1f%%", Power_Strat_Mean * 100)),
    Power_Strat_SD = ifelse(is.na(Power_Strat_SD), "NA", 
                           sprintf("%.1f%%", Power_Strat_SD * 100)),
    Power_Strat_CV = ifelse(is.na(Power_Strat_CV), "NA", 
                           sprintf("%.1f%%", Power_Strat_CV))
  )

print(power_summary_formatted, row.names = FALSE)

# 9. 最终提示
cat("\n========== 完成 ==========\n")
cat(sprintf("• 已生成 %d 页 PDF 报告（按样本量+层数+Block Size分组）\n", plot_count))
cat(sprintf("• PDF文件: %s", pdf_file))
if (exists("pdf_file_with_bookmarks") && file.exists(pdf_file_with_bookmarks)) {
  cat(sprintf("\n• 带书签的PDF: %s", pdf_file_with_bookmarks))
}
cat("\n\nPDF 特点:\n")
cat("  • 每页一张图：相同样本量 + 相同层数 + 相同Block Size\n")
cat("  • 颜色区分：不同比例（如 50:50, 40:60, 30:70）\n")
cat("  • 线型区分：实线=不分层分析, 虚线=分层分析\n")
cat("  • 标题显示：样本量、层数、Block Size\n")
cat("  • Y轴范围：60%-100%（聚焦临床相关区间）\n")
cat("  • 中文完美显示：宋体（SimSun）\n")
cat("  • 书签导航：左侧书签面板快速跳转到各分组（含Block Size）\n")
cat("\n示例分组:\n")
cat("  • 第1页: 2层-182样本-block2 → 含50:50, 40:60, 30:70比例\n")
cat("  • 第2页: 2层-182样本-block4 → 含50:50, 40:60, 30:70比例（对比block2）\n")
cat("  • 第3页: 2层-182样本-block6 → 含50:50, 40:60, 30:70比例（对比block2/4）\n")
cat("  • 第4页: 2层-20样本-block2 → 含50:50, 40:60, 30:70比例\n")
cat("\n关键价值:\n")
cat("  • 直观对比：相同样本量+层数下，不同Block Size对Power的影响\n")
cat("  • 决策支持：识别最优Block Size（如block2 vs block4 vs block6）\n")
cat("  • 风险识别：小样本+大Block Size可能导致Power不稳定（波动大）\n")
cat("\n提示: 在 Adobe Acrobat 中打开 PDF，点击左侧'书签'面板导航。\n")