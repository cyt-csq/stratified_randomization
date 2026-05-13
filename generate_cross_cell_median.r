library(openxlsx)

# ============================================================
# 精确中位矩阵生成器（用户自定义 base_factors）
# ============================================================
generate_exact_median <- function(
  overall_median_ctrl,   # 总体对照组中位，如 15
  overall_hr,            # 总体HR，如 0.5
  base_factors_a,        # A因素每层风险倍数，如 c(1, 3)
  base_factors_b,        # B因素每层风险倍数，如 c(1, 2, 4)
  prop_a,                # A各层比例，如 c(0.5, 0.5)
  prop_b                 # B各层比例，如 c(0.2, 0.3, 0.5)
) {
  # --- 校验 ---
  if (length(base_factors_a) != length(prop_a)) 
    stop("base_factors_a 与 prop_a 长度必须相同")
  if (length(base_factors_b) != length(prop_b)) 
    stop("base_factors_b 与 prop_b 长度必须相同")
  if (abs(sum(prop_a) - 1) > 1e-6 || abs(sum(prop_b) - 1) > 1e-6) 
    stop("prop_a 和 prop_b 必须各自求和为 1")
  
  overall_median_trt <- overall_median_ctrl / overall_hr
  
  # 构建 A×B 联合层
  grid <- expand.grid(
    A = seq_along(base_factors_a),
    B = seq_along(base_factors_b),
    stringsAsFactors = FALSE
  )
  grid$prop <- prop_a[grid$A] * prop_b[grid$B]
  grid$factor <- base_factors_a[grid$A] * base_factors_b[grid$B]
  
  # --- 数值求解 lambda：混合分布总体中位 = overall_median_ctrl ---
  target_fn <- function(lambda) {
    sum(grid$prop * exp(-log(2) * grid$factor * lambda)) - 0.5
  }
  lambda <- uniroot(target_fn, interval = c(1e-10, 1e6))$root
  
  # 各层精确中位
  grid$median_ctrl <- overall_median_ctrl / (grid$factor * lambda)
  grid$median_trt  <- grid$median_ctrl / overall_hr
  
  # --- 辅助：子总体精确混合中位 ---
  calc_mixture_median <- function(medians, proportions) {
    proportions <- proportions / sum(proportions)
    f <- function(t) sum(proportions * exp(-log(2) * t / medians)) - 0.5
    lo <- min(medians) * 0.01
    hi <- max(medians) * 100
    while (f(lo) < 0) lo <- lo * 0.1
    while (f(hi) > 0) hi <- hi * 10
    uniroot(f, interval = c(lo, hi))$root
  }
  
  # --- A 的每层边际中位（合并 B）---
  marginal_a <- data.frame(
    A_level = seq_along(base_factors_a),
    A_factor = base_factors_a,
    Marginal_Median_Ctrl = NA_real_,
    Marginal_Median_Trt = NA_real_
  )
  for (i in seq_along(base_factors_a)) {
    sub <- grid[grid$A == i, ]
    m <- calc_mixture_median(sub$median_ctrl, sub$prop)
    marginal_a$Marginal_Median_Ctrl[i] <- m
    marginal_a$Marginal_Median_Trt[i]  <- m / overall_hr
  }
  
  # --- B 的每层边际中位（合并 A）---
  marginal_b <- data.frame(
    B_level = seq_along(base_factors_b),
    B_factor = base_factors_b,
    Marginal_Median_Ctrl = NA_real_,
    Marginal_Median_Trt = NA_real_
  )
  for (j in seq_along(base_factors_b)) {
    sub <- grid[grid$B == j, ]
    m <- calc_mixture_median(sub$median_ctrl, sub$prop)
    marginal_b$Marginal_Median_Ctrl[j] <- m
    marginal_b$Marginal_Median_Trt[j]  <- m / overall_hr
  }
  
  # --- 总体验证 ---
  overall_check <- calc_mixture_median(grid$median_ctrl, grid$prop)
  
  list(
    lambda = lambda,
    joint_grid = grid,
    marginal_a = marginal_a,
    marginal_b = marginal_b,
    overall_input_ctrl = overall_median_ctrl,
    overall_input_trt = overall_median_trt,
    overall_check_ctrl = overall_check,
    overall_check_trt = overall_check / overall_hr
  )
}


# ============================================================
# 示例调用（A=2层，B=3层）
# ============================================================
res <- generate_exact_median(
  overall_median_ctrl = 15,
  overall_hr = 0.5,
  base_factors_a = c(1, 3),        # A1预后好(1倍)，A2预后差(3倍)
  base_factors_b = c(1, 2, 4),     # B1好，B2中，B3差
  prop_a = c(0.5, 0.5),
  prop_b = c(0.2, 0.3, 0.5)
)

# 查看结果
print(res$joint_grid)      # 联合层中位
print(res$marginal_a)      # A每层边际中位
print(res$marginal_b)      # B每层边际中位
cat("总体对照中位验证:", res$overall_check_ctrl, "\n")

# 输出到 Excel
wb <- createWorkbook()
addWorksheet(wb, "Joint_Strata")
writeData(wb, "Joint_Strata", res$joint_grid, rowNames = FALSE)
addWorksheet(wb, "Marginal_A")
writeData(wb, "Marginal_A", res$marginal_a, rowNames = FALSE)
addWorksheet(wb, "Marginal_B")
writeData(wb, "Marginal_B", res$marginal_b, rowNames = FALSE)
addWorksheet(wb, "Validation")
writeData(wb, "Validation", data.frame(
  Metric = c("Overall_Ctrl_Input", "Overall_Ctrl_Exact", 
             "Overall_Trt_Input", "Overall_Trt_Exact", "Lambda"),
  Value = c(res$overall_input_ctrl, res$overall_check_ctrl,
            res$overall_input_trt, res$overall_check_trt, res$lambda)
), rowNames = FALSE)

saveWorkbook(wb, "median_matrix_exact.xlsx", overwrite = TRUE)