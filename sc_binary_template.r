library(openxlsx)

# ============================================================================
# 1. 辅助函数
# ============================================================================

#' 将 "0.5,0.5" 解析为数值向量
parse_prop_vec <- function(str) {
  if (is.na(str) || str == "") return(NULL)
  v <- suppressWarnings(as.numeric(strsplit(as.character(str), ",")[[1]]))
  if (any(is.na(v))) stop(sprintf("无法解析比例字符串: %s", str))
  return(v)
}

#' 将对照组/试验组向量对转为 PROB_MATRIX_STR 格式
prob_vec_to_str <- function(ctrl_vec, trt_vec, digits = 4) {
  paste(
    paste(round(ctrl_vec, digits), round(trt_vec, digits), sep = ","),
    collapse = ";"
  )
}

# ============================================================================
# 2. 核心：交叉层参数生成（含校验与范围检查）
# ============================================================================

gen_cross_params <- function(param_row) {
  
  # --- 安全数值转换（防 factor 陷阱） ---
  as_num <- function(x) as.numeric(as.character(x))
  
  # 实际因素
  n_s    <- as_num(param_row$STRATA_LEVELS)
  p_s    <- parse_prop_vec(param_row$STRATA_PROPORTIONS)
  ctrl0  <- as_num(param_row$ctrl_start)
  delta  <- as_num(param_row$delta)
  
  # Latent 因素
  n_sL   <- as_num(param_row$STRATA_LEVELS_L)
  p_sL   <- parse_prop_vec(param_row$STRATA_PROPORTIONS_L)
  ctrl0L <- as_num(param_row$ctrl_start_l)
  deltaL <- as_num(param_row$delta_l)
  
  # 共同参数
  lift   <- as_num(param_row$trt_lift)
  
  # --- 基本校验 ---
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
  
  # --- 计算各因素边际概率 ---
  ctrl_s  <- ctrl0  + (0:(n_s  - 1)) * delta
  trt_s   <- ctrl_s + lift
  p_mean  <- sum(ctrl_s * p_s)          # 实际因素总体均值
  
  ctrl_sL <- ctrl0L + (0:(n_sL - 1)) * deltaL
  trt_sL  <- ctrl_sL + lift
  p_meanL <- sum(ctrl_sL * p_sL)         # Latent 因素总体均值
  
  # --- 均值一致性警告（概率可加模型要求两均值相等才严格自洽）---
  if (abs(p_mean - p_meanL) > 1e-6) {
    warning(sprintf(
      "Param_ID=%s: 实际因素均值(%.4f)与 latent 因素均值(%.4f)不一致。交叉概率以实际因素均值(%.4f)为中心，latent 因素会导致实际因素层内概率发生 %.4f 的系统性偏移。",
      param_row$Param_ID, p_mean, p_meanL, p_mean, p_meanL - p_mean
    ))
  }
  
  # --- 交叉层概率（概率尺度无交互，以实际因素均值为中心）---
  # 公式：交叉 = 实际效应 + latent 效应 - 中心
  ctrl_cross <- outer(ctrl_s, ctrl_sL, FUN = function(a, l) a + l - p_mean)
  trt_cross  <- ctrl_cross + lift
  
  # 检查概率范围（必须落在 [0,1] 内）
  if (any(ctrl_cross < 0 | ctrl_cross > 1) || any(trt_cross < 0 | trt_cross > 1)) {
    stop(sprintf(
      "Param_ID=%s: 交叉层概率超出[0,1]范围。请检查 ctrl_start/delta 与 ctrl_start_l/delta_l 的组合是否合理。",
      param_row$Param_ID
    ))
  }
  
  # --- 交叉层占比（两因素独立 → 外积）---
  prop_cross <- outer(p_s, p_sL, FUN = "*")
  
  # --- 分层随机化用概率（按实际因素分层，对 latent 积分）---
  # 层内均值 = sum_j (cross[i,j] * prop_L[j]) / prop_A[i]
  ctrl_strat <- rowSums(ctrl_cross * prop_cross) / p_s
  trt_strat  <- rowSums(trt_cross * prop_cross) / p_s
  
  # --- 完全随机化用概率（总体边际，对两个因素都积分）---
  ctrl_overall <- sum(ctrl_cross * prop_cross)
  trt_overall  <- sum(trt_cross * prop_cross)
  
  # --- 交叉明细表（供模拟时按个体真实层抽样使用）---
  cross_df <- expand.grid(
    Actual = 1:n_s,
    Latent = 1:n_sL,
    stringsAsFactors = FALSE
  )
  cross_df$Prop       <- as.vector(prop_cross)
  cross_df$Ctrl_Rate  <- as.vector(ctrl_cross)
  cross_df$Trt_Rate   <- as.vector(trt_cross)
  
  # 返回结果列表
  list(
    actual_ctrl    = ctrl_s,
    actual_trt     = trt_s,
    latent_ctrl    = ctrl_sL,
    latent_trt     = trt_sL,
    cross_ctrl     = ctrl_cross,
    cross_trt      = trt_cross,
    cross_prop     = prop_cross,
    PROB_STRAT_STR = prob_vec_to_str(ctrl_strat, trt_strat),   # 分层随机化输入
    PROB_CR_STR    = prob_vec_to_str(ctrl_overall, trt_overall), # 完全随机化输入
    cross_detail   = cross_df
  )
}

# ============================================================================
# 3. 读入 Excel 并完整处理
# ============================================================================

read_param_grid_from_excel <- function(excel_path, sheet = 1) {
  
  # --- 文件检查 ---
  if (!file.exists(excel_path)) {
    stop(sprintf("Excel 文件不存在: %s", excel_path))
  }
  
  # --- 读取 Excel ---
  param_grid <- openxlsx::read.xlsx(excel_path, sheet = sheet)
  
  # --- 必要列检查 ---
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
  
  # --- 自动补全 Param_ID ---
  if (!"Param_ID" %in% colnames(param_grid)) {
    param_grid$Param_ID <- 1:nrow(param_grid)
  }
  
  # --- 批量数值转换（防 factor 陷阱）---
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
  
  # 检查数值转换是否产生 NA
  na_check <- sapply(param_grid[intersect(num_cols, colnames(param_grid))], function(x) any(is.na(x)))
  if (any(na_check)) {
    stop(sprintf("以下列含非数值内容，请检查 Excel 格式: %s", 
                 paste(names(na_check)[na_check], collapse = ", ")))
  }
  
  # --- 默认值补全 ---
  if (!"N_POOL" %in% colnames(param_grid))          param_grid$N_POOL <- 1000
  if (!"BLOCK_SIZE" %in% colnames(param_grid))     param_grid$BLOCK_SIZE <- 4
  if (!"MEAN_SCENARIO" %in% colnames(param_grid))  param_grid$MEAN_SCENARIO <- "Custom"
  if (!"MEAN_SCENARIO_L" %in% colnames(param_grid)) param_grid$MEAN_SCENARIO_L <- "Custom_L"
  if (!"TREATMENT_RATIO_NAME" %in% colnames(param_grid)) {
    param_grid$TREATMENT_RATIO_NAME <- as.character(param_grid$TREATMENT_RATIO)
  }
  
  # --- 逐行生成交叉层参数 ---
  cross_results <- lapply(1:nrow(param_grid), function(i) {
    gen_cross_params(param_grid[i, ])
  })
  
  # --- 将结果挂回主表 ---
  param_grid$PROB_STRAT_STR <- sapply(cross_results, `[[`, "PROB_STRAT_STR")
  param_grid$PROB_CR_STR    <- sapply(cross_results, `[[`, "PROB_CR_STR")
  param_grid$cross_detail   <- I(lapply(cross_results, `[[`, "cross_detail"))
  
  # 可选：把交叉矩阵展平为字符串也存下来（方便核对）
  param_grid$cross_ctrl_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_ctrl), 4), collapse = ";")
  })
  param_grid$cross_trt_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_trt), 4), collapse = ";")
  })
  param_grid$cross_prop_flat <- sapply(cross_results, function(x) {
    paste(round(as.vector(x$cross_prop), 4), collapse = ";")
  })
  
  cat(sprintf("✓ 已从 Excel 读取 %d 行参数组合: %s\n", nrow(param_grid), excel_path))
  return(param_grid)
}

# ============================================================================
# 4. 使用示例
# ============================================================================

 param_grid <- read_param_grid_from_excel("C:\\Yuting\\AI\\kimi\\binary\\sc\\param_grid_binary_template_sc.xlsx")

# 查看分层随机化概率字符串
 print(param_grid$PROB_STRAT_STR)

# 查看完全随机化概率字符串
 print(param_grid$PROB_CR_STR)

# 查看第1行的交叉明细
 print(param_grid$cross_detail[[1]])