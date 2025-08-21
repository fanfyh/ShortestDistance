#' Calculate Descriptive Statistics for a Variable
#'
#' Computes common descriptive statistics (N, min, mean, sd, max, and counts for
#' missing, zero, and negative values) for a specified numeric variable.
#' The analysis can be performed on the entire dataset or grouped by another variable.
#'
#' @param DT A `data.table` object containing the data.
#' @param var A character string specifying the name of the numeric variable to analyze.
#' @param group_var A character string specifying the name of the grouping variable.
#'   The default is `"year"`. Set to `NULL` for an ungrouped analysis.
#'
#' @return A `data.table` containing the computed statistics. The results are also
#'   printed to the console with a descriptive header. The `data.table` is returned invisibly.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   year = rep(2019:2021, each = 100),
#'   value = c(rnorm(200, mean = 10), rep(NA, 100))
#' )
#'
#' # Grouped analysis by 'year'
#' summarise_desc(dt, "value")
#'
#' # Ungrouped analysis
#' summarise_desc(dt, "value", group_var = NULL)
#' }
#'
#' @export
summarise_desc <- function(DT, var, group_var = "year") {
    # 参数验证
    stopifnot(
        is.data.table(DT),
        var %in% names(DT),
        is.null(group_var) || group_var %in% names(DT)
    )

    # 直接构建表达式
    expr <- substitute(
        .(N = .N,
          min = min(x, na.rm = TRUE),
          mean = mean(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          max = max(x, na.rm = TRUE),
          miss_obs = sum(is.na(x)),
          zero_obs = sum(x == 0, na.rm = TRUE),
          negative_obs = sum(x < 0, na.rm = TRUE)),
        list(x = as.name(var))
    )

    # 执行计算
    if (is.null(group_var)) {
        var_stats <- DT[, eval(expr)]
    } else {
        var_stats <- DT[, eval(expr), by = group_var]
    }

    # 打印结果
    cat(sprintf("\nDescriptive stats for [%s]%s:\n",
                var,
                if (!is.null(group_var)) paste(" grouped by", group_var) else ""))
    print(var_stats)

    invisible(var_stats)
}


#' Count Missing Values by Group
#'
#' Computes the number of missing values (NA) for specified variables, optionally grouped by another variable.
#' Includes total observation count for reference.
#'
#' @param DT A `data.table` object.
#' @param vars A character vector of variable names to check for missing values.
#' @param group_var A character string specifying the grouping variable.
#'   The default is `"year"`. Set to `NULL` for an ungrouped analysis.
#'
#' @return A `data.table` with columns for the grouping variable (if applicable),
#'   `N` (total observations per group), and `NA_count_[var]` for each variable in `vars`.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(
#'   year = rep(2019:2021, each = 3),
#'   x = c(1, NA, 3, NA, NA, 6, 7, 8, NA),
#'   y = c(NA, 2, 3, NA, 5, NA, NA, 8, 9)
#' )
#'
#' # Grouped by year
#' count_na(dt, vars = c("x", "y"))
#'
#' # Ungrouped
#' count_na(dt, vars = c("x", "y"), group_var = NULL)
#' }
#'
#' @export
count_na <- function(DT, vars, group_var = "year") {
    # 参数验证
    stopifnot(
        is.data.table(DT),
        is.character(vars),
        all(vars %in% names(DT)),
        is.null(group_var) || (is.character(group_var) && length(group_var) == 1 && group_var %in% names(DT))
    )

    # 构建NA统计表达式
    na_exprs <- lapply(vars, function(v) {
        substitute(sum(is.na(var)), list(var = as.name(v)))
    })
    names(na_exprs) <- paste0("NA_", vars)

    # 构建完整表达式
    expr <- c(
        list(N = quote(.N)),
        na_exprs
    )

    # 执行计算
    if (is.null(group_var)) {
        result <- DT[, eval(as.call(c(quote(`.`), expr)))]
    } else {
        result <- DT[, eval(as.call(c(quote(`.`), expr))), by = group_var]
    }

    return(result)
}


#' Remove Negative Observations
#'
#' Filters a `data.table` to remove all rows where the specified variable
#' has a value less than zero.
#'
#' @param dt A `data.table` object.
#' @param var A character string specifying the name of the variable to check.
#'
#' @return A filtered `data.table` containing only rows where the value of `var` is greater than or equal to 0.
clean_negative <- function(dt, var) {
    dt[get(var) >= 0]
}

#' Calculate Logarithm with Zero and Negative Handling
#'
#' Computes the natural logarithm of a numeric vector. It handles non-positive values
#' by returning 0 for any value less than or equal to 1, instead of `log(0)` or an error for negative numbers.
#' This is useful for transformations where zero and negative values are not meaningful or are to be treated as a floor.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector of the same length as `x` with the logarithm applied.
newlog <- function(x) {
    # 对于负值和零，返回0，否则返回log(x)
    ifelse(x <= 1 , 0, log(x))
}


#' Winsorize Filter: Remove Observations Outside Specified Quantile Range
#'
#' Filters a `data.table` to remove observations where a specified variable falls outside
#' a given quantile range (lower, upper). This function removes extreme values,
#' it does not replace them. Can optionally group by variables before filtering.
#'
#' @param DT A `data.table` containing the data to filter.
#' @param var Character string naming the column to filter (e.g., `"revenue_ps"`).
#' @param group_vars Optional character vector of column names to group by (e.g., `c("year", "category")`).
#'                   If `NULL` (the default), the function operates on the entire dataset without grouping.
#' @param lower The lower quantile threshold (e.g., `0.05` for the 5th percentile). Default is `0`.
#' @param upper The upper quantile threshold (e.g., `0.95` for the 95th percentile). Default is `0.95`.
#'
#' @return A filtered `data.table` with only rows where `var` falls within the `[lower, upper]` quantiles.
#'         The original columns are preserved.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Sample data
#' dt <- data.table(
#'   year = rep(2019:2021, each = 100),
#'   revenue_ps = rnorm(300, mean = 100, sd = 20)
#' )
#'
#' # Basic usage (no grouping)
#' dt_filtered <- winsorize_filter(dt, "revenue_ps", lower = 0.05, upper = 0.95)
#'
#' # Grouped by 'year'
#' dt_filtered <- winsorize_filter(dt, "revenue_ps", group_vars = "year", lower = 0.1, upper = 0.9)
#' }
#'
#' @export
winsorize_filter <- function(DT, var, group_vars = NULL, lower = 0, upper = 0.95) {
    if (is.null(group_vars)) {
        DT[between(get(var),
                   quantile(get(var), lower, na.rm = TRUE),
                   quantile(get(var), upper, na.rm = TRUE))]
    } else {
        DT[, .SD[between(get(var),
                         quantile(get(var), lower, na.rm = TRUE),
                         quantile(get(var), upper, na.rm = TRUE))],
           by = group_vars]
    }
}


#' Calculate Year-on-Year Growth Rate
#'
#' Computes the percentage growth rate for a numeric vector. The function calculates `(x_t - x_{t-1}) / x_{t-1}`
#' for each element `x_t` and returns `NA` for the first element.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector of the same length as `x` containing the calculated growth rates.
#' @examples
#' calculate_growth(c(100, 110, 120))
#' # Returns: NA 0.10 0.09
calculate_growth <- function(x) {
    # 检查输入是否为数值向量
    if (!is.numeric(x)) {
        stop("Input must be a numeric vector.")
    }
    # 计算增长率并在首位插入 NA
    return(c(NA, diff(x) / lag(x, n = 1)[-1]))
}

#' Get Initial Values for Each Group
#'
#' For a given variable and year, this function identifies the value of `var`
#' at a specified `initial_period` and propagates that value forward and backward
#' for all other years. This is useful for creating a baseline or initial-period value column.
#'
#' @param var A numeric vector of the values of interest.
#' @param year A numeric vector of years corresponding to `var`.
#' @param initial_period The year to use as the initial period. The default is `1998`.
#'
#' @return A numeric vector of the same length as `var` where all values for a group
#'   are the value from the `initial_period`.
#'
#' @note This function requires the `zoo` package for `na.locf`.
get_initial_values <- function(var, year, initial_period = 1998) {
    # 检查输入是否有效
    if (length(var) != length(year)) {
        stop("`var` and `year` must have the same length.")
    }

    # 确保 initial_period 不小于数据中的最小年份
    min_year <- min(year, na.rm = TRUE)
    if (initial_period < min_year) {
        initial_period <- min_year
    }

    # 选择 initial_period 对应的值
    initial_values <- ifelse(year == initial_period, var, NA_real_)

    # 填充缺失值（向上和向下填充）
    initial_values <- zoo::na.locf(initial_values, na.rm = FALSE)  # 向上填充
    initial_values <- zoo::na.locf(initial_values, na.rm = FALSE, fromLast = TRUE)  # 向下填充

    return(initial_values)
}

#' Check for and Isolate Growth Rate Outliers
#'
#' This function calculates the growth rate of a specified variable, groups the data by `province`,
#' and identifies observations where the absolute growth rate exceeds a given `threshold`.
#' It then returns the original data for the identified outlier provinces, including a
#' 3-year window before and after each outlier event, to provide context.
#'
#' @param data A data frame or tibble containing the data. It must have columns `province` and `year`.
#' @param var The unquoted name of the variable to check for outliers (e.g., `gdp`).
#' @param threshold A numeric value specifying the growth rate threshold. The default is `0.3` (30%).
#'
#' @return A filtered data frame or tibble that includes all observations within a 3-year window
#'   around each identified outlier event.
#'
#' @note This function requires the `dplyr` and `tidyr` packages.
check_growth_outliers <- function(data, var, threshold = 0.3) {
    # 计算增长率
    data_growth <- data %>%
        group_by(province) %>%
        arrange(year) %>%
        mutate(growth_rate = ({{ var }} - lag({{ var }})) / lag({{ var }})) %>%
        ungroup()

    # 筛选异常观测
    outlier_obs <- data_growth %>%
        filter(abs(growth_rate) > threshold) %>%
        select(province, year, {{ var }}, growth_rate)

    # 获取异常观测的省份和前后三年年份
    outlier_years <- outlier_obs %>%
        mutate(year_lower = year - 3, year_upper = year + 3) %>%
        select(province, year_lower, year_upper) |>
        distinct()
    # 计算前后三年

    # 过滤出相应的年份数据
    data_filtered <- data %>%
        inner_join(outlier_years , by = "province") %>%
        filter(year >= year_lower & year <= year_upper) %>%
        select(-year_lower, -year_upper)  # 删除辅助列

    return(data_filtered)
}


#' Apply Winsorization to a Vector
#'
#' A simple wrapper function that applies winsorization to a numeric vector.
#' Winsorization caps the extreme values of a variable at a specified quantile.
#' This function uses the `psych::winsor` function.
#'
#' @param x A numeric vector to be winsorized.
#' @param trim A numeric value representing the proportion of values to be trimmed from each end
#'   (e.g., `0.025` for a 2.5% trim on each side, or `0.05` total). The default is `0.025`.
#'
#' @return A numeric vector of the same length as `x` with the winsorized values.
#' @note This function requires the `psych` package.
winsorize <- function(x, trim = 0.025) psych::winsor(x, trim = trim, na.rm = TRUE)


#' Impute Missing Values Using a Fixed-Effects Model and Record Imputation Details
#'
#' This function imputes missing values in panel data using a two-way fixed-effects model
#' (city and year fixed effects). It supports optional log transformation of the variable
#' before imputation and automatically generates a detailed log of the imputed values,
#' including context from adjacent non-missing observations.
#'
#' @param df An input data frame or tibble. It must contain the columns `cityid` and `year`.
#' @param var_to_impute The unquoted name of the variable to be imputed (e.g., `gdp`).
#'   This uses non-standard evaluation.
#' @param log_transform A logical value indicating whether to perform a log transformation
#'   on the variable before fitting the model. The default is `TRUE`.
#'
#' @return A list containing three elements:
#' \itemize{
#'   \item `filled_data` - The complete data frame with missing values imputed.
#'   \item `imputed_log` - A detailed data frame of all imputed observations, including
#'         the original `NA`, the new imputed value, and the previous and next non-missing
#'         values for context.
#'   \item `model` - The `fixest` model object used for the imputation.
#' }
#'
#' @section Implementation Details:
#' 1.  It validates the input data and necessary columns (`cityid`, `year`, `var_to_impute`).
#' 2.  It applies a log transformation (if enabled) to the non-missing values.
#' 3.  It fits a two-way fixed-effects model (`var_to_impute ~ 1 | cityid + year`).
#' 4.  It uses the model to predict and fill in the missing values.
#' 5.  It generates a detailed record of the imputations, including the context of the
#'     surrounding non-missing values.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- fill_and_export_fe(df, gdp)
#'
#' # Disable log transformation
#' result <- fill_and_export_fe(df, population, log_transform = FALSE)
#' }
#'
#' @note This function requires the `fixest`, `dplyr`, and `rlang` packages.
fill_and_export_fe <- function(df, var_to_impute, log_transform = TRUE) {
    # 参数验证 ---------------------------------------------------------------
    if (!is.data.frame(df)) stop("输入必须是 data.frame 或 tibble")

    # 动态获取变量名（兼容管道和非管道调用）
    var_expr <- rlang::enquo(var_to_impute)
    var_name <- rlang::as_name(var_expr)

    # 检查必要列是否存在
    required_cols <- c(var_name, "cityid", "year")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        stop("缺少必要列: ", paste(missing_cols, collapse = ", "))
    }

    # 数据预处理 -------------------------------------------------------------
    df_work <- df %>%
        mutate(
            # 标记原始缺失值位置
            is_na = is.na(!!var_expr),

            # 根据参数决定是否进行对数变换
            log_var = if (log_transform) {
                ifelse(is_na, NA, log(!!var_expr))
            } else {
                !!var_expr
            }
        )

    # 模型拟合 ---------------------------------------------------------------
    # 使用fixest::feols拟合双向固定效应模型
    # 模型公式：log_var ~ 1 | cityid + year
    # 即仅用城市和年份固定效应进行预测
    model <- feols(
        log_var ~ 1 | cityid + year,
        data = filter(df_work, !is.na(log_var))  # 仅使用非缺失值建模
    )

    # 缺失值填补 -------------------------------------------------------------
    df_work <- df_work %>%
        mutate(
            # 生成预测值（根据是否log变换进行反向转换）
            pred = if (log_transform) {
                exp(predict(model, newdata = .))  # 对数正态分布调整
            } else {
                predict(model, newdata = .)
            },

            # 用预测值替换原始缺失值
            !!var_name := ifelse(is_na, pred, !!var_expr)
        )

    # 生成填补记录 -----------------------------------------------------------
    imputed_log <- df_work %>%
        filter(is_na) %>%                      # 只保留被填补的观测
        arrange(cityid, year) %>%              # 按城市和年份排序
        group_by(cityid) %>%
        mutate(
            # 获取每个缺失值前后的实际值（非填补值）
            prev_value = lag(!!var_expr),        # 前一期值
            next_value = lead(!!var_expr)        # 后一期值
        ) %>%
        ungroup() %>%
        transmute(
            cityid,
            year,
            imputed_value = .data[[var_name]],   # 填补后的值
            original = NA_real_,                 # 原始值（均为NA）
            # 生成可读的填补上下文信息
            context = sprintf(
                "%d年cityid[%s]的%s填补为%.2f (前值: %s, 后值: %s)",
                year,
                cityid,
                var_name,
                imputed_value,
                ifelse(is.na(prev_value), "NA", sprintf("%.2f", prev_value)),
                ifelse(is.na(next_value), "NA", sprintf("%.2f", next_value))
            )
        )

    # 结果整理返回 -----------------------------------------------------------
    filled_data <- select(df_work, -is_na, -log_var, -pred)

    return(filled_data)
}
