#' @export
linear_interp <- function(x) {
  zoo::na.approx(x, na.rm = FALSE)
}

#' @export
linear_interp_df <- function(df,
                             var,
                             type,
                             groups = NULL,
                             time = "year",
                             txt = "linear interpolation") {
  df <- group_by_vars(df, groups)
  df <- arrange_by_time(df, time)
  df %>%
    dplyr::mutate(!!sym(var) := linear_interp(.data[[var]]),
                  !!sym(type) := type_replacer(.data[[type]], .data[[var]], txt)) %>%
    dplyr::ungroup()
}

#' @export
flat_extrap <- function(x) {
  zoo::na.approx(x, rule = 2, na.rm = FALSE)
}

#' @export
flat_extrap_df <- function(df,
                           var,
                           type,
                           groups = NULL,
                           time = "year",
                           time_limit = 2000,
                           txt = "flat extrapolation") {
  df <- group_by_vars(df, groups)
  df <- arrange_by_time(df, time)
  df %>%
    dplyr::mutate(var_recent = replace(.data[[var]], .data[[time]] < time_limit, NA),
                  !!sym(var) := flat_extrap(var_recent),
                  !!sym(type) := type_replacer(.data[[type]], .data[[var]], txt)) %>%
    dplyr::select(-var_recent) %>%
    dplyr::ungroup()
}

#' @export
min_impute <- function(x, min_x = x) {
  min_x <- min_no_inf(min_x)
  replace(x, is.na(x), min_x)
}

#' @export
min_impute_df <- function(df,
                          var,
                          type,
                          groups = NULL,
                          time = "year",
                          time_limit = 2000,
                          txt = "minimum imputation",
                          filter_col = NULL,
                          filter_fn = NULL) {
  df <- group_by_vars(df, groups)

  df <- df %>%
    dplyr::mutate(var_recent = replace(.data[[var]], .data[[time]] < time_limit, NA))
  if ((!is.null(filter_col)) & (!is.null(filter_fn))) {
    df <- df %>%
      dplyr::mutate(!!sym(var) := min_impute(.data[[var]], var_recent[filter_fn(.data[[filter_col]])]))
  } else if (!is.null(filter_col)){
    df <- df %>%
      dplyr::mutate(!!sym(var) := min_impute(.data[[var]], var_recent[.data[[filter_col]]]))
  } else {
    df <- df %>%
      dplyr::mutate(!!sym(var) := min_impute(.data[[var]], var_recent))
  }
  df %>%
    dplyr::mutate(!!sym(type) := type_replacer(.data[[type]], .data[[var]], txt)) %>%
    dplyr::select(-var_recent) %>%
    dplyr::ungroup()
}

#' @export
med_impute <- function(x, med_x = x) {
  med_x <- median(med_x, na.rm = TRUE)
  replace(x, is.na(x), med_x)
}

#' @export
med_impute_df <- function(df,
                          var,
                          type,
                          groups = NULL,
                          time = "year",
                          time_limit = 2000,
                          txt = "median imputation",
                          filter_col = NULL,
                          filter_fn = NULL) {
  df <- group_by_vars(df, groups)
  df <- df %>%
    dplyr::mutate(var_recent = replace(.data[[var]], .data[[time]] < time_limit, NA))
  if ((!is.null(filter_col)) & (!is.null(filter_fn))) {
    df <- df %>%
      dplyr::mutate(!!sym(var) := med_impute(.data[[var]], var_recent[filter_fn(.data[[filter_col]])]))
  } else if (!is.null(filter_col)){
    df <- df %>%
      dplyr::mutate(!!sym(var) := med_impute(.data[[var]], var_recent[.data[[filter_col]]]))
  } else {
    df <- df %>%
      dplyr::mutate(!!sym(var) := med_impute(.data[[var]], var_recent))
  }
  df %>%
    dplyr::mutate(!!sym(type) := type_replacer(.data[[type]], .data[[var]], txt)) %>%
    dplyr::select(-var_recent) %>%
    dplyr::ungroup()
}

#' @export
hpop_imput_proj <- function(df,
                            var,
                            type,
                            iso3 = "iso3",
                            year = "year") {
  df <- linear_interp_df(df, var, type, iso3, year)
  df
}
