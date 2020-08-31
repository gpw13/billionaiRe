#' @noRd
min_no_inf <- function(x, na.rm = TRUE) {
  x <- suppressWarnings(min(x, na.rm = na.rm))
  ifelse(is.infinite(x), NA, x)
}

#' @noRd
arrange_by_time <- function(df, time) {
  if (!is.null(time)) {
    df <- dplyr::arrange(df, .data[[time]], .by_group = TRUE)
  }
  df
}

#' @noRd
group_by_vars <- function(df, vars) {
  if (!is.null(vars)) {
    df <- dplyr::group_by(df, dplyr::across(vars))
  }
  df
}

#' @noRd
get_filter_fn <- function(x) {
  if (is.null(x)) {
    function(y) y[TRUE]
  } else if (is.function(x)) {
    x
  }
}

#' @noRd
type_replacer <- function(type, x, txt) {
  replace(type, is.na(type) & !is.na(x), txt)
}

#' @noRd
remove_below_0 <- function(x) {
  replace(x, x <= 0, NA)
}
