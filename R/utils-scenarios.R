
#' Get goal/end value
#'
#' @param value vector of values
#' @param year vector of years. Must be the same length as `value`
#' @param start_year Year at which the
#'
#' @noRd
get_goal <- function(value, year, start_year, perc_change) {
  start_year <- max(min(year), start_year)

  value[year == start_year] * (100 + perc_change) / 100
}

#' Get baseline value
#'
#' @param value vector of values
#' @param year vector of years. Must be the same length as `value`
#' @param start_year Year at which the
#'
#' @noRd
get_baseline_value <- function(value, year, baseline_year) {
  value[year == baseline_year]
}
