#' Scenario to reach a fixed target from a baseline year by a target year
#'
#' This scenario allows to reach a target value from a baseline year by target
#' year It provides values for scenarios stated as "Reach XX% in INDICATOR by
#' YEAR" or "Eliminate XX by YEAR".
#'
#' The returned scenario is a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' @inherit scenario_percent_baseline
#' @param target_value value to be achieved by scenario by `target_year`
#' @param small_is_best Logical to identify if a lower value is better than a higher
#' one (e.g. lower obesity in a positive public health outcome, so obesity rate
#' should have small_is_best = TRUE).

scenario_fixed_target <- function(df,
                                  value = "value",
                                  ind = "ind",
                                  iso3 = "iso3",
                                  year = "year",
                                  scenario = "scenario",
                                  start_year = 2018,
                                  end_year = 2025,
                                  baseline_year = start_year,
                                  target_value,
                                  target_year = end_year,
                                  scenario_name = glue::glue("{target_value}_{target_year}"),
                                  small_is_best = FALSE) {
  df %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    dplyr::mutate("baseline_value_" := get_baseline_value(.data[[value]], .data[[year]], !!baseline_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scenario_value = calculate_fixed_target(target_value, small_is_best, .data[[year]], baseline_year, target_year, .data[["baseline_value_"]]),
      !!sym(value) := dplyr::case_when(
        dplyr::case_when(
          small_is_best & .data[[value]] < scenario_value
        )
      ),
      !!sym(scenario) := scenario_name
    ) %>%
    dplyr::select(-c("baseline_value_")) %>%
    dplyr::filter(.data[[year]] >= start_year & .data[[year]] <= end_year)
}

#' Calculate fixed target from a baseline year by a target year
#'
#' The returned values are a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' @inherit scenario_percent_baseline
#' @inherit scenario_fixed_target
#' @param target_value vector of values to use as targets
#' @param baseline_value value at baseline_year
calculate_fixed_target <- function(target_value,
                                   small_is_best,
                                   year,
                                   baseline_year,
                                   target_year,
                                   baseline_value) {
  if (small_is_best) {
    dplyr::case_when(
      year >= baseline_year & year <= target_year & baseline_value > target_value ~
      baseline_value + (target_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
      year >= baseline_year & year <= target_year & baseline_value <= target_value ~
      as.numeric(baseline_value),
      TRUE ~ NA_real_
    )
  } else {
    dplyr::case_when(
      year >= baseline_year & year <= target_year & baseline_value < target_value ~
      baseline_value + (target_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
      year >= baseline_year & year <= target_year & baseline_value >= target_value ~
      as.numeric(baseline_value),
      TRUE ~ NA_real_
    )
  }
}

#' Scenario to reach a fixed targets stored in a column
#'
#' `scenario_fixed_target_col` wraps around `scenario_fixed_target` to provide
#' targets from a column specified in `target_col` rather than a single value.
#'
#' @param target_col name of column with targets
#' @inherit scenario_fixed_target
scenario_fixed_target_col <- function(df,
                                      value = "value",
                                      ind = "ind",
                                      iso3 = "iso3",
                                      year = "year",
                                      scenario = "scenario",
                                      start_year = 2018,
                                      end_year = 2025,
                                      baseline_year = start_year,
                                      target_col = "target",
                                      target_year = end_year,
                                      scenario_name = glue::glue("{target_value}_{target_year}"),
                                      small_is_best = FALSE) {
  scenario_fixed_target(df,
    target_value = df[[target_col]],
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario_name = scenario_name,
    small_is_best = small_is_best
  )
}
