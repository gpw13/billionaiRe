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
#' If `value` has values that are higher or lower than the scenario values, then
#' only `value` will be kept, depending on `small_is_best`. For instance,
#' if the scenario value is 80 and the value 75 and small_is_best is TRUE, then
#' 80 will be kept.
#'
#' @inherit scenario_percent_baseline
#' @param target_value value to be achieved by scenario by `target_year`
#' @param small_is_best Logical to identify if a lower value is better than a higher
#' one (e.g. lower obesity in a positive public health outcome, so obesity rate
#' should have small_is_best = TRUE).
#' @param default_scenario name of the default scenario to be used.
#' @inheritParams trim_values

scenario_fixed_target <- function(df,
                                  target_value,
                                  value = "value",
                                  ind = "ind",
                                  iso3 = "iso3",
                                  year = "year",
                                  scenario = "scenario",
                                  start_year = 2018,
                                  end_year = 2025,
                                  baseline_year = start_year,
                                  target_year = end_year,
                                  scenario_name = glue::glue("{target_value}_{target_year}"),
                                  small_is_best = FALSE,
                                  trim = TRUE,
                                  keep_better_values = TRUE,
                                  upper_limit = 100,
                                  lower_limit = 0,
                                  trim_years = TRUE,
                                  ind_ids = billion_ind_codes("all"),
                                  default_scenario = "default") {
  assert_columns(df, year, iso3, ind, value, scenario)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  scenario_df <- df %>%
    dplyr::filter(.data[[scenario]] == default_scenario) %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    dplyr::mutate("baseline_value_" := get_baseline_value(.data[[value]], .data[[year]], !!baseline_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scenario_value = calculate_fixed_target(target_value, small_is_best, .data[[year]], baseline_year, target_year, .data[["baseline_value_"]]),
      !!sym(value) := dplyr::case_when(
        small_is_best & .data[[value]] < scenario_value ~ as.numeric(.data[[value]]),
        !small_is_best & .data[[value]] > scenario_value ~ as.numeric(.data[[value]]),
        TRUE ~ scenario_value
      ),
      !!sym(scenario) := scenario_name
    ) %>%
    dplyr::select(-c("baseline_value_", "scenario_value")) %>%
    trim_values(
      col = value,
      value = value,
      year = year,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    )

  df %>%
    dplyr::bind_rows(scenario_df)
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
#' @inheritParams trim_values
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
                                      small_is_best = FALSE,
                                      trim = TRUE,
                                      keep_better_values = TRUE,
                                      upper_limit = 100,
                                      lower_limit = 0,
                                      trim_years = TRUE,
                                      default_scenario = "default") {
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
    small_is_best = small_is_best,
    trim = trim,
    keep_better_values = keep_better_values,
    upper_limit = upper_limit,
    lower_limit = lower_limit,
    trim_years = trim_years,
    default_scenario = default_scenario
  )
}
