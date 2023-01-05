#' Scenario to change by a fixed percentage from a baseline value by target year
#'
#' This scenario allows to change a value by a fixed percentage to a provided year
#' from a provided baseline year. It provides values for scenarios stated as
#' "Reduce INDICATOR by XX% by YEAR"
#'
#' The `percent_change` parameter is understood as a percentage change,
#' and not a percentage point change, as this is usually what intended by those
#' formulations. If it is indeed the percentage change that is required, please
#' use `scenario_aroc`. For instance, to calculate the scenario "reduce the 2018
#' value (90%) by 30% by 2025", will results to a 2025 value of 63% and not 60%.
#'
#' The returned scenario is a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams trim_values
#'
#' @param percent_change Numeric with the percentage change in **points** that is
#' to be achieved from `value_col` in `baseline_year` by `target_year`. Should be
#' expressed a percentage point and not a fraction of 100 (e.g. 6% increase = 6,
#' and not 0.06).
#' For an increase, use a positive numeric, and a negative one for a decrease.
#' @param start_year Start year for scenario, defaults to 2018.
#' @param end_year End year for scenario, defaults to 2025
#' @param baseline_year Year from which the scenario is measured.
#' Defaults to `start_year`
#' @param target_year Year by which the scenario should eventually be
#' achieved. Defaults to `end_year`
#' @param scenario_name Name of the scenario. Defaults to scenario_{percent_change}_{baseline_year}
#' @param upper_limit limit at which the indicator should be caped.
#' Can take any of "guess", or any numeric. `guess` (default) will take 100 as
#' the limit if `percent_change` is positive, and 0 if negative.
#' @param lower_limit limit at which the indicator should be caped.
#' Can take any of "guess", or 0 to 100. `guess` (default) will take 0 as the
#' limit if `percent_change` is positive, and 100 if negative.
#' @inheritParams trim_values
#' @inheritParams scenario_fixed_target
#' @inheritParams transform_hpop_data
#'
#' @return Dataframe with scenario rows
#'
#' @family basic_scenarios
#' @family percent_baseline
#'
scenario_percent_baseline <- function(df,
                                      percent_change,
                                      value_col = "value",
                                      start_year = 2018,
                                      end_year = 2025,
                                      baseline_year = start_year,
                                      target_year = end_year,
                                      scenario_col = "scenario",
                                      scenario_name = glue::glue("{percent_change}_{baseline_year}"),
                                      trim = TRUE,
                                      small_is_best = FALSE,
                                      keep_better_values = FALSE,
                                      upper_limit = "guess",
                                      lower_limit = "guess",
                                      trim_years = TRUE,
                                      start_year_trim = start_year,
                                      end_year_trim = end_year,
                                      ind_ids = billion_ind_codes("all"),
                                      default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)

  upper_limit <- guess_limit(percent_change, upper_limit, limit_type = "upper_limit")
  lower_limit <- guess_limit(percent_change, lower_limit, limit_type = "lower_limit")

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col))

  percent_baseline_df <- scenario_df %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::group_by(.data[["ind"]], .data[["iso3"]]) %>%
    dplyr::mutate(
      "_goal_value" := get_goal(.data[[value_col]], .data[["year"]], !!baseline_year, !!percent_change),
      "_baseline_value" := get_baseline_value(
        .data[[value_col]],
        .data[["year"]],
        .data[["type"]],
        .data[[scenario_col]],
        default_scenario,
        start_year,
        type_filter = c("all")),
      "_baseline_year" := get_baseline_year(
        .data[["year"]],
        .data[["type"]],
        .data[[scenario_col]],
        default_scenario,
        start_year,
        type_filter = c("projected", "imputed", "reported", "estimated"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      scenario_value = calculate_percent_change_baseline(
        .data[["_baseline_value"]],
        .data[["_goal_value"]],
        .data[["year"]],
        !!start_year,
        !!target_year,
        !!baseline_year
      ),
      !!sym(scenario_col) := scenario_name
    ) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["year"]] >= start_year & .data[["year"]] <= baseline_year ~ as.numeric(.data[[value_col]]),
      TRUE ~ as.numeric(.data[["scenario_value"]])
    )) %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = TRUE,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim
    ) %>%
    dplyr::select(-c("_goal_value", "_baseline_value", "_baseline_year"))

  df %>%
    dplyr::bind_rows(percent_baseline_df)
}


#' Calculate percent change from baseline
#'
#' @inheritParams scenario_percent_baseline
#' @param baseline_value vector with the baseline value to be used
#' @param goal_value vector with the goal value to be used
#' @param year (vector) vector of years
#'
#' @noRd
#'
#' @keywords internal
#'
calculate_percent_change_baseline <- function(baseline_value, goal_value, year, start_year, target_year, baseline_year) {
  dplyr::case_when(year > start_year & year <= target_year~
                     baseline_value + (goal_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
                   year == start_year & year <= target_year ~ as.numeric(baseline_value),
                   TRUE ~ NA_real_
  )
}
