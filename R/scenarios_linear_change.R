#' Scenario to add a linear  point change
#'
#' Scenario to add a `linear_value` percentage point change to `baseline_value`
#' from a `baseline_year`. It provides values for scenarios stated as "Increase
#' INDICATOR by XX% points".
#'
#' The calculation is done by taking the `baseline_year` `value_col` and adding the
#' `linear_value` times the number of years between the baseline year and the
#' current year. For instance, if `baseline_year` is 2018, `linear_value` is 2,
#' and `baseline_year` `value_col` is 10, then 2019 `value_col` will be 12, 2020 14,
#' etc.
#'
#' It differs from `scenario_aroc` `percent_change` in two ways: it is not
#' compounded and it adds percentage points and not percentage of values.
#'
#' `upper_limit` and `lower_limit` allow to trim values when they are exceeding
#' the bounds after calculations. If values were already exceeding the bounds
#' before calculations, they are kept.
#'
#' @param linear_value vector indicating the increase to apply.
#' @param upper_limit numeric indicating the upper bound of the data after
#' calculation. If `value_col` is already higher before calculation it will be kept
#' @param lower_limit numeric indicating the lower bound of the data after
#' calculation. If `value_col` is already lower before calculation it will be kept
#' @inherit scenario_percent_baseline
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#'
scenario_linear_change <- function(df,
                                   linear_value,
                                   value_col = "value",
                                   start_year = 2018,
                                   end_year = 2025,
                                   baseline_year = start_year,
                                   target_year = end_year,
                                   scenario_name = glue::glue("linear_change"),
                                   scenario_col = "scenario",
                                   trim = TRUE,
                                   small_is_best = FALSE,
                                   keep_better_values = FALSE,
                                   upper_limit = 100,
                                   lower_limit = 0,
                                   trim_years = TRUE,
                                   ind_ids = billion_ind_codes("all"),
                                   default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)
  assert_numeric(linear_value)

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col))

  scenario_linear_change <- scenario_df %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
    dplyr::mutate(
      baseline_value = get_baseline_value(.data[[value_col]], .data[["year"]], baseline_year),
      scenario_value = dplyr::case_when(
        .data[["year"]] >= baseline_year ~ .data[["baseline_value"]] + (linear_value * (.data[["year"]] - baseline_year))
      ),
      !!sym(scenario_col) := scenario_name
    ) %>%
    dplyr::ungroup() %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    ) %>%
    dplyr::select(-c("baseline_value"))

  df %>%
    dplyr::bind_rows(scenario_linear_change)
}

#' Scenario to add a linear percentage point change stored in a column
#'
#' `scenario_linear_change_col` wraps around
#' `scenario_linear_change` to provide linear values from a column
#' specified in `linear_value` rather than a single value.
#'
#' @param linear_value_col name of column with linear values
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_linear_change_col <- function(df,
                                       linear_value_col,
                                       value_col = "value",
                                       start_year = 2018,
                                       end_year = 2025,
                                       baseline_year = start_year,
                                       target_year = end_year,
                                       scenario_col = "scenario",
                                       scenario_name = glue::glue("linear_change"),
                                       trim = TRUE,
                                       small_is_best = FALSE,
                                       keep_better_values = FALSE,
                                       upper_limit = 100,
                                       lower_limit = 0,
                                       trim_years = TRUE,
                                       ind_ids = billion_ind_codes("all"),
                                       default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)
  assert_strings(linear_value_col)

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  linear_value_col_df <- df %>%
    dplyr::select(dplyr::all_of(c("iso3", "ind", linear_value_col))) %>%
    dplyr::distinct()

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col)) %>%
    dplyr::select(-tidyselect::all_of(linear_value_col)) %>%
    dplyr::left_join(linear_value_col_df, by = c("iso3", "ind"))

  scenario_linear_change_col_df <- scenario_df %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
    dplyr::mutate(
      baseline_value = get_baseline_value(.data[[value_col]], .data[["year"]], baseline_year),
      scenario_value = dplyr::case_when(
        .data[["year"]] >= baseline_year ~ .data[["baseline_value"]] + (.data[[linear_value_col]] * (.data[["year"]] - baseline_year))
      ),
      !!sym(scenario_col) := scenario_name
    ) %>%
    dplyr::ungroup() %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      baseline_col = "baseline_value",
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    ) %>%
    dplyr::select(- tidyselect::all_of(c("baseline_value", linear_value_col)))

  df %>%
    dplyr::bind_rows(scenario_linear_change_col_df)
}
