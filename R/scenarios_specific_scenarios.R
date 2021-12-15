#' Scenario to pick the best scenario out of a list of scenarios
#'
#' `scenario_best_of` picks the best value between multiple scenarios specified
#' in `scenario_names`. `small_is_best` allows to pick if higher value or
#' smaller values are best.
#' @param scenario_names names of the scenario to pick from.
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_best_of <- function(df,
                             scenario_names,
                             value = "value",
                             ind = "ind",
                             iso3 = "iso3",
                             year = "year",
                             start_year = 2018,
                             end_year = 2025,
                             target_year = 2025,
                             scenario_name = glue::glue("best_of_{paste0(scenario_names, collapse = '_')}"),
                             scenario = "scenario",
                             trim = TRUE,
                             small_is_best = FALSE,
                             keep_better_values = TRUE,
                             upper_limit = 100,
                             lower_limit = 0,
                             trim_years = TRUE,
                             ind_ids = billion_ind_codes("all")) {
  assert_columns(df, year, iso3, ind, scenario, value)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)
  assert_ind_start_end_year(df, iso3, year, value, target_year, target_year, ind, scenario, ind_ids = ind_ids[unique(df[[ind]])])

  best <- df %>%
    dplyr::filter(.data[[year]] == target_year) %>%
    dplyr::mutate(scenario_value = .data[[value]])

  if (small_is_best) {
    best <- best %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::filter(.data[["scenario_value"]] == min(.data[["scenario_value"]]))
  } else {
    best <- best %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::filter(.data[["scenario_value"]] == max(.data[["scenario_value"]]))
  }

  best_df <- df %>%
    dplyr::mutate(scenario_value = .data[[value]]) %>%
    dplyr::semi_join(best, by = c(iso3, ind, scenario)) %>%
    trim_values(
      col = "scenario_value",
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
    ) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name)

  df %>%
    dplyr::bind_rows(best_df)
}

#' Scenario establish a business as usual scenario
#'
#' `scenario_bau` filters for values between start_year and end_year for `default_scenario` and
#' returns values in value.
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_bau <- function(df,
                         value = "value",
                         ind = "ind",
                         iso3 = "iso3",
                         year = "year",
                         start_year = 2018,
                         end_year = 2025,
                         scenario_name = glue::glue("business_as_usual"),
                         scenario = "scenario",
                         trim = TRUE,
                         small_is_best = FALSE,
                         keep_better_values = TRUE,
                         upper_limit = 100,
                         lower_limit = 0,
                         trim_years = TRUE,
                         ind_ids = billion_ind_codes("all"),
                         default_scenario = "default") {
  assert_columns(df, year, iso3, ind, scenario, value)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  bau <- df %>%
    dplyr::filter(
      .data[[year]] %in% start_year:end_year,
      .data[[scenario]] == default_scenario
    ) %>%
    dplyr::mutate(scenario_value = .data[[value]]) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name) %>%
    trim_values(
      col = "scenario_value",
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
    dplyr::bind_rows(bau)
}
