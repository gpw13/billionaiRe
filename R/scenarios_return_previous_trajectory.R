#' Scenario return to previpus trajectory
#'
#' This scenario returns to the `scenario_previous_trajectory` after the last
#' value of `scenario_shock`.
#'
#' @inherit scenario_dip_recover
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams trim_values
#' @inherit scenario_aroc
#' @inheritParams scenario_dip_recover_iso3
#' @param scenario_shock (character) name of the scenario with the shock
#' @param scenario_previous_trajectory (character) name of the scenario with the previous trajectories.
#' @param scenario_col (character) name of the column with the scenarios.
scenario_return_previous_trajectory <- function(df,
                                                dip_year = 2020,
                                                recovery_year = 2022,
                                                start_year = 2018,
                                                end_year = 2025,
                                                value_col = "value",
                                                scenario_col = "scenario",
                                                scenario_name = "return_previous_trajectory",
                                                scenario_shock = "covid_shock",
                                                scenario_previous_trajectory = "pre_covid_trajectory",
                                                trim = TRUE,
                                                small_is_best = FALSE,
                                                keep_better_values = FALSE,
                                                upper_limit = 100,
                                                lower_limit = 0,
                                                trim_years = TRUE,
                                                start_year_trim = start_year,
                                                end_year_trim = end_year,
                                                source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y"))){

  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col, "type")
  assert_scenario_in_df(df, scenario = c(scenario_shock, scenario_previous_trajectory), scenario_col = scenario_col)

  full_df <- tidyr::expand_grid(
    "year" := dip_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
  )

  df_last_year_shock <- df %>%
    dplyr::filter(.data[[scenario_col]] == scenario_shock) %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::filter(.data[["year"]] == max(.data[["year"]]), .data[["year"]] >= dip_year) %>%
    dplyr::select(tidyselect::all_of(c("iso3", "ind", last_shock_year = "year", last_shock_value = value_col)))

  df_return_previous_trajectory <- df %>%
    dplyr::filter(.data[[scenario_col]] == scenario_previous_trajectory) %>%
    dplyr::left_join(df_last_year_shock, by = c("iso3", "ind")) %>%
    dplyr::full_join(full_df, by = c("year", "iso3", "ind")) %>%
    dplyr::mutate("{scenario_col}" := scenario_name,
                  scenario_value = dplyr::case_when(
                    .data[["year"]] > .data[["last_shock_year"]] & .data[["year"]] >= recovery_year ~ as.numeric(.data[[value_col]]),
                    .data[["year"]] > .data[["last_shock_year"]] & .data[["year"]] < recovery_year ~ as.numeric(.data[["last_shock_value"]]),
                    TRUE ~ NA_real_
                    ),
                  "type" := "projected"
    ) %>%
    dplyr::filter(.data[["year"]] >= dip_year, !.data[["type"]] %in% c("reported", "estimated"), !is.na(.data[["scenario_value"]])) %>%
    trim_values(
      col = "scenario_value", value_col = value_col, trim = trim, small_is_best = small_is_best,
      keep_better_values = keep_better_values, upper_limit = upper_limit,
      lower_limit = lower_limit, trim_years = trim_years,
      start_year_trim = start_year_trim, end_year_trim = end_year_trim
    ) %>%
    dplyr::select(-"last_shock_year", - "last_shock_value")

  df %>%
    dplyr::bind_rows(df_return_previous_trajectory) %>%
    dplyr::distinct()
}
