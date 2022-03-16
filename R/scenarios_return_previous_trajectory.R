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
#' @param ind_col Column name of column with indicator names.
#' @param iso3_col (character) name of column with the ISO3 country codes
#' @param ind (character) name of the indicator on which to calculate the
#'    scenario
#' @param scenario_shock (character) name of the scenario with the shock
#' @param scenario_previous_trajectory (character) name of the scenario with the previous trajectories.
#' @param scenario_col (character) name of the column with the scenarios.
scenario_return_previous_trajectory <- function(df,
                                                year = "year",
                                                ind_col = "ind",
                                                iso3_col = "iso3",
                                                dip_year = 2020,
                                                recovery_year = 2022,
                                                start_year = 2018,
                                                end_year = 2025,
                                                value = "value",
                                                scenario_col = "scenario",
                                                scenario_name = "return_previous_trajectory",
                                                type_col = "type",
                                                scenario_shock = "covid_shock",
                                                scenario_previous_trajectory = "pre_covid_trajectory",
                                                trim = TRUE,
                                                small_is_best = FALSE,
                                                keep_better_values = FALSE,
                                                upper_limit = 100,
                                                lower_limit = 0,
                                                trim_years = TRUE,
                                                source_col = "source",
                                                source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y"))){

  assert_columns(df, year, iso3_col, ind_col, value, scenario_col, type_col)
  assert_scenario_in_df(df, scenario = c(scenario_shock, scenario_previous_trajectory), scenario_col = scenario_col)

  full_df <- tidyr::expand_grid(
    "{year}" := dip_year:end_year,
    "{iso3_col}" := unique(df[[iso3_col]]),
    "{ind_col}" := unique(df[[ind_col]]),
  )

  df_last_year_shock <- df %>%
    dplyr::filter(.data[[scenario_col]] == scenario_shock) %>%
    dplyr::group_by(.data[[iso3_col]], .data[[ind_col]]) %>%
    dplyr::filter(.data[[year]] == max(.data[[year]]), .data[[year]] >= dip_year) %>%
    dplyr::select(.data[[iso3_col]], .data[[ind_col]], last_shock_year = .data[[year]], last_shock_value = .data[[value]])

  df_return_previous_trajectory <- df %>%
    dplyr::filter(.data[[scenario_col]] == scenario_previous_trajectory) %>%
    dplyr::left_join(df_last_year_shock, by = c(iso3_col, ind_col)) %>%
    dplyr::full_join(full_df, by = c(year, iso3_col, ind_col)) %>%
    dplyr::mutate("{scenario_col}" := scenario_name,
                  scenario_value = dplyr::case_when(
                    .data[[year]] > .data[["last_shock_year"]] & .data[[year]] >= recovery_year ~ as.numeric(.data[[value]]),
                    .data[[year]] > .data[["last_shock_year"]] & .data[[year]] < recovery_year ~ as.numeric(.data[["last_shock_value"]]),
                    TRUE ~ NA_real_
                    ),
                  "{type_col}" := "projected"
    ) %>%
    dplyr::filter(.data[[year]] >= dip_year, !.data[[type_col]] %in% c("reported", "estimated"), !is.na(.data[["scenario_value"]])) %>%
    trim_values(
      col = "scenario_value", value = value, year = year, trim = trim, small_is_best = small_is_best,
      keep_better_values = keep_better_values, upper_limit = upper_limit,
      lower_limit = lower_limit, trim_years = trim_years, start_year = start_year, end_year = end_year
    ) %>%
    dplyr::select(-"last_shock_year", - "last_shock_value")

  df %>%
    dplyr::bind_rows(df_return_previous_trajectory) %>%
    dplyr::distinct()
}
