#' Calculate UHC contribution
#'
#' `calculate_uhc_contribution()` calculates country-level UHC contributions, in
#' total population and percents, based on indicator level data. Calculates it
#' for each country-year combination in the provided data, and for specific
#' scenarios if specified.
#'
#' @inherit calculate_uhc_billion return details params
#' @inherit calculate_hpop_contributions params
#' @inherit add_hpop_populations params
#' @inheritParams recycle_data_scenario_single
#'
#' @export
calculate_uhc_contribution <- function(df,
                                       value_col = "value",
                                       start_year = 2018,
                                       end_year = 2019:2025,
                                       pop_year = 2025,
                                       transform_value_col = "transform_value",
                                       contribution_col = stringr::str_replace(transform_value_col, "transform_value", "contribution"),
                                       contribution_pct_col = paste0(contribution_col, "_percent"),
                                       scenario_col = NULL,
                                       scenario_reported_estimated = "routine",
                                       scenario_covid_shock = "covid_shock",
                                       scenario_reference_infilling = "reference_infilling",
                                       ind_ids = billion_ind_codes("uhc", include_calculated = TRUE, include_subindicators = TRUE),
                                       default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", scenario_col, transform_value_col)
  assert_same_length(transform_value_col, contribution_col)
  assert_same_length(contribution_col, contribution_pct_col)
  assert_years(start_year, end_year)
  assert_unique_rows(df, scenario_col, ind_ids)

  base_scenarios <- c(
    scenario_reported_estimated,
    scenario_covid_shock,
    scenario_reference_infilling
  )

  if (!is.null(scenario_col)) {
    df_base_scenario <- df %>%
      dplyr::filter(
        .data[[scenario_col]] %in% base_scenarios
      )

    df <- remove_unwanted_scenarios(
      df,
      scenario_col,
      base_scenarios
    )
  }

  assert_data_contributions(df,
                            value_col,
                            scenario_col,
                            start_year,
                            end_year,
                            billion = "uhc",
                            ind_ids
  )

  df <- billionaiRe_add_columns(df, c(contribution_col, contribution_pct_col), NA_real_) %>%
    dplyr::mutate("_population_temp" := wppdistro::get_population(.data[["iso3"]], year = pop_year))

  for (i in 1:length(contribution_col)) {

    if(!is.null(scenario_col)){
      df <- df %>%
        dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
        dplyr::mutate(
          baseline_value = .data[[transform_value_col[i]]][.data[["year"]] == start_year & .data[[scenario_col]] == default_scenario]
        )
    }else{
      df <- df %>%
        dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
        dplyr::mutate(
          baseline_value = .data[[transform_value_col[i]]][.data[["year"]] == start_year]
        )
    }

    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", scenario_col)))) %>%
      dplyr::mutate(
        !!sym(contribution_pct_col[i]) := dplyr::case_when(
          !(.data[["ind"]] %in% ind_ids) ~ .data[[contribution_pct_col[i]]],
          !(.data[["year"]] %in% end_year) ~ .data[[contribution_pct_col[i]]],
          TRUE ~ (.data[[transform_value_col[i]]] - .data[["baseline_value"]])
        ),
        !!sym(contribution_col[i]) := dplyr::case_when(
          !(.data[["ind"]] %in% ind_ids) ~ .data[[contribution_col[i]]],
          !(.data[["year"]] %in% end_year) ~ .data[[contribution_col[i]]],
          TRUE ~ .data[["_population_temp"]] * .data[[contribution_pct_col[i]]] / 100
        )
      )
  }

  if (!is.null(scenario_col)) {
    df <- dplyr::bind_rows(df, df_base_scenario)
  }

  dplyr::select(df, -c("_population_temp", "baseline_value")) %>%
    dplyr::ungroup()
}
