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
                                       scenario = NULL,
                                       scenario_reported_estimated = "routine",
                                       scenario_covid_shock = "covid_shock",
                                       scenario_reference_infilling = "reference_infilling",
                                       ind_ids = billion_ind_codes("uhc", include_calculated = TRUE)) {
  assert_columns(df, "year", "iso3", "ind", scenario, transform_value_col)
  assert_same_length(transform_value_col, contribution_col)
  assert_same_length(contribution_col, contribution_pct_col)
  assert_years(start_year, end_year)
  assert_unique_rows(df, scenario, ind_ids)

  base_scenarios <- c(
    scenario_reported_estimated,
    scenario_covid_shock,
    scenario_reference_infilling
  )

  if (!is.null(scenario)) {
    df_base_scenario <- df %>%
      dplyr::filter(
        .data[[scenario]] %in% base_scenarios,
        .data[["ind"]] %in% ind_ids
      )

    df <- remove_unwanted_scenarios(
      df,
      scenario,
      base_scenarios
    )
  }

  assert_data_contributions(df, value_col, scenario, start_year,
    end_year,
    billion = "uhc",
    ind_ids
  )

  df <- billionaiRe_add_columns(df, c(contribution_col, contribution_pct_col), NA_real_) %>%
    dplyr::mutate("_population_temp" := wppdistro::get_population(.data[["iso3"]], year = pop_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", scenario))))

  for (i in 1:length(contribution_col)) {
    df <- df %>%
      dplyr::mutate(
        !!sym(contribution_pct_col[i]) := dplyr::case_when(
          !(.data[["ind"]] %in% ind_ids) ~ .data[[contribution_pct_col[i]]],
          !(.data[["year"]] %in% end_year) ~ .data[[contribution_pct_col[i]]],
          TRUE ~ (.data[[transform_value_col[i]]] - .data[[transform_value_col[i]]][.data[["year"]] == !!start_year])
        ),
        !!sym(contribution_col[i]) := dplyr::case_when(
          !(.data[["ind"]] %in% ind_ids) ~ .data[[contribution_col[i]]],
          !(.data[["year"]] %in% end_year) ~ .data[[contribution_col[i]]],
          TRUE ~ .data[["_population_temp"]] * .data[[contribution_pct_col[i]]] / 100
        )
      )
  }

  if (!is.null(scenario)) {
    df <- dplyr::bind_rows(df, df_base_scenario)
  }

  dplyr::select(df, -"_population_temp") %>%
    dplyr::ungroup()
}
