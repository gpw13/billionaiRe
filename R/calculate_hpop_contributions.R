#' Calculate HPOP Indicator Contributions
#'
#' `calculate_hpop_contributions()` calculates indicator-level contributions and
#' changes for the HPOP Billion.
#'
#' @param start_year Base year for contribution calculation, defaults to 2018.
#' @param end_year End year(s) for contribution calculation, defaults to 2019 to 2025.
#' @param transform_value_col Column name of column(s) with transformed indicator values,
#'     used to calculate contributions.
#' @param contribution_col Column name of column(s) to store contribution (population)
#'     values. Must be the same length as `transform_value_col`.
#' @param contribution_pct_col Column name of column(s) to store contribution (percent)
#'     values. Must be the same length as `transform_value_col`.
#' @param contribution_pct_total_pop_col Column name of column(s) to store contribution
#' (percent of total population of the country) values. Must be the same length
#' as `transform_value_col`.
#' @param scenario_col Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#'
#' @inherit transform_hpop_data return details params
calculate_hpop_contributions <- function(df,
                                         start_year = 2018,
                                         end_year = 2019:2025,
                                         transform_value_col = "transform_value",
                                         contribution_col = stringr::str_replace(transform_value_col, "transform_value", "contribution"),
                                         contribution_pct_col = paste0(contribution_col, "_percent"),
                                         contribution_pct_total_pop_col = paste0(contribution_col, "_percent_total_pop"),
                                         scenario_col = NULL,
                                         ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, "year", "iso3", "ind", "population", transform_value_col, scenario_col)
  assert_ind_ids(ind_ids, "hpop")
  assert_unique_rows(df, scenario_col, ind_ids)
  assert_same_length(transform_value_col, contribution_col)
  assert_years(start_year, end_year)

  # add columns if not already existing
  df <- billionaiRe_add_columns(df, c(contribution_col, contribution_pct_col, contribution_pct_total_pop_col), NA_real_)

  total_pop <- df %>%
    dplyr::ungroup() %>%
    dplyr::select("iso3") %>%
    dplyr::distinct() %>%
    dplyr::mutate("total_pop" := wppdistro::get_population(.data[["iso3"]], year = max(end_year)))

  # calculate relevant contributions for each contribution column
  df <- dplyr::group_by(df, dplyr::across(dplyr::any_of(c("iso3", "ind", scenario_col)))) %>%
    dplyr::left_join(total_pop, by = "iso3")


  for (i in seq(contribution_col)) {
    df <- dplyr::mutate(
      df,
      !!sym(contribution_pct_col[i]) := ifelse(
        !(.data[["ind"]] %in% ind_ids) | !(.data[["year"]] %in% end_year),
        .data[[contribution_pct_col[i]]],
        (.data[[transform_value_col[i]]] - .data[[transform_value_col[i]]][.data[["year"]] == !!start_year])
      ),
      !!sym(contribution_col[i]) := ifelse(
        !(.data[["ind"]] %in% ind_ids) | !(.data[["year"]] %in% end_year),
        .data[[contribution_col[i]]],
        .data[["population"]] * .data[[contribution_pct_col[i]]] / 100
      ),
      !!sym(contribution_pct_total_pop_col[i]) := ifelse(
        !(.data[["ind"]] %in% ind_ids) | !(.data[["year"]] %in% end_year),
        .data[[contribution_pct_total_pop_col[i]]],
        .data[[contribution_col[i]]] / .data[["total_pop"]] * 100
      )
    )
  }

  dplyr::ungroup(df)
  dplyr::select(df, -"total_pop")
}
