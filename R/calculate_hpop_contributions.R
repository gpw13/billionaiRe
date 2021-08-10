#' Calculate HPOP Indicator Contributions
#'
#' `calculate_hpop_contributions()` calculates indicator-level contributions and
#' changes for the HPOP Billion.
#'
#' @param year Column name of column with years.
#' @param start_year Base year for contribution calculation, defaults to 2018.
#' @param end_year End year(s) for contribution calculation, defaults to 2019 to 2023.
#' @param population Column name of column with population figures.
#' @param transform_value Column name of column(s) with transformed indicator values,
#'     used to calculate contributions.
#' @param contribution Column name of column(s) to store contribution (population)
#'     values. Must be the same length as `transform_value`.
#' @param contribution_pct Column name of column(s) to store contribution (percent)
#'     values. Must be the same length as `transform_value`.
#' @param contribution_pct_total_pop Column name of column(s) to store contribution
#' (percent of total population of the country) values. Must be the same length
#' as `transform_value`.
#' @param scenario Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#'
#' @inherit transform_hpop_data return details params
calculate_hpop_contributions <- function(df,
                                         year = "year",
                                         start_year = 2018,
                                         end_year = 2019:2023,
                                         iso3 = "iso3",
                                         ind = "ind",
                                         population = "population",
                                         transform_value = "transform_value",
                                         contribution = stringr::str_replace(transform_value, "transform_value", "contribution"),
                                         contribution_pct = paste0(contribution, "_percent"),
                                         contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                         scenario = NULL,
                                         ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, year, iso3, ind, population, transform_value, scenario)
  assert_ind_ids(ind_ids, "hpop")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)
  assert_same_length(transform_value, contribution)
  assert_years(start_year, end_year)

  # add columns if not already existing
  df <- billionaiRe_add_columns(df, c(contribution, contribution_pct,contribution_pct_total_pop), NA_real_)

  total_pop <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(.data[[iso3]]) %>%
    dplyr::distinct() %>%
    dplyr::mutate("total_pop" := wppdistro::get_population(.data[[iso3]], year = max(end_year)))

  # calculate relevant contributions for each contribution column
  df <- dplyr::group_by(df, dplyr::across(dplyr::any_of(c(iso3, ind, scenario)))) %>%
    dplyr::left_join(total_pop, by = iso3)


  for (i in seq(contribution)) {
    df <- dplyr::mutate(df,
                        !!rlang::sym(contribution_pct[i]) := ifelse(
                          !(.data[[ind]] %in% ind_ids) | !(.data[[year]] %in% end_year),
                          .data[[contribution_pct[i]]],
                          (.data[[transform_value[i]]] - .data[[transform_value[i]]][.data[[year]] == !!start_year])
                        ),
                        !!rlang::sym(contribution[i]) := ifelse(
                          !(.data[[ind]] %in% ind_ids) | !(.data[[year]] %in% end_year),
                          .data[[contribution[i]]],
                          .data[[population]] * .data[[contribution_pct[i]]] / 100
                        ),
                        !!rlang::sym(contribution_pct_total_pop[i]) := ifelse(
                          !(.data[[ind]] %in% ind_ids) | !(.data[[year]] %in% end_year),
                          .data[[contribution_pct_total_pop[i]]],
                          .data[[contribution[i]]] /.data[["total_pop"]] * 100
                        )
                      )
  }

  dplyr::ungroup(df)
  dplyr::select(df, -"total_pop")
}

