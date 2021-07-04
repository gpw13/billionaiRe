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
#' @param contribution Glue specification used to transform `transform_value` column
#'     names into contribution column names. Defaults to replacing the string
#'     `"transform_value"` with `"contribution"`.
#' @param scenario Column name of column with scenario identifiers. Useful for
#'     calculating contributions on data in long format rather than wide format.
#'
#' @inherit transform_hpop_data return details params
#'
#' @export
calculate_hpop_contributions <- function(df,
                                         year = "year",
                                         start_year = 2018,
                                         end_year = 2019:2023,
                                         iso3 = "iso3",
                                         ind = "ind",
                                         population = "population",
                                         transform_value = "transform_value",
                                         contribution = stringr::str_replace(transform_value, "transform_value", "contribution"),
                                         scenario = NULL,
                                         ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, year, iso3, ind, population, transform_value, scenario)
  assert_ind_ids(ind_ids, "hpop")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)
  assert_same_length(transform_value, contribution)
  assert_years(start_year, end_year)

  # add columns if not already existing
  df <- billionaiRe_add_columns(df, contribution, NA_real_)

  # calculate relevant contributions for each contribution column
  df <- dplyr::group_by(df, dplyr::across(dplyr::any_of(c(iso3, ind, scenario))))

  for (i in length(contribution)) {
    df <- dplyr::mutate(df, !!sym(contribution[i]) := calculate_hpop_contribution_vector(.data[[contribution[i]]],
                                                                                         .data[[ind]],
                                                                                         ind_ids,
                                                                                         .data[[year]],
                                                                                         !!start_year,
                                                                                         !!end_year,
                                                                                         .data[[transform_value[i]]],
                                                                                         .data[[population]]))
  }

  dplyr::ungroup(df)
}


#' Calculate HPOP contribution in a vector
#'
#' Done this to avoid using [dplyr::case_when()]  because that evaluates all
#' RHS arguments, which produces errors for indicators without sufficient data.
#'
#' @inheritParams calculate_hpop_contributions
#' @param contribution Contribution vector
#' @param ind Ind vector
#' @param year Year vector
#' @param pop Population vector
#' @param transform_value Transformed value vector
calculate_hpop_contribution_vector <- function(contribution,
                                               ind,
                                               ind_ids,
                                               year,
                                               start_year,
                                               end_year,
                                               transform_value,
                                               pop) {
  if (!any(ind %in% ind_ids)) {
    contribution
  } else {
    ifelse(!(year %in% end_year),
           contribution,
           pop * (transform_value - transform_value[year == start_year]) / 100)
  }
}
