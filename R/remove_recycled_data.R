#' Remove recycled values from `df`
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific country, year, and indicator.
#' @param ind Column name of column with indicator names.
#' @param recycled name of boolean recycle column with whether the data is
#' recycled or not. Default to "recycle".
#'
#' @inheritParams recycle_data
#'
#' @return Data frame in long format without recycled values
#'
#' @export
remove_recycled_data <- function(df,
                                 ind = "ind",
                                 recycled = "recycled",
                                 scenario = "scenario",
                                 scenario_reported_estimated = "routine",
                                 scenario_covid_shock = "covid_shock",
                                 scenario_reference_infilling = "reference_infilling") {
  calculated_inds <- billionaiRe::indicator_df %>%
    dplyr::filter(.data[["calculated"]] | .data[["ind"]] == "surviving_infants") %>%
    dplyr::pull(.data[["ind"]])

  df %>%
    dplyr::mutate(!!sym(recycled) := dplyr::case_when(
      .data[[ind]] %in% calculated_inds & is.na(.data[[recycled]]) ~ FALSE,
      TRUE ~ recycled
    )) %>%
    dplyr::filter(!.data[[recycled]])
}
