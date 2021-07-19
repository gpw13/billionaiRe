#' Count number of data points since specified date
#'
#' `count_since` takes a `billionaire::load_billion_data()` dataframe and
#' count the number of data points exists for each indicator in `df` since the
#' specified `year`.
#'
#' @inheritParams summarize_hpop_country_data
#' @param year_specified numeric
#'
#' @return data frame

count_since <- function(df, year_specified, year, ind, iso3) {
  assert_columns(df, year, ind, iso3)
  assert_numeric(year_specified)

  count_df <- df %>%
    dplyr::filter(.data[[year]] >= !!year_specified) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::summarise(!!rlang::sym(glue::glue("count_{year_specified}")) := dplyr::n(), .groups = "drop")
}

#' Get order of indicator
#'
#' @param ind character vector of indicators
#'
#' @return character vector

get_ind_order <- function(ind){
  data.frame(ind = ind) %>%
    dplyr::left_join(billionaiRe::indicator_df, by = c(ind = "ind")) %>%
    dplyr::pull("order")
}
