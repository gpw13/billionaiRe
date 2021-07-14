#' Count number of data points since specified date
#'
#' `count_since` takes a `billionaire::load_billion_data()` dataframe and
#' count the number of data points exists for each indicator in `df` since the
#' specified `year`.
#'
#' @inheritParams transform_hpop_data
#' @param year numeric
#'
#' @return data frame

count_since <- function(df, year) {
  billionaiRe:::assert_mart_columns(df)
  billionaiRe:::assert_numeric(year)

  count_df <- df  %>%
    dplyr::filter(year >= year) %>%
    dplyr::group_by(iso3, ind) %>%
    dplyr::summarise(!!rlang::sym(glue::glue("count_{year}")):= dplyr::n(), .groups = "drop")

}
