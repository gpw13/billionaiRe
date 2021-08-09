#' Count number of data points since specified date
#'
#' `count_since` takes a `billionaire::load_billion_data()` dataframe and
#' count the number of data points exists for each indicator in `df` since the
#' specified `year`.
#'
#' @inheritParams export_country_summary_xls
#' @param year_specified numeric
#'
#' @return data frame

count_since <- function(df, year_specified, year, ind, iso3, type_col) {
  assert_columns(df, year, ind, iso3, type_col)
  assert_numeric(year_specified)

  df %>%
    dplyr::filter(.data[[type_col]] %in% c("estimated", "reported")) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::filter(.data[[year]] >= !!year_specified) %>%
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

#' Get latest reported data frame
#'
#' `get_latest_reported()` gets the latest reported data available for estimated
#' or reported data. Used in write functions.
#'
#' @inherit export_hpop_country_summary_xls
get_latest_reported_df <- function(df, iso3, ind, type_col, year, value, transform_value = NULL, source_col){
  df %>%
    dplyr::filter(.data[[type_col]] %in% c("estimated", "reported")) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::filter(.data[[year]] == max(.data[[year]])) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(ind, value,transform_value, year,
                                  type_col, source_col))) %>%
    dplyr::mutate(!!sym(year) := as.integer(.data[[year]]))
}

#' Get baseline and projections data frame for specified dates
#'
#' `get_latest_reported()` gets the latest reported data available for estimated
#' or reported data. Used in write functions.
#'
#' @inherit export_hpop_country_summary_xls

get_baseline_projection_df <- function(df, iso3, ind, type_col, year, value, transform_value,start_year, end_year, source_col){
  df %>%
    dplyr::filter(.data[[year]] %in% c(!!start_year, max(!!end_year))) %>%
    dplyr::select(dplyr::all_of(c(ind, year, value, transform_value, type_col,
                                  source_col, iso3))) %>%
    dplyr::group_by(!!rlang::sym(ind), !!rlang::sym(iso3)) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(year),
      values_from = c(dplyr::all_of(c(value, transform_value)), .data[[type_col]], .data[[source_col]])
    ) %>%
    dplyr::mutate(empty1 = NA, .after = glue::glue("{value}_{max(end_year)}")) %>%
    dplyr::mutate(empty2 = NA, .after = glue::glue("{transform_value}_{max(end_year)}")) %>%
    dplyr::mutate(empty3 = NA, .after = glue::glue("{type_col}_{max(end_year)}")) %>%
    dplyr::ungroup()

}
