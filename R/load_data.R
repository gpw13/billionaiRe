#' Load Raw Billions Indicator Data
#'
#' `load_billion_data()` provides access to the raw inputs to the Billions stored
#' within the World Health Organization's xMart4 database. By default, it filters
#' the loaded data to take the most recently uploaded data for each indicator,
#' country, and year. The user can also specify to take the latest data before a
#' certain time period, or to extract all data from the database.
#'
#' This function requires that the user have the [xmart4](https://github.com/caldwellst/xmart4) package
#' installed and setup to access the GPW13 mart. Details are available on the GitHub
#' landing page linked above.
#'
#' @param billion Billions data to load, one of "hep" (default), "hpop", "uhc", or "all".
#' @param mart_table Name of xMart4 table in the GPW13 mart to load data from. Currently,
#'     pulls from the full input data to the Billions by default, but can also pull
#'     from the table containing data needing DDI infilling and projections or
#'     those already infilled and projected by technical programmes.
#' @param date_filter One of `NULL`, "latest", or a single date string. The date
#'     string needs to be in ISO6801 format, such as "1989-4-4" or "1988-06-21".
#' @param ... Additional arguments passed to `xmart4::xmart4_table()`. Use if
#'     you need to provide additional token specifications for Azure authentication.
#'
#' @return A data frame.
#'
#' @export
load_billion_data <- function(billion = c("hep", "hpop", "uhc", "all"),
                              mart_table = c("RAW_INDICATOR", "RAW_UNPROJ_DATA", "RAW_PROJ_DATA"),
                              date_filter = "latest",
                              ...) {
  requireNamespace("xmart4", quietly = TRUE)
  billion <- rlang::arg_match(billion)
  mart_table <- rlang::arg_match(mart_table)
  assert_date_filter(date_filter)

  df <- load_billion_table(mart_table, ...)
  df <- dplyr::rename_with(df, tolower)
  df <- filter_billion_inds(df, billion)
  df <- filter_billion_date(df, date_filter)
  df
}

#' @noRd
filter_billion_inds <- function(df, billion) {
  if (billion != "all") {
    inds <- billion_ind_codes(billion)
    df <- dplyr::filter(df, .data[["ind"]] %in% inds)
  }
  df
}

#'@noRd
filter_billion_date <- function(df, date_filter) {
  if (!is.null(date_filter)) {
    df <- dplyr::group_by(df,
                          .data[["ind"]],
                          .data[["iso3"]],
                          .data[["year"]])
    if (date_filter != "latest") {
      date_filter <- lubridate::as_date(date_filter)
      if (date_filter < min(df[["upload_date"]])) {
        warning("`date_filter` is before the first upload date of the Billions data, returning an empty data frame.",
                call. = FALSE)
      }
      df <- dplyr::filter(df, .data[["upload_date"]] <= date_filter)
    }
    df <- dplyr::filter(df, .data[["upload_date"]] == max(.data[["upload_date"]], -Inf))
    df <- dplyr::ungroup(df)
  }
  df
}

#' @noRd
assert_date_filter <- function(fltr) {
  if (!is.null(fltr)) {
    if (fltr != "latest") {
      if (!is.character(fltr) | length(fltr) > 1 | !stringr::str_detect(fltr, "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}")) {
        stop("`date_filter` needs to be either 'latest', NULL, or a single hyphen delimited ISO 8601 date string (e.g. '1988-06-21').",
             call. = FALSE)
      }
    }
  }
}

#' @noRd
load_billion_table <- function(tbl, ...) {
  xmart4::xmart4_table("GPW13", tbl,
                       col_types = readr::cols(
                         ISO3 = readr::col_character(),
                         YEAR = readr::col_double(),
                         IND = readr::col_character(),
                         UPLOAD_DATE = readr::col_date(format = ""),
                         VALUE = readr::col_double(),
                         LOW = readr::col_double(),
                         HIGH = readr::col_double(),
                         USE_DASH = readr::col_logical(),
                         USE_CALC = readr::col_logical(),
                         SOURCE = readr::col_character(),
                         TYPE = readr::col_character(),
                         TYPE_DETAIL = readr::col_character(),
                         OTHER_DETAIL = readr::col_character(),
                         UPLOAD_DETAIL = readr::col_character()
                       ),
                       ...)
}
