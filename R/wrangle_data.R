#' Put GHO data into billionaiRe format
#'
#' `wrangle_gho_data()` takes data returned from the GHO OData API, which should
#' be obtained using [ghost::gho_data()], and transforms it into data that is
#' ready to be put into the xMart database and used within the billionaiRe
#' package. Currently, providing source and type inputs these values into the
#' entire data frame, so does not work if multiple indicators have been loaded
#' into the GHO data frame.
#'
#' @param df A data frame in GHO format, returned from [ghost::gho_data()].
#' @param source Character string of source to be provided to the data frame if
#'     the source is not already available in the GHO. If `NULL`, the source
#'     column is generated from the GHO's `DataSourceDim` column.
#' @param type Character string of type to be provided to the data frame. If
#'     `NULL`, the type column is just filled with `NA_character_`.
#' @param ind Character string of the indicator to be provided to the data frame. If
#'     `NULL`, the indicator is determined by applying the `convert_ind_codes` function
#'     on the `IndicatorCode` field of the GHO data.
#'
#' @return A data frame.
#'
#' @export
wrangle_gho_data <- function(df,
                             source = NULL,
                             type = NULL,
                             ind = NULL) {
  assert_df(df)
  assert_string(source, 1)
  assert_string(type, 1)

  # Ensure that the data frame only pertains to a single indicator
  assert_homogeneous_col(df, "IndicatorCode")

  output = df %>%
    dplyr::transmute("iso3" := .data[["SpatialDim"]],
                     "year" := .data[["TimeDim"]],
                     "ind" := ifelse(is.null(ind),
                                     convert_ind_codes(.data[["IndicatorCode"]], from = "gho_code", to = "analysis_code"),
                                     ind),
                     "value" := .data[["NumericValue"]],
                     "lower" := .data[["Low"]],
                     "upper" := .data[["High"]],
                     "source" := ifelse(is.null(source),
                                        .data[["DataSourceDim"]],
                                        source),
                     "type" := ifelse(is.null(type),
                                      NA_character_,
                                      type),
                     "other_detail" := .data[["Comments"]]) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
    dplyr::arrange(.data[["iso3"]], .data[["year"]])

  # Warn user if any of the rows don't specify an indicator
  warning_col_missing_values(output, "ind", "any")

  # Warn user if any of the rows don't specify a source
  warning_col_missing_values(output, "source", "any")

  output
}

#' Put UNSD data into billionaiRe format
#'
#' `wrangle_unsd_data()` takes data returned from the UNSD API, which should
#' be obtained using [goalie::sdg_data()], and transforms it into data that is
#' ready to be put into the xMart database and used within the billionaiRe
#' package. Currently, providing source and type inputs these values into the
#' entire data frame, so does not work if multiple indicators have been loaded
#' into the SDG data frame. As well, the UNSD series code is kept in the data
#' frame until `indicators_df` contains UNSD codes and can convert between the
#' two.
#'
#' @param df A data frame in UNSD format, returned from [goalie::sdg_data()].
#' @param source Character string of source to be provided to the data frame if
#'     the source is not already available in the UNSD database. If `NULL`,
#'     the source column is generated from the UNSD's `Source` column.
#' @param type Character string of type to be provided to the data frame. If
#'     `NULL`, the type column is generated from the UNSD's `Nature` column.
#'     "C" and "CA" are turned to "reported", while "E" and "M" are "estimated".
#'
#' @return A data frame.
#'
#' @export
wrangle_unsd_data <- function(df,
                              source = NULL,
                              type = NULL) {
  assert_df(df)
  assert_string(source, 1)
  assert_string(type, 1)

  df %>%
    dplyr::transmute("iso3" := whoville::codes_to_iso3(.data[["GeoAreaCode"]], type = "m49"),
                     "year" := .data[["TimePeriod"]],
                     "ind" := .data[["SeriesCode"]],
                     "value" := .data[["Value"]],
                     "lower" := .data[["LowerBound"]],
                     "upper" := .data[["UpperBound"]],
                     "source" := ifelse(is.null(source),
                                        .data[["Source"]],
                                        source),
                     "type" := dplyr::case_when(
                       !is.null(type) ~ type,
                       .data[["Nature"]] %in% c("C", "CA") ~ "reported",
                       .data[["Nature"]] %in% c("E", "M") ~"estimated"
                     ),
                     "other_detail" := .data[["FootNote"]]) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
    dplyr::arrange(.data[["iso3"]], .data[["year"]])
}
