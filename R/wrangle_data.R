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


#' Wrangle GHO data with TOTL/RUR/URB dimensions
#'
#' @param df A data frame in GHO format, returned from [ghost::gho_data()].
#' @param source Character string of source to be provided to the data frame.
#' If `NULL`, the source column is generated from the GHO's `DataSourceDim` column.
#' If not `NULL`, it overrides the source provided by the GHO.
#' @param type Character string of type to be provided to the data frame. If
#'     `NULL`, the type column is filled with `NA_character_`.
#' @param ind Character string of the indicator name to be provided to the data frame.
#' This is a required argument and will raise an error if not provided.
#' If only a RUR or URB values is available, the indicator name has `_rural` or `_urban`
#' appended to it in the output data frame.
#' @param id_cols Character vector of the columns that are the same regardless of
#' the TOTL/RUR/URB dimension. Used as the argument of the same name in `pivot_wider`.
#' @param names_from,values_from A pair of character vectors used as the arguments
#' of the same name in `pivot_wider`.
#'
#' @return A data frame
#'
#' @section TODO:
#' * Convert to more generic `unspool_gho_dim` function which can work with any
#' other `DimType`, and not just TOTL/RUR/URB.
#' * Re-write to make it work better with `wrangle_gho_data` to avoid the significant amount
#' of redundant logic. This means that, eventually, users may do
#' `gho_data(.) %>% unspool_gho_dim(.) %>% wrangle_gho_data(.)`
#' @export
wrangle_rural_urban_gho_data <- function(df,
                                         source = NULL,
                                         type = NULL,
                                         ind = NULL,
                                         id_cols = c("SpatialDim", "TimeDim"),
                                         names_from = "Dim1",
                                         values_from = c("NumericValue", "High", "Low", "DataSourceDim", "Comments")) {
  assert_df(df)
  assert_string(source, 1)
  assert_string(type, 1)

  # Ensure that the indicator name is provided and that it's a string
  # ind still has a default value of NA and is not changed to a required argument to maintain
  # same input order as wrangle_gho_data
  assert_arg_exists(ind)
  assert_string(ind, 1)

  # Ensure that the data frame only pertains to a single indicator
  assert_homogeneous_col(df, "IndicatorCode")

  # Used for the transmute later
  make_expr <- function(prefix, suffix) {
    rlang::parse_expr(glue::glue('!is.na(.data[[\"{prefix}_{suffix}\"]]) ~ .data[[\"{prefix}_{suffix}\"]]'))
  }

  # Used for the transmute later
  make_conds <- function(prefixes, suffixes) {
    purrr::map2(prefixes, suffixes, make_expr)
  }

  output <- df %>%
    # Pivot wider to accommodate instance where only rural/urban data is available
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of(id_cols),
      names_from = names_from,
      values_from = tidyselect::all_of(values_from)
    ) %>%
    # Filter to keep only rows where at least one of TOTL/RUR/URB values is available
    dplyr::filter(
      dplyr::if_any(dplyr::starts_with("NumericValue_"), ~ !is.na(.))
    ) %>%
    # Final transmutations for output data frame
    dplyr::transmute(
      "iso3" := .data[["SpatialDim"]],
      "year" := .data[["TimeDim"]],

      # If a total value doesn't exist, use the rural/urban indicator name
      "ind" := dplyr::case_when(
        !is.na(.data[["NumericValue_TOTL"]]) ~ glue::glue("{ind}"),
        !is.na(.data[["NumericValue_RUR"]]) ~ glue::glue("{ind}_rural"),
        !is.na(.data[["NumericValue_URB"]]) ~ glue::glue("{ind}_urban")
      ),

      # If a total value doesn't exist, use the rural/urban value
      "value" := dplyr::case_when(
        !!!make_conds(prefixes = c("NumericValue"), suffixes = c("TOTL", "RUR", "URB"))
      ),

      # If a total low doesn't exist, use the rural/urban low
      "lower" := dplyr::case_when(
        !!!make_conds(prefixes = c("Low"), suffixes = c("TOTL", "RUR", "URB"))
      ),

      # If a total high doesn't exist, use the rural/urban high
      "upper" := dplyr::case_when(
        !!!make_conds(prefixes = c("High"), suffixes = c("TOTL", "RUR", "URB"))
      ),

      # If a total source doesn't exist, use the rural/urban source
      "DataSourceDim" := dplyr::case_when(
        !!!make_conds(prefixes = c("DataSourceDim"), suffixes = c("TOTL", "RUR", "URB"))
      ),

      # If a data source is explicitly provided, override the sources from the DataSourceDim column
      # Follows the same logic as `wrangle_gho_data` by giving priority to explicit source over GHO source
      "source" := ifelse(
        !is.null(source), source, .data[["DataSourceDim"]]
      ),

      # If a total comment doesn't exist, use the rural/urban comment
      "other_detail" := dplyr::case_when(
        !!!make_conds(prefixes = c("Comments"), suffixes = c("TOTL", "RUR", "URB"))
      ),

      # If a type argument is provided, use that; otherwise NA
      "type" := ifelse(
        !is.null(type), type, NA_character_
      ),
      use_dash = TRUE,
      use_calc = TRUE,
      type_detail = NA,
      upload_detail = NA
    ) %>%

    # Remove unnecessary DataSourceDim column
    dplyr::select(-"DataSourceDim") %>%

    # Filter to keep only WHO members
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%

    # Arrange in ascending order of iso3, year
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
