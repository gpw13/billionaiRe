#' Get names of xMart4 columns
#'
#' Returns a character vector with the names of all the columns expected by the
#' Triple Billions xMart tables.
#'
#' @param data_type (string): the type of data
#' * `wrangled_data` (default): raw data that has been wrangled into a suitable
#'   form for analysis.
#' * `projected_data`:  data that has been fully projected to the target year but
#'   has not yet been transformed or calculated upon.
#' * `final_data`: the complete set of billions data with transformed values,
#' contributions, and all calculations available.
#'
#' @return character vector
#' @export
xmart_cols <- function(data_type = c("wrangled_data", "projected_data", "final_data")) {
  data_type <- rlang::arg_match(data_type)
  key_cols <- c("iso3", "year", "ind", "scenario")
  value_cols <- c("value", "lower", "upper")
  other_cols <- c(
    "use_dash", "use_calc", "source", "type", "scenario_detail", "type_detail",
    "other_detail", "upload_detail"
  )

  if (data_type == "final_data") {
    value_cols <- c(
      value_cols, "transform_value", "transform_lower", "transform_upper",
      "contribution", "contribution_percent", "contribution_percent_total_pop",
      "population", "level"
    )
  }
  return(c(key_cols, value_cols, other_cols))
}

#' Get the column types for xMart columns
#'
#' A helper function for specifying the expected column types for the Triple
#' Billions data.
#'
#' @inheritParams xmart_cols
#'
#' @return a named vector with column specifications
#' @export
xmart_col_types <- function(data_type = c("wrangled_data", "projected_data", "final_data")) {
  data_type <- rlang::arg_match(data_type)

  key_cols <- c(
    iso3 = "character",
    year = "integer",
    ind = "character",
    scenario = "character"
  )

  value_cols <- c(value = "double", lower = "double", upper = "double")

  other_cols <- c(
    use_dash = "logical",
    use_calc = "logical",
    source = "character",
    type = "character",
    scenario_detail = "character",
    type_detail = "character",
    other_detail = "character",
    upload_detail = "character"
  )

  if (data_type == "final_data") {
    value_cols <- c(
      value_cols,
      transform_value = "double",
      transform_lower = "double",
      transform_upper = "double",
      contribution = "double",
      contribution_percent = "double",
      contribution_percent_total_pop = "double",
      population = "double",
      level = "integer"
    )
  }

  return(c(key_cols, value_cols, other_cols))
}

#' Check data frame for xMart4 columns
#'
#' Tests to see if the given data frame has all the columns required by the Triple Billions
#' xMart4 tables.The test does not take column order into consideration
#' (e.g., a,b,c and c,a,b will both pass)
#'
#' @param df data.frame
#'
#' @return bool
#' @export
has_xmart_cols <- function(df) {
  identical(sort(names(df)), sort(xmart_cols()))
}

#' Add missing rows for xMart upload
#'
#' Compares the rows in a given data frame to the existing xMart data. If the data frame
#' is missing any combinations of `c(iso3, ind, year)` found in xMart, it appends
#' these rows to the data frame,with NA values for `value, upper, lower, source, type`
#' and `other_detail`.
#'
#' @param df data frame
#' @param billion the Billion that the indicator belongs to
#' @param ind_code the GPW13 code for the indicator(s)
#' @param projected boolean value indicating where the indicator has already been
#' projected by the technical programme
#'
#' @return a data frame
#' @export
add_missing_xmart_rows <- function(df, billion, ind_code, projected) {
  proj <- ifelse(projected == TRUE, "proj_data", "unproj_data")

  exist_df <- load_billion_data_legacy(billion, proj) %>%
    dplyr::filter(.data[["ind"]] %in% ind_code) %>%
    dplyr::select(xmart_cols())

  anti_df <- dplyr::anti_join(exist_df, df, by = c("iso3", "ind", "year")) %>%
    dplyr::mutate(
      dplyr::across(c("value", "lower", "upper", "source", "type", "other_detail"), ~NA)
    )

  if (nrow(anti_df) > 0) {
    final_df <- dplyr::bind_rows(df, anti_df)
  } else {
    final_df <- df
  }

  final_df
}

#' Save the output to disk after ensuring column specs
#'
#' Helper functions that saves a wrangled output data frame to the disk if it has
#' the correct columns as required by the Triple Billions xMart tables.
#'
#' The function returns a data frame (like `readr::write_csv()`) in order to allow
#' it to work with pipes better.
#'
#' @param df data frame the output
#' @param path the path where the output should be saved
#' @inheritParams xmart_cols
#' @param na_rm (logical) Specifies whether to remove rows where `value` is missing.
#'   Defaults to `FALSE`.
#' @param compression Compression algorithm to use for parquet format. `"gzip"` by
#'   default
#'
#' @return A data frame. Note that this is the modified version of in the input
#    data frame that is saved to disk. As such, any modifications required by
#'   the function (such as from removing empty rows when `na_rm = TRUE`) are carried
#'   over to the output.
#'
#' @export
save_wrangled_output <- function(df,
                                 path,
                                 data_type = c("wrangled_data", "projected_data", "final_data"),
                                 na_rm = FALSE,
                                 compression = "gzip") {
  data_type <- rlang::arg_match(data_type)
  assert_df(df)
  assert_strings(path, compression)
  assert_type(na_rm, "logical")
  assert_columns(df, xmart_cols(data_type))
  assert_col_types(df, xmart_col_types(data_type))
  assert_distinct_rows(df, c("ind", "iso3", "year", "scenario"))

  # If the value is not NA, check that the expected columns are also not NA
  assert_col_paired_with(
    df,
    "value",
    c("iso3", "year", "ind", "scenario", "type", "source", "use_dash", "use_calc")
  )

  # Ensure that the number of rows is greater than 0
  stopifnot(nrow(df) > 0)

  ext <- stringr::str_split(path, "\\.")[[1]][[2]]
  assert_x_in_y(ext, c("csv", "parquet"))

  output_df <- df %>%
    # Defensive ungroup in case the input is grouped
    dplyr::ungroup() %>%
    filter_billion_na(na_rm) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
    dplyr::select(xmart_cols(data_type)) %>%
    dplyr::arrange(.data[["ind"]], .data[["iso3"]], .data[["year"]])

  if (ext == "csv") {
    readr::write_csv(output_df, path, na = "")
    message("Output saved to disk.\n")
  } else if (ext == "parquet") {
    arrow::write_parquet(output_df, path, compression = compression)
    cat("Output saved to disk.\n")
  }

  output_df
}
