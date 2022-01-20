#' Get names of xMart4 columns
#'
#' Returns a character vector with the names of all the columns expected by the
#' Triple Billions xMart tables.
#'
#' @param data_type (string): the type of data
#'
#' @return character vector
#' @export
xmart_cols <- function(data_type = c("wrangled_data", "projected_data", "final_data")) {
  data_type <- rlang::arg_match(data_type)

  key_cols <- c("iso3", "year", "ind")
  value_cols <- c("value", "lower", "upper")
  other_cols <- c(
    "use_dash", "use_calc", "source", "type", "type_detail",
    "other_detail", "upload_detail"
  )

  # TODO: to be added after scenarios are fully integrated into the data pipeline
  # if (data_type != "wrangled_data") {
  #   key_cols = c(key_cols, "scenario", "scenario_detail")
  # }

  if (data_type == "final_data") {
    value_cols <- c(
      value_cols, "transform_value", "transform_lower", "transform_upper",
      "contribution", "contribution_percent", "contribution_percent_total_pop",
      "population", "level"
    )
  }

  return(c(key_cols, value_cols, other_cols))
}

#' Get the col_types for xMart columns
#'
#' A helper function for specifying the column types when reading/writing Triple
#' Billions csv files.
#'
#' @return a list with column specifications
#' @export
xmart_col_types <- function() {
  list(
    iso3 = readr::col_character(),
    year = readr::col_integer(),
    ind = readr::col_character(),
    value = readr::col_double(),
    lower = readr::col_double(),
    upper = readr::col_double(),
    use_dash = readr::col_logical(),
    use_calc = readr::col_logical(),
    source = readr::col_character(),
    type = readr::col_character(),
    type_detail = readr::col_character(),
    other_detail = readr::col_character(),
    upload_detail = readr::col_character()
  )
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
#' @param compression Compression algorithm to use for parquet format. "snappy" by
#'   default
#'
#' @return a data frame. This is the modified dataframe that's saved to disk if
#' the data frame has all the columns expected by xMart. Otherwise, it simply return
#' the input data frame.
#'
#' @export
save_wrangled_output <- function(df, path, compression = "snappy") {
  assert_df(df)
  assert_string(path, 1)

  ext <- stringr::str_split(path, "\\.")[[1]][[2]]

  if (has_xmart_cols(df)) {
    output_df <- df %>%
      dplyr::ungroup() %>%
      dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
      dplyr::select(xmart_cols()) %>%
      dplyr::arrange(.data[["ind"]], .data[["iso3"]], .data[["year"]])

    if (ext == "csv") {
      readr::write_csv(output_df, path, na = "")
      message("Output saved to disk.")
    } else if (ext == "parquet") {
      arrow::write_parquet(output_df, path, compression = compression)
      message("Output saved to disk.")
    } else {
      warning("Unknown extension. The output was not saved to disk.")
      output_df <- df
    }
  } else {
    warning("The output data frame did not have the correct columns. The output was not saved to disk.")
    output_df <- df
  }

  output_df
}
