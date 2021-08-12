#' Get names of xMart4 columns
#'
#' Returns a character vector with the names of all the columns expected by the
#' Triple Billions xMart tables.
#'
#' @return character vector
#' @export
xmart_cols <- function() {
  c(
    "iso3", "year", "ind", "value", "lower", "upper", "use_dash", "use_calc",
    "source", "type", "type_detail", "other_detail", "upload_detail"
  )
}

#' Get the col_types for xMart columns
#'
#' A helper function for specifying the column types when reading/writing Triple
#' Billions csv files.
#'
#' @return a list with column specifications
#' @export
#'
#' @examples
#' `read_csv("hpop_sanitation.csv", col_types = xmart_col_types)`
xmart_col_types = function() {
  list(
    iso3 = col_character(),
    year = col_integer(),
    ind = col_character(),
    value = col_double(),
    lower = col_double(),
    upper = col_double(),
    use_dash = col_logical(),
    use_calc = col_logical(),
    source = col_character(),
    type = col_character(),
    type_detail = col_character(),
    other_detail = col_character(),
    upload_detail = col_character()
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

  exist_df <- load_billion_data(billion, proj) %>%
    dplyr::filter(.data[["ind"]] %in% ind_code) %>%
    dplyr::select(xmart_cols())

  anti_df <- dplyr::anti_join(exist_df, df, by = c("iso3", "ind", "year")) %>%
    dplyr::mutate(
      dplyr::across(c("value", "lower", "upper", "source", "type", "other_detail"), ~ NA)
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
#'
#' @return a data frame. This is the modified dataframe that's saved to disk if
#' the data frame has all the columns expected by xMart. Otherwise, it simply return
#' the input data frame.
#'
#' @export
save_wrangled_output <- function(df, path) {
  assert_df(df)
  assert_string(path, 1)

  if (has_xmart_cols(df)) {
    output_df <- df %>%
      dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
      dplyr::select(xmart_cols()) %>%
      dplyr::arrange(.data[["iso3"]], .data[["year"]]) %>%
      readr::write_csv(path, na = "")

    message("Output saved to disk.")
    return(output_df)
  } else {
    warning("The output data frame did not have the correct columns. The output was not saved to disk.")
    return(df)
  }
}
