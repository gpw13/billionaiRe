#' Get names of xMart4 columns
#'
#' Returns a character vector with the names of all the columns expected by the
#' Triple Billions xMart tables.
#'
#' @return character vector
#' @export
xmart_cols <- function() {
  c("iso3", "year","ind", "value", "lower", "upper", "use_dash", "use_calc",
    "source", "type", "type_detail", "other_detail", "upload_detail")
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
  identical( sort(names(df)), sort(xmart_cols()) )
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
    output_df = df %>%
      dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
      dplyr::select(xmart_cols()) %>%
      dplyr::arrange(.data[["iso3"]], .data[["year"]]) %>%
      readr::write_csv(path, na="")

    return(output_df)
  } else {
    warning("The output data frame did not have the correct columns. The output was not saved to disk.")
    return(df)
  }
}