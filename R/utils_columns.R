#' Add columns to data frame if not already existing
#'
#' The function takes in a data frame, a vector of column names,
#' and a vector of base fill to use for the column. If the column doesn't already
#' exist, it is added to the data frame and filled with the relevant fill value.
#'
#' @param df A data frame.
#' @param cols A character vector of column names to add.
#' @param fill A list of fill to be used for each column. If length 1, same fill
#'     is used for each column. Can also be a vector, but lists allow different types
#'     of fill, e.g. `fill = list(NA_character_, NA_real_)`.
#'
#' @return A data frame.
billionaiRe_add_columns <- function(df, cols, fill) {
  if (length(fill) == 1 & length(cols) != 1) {
    fill <- rep(fill, length(cols))
  }

  names(fill) <- cols
  tibble::add_column(df, !!!fill[setdiff(cols, names(df))])
}
