#' Create empty (no rows) data frame from a character vector
#'
#' Used in openxlsx-based function to go around limitation of writing only data
#' frames and thus in long. This allows to write in long.
#'
#' @param vec character vector

vec2emptyDF <- function(vec) {
  stopifnot(is.character(vec))
  df <- data.frame(matrix(ncol = length(vec), nrow = 0))
  names(df) <- vec

  return(df)
}

#' Removes unused WASH indicator from indicator list
#'
#' `remove_unused_wash_ind` removes unused WASH indicators from indicator list to
#' provide a clean list to be printed.
#'
#' @param ind_list character vector containing the list of indicator to be treated
#' @param correct_wash character vector with the exact indicators to keep for WASH
remove_unused_wash_ind <- function(ind_list, correct_wash) {
  wash_ind <- unique(stringr::str_extract(ind_list, paste0("^", correct_wash, "$", collapse = "|")))
  wash_ind <- wash_ind[!is.na(wash_ind)]
  c(ind_list[!stringr::str_detect(ind_list, "water|sanitation")], wash_ind)
}

#' Convert WASH names to remove the urban/rural element
#'
#' @param ind_list character vector with the indicators to be changed
convert_wash_name <- function(ind_list) {
  ind_list <- stringr::str_replace(ind_list, "^water.*", "water")
  ind_list <- stringr::str_replace(ind_list, "^hpop_sanitation.*", "hpop_sanitation")
  return(ind_list)
}

#' Force merge cell
#'
#' `mergeCellForced` wraps around [openxlsx::removeCellMerge()] and [openxlsx::mergeCells()]
#' to merge cells if there are merged cell in the specified range.
#'
#' @inheritParams openxlsx::mergeCells

mergeCellForced <- function(wb, sheet, cols, rows) {
  openxlsx::removeCellMerge(wb, sheet, cols, rows)
  openxlsx::mergeCells(wb, sheet, cols, rows)
}
