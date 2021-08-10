#' Create empty (no rows) data frame from a character vector
#'
#' Used in openxlsx-based function to go around limitation of writing only data
#' frames and thus in long. This allows to write in long.
#'
#' @param vec character vector

vec2emptyDF <- function(vec){
  stopifnot(is.character(vec))
  df <-data.frame(matrix(ncol = length(vec), nrow = 0))
  names(df) <- vec

  return(df)
}

#' Convert WASH names to remove the urban/rural element
#'
#' @param ind_list character vector with the indicators to be changed
convert_wash_name<-function(ind_list) {
  ind_list[c("water_urban", "water_rural")] <- ind_list["water"]
  ind_list[c("hpop_sanitation_urban", "hpop_sanitation_rural")] <- ind_list[c("hpop_sanitation")]
  return(ind_list)
}

#' Force merge cell
#'
#' `mergeCellForced` wraps around [openxlsx::removeCellMerge()] and [openxlsx::mergeCells()]
#' to merge cells if there are merged cell in the specified range.
#'
#' @inheritParams openxlsx::mergeCells

mergeCellForced <- function(wb, sheet, cols, rows){
  openxlsx::removeCellMerge(wb, sheet, cols, rows)
  openxlsx::mergeCells(wb, sheet, cols, rows)
}
