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
