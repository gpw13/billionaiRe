
#' Helper to reduce type to unique value
#'
#' Used with `pathogen_calc()`
#'
#' @param value Vector of values
#' @param type Vector of types
#'
#' @keywords internal
#'
reduce_type <- function(value, type) {
  if (all(is.na(value))) {
    return(NA_character_)
  } else {
    ut <- unique(type[!is.na(type)])
    if (length(ut) == 1) {
      return(ut)
    } else if ("projected" %in% ut) {
      return("projected")
    } else if ("imputed" %in% ut) {
      return("imputed")
    } else {
      return("estimated")
    }
  }
}
