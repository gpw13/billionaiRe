
#' Helper to reduce type to unique value
#'
#' Used with `pathogen_calc()`
#'
#' @param value Vector of values
#' @param type Vector of types
#' @param ind Vector of indicators names
reduce_type <- function(value, type, ind) {
  if (all(is.na(value))) {
    return(NA_character_)
  } else {
    if ("surviving_infants" %in% ind) {
      value <- value[-grep("surviving_infants", ind)]
      type <- type[-grep("surviving_infants", ind)]
    }
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
