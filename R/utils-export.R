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

#' Convert WASH names to remove the urban/rural element
#'
#' @param ind_list character vector with the indicators to be changed
convert_wash_name <- function(ind_list) {
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

mergeCellForced <- function(wb, sheet, cols, rows) {
  openxlsx::removeCellMerge(wb, sheet, cols, rows)
  openxlsx::mergeCells(wb, sheet, cols, rows)
}

#'  Get data frame with just one scenario
#'
#'  Get just one scenario in the data frame. Is used to avoid issues with when
#'  multiple scenarios are present in export functions, as scenarios are not implemented
#'  at the moment.
#'
#'  @param df data.frame to be treated
#'  @param scenario character string identifying the scenario column in `df`
#'
#'  @return A data frame with one scenario
#'
get_df_one_scenario <- function(df, scenario) {
  scenario <- ifelse(is.null(scenario), "scenario", scenario)
  if (length(df[["scenario"]]) > 1) {
    warning(paste0(
      "More than one scenario found in column ", names(df)[grep("scenario", names(df))],
      "If 'default' is present, this scenario will used. Otherwise, the first scenario will be used."
    ))
    scenario_to_use <- ifelse("default" %in% unique(df[["scenario"]]), "default", unique(df[["scenario"]][1]))
    df <- dplyr::filter(df, !!sym("scenario") == !!scenario_to_use)
  }
}
