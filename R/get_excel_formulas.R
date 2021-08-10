#' Get the transformation formula for a specified indicator and cell reference
#'
#' `get_transform_formula_single()` gets the right transformation Excel formula
#' for the specified indicator at cell reference. Used in `get_transform_formula_single`.
#'
#' @param ind character with the indicator code (`ind` in `billionaiRe::indicator_df()`)
#' @param raw_col integer identifying the column where the raw value (pre-transformation)
#'  is located.
#' @param raw_row integer identifying the row where the raw value (pre-transformation)
#'  is located.
#'
#'  @return character vector with the formula for the indicator, column, row combination
#'  specified. If indicator is not found, "" is returned.
get_transform_formula_single <- function(ind, raw_col, raw_row) {
  # TODO: add HEP and HPOP indicators formula
  raw_cell <- glue::glue("{openxlsx::int2col(raw_col)}{raw_row}")

  if (ind == "bp") {
    formula <- glue::glue('=round(IF({raw_cell}<>"",IF((100-{raw_cell})<=50,0,((100-{raw_cell})-50)/(100-50)*100),""),2)')
  } else if (ind == "fpg") {
    formula <- glue::glue('=round(IF({raw_cell}<>"",IF({raw_cell}<=5.1,100,IF({raw_cell}>=7.1,100,(7.1-{raw_cell})/(7.1-5.1)*100)),""),2)')
  } else if (ind == "uhc_tobacco") {
    formula <- glue::glue('=round(IF({raw_cell}<>"",100-{raw_cell},""),2)')
  } else if (ind == "beds") {
    formula <- glue::glue('=round(IF({raw_cell}<>"",IF({raw_cell}<18,{raw_cell}/18*100,100),""),2)')
  } else if (ind == "hwf") {
    formula <- glue::glue('=round(IF({raw_cell}<>"",IF({raw_cell}<154.74,{raw_cell}/154.74*100,100),""),2)')
  } else {
    formula <- ""
  }
  return(formula)
}

#' Get the transformation formula for a specified indicator
#'
#' `get_transform_formula()` gets the right transformation Excel formula for the
#' specified indicator. Used in `write_tranform_formula`.
#'
#' @param raw_rows integer vector identifying rows indices where raw values for
#' transformation are located. Must be the same length as `ind`
#' @inherit get_transform_formula_single

get_transform_formula <- function(ind, raw_col, raw_rows) {
  purrr::map2_chr(ind, raw_rows, ~ get_transform_formula_single(.x, raw_col, .y)) %>%
    as_excel_formula()
}

#' Coerce coerce column to Excel Formula
#'
#' `as_excel_formula_column` coerce a specified column into an Excel formula to be
#' used in [openxlsx::writeFormula()] or [openxlsx::writeData()].
#' This transformation allows to avoid issue of Excel formula being interpreted
#' as normal text by Excel.
#'
#' @param col integer or character vector identifying the column to be coerced
#' @inherit openxlsx::writeFormula
#' @seealso [openxlsx::writeFormula()]
#'
as_excel_formula <- function(col, array = FALSE) {
  class(col) <- c("character", ifelse(array, "array_formula", "formula"))

  return(col)
}
