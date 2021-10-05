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
#' @inheritParams transform_hpop_data
#' @param iso3 Country ISO3 code.
#' @return character vector with the formula for the indicator, column, row combination
#' specified. If indicator is not found, "" is returned.
get_transform_formula_single <- function(ind, raw_col, raw_row, ind_ids, iso3) {
  # TODO: add HEP and HPOP indicators formula
  raw_cell <- glue::glue("{openxlsx::int2col(raw_col)}{raw_row}")

  if (ind %in% ind_ids["bp"]) {
    formula <- glue::glue('=IF({raw_cell}<>"",IF((100-{raw_cell})<=50,0,round(((100-{raw_cell})-50)/(100-50)*100,2)),"")')
  } else if (ind %in% ind_ids["fpg"]) {
    formula <- glue::glue('=IF({raw_cell}<>"",IF({raw_cell}<=5.1,100,IF({raw_cell}>=7.1,100,round((7.1-{raw_cell})/(7.1-5.1)*100,2))),"")')
  } else if (ind %in% ind_ids[c("uhc_tobacco", "fh", "stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese", "pm25")]) {
    formula <- glue::glue('=IF({raw_cell}<>"",round(100-{raw_cell},2),"")')
  } else if (ind == "beds") {
    formula <- glue::glue('=IF({raw_cell}<>"",IF({raw_cell}<18,round({raw_cell}/18*100,2),100),"")')
  } else if (ind == "hwf") {
    formula <- glue::glue('=IF({raw_cell}<>"",IF({raw_cell}<154.74,round({raw_cell}/154.74*100,2),100),"")')
  } else if (ind %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "fuel")]) {
    formula <- glue::glue('=IF({raw_cell}<>"",{raw_cell}, "")')
  } else if (ind %in% ind_ids["suicide"]) {
    formula <- glue::glue('=if({raw_cell}<>"", round(100 - ({raw_cell} * 5 * 20 * 100 / 100000), 2), "")')
  } else if (ind %in% ind_ids["alcohol"]) {
    formula <- glue::glue('=if({raw_cell}<>"", round(100 - ({raw_cell} * 4), 2), "")')
  } else if (ind %in% ind_ids["alcohol"]) {
    formula <- glue::glue('=if({raw_cell}<>"", round(100 - ({raw_cell} * 4), 2), "")')
  } else if (ind %in% ind_ids["road"]) {
    sdi_rti <- get_sdi_ratio(iso3)
    formula <- glue::glue('=if({raw_cell}<>"", round(100 - ({raw_cell} * 5 * {sdi_rti} / 1000), 2), "")')
  } else if (ind %in% ind_ids["transfats"]) {
    formula <- glue::glue('=if({raw_cell}<>"", (100 - 14.3 + 2.1 * {raw_cell} / 100), "")')
  } else {
    formula <- ""
  }
  return(formula)
}


#' Coerce coerce column to Excel Formula
#'
#' `as_excel_formula_column` coerce a specified column into an Excel formula to be
#' used in [openxlsx::writeFormula()] or [openxlsx::writeData()].
#' This transformation allows to avoid issue of Excel formula being interpreted
#' as normal text by Excel.
#'
#' @param col integer or character vector identifying the column to be coerced
#' @inheritParams openxlsx::writeFormula
as_excel_formula <- function(col, array = FALSE) {
  class(col) <- c("character", ifelse(array, "array_formula", "formula"))

  return(col)
}

#' Get the transformation formula for a specified indicator
#'
#' `get_transform_formula()` gets the right transformation Excel formula for the
#' specified indicator. Used in `write_tranform_formula`.
#'
#' @param raw_rows integer vector identifying rows indices where raw values for
#' transformation are located. Must be the same length as `ind`
#' @inheritParams transform_hpop_data
#' @inherit get_transform_formula_single
#' @param iso3 Country ISO3 code.
get_transform_formula <- function(ind, raw_col, raw_rows, ind_ids, iso3) {
  as_excel_formula(purrr::map2_chr(ind, raw_rows, ~ get_transform_formula_single(.x, raw_col, .y, ind_ids, iso3)))
}
