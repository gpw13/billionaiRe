#' Write permanent sheets
#'
#' `write_permanent_sheets()` writes and styles the Intro and Indicator List sheets
#' that are present in all cases taken into account by the different functions.
#'
#' @inheritParams export_all_countries_summaries_xls
#' @inheritParams style_header_hpop_summary_sheet
#'
#' @return A [openxlsx::createWorkbook()] object
write_permanent_sheets <- function(billion, start_col, start_row) {
  wb_file <- system.file("extdata",
    "country_summary_template.xlsx",
    package = "billionaiRe"
  )

  wb <- openxlsx::loadWorkbook(wb_file)

  openxlsx::writeData(wb,
    sheet = "Intro",
    x = glue::glue("Date Summary Generated: {lubridate::today()}"),
    startCol = start_col,
    startRow = start_row,
    colNames = FALSE
  )

  wb <- write_indicator_list_sheet(wb,
    sheet_name = "Indicator List",
    billion,
    start_col = start_col,
    start_row = start_row + 1
  )
  return(wb)
}
