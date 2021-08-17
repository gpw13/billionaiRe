#' Style table headers for HPOP
#'
#' Used within `style_hpop_indicators()`, `style_hpop_latest()`
#'
#' @inherit write_latest_reported_hpop_summary
#'

style_hpop_headers <- function(wb, sheet_name, bounds) {
  mergeCellForced(wb,
    sheet = sheet_name,
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"]
  )

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles(
      style_category = "datatable_header",
      billion = "hpop",
      billion_fgFill = "main"
    ),
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )
  purrr::map(c(bounds["start_col"]:bounds["end_col"]), ~ mergeCellForced(wb,
    sheet = sheet_name,
    rows = (bounds["start_row"] + 1):(bounds["start_row"] + 2),
    cols = .x
  ))

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles(
      style_category = "sub_datatable_header",
      billion = "hpop",
      billion_fgFill = "light"
    ),
    rows = (bounds["start_row"] + 1):(bounds["start_row"] + 2),
    cols = bounds["start_col"]:(bounds["end_col"]),
    gridExpand = TRUE
  )

  return(wb)
}

#' Style table headers for UHC
#'
#' Used within `style_data_headers_uhc_summary()`
#'
#' @inherit write_latest_reported_hpop_summary
#'

style_uhc_headers <- function(wb, sheet_name, bounds) {
  mergeCellForced(wb,
    sheet = sheet_name,
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"]
  )

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "main",
      style_category = "datatable_header"
    ),
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )
  purrr::map(c(bounds["start_col"]:bounds["end_col"]), ~ mergeCellForced(wb,
    sheet = sheet_name,
    rows = (bounds["start_row"] + 1):(bounds["start_row"] + 2),
    cols = .x
  ))

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header"
    ),
    rows = (bounds["start_row"] + 1):(bounds["start_row"] + 2),
    cols = bounds["start_col"]:(bounds["end_col"]),
    gridExpand = TRUE
  )

  return(wb)
}
