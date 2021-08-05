#' Style table headers
#'
#' Used within `style_hpop_indicators()`, `style_hpop_latest()`
#'
#' @inherit write_latest_reported_hpop_summary
#'

style_hpop_headers <- function(wb, sheet_name, bounds){

  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds["start_row"],
                  cols = bounds["start_col"]: bounds["end_col"])

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$hpop_main_data_header,
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )
  purrr::map(c(bounds["start_col"]:bounds["end_col"]), ~ mergeCellForced(wb, sheet = sheet_name,
                  rows = (bounds["start_row"]+1):(bounds["start_row"]+2),
                  cols = .x))

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$hpop_sec_data_header,
    rows =  (bounds["start_row"]+1):(bounds["start_row"]+2),
    cols = bounds["start_col"]:(bounds["end_col"]),
    gridExpand = TRUE
  )

  return(wb)
}
