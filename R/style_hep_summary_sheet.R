#' Style HEP summary worksheet header
#'
#' `style_header_hep_summary_sheet` styles the title and sub-title of the summary
#' worksheet header.
#' @inherit style_data_headers_uhc_summary
#'
style_header_hep_summary_sheet <- function(wb, sheet_name, boxes_bounds) {
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles(style_category = "title"),
                     rows = boxes_bounds$sheet_header["start_row"],
                     cols = boxes_bounds$sheet_header["start_col"]
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles(style_category = "subtitle"),
                     rows = boxes_bounds$sheet_header["start_row"] + 2,
                     cols = boxes_bounds$sheet_header["start_col"]
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles(
                       style_category = "normal_text",
                       fontSize = 10
                     ),
                     rows = c((boxes_bounds$sheet_header["start_row"] + 3):(boxes_bounds$sheet_header["start_row"] + 5)),
                     cols = boxes_bounds$sheet_header["start_col"], gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles(
                       billion = "hep",
                       billion_fgFill = "light",
                       fontSize = 10,
                       textDecoration = "bold",
                       numFmt = "0.00",
                       halign = "right"
                     ),
                     rows = (boxes_bounds$sheet_header["start_row"] + 3):(boxes_bounds$sheet_header["start_row"] + 5),
                     cols = (boxes_bounds$sheet_header["start_col"] + 6),
                     gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles(
                       billion = "uhc",
                       billion_fgFill = "light",
                       fontSize = 10,
                       textDecoration = "bold",
                       numFmt = "0.00",
                       halign = "left"
                     ),
                     rows = (boxes_bounds$sheet_header["start_row"] + 3):(boxes_bounds$sheet_header["start_row"] + 5),
                     cols = (boxes_bounds$sheet_header["start_col"] + 7),
                     gridExpand = TRUE
  )

  return(wb)
}
