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
      fontColour = "white",
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
      billion = "hep",
      billion_fgFill = "light",
      fontSize = 10,
      fontColour = "white",
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

#' Style HEP summary worksheet data headers
#'
#' `style_data_headers_hep_summary()` styles headers of the data part of the HEP
#' summary worksheet: indicators, latest reported, and baseline/projection headers
#'
#' @inherit style_header_hpop_summary_sheet
#' @inheritParams write_sheet_header_hpop_summary
style_data_headers_hep_summary <- function(wb, sheet_name, boxes_bounds) {

  # Tracer area / tracer indicator headers
  indic_bounds <- c(
    start_col = as.integer(boxes_bounds$data_header["start_col"]),
    end_col = as.integer(boxes_bounds$data_header["start_col"] + 1),
    start_row = as.integer(boxes_bounds$data_header["start_row"]),
    end_row = as.integer(boxes_bounds$data_header["end_row"])
  )
  wb <- style_hep_headers(wb, sheet_name, bounds = indic_bounds)

  # Syling Latest reported data headers
  wb <- style_hep_headers(wb, sheet_name, bounds = boxes_bounds$latest_reported_data)

  # Styling main baseline/projection header
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      billion = "hep",
      billion_fgFill = "main",
      style_category = "datatable_header",
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = boxes_bounds$baseline_projection_data["start_row"],
    gridExpand = TRUE
  )

  # Styling baseline/projection sub-header
  # Back ground color
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1):((boxes_bounds$baseline_projection_data["start_row"] + 2)),
    gridExpand = TRUE
  )
  # Raw values
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["start_col"] + 1),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["start_col"] + 1),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1)
  )

  # levels values
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 3):(boxes_bounds$baseline_projection_data["start_col"] + 4),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 3):(boxes_bounds$baseline_projection_data["start_col"] + 4),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1)
  )


  # Types
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 6):(boxes_bounds$baseline_projection_data["start_col"] + 7),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )
  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 6):(boxes_bounds$baseline_projection_data["start_col"] + 7),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1)
  )

  # Sources
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 9):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 9):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1)
  )


  # years row
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$data_header["end_row"]),
    gridExpand = TRUE
  )

  # Width columns
  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$data_header["start_col"]:(boxes_bounds$data_header["start_col"] + 1)),
    widths = c(13.82, 27.64)
  )
  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$latest_reported_data["start_col"]:(boxes_bounds$latest_reported_data["end_col"])),
    widths = c(7.18, 7.18, 7.18, 7.18, 26)
  )

  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$baseline_projection_data["start_col"]:(boxes_bounds$baseline_projection_data["end_col"])),
    widths = c(7, 7, 0.5, 7, 7, 0.5, 7.22, 7.22, 0.5, 26, 26)
  )


  return(wb)
}
