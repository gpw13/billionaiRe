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
    widths = c(13.82, 30)
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
      style_category = "sub_datatable_header"
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


#' Style HEP Pillars summary sheet
#'
#' `style_hep_pillar()` styles the HEP Pillars (Prevent, Prepare, Detect and Respond)
#'  box/section of the HEP summary sheet. Used within `write_data_boxes_hep_summary()`.
#'
#' @param data_type named list with latest_reported and baseline_projection data
#' types. Passed to `style_data()`
#' @param pillar character Pillar identifying the pillar to style. Must be one of
#' * RMNCH
#' * infectious
#' * NCD
#' * service
#' @param fade Boolean with TRUE if rows in `fade_row` should be faded
#' @param fade_row integer identifying which row contains data no to show
#' @inherit style_header_hpop_summary_sheet
#' @inherit write_sheet_header_hpop_summary

style_hep_pillar <- function(wb, sheet_name, boxes_bounds, data_type,
                             pillar = c("prepare", "prevent", "detect_respond"),
                             fade = FALSE,
                             fade_row = 0) {
  pillar <- rlang::arg_match(pillar)

  # Style box header
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light2",
      fontSize = 10,
      fontColour = "black",
      halign = "left",
      valign = "center",
      textDecoration = "bold",
    ),
    rows = boxes_bounds[[pillar]]["start_row"],
    cols = boxes_bounds[[pillar]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"],
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      billion = "hep",
      billion_fgFill = "light2",
      fontSize = 10,
      fontColour = "black",
      halign = "left",
      valign = "center",
      textDecoration = "bold",
    ),
    rows = boxes_bounds[[pillar]]["start_row"],
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"],
    gridExpand = TRUE
  )

  # Style boxes indicators
  data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):(boxes_bounds[[pillar]]["end_row"] - 1)
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      style_category = "normal_text",
      border = "bottom",
      borderColour = "grey",
      valign = "center",
      wrapText = FALSE
    ),
    rows = data_rows,
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      style_category = "normal_text",
      border = "bottom",
      borderColour = "black",
      valign = "center"
    ),
    rows = data_rows[length(data_rows)],
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      style_category = "normal_text",
      fontColour = "grey",
      border = "bottom",
      borderColour = "grey",
      valign = "center",
      wrapText = FALSE
    ),
    rows = fade_row,
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )

  # Styling data rows
  wb <- style_data(
    data_type = data_type$latest_reported, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["latest_reported_data"]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"],
    fade = fade, fade_row = fade_row
  )

  wb <- style_data(
    data_type = data_type$baseline_projection, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"],
    fade = fade, fade_row = fade_row
  )

  hep_pillar_average_text <- excel_styles(
    style_category = "data",
    type_data = "numeric",
    fgFill = "#F2F2F2",
    halign = "right",
    valign = "center",
    textDecoration = c("bold", "italic"),
    numFmt = "TEXT"
  )
  openxlsx::addStyle(wb, sheet_name,
    style = modifyStyle(hep_pillar_average_text,
      halign = "right",
      valign = "center",
      textDecoration = c("bold", "italic"),
      numFmt = "TEXT"
    ),
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[[pillar]]["start_col"] + 1,
    gridExpand = TRUE
  )

  hep_pillar_average_data <- excel_styles(
    style_category = "data",
    type_data = "numeric",
    fgFill = "#F2F2F2",
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )
  # Styling averages rows
  openxlsx::addStyle(wb, sheet_name,
    style = hep_pillar_average_data,
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[["latest_reported_data"]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"]
  )

  openxlsx::addStyle(wb, sheet_name,
    style = hep_pillar_average_data,
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"]
  )

  return(wb)
}

#' Style HEP Summary box in summary sheet
#'
#' `style_summary_box_hep_summary()` styles the HEP summary box/section of the
#' HEP summary sheet. Used within `write_summary_box_hep_summary()`.
#'
#' @inherit style_header_hpop_summary_sheet
#' @inherit write_sheet_header_hpop_summary
style_summary_box_hep_summary <- function(wb, sheet_name, boxes_bounds) {
  bold_rows <- c(
    boxes_bounds[["summary"]]["start_row"],
    boxes_bounds[["summary"]]["start_row"] + 1,
    boxes_bounds[["summary"]]["end_row"] - 1,
    boxes_bounds[["summary"]]["end_row"]
  )

  normal_rows <- c(
    boxes_bounds[["summary"]]["start_row"] + 2
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep", billion_fgFill = "light2",
      textDecoration = "bold",
      numFmt = "TEXT",
      fontSize = 8
    ),
    rows = bold_rows,
    cols = boxes_bounds[["summary"]]["start_col"] + 1,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "hep", billion_fgFill = "light2",
      numFmt = "TEXT",
      fontSize = 8
    ),
    rows = normal_rows,
    cols = boxes_bounds[["summary"]]["start_col"] + 1,
    gridExpand = TRUE
  )

  col_latest_last <- boxes_bounds[["latest_reported_data"]]["end_col"]

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "hep", billion_fgFill = "light2",
      fontSize = 8
    ),
    rows = normal_rows,
    cols = col_latest_last,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "hep", billion_fgFill = "light2",
      textDecoration = "bold",
      fontSize = 8
    ),
    rows = bold_rows,
    cols = col_latest_last,
    gridExpand = TRUE
  )


  col_raw_start_year <- boxes_bounds[["baseline_projection_data"]]["start_col"]
  col_raw_end_year <- boxes_bounds[["baseline_projection_data"]]["start_col"] + 1

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "hep", billion_fgFill = "light2",
      fontSize = 8
    ),
    rows = normal_rows,
    cols = col_raw_start_year:col_raw_end_year,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "hep", billion_fgFill = "light2",
      textDecoration = "bold",
      fontSize = 8
    ),
    rows = bold_rows,
    cols = col_raw_start_year:col_raw_end_year,
    gridExpand = TRUE
  )

  return(wb)
}
