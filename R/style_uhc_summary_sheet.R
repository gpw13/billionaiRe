#' Style UHC summary worksheet header
#'
#' `style_header_uhc_summary_sheet` styles the title and sub-title of the summary
#' worksheet header.
#' @inherit style_data_headers_uhc_summary
#'
style_header_uhc_summary_sheet <- function(wb, sheet_name, boxes_bounds) {
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
      billion = "uhc",
      billion_fgFill = "light",
      fontSize = 10,
      textDecoration = "bold",
      numFmt = "0.00",
      halign = "right"
    ),
    rows = (boxes_bounds$sheet_header["start_row"] + 3):(boxes_bounds$sheet_header["start_row"] + 5),
    cols = (boxes_bounds$sheet_header["start_col"] + 4),
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
    cols = (boxes_bounds$sheet_header["start_col"] + 5),
    gridExpand = TRUE
  )

  return(wb)
}

#' Style UHC summary worksheet data headers
#'
#' `style_data_headers_uhc_summary()` styles headers of the data part of the UHC
#' summary worksheet: indicators, latetest reported, and baseline/projection headers
#'
#' @inherit style_header_hpop_summary_sheet
#' @inheritParams write_sheet_header_hpop_summary
style_data_headers_uhc_summary <- function(wb, sheet_name, boxes_bounds) {

  # Tracer area / tracer indicator headers
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      billion = "uhc",
      billion_fgFill = "main",
      style_category = "datatable_header",
      halign = "left"
    ),
    rows = boxes_bounds$data_header["start_row"] + 2,
    cols = boxes_bounds$data_header["start_col"]:(boxes_bounds$data_header["start_col"] + 1),
    gridExpand = TRUE
  )
  # Merging latest reported
  mergeCellForced(wb,
    sheet = sheet_name,
    rows = boxes_bounds$latest_reported_data["start_row"],
    cols = (boxes_bounds$latest_reported_data["start_col"]):(boxes_bounds$latest_reported_data["end_col"])
  )
  # Syling Latest reported data headers
  wb <- style_uhc_headers(wb, sheet_name, bounds = boxes_bounds$latest_reported_data)

  mergeCellForced(wb,
    sheet = sheet_name,
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = boxes_bounds$baseline_projection_data["start_row"]
  )

  # Styling main baseline/projection header
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      billion = "uhc",
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
      billion = "uhc",
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
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "left",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["start_col"] + 1),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )

  # Transformed values
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "left",
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
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 6),
    rows = (boxes_bounds$baseline_projection_data["start_col"] + 1):(boxes_bounds$baseline_projection_data["start_col"] + 2)
  )

  # Types
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "left",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 8):(boxes_bounds$baseline_projection_data["start_col"] + 9),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )
  # Sources
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "left",
      border = "bottom",
      borderColour = "black",
      borderStyle = "thin"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 11):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
    gridExpand = TRUE
  )

  # years row
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
    rows = (boxes_bounds$data_header["end_row"]),
    gridExpand = TRUE
  )
  # Direction of change
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light",
      style_category = "sub_datatable_header",
      halign = "center"
    ),
    cols = (boxes_bounds$baseline_projection_data["start_col"] + 6),
    rows = (boxes_bounds$baseline_projection_data["start_col"] + 1):(boxes_bounds$baseline_projection_data["start_col"] + 2),
    gridExpand = TRUE
  )

  # Width columns
  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$data_header["start_col"]:(boxes_bounds$data_header["start_col"] + 1)),
    widths = 27.89
  )

  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$latest_reported_data["start_col"]:(boxes_bounds$latest_reported_data["end_col"])),
    widths = c(9.44, 9.44, 5.44, 7.22, 26)
  )

  openxlsx::setColWidths(wb,
    sheet = sheet_name,
    cols = c(boxes_bounds$baseline_projection_data["start_col"]:(boxes_bounds$baseline_projection_data["end_col"])),
    widths = c(7, 7, 0.5, 7, 7, 0.5, 7, 0.5, 7.22, 7.22, 0.5, 26, 26)
  )


  return(wb)
}

#' Style UHC Pillars summary sheet
#'
#' `style_uhc_pillar()` styles the UHC Pillars (RMNCH, infectious diseases, etc.)
#'  box/section of the UHC summary sheet. Used within `write_RMNCH_uhc_summary()`.
#'
#' @param data_type named list with latest_reported and baseline_projection data
#' types. Passed to `style_data()`
#' @param pillar character Pillar identifying the pillar to style. Must be one of
#' * RMNCH
#' * infectious
#' * NCD
#' * service
#' @param no_show Boolean with TRUE if data in `no_show_row` should be displayed
#' @param no_show_row integer identifying which row contains data no to show
#' @inherit style_header_hpop_summary_sheet
#' @inherit write_sheet_header_hpop_summary

style_uhc_pillar <- function(wb, sheet_name, boxes_bounds, data_type,
                             pillar = c("RMNCH", "infec_diseases", "ncd", "service_cap_access"),
                             no_show = TRUE,
                             no_show_row = 2) {
  pillar <- rlang::arg_match(pillar)

  # Style box header
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      billion = "uhc",
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
      billion = "uhc",
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


  normal_text_vCentered <- excel_styles(
    style_category = "normal_text",
    border = "bottom",
    borderColour = "grey",
    valign = "center"
  )

  # Style boxes indicators
  data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):(boxes_bounds[[pillar]]["end_row"] - 1)
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      style_category = "normal_text",
      border = "bottom",
      borderColour = "grey",
      valign = "center",
      wrapText = TRUE
    ),
    rows = data_rows,
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = modifyStyle(
      style = normal_text_vCentered,
      borderColour = "black"
    ),
    rows = data_rows[length(data_rows)],
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )

  # Styling data rows
  wb <- style_data(
    data_type = data_type$latest_reported, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["latest_reported_data"]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"],
    no_show = no_show, no_show_row = no_show_row
  )

  wb <- style_data(
    data_type = data_type$baseline_projection, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"],
    no_show = no_show, no_show_row = no_show_row
  )

  uhc_pillar_average_text <- excel_styles(
    style_category = "data",
    type_data = "numeric",
    fgFill = "#D9D9D9",
    halign = "right",
    valign = "center",
    textDecoration = c("bold", "italic"),
    numFmt = "TEXT"
  )
  openxlsx::addStyle(wb, sheet_name,
    style = modifyStyle(uhc_pillar_average_text,
      halign = "right",
      valign = "center",
      textDecoration = c("bold", "italic"),
      numFmt = "TEXT"
    ),
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[[pillar]]["start_col"] + 1,
    gridExpand = TRUE
  )

  uhc_pillar_average_data <- excel_styles(
    style_category = "data",
    type_data = "numeric",
    fgFill = "#D9D9D9",
    halign = "center",
    valign = "center",
    textDecoration = "bold"
  )
  # Styling averages rows
  openxlsx::addStyle(wb, sheet_name,
    style = uhc_pillar_average_data,
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[["latest_reported_data"]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"]
  )

  openxlsx::addStyle(wb, sheet_name,
    style = uhc_pillar_average_data,
    rows = boxes_bounds[[pillar]]["end_row"],
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"]
  )


  return(wb)
}

style_asc_uhc_data_summary <- function(wb, sheet_name, boxes_bounds, data_type,
                                       pillar = "fin_hardship",
                                       no_show = TRUE,
                                       no_show_cols,
                                       no_show_rows) {
  pillar <- rlang::arg_match(pillar)

  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light2",
      fontSize = 10,
      fontColour = "black",
      halign = "left",
      valign = "center",
      textDecoration = "bold",
    ),
    rows = boxes_bounds[[pillar]]["start_row"],
    cols = boxes_bounds[[pillar]]["start_col"]:boxes_bounds[[pillar]]["end_col"],
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = excel_styles(
      billion = "uhc",
      billion_fgFill = "light2",
      fontSize = 8,
      fontColour = "black",
      halign = "center",
      valign = "center",
      textDecoration = "bold",
      numFmt = "0.00"
    ),
    rows = boxes_bounds[[pillar]]["start_row"],
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"],
    gridExpand = TRUE
  )
  data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):(boxes_bounds[[pillar]]["end_row"])

  normal_text_vCentered <- excel_styles(
    style_category = "normal_text",
    border = "bottom",
    borderColour = "grey",
    valign = "center",
    wrapText = TRUE
  )

  openxlsx::addStyle(wb, sheet_name,
    style = normal_text_vCentered,
    rows = data_rows,
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet_name,
    style = modifyStyle(normal_text_vCentered, borderColour = "black"),
    rows = data_rows[length(data_rows)],
    cols = boxes_bounds[[pillar]]["start_col"]:(boxes_bounds[[pillar]]["start_col"] + 1),
    gridExpand = TRUE
  )

  style_data(
    data_type = data_type$latest_reported, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["latest_reported_data"]]["start_col"]:boxes_bounds[["latest_reported_data"]]["end_col"]
  )

  style_data(
    data_type = data_type$baseline_projection, wb, sheet_name,
    rows = data_rows,
    cols = boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"]
  )

  if (no_show) {
    openxlsx::addStyle(wb,
      sheet_name,
      style = excel_styles(style_category = "normal_text", hide = TRUE),
      rows = no_show_rows,
      cols = no_show_cols,
      gridExpand = TRUE
    )
  }

  return(wb)
}

style_summary_box_uhc_summary <- function(wb, sheet_name, boxes_bounds) {
  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc", billion_fgFill = "light",
      textDecoration = "bold",
      numFmt = "TEXT",
      fontSize = 8
    ),
    rows = c(
      boxes_bounds[["summary"]]["start_row"],
      boxes_bounds[["summary"]]["end_row"] - 1,
      boxes_bounds[["summary"]]["end_row"]
    ),
    cols = boxes_bounds[["summary"]]["start_col"] + 1,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      billion = "uhc", billion_fgFill = "light",
      numFmt = "TEXT",
      fontSize = 8
    ),
    rows = c(
      boxes_bounds[["summary"]]["start_row"] + 1,
      boxes_bounds[["summary"]]["start_row"] + 2
    ),
    cols = boxes_bounds[["summary"]]["start_col"] + 1,
    gridExpand = TRUE
  )

  col_trans_start_year <- boxes_bounds[["baseline_projection_data"]]["start_col"] + 3
  col_trans_end_year <- boxes_bounds[["baseline_projection_data"]]["start_col"] + 4

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "uhc", billion_fgFill = "light",
      fontSize = 8
    ),
    rows = c(
      boxes_bounds[["summary"]]["start_row"],
      boxes_bounds[["summary"]]["end_row"]
    ),
    cols = col_trans_start_year:col_trans_end_year,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "numeric",
      billion = "uhc", billion_fgFill = "light",
      textDecoration = "bold",
      fontSize = 8
    ),
    rows = c(
      boxes_bounds[["summary"]]["start_row"],
      boxes_bounds[["summary"]]["end_row"]
    ),
    cols = col_trans_start_year:col_trans_end_year,
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name,
    style = excel_styles(
      type_data = "integer",
      billion = "uhc", billion_fgFill = "light",
      textDecoration = "bold",
      fontSize = 8
    ),
    rows = c(boxes_bounds[["summary"]]["end_row"] - 1),
    cols = col_trans_start_year:col_trans_end_year,
    gridExpand = TRUE
  )
  return(wb)
}
