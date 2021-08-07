#' Style UHC summary worksheet header
#'
#' `style_header_uhc_summary_sheet` styles the title and sub-title of the summary
#' worksheet header.
#' @inherit style_header_hpop_summary_sheet
#'
style_header_uhc_summary_sheet <- function(wb, sheet_name, bounds){
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$title,
                     rows = bounds["start_row"],
                     cols = bounds["start_col"]
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$sub_title,
                     rows = bounds["start_row"] +2 ,
                     cols = bounds["start_col"]
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_text_10p,
                     rows = c((bounds["start_row"] +3):(bounds["start_row"]+5)) ,
                     cols = bounds["start_col"], gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$bold_uhc_hR_2dp,
                     rows = (bounds["start_row"]+3):(bounds["start_row"]+5),
                     cols = (bounds["start_col"] + 4),
                     gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$bold_uhc_hL_2dp,
                     rows = (bounds["start_row"]+3):(bounds["start_row"]+5),
                     cols = (bounds["start_col"] + 5),
                     gridExpand = TRUE
  )

  return(wb)
}

style_data_headers_uhc_summary <- function(wb, sheet_name, boxes_bounds){


  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$uhc_main_data_header_left_align,
                     rows = boxes_bounds$data_header["start_row"]+2,
                     cols = boxes_bounds$data_header["start_col"]:(boxes_bounds$data_header["start_col"]+1),
                     gridExpand = TRUE
  )

  mergeCellForced(wb, sheet = sheet_name,
                  rows = boxes_bounds$latest_reported_data["start_row"],
                  cols = (boxes_bounds$latest_reported_data["start_col"]):(boxes_bounds$latest_reported_data["end_col"])
  )

  wb <- style_hpop_headers(wb, sheet_name, bounds = boxes_bounds$latest_reported_data)

  mergeCellForced(wb, sheet = sheet_name,
                  cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
                  rows = boxes_bounds$baseline_projection_data["start_row"]
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$uhc_main_data_header,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["end_col"]),
                     rows = boxes_bounds$baseline_projection_data["start_row"],
                     gridExpand = TRUE
  )

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header_border_left_align,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data["start_col"]+1),
                     rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header_border_left_align,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]+3):(boxes_bounds$baseline_projection_data["start_col"]+4),
                     rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
                     gridExpand = TRUE)

  mergeCellForced(wb, sheet = sheet_name,
                  cols = (boxes_bounds$baseline_projection_data['start_col']+6),
                  rows = (boxes_bounds$baseline_projection_data["start_col"]+1):(boxes_bounds$baseline_projection_data['start_col']+2)
  )

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header_border_left_align,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]+8):(boxes_bounds$baseline_projection_data["start_col"]+9),
                     rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header_border_left_align,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]+11):(boxes_bounds$baseline_projection_data["end_col"]),
                     rows = (boxes_bounds$baseline_projection_data["start_row"] + 1),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header,
                     cols = (boxes_bounds$baseline_projection_data["start_col"]):(boxes_bounds$baseline_projection_data['end_col']),
                     rows = (boxes_bounds$data_header["end_row"]),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$uhc_sec_data_header,
                     cols = (boxes_bounds$baseline_projection_data['start_col']+6),
                     rows = (boxes_bounds$baseline_projection_data["start_col"]+1):(boxes_bounds$baseline_projection_data['start_col']+2),
                     gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet = sheet_name,
                         cols = c(boxes_bounds$data_header["start_col"]:(boxes_bounds$data_header['start_col']+1)),
                         widths = 27.89
  )

  openxlsx::setColWidths(wb, sheet = sheet_name,
                         cols = c(boxes_bounds$latest_reported_data["start_col"]:(boxes_bounds$latest_reported_data['end_col'])),
                         widths = c(9.44, 9.44,5.44, 7.22, 26)
  )

  openxlsx::setColWidths(wb, sheet = sheet_name,
                         cols = c(boxes_bounds$baseline_projection_data["start_col"]:(boxes_bounds$baseline_projection_data['end_col'])),
                         widths = c(7, 7, 0.5, 7,7,0.5, 7, 0.5, 7.22, 7.22, 0.5, 26, 26)
  )


  return(wb)
}
