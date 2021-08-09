#' Style HPOP summary sheet indicators in main data frame
#'
#' Used within `write_indicators_hpop_summary()`
#'
#' @inherit write_latest_reported_hpop_summary
#' @inheritParams style_data_single
#' @inherit write_baseline_projection_hpop_summary

style_hpop_indicators <- function(wb, sheet_name, bounds, data_type){

  wb <- style_hpop_headers(wb, sheet_name, bounds)

  style_data(data_type, wb, sheet_name,
             rows = (bounds["start_row"]+2):bounds["end_row"],
             cols = bounds["start_col"]:(bounds["end_col"])
  )

  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         widths = c(9.09, 22.18),
                         cols = c(bounds["start_col"], bounds["end_col"])
                         )

  return(wb)
}

#' Style HPOP summary sheet latest reported in main data frame
#'
#' Used within `write_latest_reported_hpop_summary()`
#'
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary
style_hpop_latest <- function(wb, sheet_name, bounds, data_type){

  wb <- style_hpop_headers(wb, sheet_name, bounds)

  style_data(data_type, wb, sheet_name,
             rows = (bounds["start_row"]+3):bounds["end_row"],
             cols = bounds["start_col"]:(bounds["end_col"])
  )

  openxlsx::conditionalFormatting(wb,
                                  sheet_name,
                                  cols = bounds["start_col"]+grep("year", names(data_type))-1,
                                  rows = (bounds["start_row"]+3):bounds["end_row"],
                                  rule = glue::glue("{openxlsx::int2col(bounds['end_col'])}{bounds['start_row']+3} > 1"),
                                  style = excel_styles()$normal_data_wrapped_bold_int)

  openxlsx::conditionalFormatting(wb,
                                  sheet_name,
                                  cols = bounds["start_col"]+grep("year", names(data_type))-1,
                                  rows = bounds["end_row"],
                                  rule = glue::glue("{openxlsx::int2col(bounds['end_col'])}{bounds['start_row']+3} > 1"),
                                  style = excel_styles()$normal_data_wrapped_bold_int_black_border)

  #TODO: adapt to take more than one value
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         widths = c(8.64, 8.64, 6.64, 8.64, 17.91,9.55, 9.55),
                         cols = c(bounds["start_col"]:bounds["end_col"])
  )

  return(wb)
}

#' Style HPOP summary sheet latest reported in main data frame
#'
#' Used within `write_baseline_projection_hpop_summary()`
#'
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary

style_hpop_baseline_projection <- function(wb, sheet_name, bounds, data_type){

  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds["start_row"],
                  cols = bounds["start_col"]: bounds["end_col"])

  openxlsx::addStyle(wb, sheet_name,
                     style = excel_styles()$hpop_main_data_header,
                     cols = c(bounds['start_col']:bounds['end_col']),
                     rows = bounds['start_row'],
                     gridExpand = TRUE)

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$hpop_main_data_header,
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )

  #TODO: adapt to take more than one value (at the moment if length(value) >1, cells merging and styling won't work)
  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds['start_row']+1,
                  cols = c(bounds['start_col']:(bounds['start_col']+1))
  )
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header_border,
                     rows = bounds['start_row']+1,
                     cols = (bounds['start_col']:(bounds['start_col']+1))
  )

  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds['start_row']+1,
                  cols = ((bounds['start_col']+3):(bounds['start_col']+4))
  )

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header_border,
                     rows = bounds['start_row']+1,
                     cols = ((bounds['start_col']+3):(bounds['start_col']+4)),
                     gridExpand = TRUE
                     )

  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds['start_row']+1,
                  cols = ((bounds['start_col']+6):(bounds['start_col']+7))
  )
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header_border,
                     rows = bounds['start_row']+1,
                     cols = ((bounds['start_col']+6):(bounds['start_col']+7)),
                     gridExpand = TRUE
  )

  mergeCellForced(wb, sheet = sheet_name,
                  rows = bounds['start_row']+1,
                  cols = ((bounds['start_col']+9):(bounds['start_col']+10))
  )
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header_border,
                     rows = bounds['start_row']+1,
                     cols = ((bounds['start_col']+9):(bounds['start_col']+10)),
                             gridExpand = TRUE
  )
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header_left_align,
                     rows = bounds['start_row']+2,
                     cols = bounds['start_col']:bounds['end_col'],
                     gridExpand = TRUE)

  wb <- style_data(data_type, wb, sheet_name,
                   rows = (bounds["start_row"]+3):(bounds["end_row"]),
                   cols = (bounds["start_col"]):bounds["end_col"])

  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         widths = c(6.09, 6.09, 0.5, 6.09, 6.09, 0.5, 8.64, 8.64, 0.5, 17.91, 17.91),
                         cols = c(bounds["start_col"]: bounds["end_col"])
  )

  return(wb)
}
#' Style HPOP summary sheet contribution to billion
#'
#' Used within `write_bilion_contrib_ind_hpop_summary()`
#'
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary

style_billion_contrib_ind_hpop <- function(wb, sheet_name, bounds, data_type){
  wb <- style_hpop_headers(wb, sheet_name, bounds)

  style_data(data_type, wb, sheet_name,
             rows = (bounds["start_row"]+3):bounds["end_row"],
             cols = bounds["start_col"]:(bounds["end_col"])
  )

  #TODO: To be adapted to take multiple values in `value`
  openxlsx::setColWidths(wb,
                         sheet = sheet_name,
                         widths = c(12.55, 12.55, 12.55, 12.55),
                         cols = c(bounds["start_col"]:bounds["end_col"])
  )

  openxlsx::conditionalFormatting(wb,
                                  sheet_name,
                                  cols = (bounds["end_col"]-2):bounds["end_col"],
                                  rows = (bounds["start_row"]+3):bounds["end_row"],
                                  rule = "<0",
                                  style = excel_styles()$normal_data_wrapped_red)

  openxlsx::conditionalFormatting(wb,
                                  sheet_name,
                                  cols = (bounds["end_col"]-2):bounds["end_col"],
                                  rows = bounds["end_row"],
                                  rule = "<0",
                                  style = excel_styles()$normal_data_wrapped_red_black_border)
  return(wb)
}

#' Style HPOP billion contribution all indicators summary box
#'
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary

style_hpop_billion_contribution <- function(wb, sheet_name, bounds){

  for(i in bounds['start_row']:bounds['end_row']){
    mergeCellForced(wb, sheet_name,
                    rows = i,
                    cols = (bounds['start_col']):(bounds['start_col']+1))
  }

  mergeCellForced(wb, sheet_name,
                  rows = bounds['start_row'],
                  cols = (bounds['start_col']+2):(bounds['end_col']))

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_main_data_header,
                     rows = bounds['start_row'],
                     cols = bounds['start_col']:bounds['end_col'],
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_main_data_header,
                     rows = bounds['start_row']+1,
                     cols = bounds['start_col']:(bounds['start_col']+1),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$hpop_sec_data_header,
                     rows = bounds['start_row']+1,
                     cols = (bounds['start_col']+2):(bounds['end_col']),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_text_faded,
                     rows = (bounds['start_row']+2):(bounds['start_row']+3),
                     cols = (bounds['start_col']):(bounds['start_col']+1),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_int_faded,
                     rows = (bounds['start_row']+2):(bounds['start_row']+4),
                     cols = (bounds['start_col']+2):(bounds['end_col']),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_int_faded_black_border,
                     rows = (bounds['end_row']),
                     cols = (bounds['start_col']+2),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_wrapped_bold_int,
                     rows = (bounds['end_row']-1),
                     cols = bounds['end_col'],
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_wrapped_bold_int,
                     rows = (bounds['end_row']-1),
                     cols = bounds['start_col'],
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$bold_data_dec_black_border,
                     rows = (bounds['end_row']),
                     cols = (bounds['end_col']),
                     gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$bold_data_dec_black_border,
                     rows = (bounds['end_row']),
                     cols = (bounds['start_col']),
                     gridExpand = TRUE)

  return(wb)
}

#' Style HPOP summary worksheet header
#'
#' `style_header_hpop_summary_sheet` styles the title and sub-title of the worksheet header.
#' @param start_row integer start row of styling. Passed to [openxlsx::addStyle()]
#' @param start_col integer start col of styling. Passed to [openxlsx::addStyle()]
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary
#'
style_header_hpop_summary_sheet <- function(wb, sheet_name, start_row, start_col){
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$title,
                     rows = start_row,
                     cols = start_col
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$sub_title,
                     rows = start_row +2 ,
                     cols = start_col
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_text_10p,
                     rows = c((start_row +3):(start_row+5)) ,
                     cols = start_col, gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$bold_hpop_blue_hR_2dp,
                     rows = (start_row+3):(start_row+5),
                     cols = (start_col + 4),
                     gridExpand = TRUE
                     )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$bold_hpop_blue_hL_2dp,
                     rows = (start_row+3):(start_row+5),
                     cols = (start_col + 5),
                     gridExpand = TRUE
                     )

  return(wb)
}
