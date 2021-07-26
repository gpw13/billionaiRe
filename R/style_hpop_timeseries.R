#' Styles HPOP timerserie
#'
#' @inheritParams write_hpop_timeseries_sheet

style_hpop_timeseries <- function(df, wb, sheet_name, start_row, start_col, ind,
                                  year, type_col, df_wide, trans_value){

  openxlsx::mergeCells(wb, sheet = sheet_name,
                       cols = start_col, rows = start_row:(start_row+1))

  openxlsx::mergeCells(wb, sheet = sheet_name,
                       cols = (start_col+1):(ncol(df_wide)), rows = start_row)

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$dark_blue_header,
                     rows =start_row, cols = c(start_col:(ncol(df_wide)))
  )
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$dark_blue_header,
                     rows = start_row:(start_row+1), cols = start_col
  )
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$light_blue_header,
                     rows = c((start_row+1)), cols = c((start_col+1):(ncol(df_wide))),
                     gridExpand = TRUE
  )
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_data_wrapped_dec,
                     rows = c((start_row+2):(start_row+nrow(df_wide)+1)), cols = c(start_col:(ncol(df_wide))),
                     gridExpand = TRUE
  )
  timeseries_style(df, wb, sheet_name, start_row = start_row+1 , start_col = start_col,
                   ind, year, type_col)

  return(wb)
}
