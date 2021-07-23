#' Write indicator list sheet
#'
#' @inherit write_main_df
#'
write_indicator_list_sheet <- function(df, wb, sheet_name,
                                       start_row, start_col){
  ## White background
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$white_bckgrd,
                     rows = c(1:(start_row + nrow(df) + 5)),
                     cols = c(1:(start_col + ncol(df)+2)), gridExpand = TRUE
  )

  ## Write indicator list
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = df,
                      startCol = start_col, startRow = start_row
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$dark_blue_header,
                     rows = start_row, cols = c(start_col:(ncol(df)+1))
  )
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_data_wrapped_int,
                     rows = c((start_row + 1):(nrow(df)+2)), cols = c(2:(start_col+ncol(df)-1)),
                     gridExpand = TRUE
  )

  for (i in seq(start_col:(ncol(df) + 1))) {
    openxlsx::setColWidths(
      wb,
      sheet = sheet_name,
      cols = i,
      widths = "auto",
      ignoreMergedCells = FALSE
    )
  }

  return(wb)
}
