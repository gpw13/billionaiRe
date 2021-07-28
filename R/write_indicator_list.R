#' Write indicator list sheet
#'
#' @inherit write_main_df
#'
write_indicator_list_sheet <- function(df, wb, sheet_name,
                                       start_row, start_col){

  ## Write indicator list
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = df,
                      startCol = start_col, startRow = start_row
  )

  wb <- style_indicator_list_sheet(df, wb, sheet_name, start_row, start_col)

  return(wb)
}
