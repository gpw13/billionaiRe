#' Write indicator list sheet
#'
#' @inherit write_baseline_projection_hpop_summary
#' @inheritParams style_header_hpop_summary_sheet
#' @inheritParams export_all_countries_summaries_xls
#'
write_indicator_list_sheet <- function(wb, sheet_name,
                                       billion,
                                       start_row, start_col){

  if(billion == "all"){
    billion <- c("hpop", "hep", "uhc")
  }

  indicator_list <- openxlsx::readWorkbook(wb, sheet = "Indicator List",
                                           startRow = start_row) %>%
    dplyr::filter(!!sym("Billion") %in% toupper(billion))

  names(indicator_list) <- stringr::str_replace_all(names(indicator_list), "\\.", " ")

  ## Write indicator list
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = indicator_list,
                      startCol = start_col, startRow = start_row
  )

  openxlsx::deleteData(wb, sheet = sheet_name,
                       cols = start_col:(start_col+30),
                       rows = (start_row + nrow(indicator_list)+1):(start_row + nrow(indicator_list)+100),
                       gridExpand = T)

  wb <- style_indicator_list_sheet(df = indicator_list, wb = wb, sheet_name, start_row, start_col,
                                   end_col = start_col+ncol(indicator_list)-1,
                                   end_row = start_row + nrow(indicator_list))

  return(wb)
}

#' Styles indicator sheet
#'
#' Used in `write_indicator_list_sheet()` to style the table.
#' @inherit style_hpop_indicators
#' @inherit style_header_hpop_summary_sheet
#' @inherit write_indicator_list_sheet
#' @param df data frame with the indicators to be styled
#' @param end_col integer identifying end column.
#' @param end_row integer identifying end row.

style_indicator_list_sheet <- function(df, wb, sheet_name, start_row, end_row, start_col, end_col){


  openxlsx::addStyle(wb,
                     sheet = sheet_name,
                     style = excel_styles()$title,
                     cols = 2,
                     rows = 2)

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = openxlsx::createStyle(
                       fontName = "Calibri",
                       fontColour = "white",
                       fontSize = 10,
                       border = c("top", "bottom"),
                       borderStyle = "thin",
                       fgFill = "grey"
                     ),
                     rows = start_row, cols = c(start_col:(end_col))
  )

  args <- list("billion" = list("HPOP", "HEP", "UHC"),
               "fgFill" = list("#B2DCEF", "#D1D9EB", "#A3DCCC"))

  purrr::pwalk(args,
               billion_styler,
               wb = wb,
               df = df,
               sheet_name = sheet_name,
               start_row = start_row,
               start_col = start_col,
               end_col = end_col)

  openxlsx::addStyle(wb, sheet = sheet_name,
                       cols = start_col:(start_col+30),
                       rows = (start_row + nrow(df)+1):(start_row + nrow(df)+100),
                     style = excel_styles()$white_bckgrd,
                       gridExpand = T)


  return(wb)
}

#' Generate a style with variable color or bolding
#'
#' `add_style_wrapper()` adds a style for use within [timeseries_style()],
#' where the font color and decoration needs to vary based on data type. Wraps
#' around [openxlsx::addStyle()] and [openxlsx::createStyle()].
#'
#' @inheritParams timeseries_style
#' @param rows Rows to apply style to, passed to `openxlsx::add_style()`.
#' @param cols Columns to apply style to, passed to `openxlsx::add_style()`.
#' @param fgFill Background color, passed to [openxlsx::createStyle()].
add_style_wrapper_billion <- function(wb,
                              sheet_name,
                              rows,
                              cols,
                              fgFill) {
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    rows = rows,
    cols = cols,
    style = openxlsx::createStyle(
      fontName = "Calibri",
      fontSize = 8,
      halign = "left",
      wrapText = TRUE,
      numFmt = "TXT",
      fgFill = fgFill,
      border = "bottom",
      borderStyle = "thin"
    )
  )
}

#' @inheritParams add_style_wrapper_billion
#' @inheritParams timeseries_style
billion_styler <- function(wb,
                        sheet_name,
                        df,
                        billion,
                        start_row,
                        start_col,
                        end_col,
                        fgFill) {
  # map across matching row/column values
  inds <- which(df[,"Billion"] == billion, arr.ind = TRUE)

  purrr::walk(inds,
               ~ add_style_wrapper_billion(
                 wb = wb,
                 sheet_name = sheet_name,
                 rows = as.numeric(.x) + start_row,
                 cols = start_col:end_col,
                 fgFill = fgFill
               ))
}

