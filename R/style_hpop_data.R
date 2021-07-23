style_hpop_data <- function(df, wb, sheet_name,
                            start_row,
                            start_col,
                            nrow_main_df,
                            ncol_main_df,
                            l_paired_list_sentences,
                            l_contrib_header
                            ) {
  # HERE HERE HERE DOCUMENT!!
  start_row_subH <- start_row + 1
  start_row_subH_low <- start_row + 2
  start_row_data <- start_row + 3

  start_col_baseline <- start_col + 2
  start_col_baseline_trans <- start_col_baseline + l_paired_list_sentences
  start_col_baseline_type <- start_col_baseline_trans +  l_paired_list_sentences
  start_col_contrib <- start_col_baseline_type + 5
  start_col_latest <- start_col_contrib + l_contrib_header

  cols_headers <- c(start_col:(start_col_baseline-1),
                    start_col_baseline: (start_col_contrib-1),
                    start_col_contrib:(start_col_latest-1),
                    start_col_latest:ncol_main_df
  )

  # Merge header cells
  openxlsx::removeCellMerge(wb, sheet = sheet_name,
                            cols = start_col:ncol_main_df,
                            rows = start_row:start_row_subH_low)
  ## Units
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col:(start_col_baseline-1),
    rows = start_row
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col,
    rows = start_row_subH:start_row_subH_low
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col+1,
    rows = start_row_subH:start_row_subH_low
  )

  ## Baseline Proj
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_baseline:(start_col_contrib-1),
    rows = start_row
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_baseline:(start_col_baseline_trans-1),
    rows = start_row_subH
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_baseline_trans:(start_col_baseline_type-1),
    rows = start_row_subH
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_baseline_type:(start_col_baseline_type+1),
    rows = start_row_subH
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = (start_col_baseline_type+2):(start_col_contrib-2),
    rows = start_row_subH
  )
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = (start_col_contrib-1),
    rows = c(start_row_subH:start_row_subH_low)
  )
  ## Contribs
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_contrib:(start_col_latest-1),
    rows = start_row
  )
  for(i in start_col_contrib:ncol_main_df){
    openxlsx::mergeCells(
      wb, sheet = sheet_name,
      cols = i,
      rows = start_row_subH:start_row_subH_low
    )
  }
  ## Latest
  openxlsx::mergeCells(
    wb, sheet = sheet_name,
    cols = start_col_latest:(ncol_main_df),
    rows = start_row
  )

  # Style header

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$dark_blue_header,
    rows = start_row,
    cols = cols_headers
  )

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$light_blue_header,
    rows = start_row_subH,
    cols = cols_headers
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = excel_styles()$light_blue_header,
    rows = start_row_subH_low,
    cols = cols_headers
  )

  # Style data table

  style_data(df,
             wb, sheet_name = sheet_name,
             rows = c(start_row_data:(nrow_main_df)),
             cols = cols_headers)
}

#' Style data according to its type
#'
#' @inheritParams write_main_df
#' @inheritParams openxlsx::addStyle
#'
style_data <- function(df, wb, sheet_name ,
                       rows,
                       cols){

  for(i in seq(ncol(df))){
    if(dplyr::type_sum(df[[i]]) == "dbl"){
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_dec,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
    }else if(dplyr::type_sum(df[[i]]) == "date"){
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_date,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
    }else{
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_int,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
    }
  }
}

