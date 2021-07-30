#' Style HPOP data sheet
#'
#' Applies styles to the data worksheet for country summary outputs
#'
#' @inheritParams write_main_df
#' @param length_baseline_header integer with length of the baseline sub-header
#' @param length_contrib_header integer with length of the billion's contributions
#'  sub-header
#'

style_hpop_main_data <- function(df, wb, sheet_name,
                            start_row,
                            start_col,
                            length_baseline_header,
                            length_contrib_header
                            ) {

  nrow_main_df <- nrow(df)
  ncol_main_df <- ncol(df)

  start_row_subH <- start_row + 1
  start_row_subH_low <- start_row + 2
  start_row_data <- start_row + 3

  start_col_baseline <- start_col + 2
  start_col_baseline_trans <- start_col_baseline + length_baseline_header
  start_col_baseline_type <- start_col_baseline_trans +  length_baseline_header
  start_col_contrib <- start_col_baseline_type + 5
  start_col_latest <- start_col_contrib + length_contrib_header

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
             rows = c(start_row_data:(start_row_data+nrow_main_df -1)),
             cols = cols_headers)

  return(wb)
}

#' Style HPOP billion contribution all indicators summary box
#'
#' @inheritParams style_hpop_main_data
#'

style_hpop_billion_contribution <- function(df, wb, sheet_name,
                                            start_row,
                                            start_col){

  # Merge cells
  for(i in start_row:(start_row+5)){
    openxlsx::mergeCells(wb, sheet_name, cols = c(start_col:(start_col +1)), rows = i)
  }
  openxlsx::mergeCells(wb, sheet_name, cols = c((start_col +2):(start_col +1+ ncol(df))), rows = start_row)

  # Styles
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$dark_blue_header,
                     rows = start_row,
                     cols = start_col:(start_col +1+ ncol(df)))
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$dark_blue_header,
                     rows = start_row + 1,
                     cols = start_col:(start_col + 1))
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$light_blue_header,
                     rows = start_row+1,
                     cols = (start_col):(start_col+1+ncol(df)),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_wrapped_dec,
                     rows = (start_row+2):(start_row+1+nrow(df)),
                     cols = (start_col):(start_col+1+ncol(df)),
                     gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name,
                     style = excel_styles()$normal_data_wrapped_bold_dec,
                     rows = (start_row+4):(start_row+1+nrow(df)),
                     cols = (start_col):(start_col+1+ncol(df)),
                     gridExpand = TRUE)
  return(wb)
}

#' Style data according to its type
#'
#' @inheritParams write_main_df
#' @inheritParams openxlsx::addStyle
#'
style_data <- function(df, wb, sheet_name ,
                       rows,
                       cols){
  #TODO: Use a purrr version, similar to `timeseries_style`
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

#' Style worksheet header
#'
#' `style_header` styles the title and sub-title of the worksheet header.
#' @inherit style_hpop_main_data
#'
style_header <- function(wb, sheet_name, start_row, start_col){
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$title,
                     rows = start_row,
                     cols = start_col
  )

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$sub_title,
                     rows = start_row +2 ,
                     cols = start_col, gridExpand = TRUE
  )

  return(wb)
}

#' Styles data worksheet notes
#'
#' @inherit style_hpop_main_data
#' @param  nrow_notes integer with number of rows in the notes data frame being
#'    styled
#' @param end_col integer indicating where the columns should stop being merged

style_notes_data <- function(df, wb, sheet_name, start_row, start_col, nrow_notes, end_col){
  for(i in seq(nrow_notes)){
    openxlsx::mergeCells(wb, sheet = sheet_name,
                         cols = 1:end_col,
                         rows = (start_row+1+i))
  }

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_data_int,
                     rows = (start_row+1):(start_row+nrow(df)),
                     cols = c(1:end_col), gridExpand = TRUE
  )

  return(wb)
}

#' Set column width based on type of column in data frame for openxlsx export
#'
#' @inherit write_main_df
#' @inherit transform_hpop_data
#'

get_col_width_hpop <- function(df, value, transform_value, type_col, source_col,
                               contribution, contribution_pct, contribution_pct_pop_total,
                               year,
                               start_year, end_year){
  medium_width <- 14
  value_width <- 11
  source_width <- 49
  names_df <- names(df)
  value_regex <- c(glue::glue("^{value}_{start_year}$"),
                   glue::glue("^{value}_{max(end_year)}$"),
                   glue::glue("^{value}$"),
                   glue::glue("^{transform_value}_{start_year}$"),
                   glue::glue("^{transform_value}_{max(end_year)}$"),
                   glue::glue("^{transform_value}$"),
                   glue::glue("{type_col}"),
                   glue::glue("{year}"), "upload_date")
  value_cols <- sort(unlist(lapply(value_regex, grep, names_df)))
  names_df[value_cols] <- value_width

  medium_length_regex <- c("short_name",
                           glue::glue("^change_{transform_value}"),
                           glue::glue("^{contribution_pct}$"),
                           glue::glue("^{contribution}$"),
                           glue::glue("^{contribution_pct_pop_total}$"),
                           "^count_", "population")
  medium_cols <- sort(unlist(lapply(medium_length_regex, grep, names_df)))
  names_df[medium_cols] <- medium_width

  source_cols <-sort(unlist(lapply(source_col, grep, names_df)))
  names_df[source_cols] <- source_width

  names_df <- as.numeric(names_df)

  assert_same_length(names_df[!is.na(names_df)],names(df))
  return(names_df)

}

