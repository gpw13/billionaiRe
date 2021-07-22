#' Write the main data frame to the data sheet
#'
#' @param df data frame to be written
#' @param wb openxlsx workbook to be edited.
#' @param start_row numeric start row of the table in the excel sheet
#' @param start_col numeric start row of the table in the excel sheet
#' @param sheet_name character name of the sheet to update
#' @inherit export_country_summary_xls params return
#'
write_main_df <-
  function(df,
           wb,
           start_row,
           start_col,
           start_year,
           end_year,
           sheet_name,
           value,
           transform_value,
           type_col,
           source_col) {

    n_value <- length(value)

    # Write column headers
    ## Indicators headers
    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Indicators",
      startCol = start_col, startRow = start_row, colNames = FALSE
    )

    start_row_subH <- start_row + 1
    start_row_subH_low <- start_row + 2
    start_row_data <- start_row + 3

    start_col_baseline <- start_col + 2

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = vec2emptyDF(c("Indicator transformed", "Unit transformed")),
      startCol = start_col, startRow = start_row_subH, colNames = TRUE
    )

    ## Baseline and projections headers
    start_col_baseline <- start_col + 2

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = glue::glue("{start_year} Baseline, and {max(end_year)} Projection"),
      startCol = start_col_baseline, startRow = start_row, colNames = FALSE
    )

    baseline_proj_header <- vec2emptyDF(c("Raw Value", rep("", n_value*2-1),
                                          "Transform Value", rep("", n_value*2-1),
                                          "Type", "",
                                          "Source", "",
                                          "Last update"))

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = baseline_proj_header,
      startCol = start_col_baseline, startRow = start_row_subH, colNames = TRUE
    )

    ### Baseline and proj years sub-header
    sentence_v <- stringr::str_to_title(value)
    sentence_v_syear <- glue::glue("{sentence_v} {start_year}")
    sentence_v_eyear <- glue::glue("{sentence_v} {max(end_year)}")
    paired_list_sentences <- unlist(lapply(sentence_v_syear, function(a) lapply(sentence_v_eyear, function (b) c(a, b))), recursive=FALSE)
    paired_list_sentences <- vec2emptyDF(unlist(paired_list_sentences[seq(1, length(paired_list_sentences), n_value+1)]))

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = paired_list_sentences,
      startCol = start_col_baseline, startRow = start_row_subH_low, colNames = TRUE
    )

    start_col_baseline_trans <- start_col_baseline + length(paired_list_sentences)

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = paired_list_sentences,
      startCol = start_col_baseline_trans, startRow = start_row_subH_low, colNames = TRUE
    )

    start_col_baseline_type <- start_col_baseline_trans +  length(paired_list_sentences)

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = vec2emptyDF(as.character(rep(c(start_year, max(end_year)),2))),
      startCol = start_col_baseline_type, startRow = start_row_subH_low, colNames = TRUE
    )

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Last update",
      startCol = start_col_baseline_type + 4, startRow = start_row_subH_low
    )

    ## Billion contribution header

    start_col_contrib <- start_col_baseline_type + 5

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Contribution to the Billion",
      startCol = start_col_contrib, startRow = start_row, colNames = FALSE
    )

    ### Billion contribution sub-header
    contrib_headers <- c(
      glue::glue(
        "Change in Transformed {sentence_v} over {start_year}-{max(end_year)} (%)"
      ),
      glue::glue("UN Population {max(end_year)}"),
      glue::glue("Contribution {sentence_v} {max(end_year)}"),
      glue::glue("Contribution {sentence_v} {max(end_year)} (% Total Population)")
    ) %>% vec2emptyDF()

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = contrib_headers,
      startCol = start_col_contrib, startRow = start_row_subH, colNames = TRUE
    )

    ## Latest reported header
    start_col_latest <- start_col_contrib + length(contrib_headers)

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Latest Reported/Estimated Data Available",
      startCol = start_col_latest, startRow = start_row, colNames = FALSE
    )

    latest_rep_headers <-c(
      glue::glue("Raw {sentence_v}"),
      glue::glue("Transformed {sentence_v}"),
      "Year", "Type", "Source",
      "Number of values reported/estimated (since 2000)",
      "Number of values reported/estimated (since 2012)") %>%
      vec2emptyDF()

    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = latest_rep_headers,
      startCol = start_col_latest,
      startRow = start_row_subH,
      colNames = TRUE
    )

    # Merge header cells
    openxlsx::removeCellMerge(wb, sheet = sheet_name,
                              cols = start_col:ncol(df),
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
    for(i in start_col_contrib:ncol(df)){
      openxlsx::mergeCells(
        wb, sheet = sheet_name,
        cols = i,
        rows = start_row_subH:start_row_subH_low
      )
    }
    ## Latest
    openxlsx::mergeCells(
      wb, sheet = sheet_name,
      cols = start_col_latest:(ncol(df)),
      rows = start_row
    )

    # Write df
    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = df,
      startCol = start_col,
      startRow = start_row_data,
      colNames = FALSE
    )

    cols_headers <- c(start_col:(start_col_baseline-1),
                      start_col_baseline: (start_col_contrib-1),
                      start_col_contrib:(start_col_latest-1),
                      start_col_latest:ncol(df)
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
               rows = c(start_row_data:(start_row_data + nrow(df)-1)),
               cols = cols_headers)

    openxlsx::setColWidths(
      wb,
      sheet = sheet_name,
      cols = start_col:ncol(df),
      widths = get_col_width(df, value, transform_value, type_col, source_col,
                             start_year, end_year),
      ignoreMergedCells = FALSE
    )

    return(wb)
  }

#' Write the HPOP billion contribution box
#'
#' @inherit write_main_df

write_hpop_billion_contrib <- function(df,
                                       wb,
                                       value,
                                       start_row,
                                       start_col,
                                       start_year,
                                       end_year,
                                       sheet_name){
  #Headers and rows names
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = data.frame(
                        c1 = c("Contribution to Billion", "(All indicators)"),
                        c2 = c(NA, NA),
                        c3 = c("Corrected for Double Counting", NA)
                      ),
                      startCol = start_col, startRow = start_row, colNames = FALSE
  )
  ## Get nice looking labels
  labels <- lapply(stringr::str_to_sentence(value), function(x){
      c(paste0(x, " corrected"), paste0(x, " not corrected"))
  }) %>% unlist()
  ## Write labels
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = vec2emptyDF(labels),
                      startCol = start_col + 2,
                      startRow = start_row + 1,
                      colNames = TRUE)
  ## Write rows names
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = data.frame(c1 = c(
                        "Newly healthier lives",
                        "Newly unhealthier lives",
                        "Contribution (population)",
                        "% with healthier lives"
                      )),
                      startCol = start_col, startRow = start_row+2, colNames = FALSE
  )
  # Write data
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = df,
                      startCol = start_col+2, startRow = start_row+2, colNames = FALSE
  )
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

#' Write notes for data sheet
#'
#' @inherit write_main_df params return
#' @param end_col integer indicating where the columns should stop being merged

write_notes_data <- function(df,
                             wb,
                             start_row,
                             start_col,
                             end_col,
                             sheet_name){

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = df,
                      startCol = start_col,
                      startRow = start_row,
                      headerStyle = excel_styles()$bold)

  for(i in seq(nrow(df)-1)){
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

#' Write and style sheet header
#' @inherit write_notes_data
#' @inherit export_hpop_country_summary_xls
#' @param billion_long character Long name of the billion to be written
write_sheet_header <- function(wb, sheet_name, billion_long, iso, start_col, start_row){
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = glue::glue("Country contribution to GPW13 {billion_long} billion target"),
                      startCol = start_col, startRow = start_row, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  openxlsx::writeData(wb,sheet = sheet_name, x = country_name,
                      startCol = start_col, startRow = start_row + 2)

  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$title,
                     rows = start_row,
                     cols = start_col
  )
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$sub_title,
                     rows = start_row,
                     cols = start_col, gridExpand = TRUE
  )

  return(wb)

}
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

#' Write times series sheet
#'
#' @inherit write_main_df
#' @param df_ind data frame containing information on indicators
#' @inherit export_hpop_country_summary_xls
#'

write_timeseries_sheet <- function(df, wb, sheet_name,
                                   start_row, start_col, transform_value,
                                   df_ind, ind){

  # timesseries_styles <- timeseries_style(wb, iso, df, sheet_name)

  # openxlsx::writeData(wb,
  #                     sheet = sheet_name, x = "Values are in bold if reported; normal if estimated; and red if imputed/projected",
  #                     startCol = 2, startRow = 5 + nrow(df) + 1
  # )

  for(i in seq_along(transform_value)){
    this_df <- df_ind %>%
      dplyr::left_join(df[[transform_value[i]]], by = ind) %>%
      dplyr::select(-sym("transformed_value"), -sym("unit_transformed"), -ind)

    if(i > 1){
      nrows_sofar <- sum(unlist(lapply(1:(i-1), function(x)nrow(df[[x]])+2)))
      start_row_new <-  start_row + nrows_sofar + (2*(i-1))
    }else{
      start_row_new <- start_row
    }

    openxlsx::writeData(wb, sheet = sheet_name, x = "Transformed indicator",
                        startCol = start_col, startRow = start_row_new
                        )
    openxlsx::mergeCells(wb, sheet = sheet_name,
                         cols = start_col, rows = start_row_new:(start_row_new+1))
    openxlsx::writeData(wb, sheet = sheet_name,
                        x = vec2emptyDF(glue::glue("Time series: {transform_value[i]}")),
                        startCol = start_col+1, startRow = start_row_new,
                        colNames = TRUE
    )
    openxlsx::mergeCells(wb, sheet = sheet_name,
                         cols = (start_col+1):(ncol(this_df)+1), rows = start_row_new)

    years_list <- names(this_df)[2:ncol(this_df)]
    openxlsx::writeData(wb, sheet = sheet_name,
                        x = vec2emptyDF(years_list),
                        startCol = start_col+1, startRow = start_row_new+1,
                        colNames = TRUE
    )
    openxlsx::writeData(wb,
                        sheet = sheet_name, x = this_df,
                        startCol = start_col, startRow = start_row_new + 2,
                        colNames = FALSE
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$dark_blue_header,
                       rows =start_row_new, cols = c(start_col:(ncol(this_df)+1))
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$dark_blue_header,
                       rows = start_row_new:(start_row_new+1), cols = start_col
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$light_blue_header,
                       rows = c((start_row_new+1)), cols = c((start_col+1):(ncol(this_df)+1)),
                       gridExpand = TRUE
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$normal_data_wrapped_dec,
                       rows = c((start_row_new+2):(start_row_new+nrow(this_df)+1)), cols = c(start_col:(ncol(this_df)+1)),
                       gridExpand = TRUE
    )
  }
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = start_col,
    widths = "auto",
    ignoreMergedCells = FALSE
  )
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = (start_col+1):(ncol(this_df)+1),
    widths = 6,
    ignoreMergedCells = FALSE
  )

  return(wb)
}
