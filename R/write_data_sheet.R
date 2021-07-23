#' Write data sheet
#'
#'
write_data_sheet_HPOP <- function(df, wb, sheet_name, start_year, end_year, value,year,
                             iso3,iso,ind,population,scenario,ind_ids,
                             transform_value, type_col, source_col,
                             contribution){
  # Get main data frame for data sheet
  main_df <- summarize_hpop_data(df, year = year, iso3, ind, value, transform_value,
                                 population, scenario, type_col, source_col,
                                 ind_ids, start_year,end_year) %>%
    dplyr::select(-.data[[iso3]])


  # Set white background
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$white_bckgrd,
                     rows = c(1:(8 + nrow_main_df + 6+5)),
                     cols = c(1:(nrow_main_df+3)), gridExpand = TRUE
  )

  ## Write header
  wb <- write_sheet_header(wb, sheet_name = sheet_name,
                           billion_long = "Healthier Population",
                           iso, start_col = 1, start_row = 2)

  # Write main table

  wb <- write_main_df(main_df, wb,
                      start_row = 6, start_col = 1, start_year = start_year,
                      end_year = end_year, sheet_name = sheet_name, value = value,
                      transform_value = transform_value, type_col = type_col,
                      source_col = source_col)

  end_main_table <- 8+nrow(main_df)

  df_hpop_contrib <- summarize_hpop_billion_contribution(df,year = year,
                                                         end_year = end_year,ind = ind,
                                                         contribution = contribution)


  wb <- write_hpop_billion_contrib(dplyr::select(df_hpop_contrib, -.data[[ind]]),
                                   wb, value,
                                   start_row =end_main_table+2 , start_col = 7,
                                   start_year = start_year, end_year = end_year,
                                   sheet_name = sheet_name)
  # Write notes
  notes <- data.frame(`Notes:` = c(
    "Values might be slightly different than dashboard values because of rounding.",
    "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
    "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations"
  ))

  wb <- write_notes_data(notes, wb,
                         sheet_name = sheet_name,
                         start_row = end_main_table + 2,
                         start_col = 1,
                         end_col = 5)

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
