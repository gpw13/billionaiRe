#' Write data sheet
#'
#' `write_hpop_summary_sheet` write all the content and styling found in the HPOP data sheet
#'
#' @inherit export_country_summary_xls
#' @inherit write_main_df
#' @param ind_df data frame containing the indicators in the correct order and format to be used.
#'
write_hpop_summary_sheet <- function(df, wb, sheet_name,iso,
                                 start_year = 2018,
                                 end_year = 2019:2023,
                                 value = "value",
                                 year = "year",
                                 iso3 = "iso3",
                                 ind = "ind",
                                 population = "population",
                                 scenario = NULL,
                                 ind_ids = billion_ind_codes("hpop"),
                                 transform_value = "transform_value",
                                 type_col = "type",
                                 source_col = "source",
                                 contribution = "contribution",
                                 contribution_pct = paste0(contribution, "_percent"),
                                 contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                 ind_df
                                 ){
  # TODO: Split summarize_hpop_data into even smaller functions for each "module" of the main summary sheet.
  # Get main data frame for data sheet
  # main_df <- summarize_hpop_data(df = df,
  #                                year = year,
  #                                iso3 = iso3,
  #                                ind = ind,
  #                                value = value,
  #                                transform_value = transform_value,
  #                                contribution = contribution,
  #                                contribution_pct = contribution_pct,
  #                                contribution_pct_pop_total = contribution_pct_pop_total,
  #                                population = population,
  #                                scenario = scenario,
  #                                type_col = type_col,
  #                                source_col = source_col,
  #                                ind_ids = ind_ids,
  #                                start_year = start_year,
  #                                end_year = end_year,
  #                                ind_df = ind_df) %>%
  #   dplyr::select(-.data[[iso3]])

  indicators <- ind_df %>%
    dplyr::select("ind","sdg", "short_name")

  ## Write header
  wb <- write_sheet_header_hpop_summary(wb, sheet_name = sheet_name,
                           billion_long = "Healthier Population",
                           iso, start_col = 1, start_row = 2)

  # Write main table
  wb <- write_main_df(main_df, wb,
                      start_row = 6, start_col = 1, start_year = start_year,
                      year = year,
                      end_year = end_year, sheet_name = sheet_name, value = value,
                      transform_value = transform_value, type_col = type_col,
                      source_col = source_col,
                      contribution = contribution,
                      contribution_pct = contribution_pct,
                      contribution_pct_pop_total = contribution_pct_pop_total)

  end_main_table <- 8+nrow(main_df)

  # Get the HPOP billion contribution table
  df_hpop_contrib <- summarize_hpop_billion_contribution(df,year = year,
                                                         end_year = end_year,ind = ind,
                                                         contribution = contribution,
                                                         contribution_pct = contribution_pct)


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

  style_hpop_billion_contribution(df, wb, sheet_name, start_row, start_col)
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
                      startRow = start_row)

  wb <- style_notes_data(df, wb, sheet_name, start_row, start_col, nrow_notes = nrow(df), end_col)

  return(wb)
}

#' Write and style sheet header
#' @inherit write_notes_data
#' @inherit export_hpop_country_summary_xls
#' @param billion_long character Long name of the billion to be written
write_sheet_header_hpop_summary <- function(wb, sheet_name, billion_long, iso, start_col, start_row, end_year,value, boxes_bounds){
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = glue::glue("Country contribution to GPW13 {billion_long} billion"),
                      startCol = start_col, startRow = start_row, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  openxlsx::writeData(wb,sheet = sheet_name, x = country_name,
                      startCol = start_col, startRow = start_row + 2)

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = c(glue::glue("Projected number of newly healthier lives by {max(end_year)}"),
                            glue::glue("% of country population projected to be newly healthier by {max(end_year)}")),
                      startCol = start_col, startRow = start_row + 3)

  openxlsx::writeFormula(wb, sheet = sheet_name,
                         x = c(glue::glue("={openxlsx::int2col(boxes_bounds$contribution['start_col']+2)}{boxes_bounds$billion_contribution['end_row']-1}/1000"),
                               glue::glue("={openxlsx::int2col(boxes_bounds$contribution['start_col']+2)}{boxes_bounds$billion_contribution['end_row']}")),
                         startRow = start_row + 3,
                         startCol = start_col + 5
  )


  wb <- style_header(wb, sheet_name, start_row = start_row, start_col = start_col)

  return(wb)

}
