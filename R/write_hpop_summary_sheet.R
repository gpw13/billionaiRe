#' Write HPOP summary sheet
#'
#' `write_hpop_summary_sheet()` writes all the content and styling for the HPOP
#'  summary sheet. Used within `export_hpop_country_summary_xls()`
#'
#' @inherit export_country_summary_xls
#' @inherit write_baseline_projection_hpop_summary
#' @param df Data frame in long format filtered for a specific country, where 1 row corresponds
#'    to a specific year, and indicator.
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
                                 transform_value = "transform_value",
                                 type_col = "type",
                                 source_col = "source",
                                 contribution = "contribution",
                                 contribution_pct = paste0(contribution, "_percent"),
                                 contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                 ind_df
                                 ){

  indicators <- ind_df %>%
    dplyr::select("ind","sdg", "short_name")

  boxes_bounds <- list(indicators = c(start_col = 1,
                                      end_col = 2,
                                      start_row = 9,
                                      end_row = 9+2+nrow(indicators)),
                       latest = c(start_col = 3,
                                  end_col = 2+5+(2*length(value)),
                                  start_row = 9,
                                  end_row = 9+2+nrow(indicators)),
                       baseline_proj = c(start_col = 2+5+(2*length(value))+2,
                                         end_col = 2+5+(2*length(value))+1+(3*length(value))*2+5,
                                         start_row = 9,
                                         end_row = 9+2+nrow(indicators)),
                       contribution = c(start_col = 2+5+(2*length(value))+1+(3*length(value))*2+5+2,
                                        end_col = 2+5+(2*length(value))+1+(3*length(value))*2+5+1+(length(value)*3)+1,
                                        start_row = 9,
                                        end_row = 9+2+nrow(indicators)),
                       billion_contribution = c(start_col = 2+5+(2*length(value))+1+(3*length(value))*2+5+2,
                                                end_col = 2+5+(2*length(value))+1+(3*length(value))*2+5+2 + 3,
                                                start_row = 9+2+nrow(indicators)+2,
                                                end_row = 9+2+nrow(indicators)+1+6),
                       notes = c(start_col = 1,
                                 end_col = 5,
                                 start_row = 9+2+nrow(indicators)+2,
                                 end_row = 9+2+nrow(indicators)+1+5)
  )


  ## Write header
  wb <- write_sheet_header_hpop_summary(wb, sheet_name = sheet_name,
                           iso, start_col = 1, start_row = 2, end_year = end_year, boxes_bounds = boxes_bounds)

  wb <- write_indicators_hpop_summary(ind_df,
                                      wb,
                                      sheet_name,
                                      bounds = boxes_bounds$indicators)


  wb <- write_latest_reported_hpop_summary(df = df,
                                   wb = wb,
                                   ind_df = ind_df,
                                   sheet_name = sheet_name,
                                   type_col = type_col,
                                   iso3 = iso3,
                                   ind = ind,
                                   year = year,
                                   value = value,
                                   transform_value = transform_value,
                                   source_col = source_col,
                                   year_counts = c(2000,2015),
                                   bounds = boxes_bounds$latest)

  wb <- write_baseline_projection_hpop_summary(df = df,
                                 wb = wb,
                                 sheet_name = sheet_name,
                                 ind_df = ind_df,
                                 year = year,
                                 start_year = start_year,
                                 end_year = end_year,
                                 ind = ind,
                                 value = value,
                                 transform_value = transform_value,
                                 type_col = type_col,
                                 source_col = source_col,
                                 iso3 = iso3,
                                 bounds = boxes_bounds$baseline_proj
                                 )

  wb <- write_bilion_contrib_ind_hpop_summary(df = df,
                                      wb = wb,
                                      sheet_name = sheet_name,
                                      year = year,
                                      start_year = start_year,
                                      end_year = end_year,
                                      ind = ind,
                                      contribution_pct = contribution_pct,
                                      population = population,
                                      contribution = contribution,
                                      contribution_pct_pop_total = contribution_pct_pop_total,
                                      ind_df = ind_df,
                                      bounds = boxes_bounds$contribution)


  wb <- write_billion_contribution_hpop_summary(df = df,
                                        wb = wb,
                                        sheet_name = sheet_name,
                                        contribution = contribution,
                                        contribution_pct = contribution_pct,
                                        ind = ind,
                                        year = year,
                                        end_year = end_year,
                                        bounds = boxes_bounds$billion_contribution,
                                        iso = iso,
                                        boxes_bounds = boxes_bounds)
  # Write notes
  notes <- data.frame(notes =c(
    "Values might be slightly different than dashboard values because of rounding.",
    'For more information, please refer to the GPW13 dashboard, section "Reference", which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:',
     "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations",
    "* Values are in bold if there is more than one data source since 2015"
  ))

  wb <- write_notes(notes, wb,
                    notes_title = "Notes:",
                    sheet_name = sheet_name,
                    bounds = boxes_bounds$notes
                    )

  # Inter sheet (data for Chart)
  wb <- write_hpop_inter(wb, sheet_name = "HPOP_Inter", data_sheet_name = sheet_name,
                         ind_df, start_year, end_year, start_col = 1, start_row = 2,
                         transform_value, summary_bounds = boxes_bounds)
  return(wb)
}

#' Write and style sheet header
#' @inherit write_baseline_projection_hpop_summary
#' @inherit export_hpop_country_summary_xls
#' @inherit style_header_hpop_summary_sheet
#' @param boxes_bounds named list of bounds for data frame boxes to be written in sheet.

write_sheet_header_hpop_summary <- function(wb, sheet_name, iso, start_col, start_row, end_year,value, boxes_bounds){
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = glue::glue("Country contribution to GPW13 Healthier Population billion"),
                      startCol = start_col, startRow = start_row, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  country_pop_end_year <- wppdistro::get_population(iso, year = max(end_year))
  openxlsx::writeData(wb,sheet = sheet_name, x = country_name,
                      startCol = start_col, startRow = start_row + 2)

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = c(glue::glue("Projected number of newly healthier lives by {max(end_year)}"),
                            glue::glue("% of country population projected to be newly healthier by {max(end_year)}"),
                            glue::glue("{country_name} population in {max(end_year)} (Source: World Population Prospects)")),
                      startCol = start_col, startRow = start_row + 3)

  openxlsx::writeFormula(wb, sheet = sheet_name,
                         x = c(glue::glue("={openxlsx::int2col(boxes_bounds$contribution['start_col']+2)}{boxes_bounds$billion_contribution['end_row']-1}/1000"),
                               glue::glue("={openxlsx::int2col(boxes_bounds$contribution['start_col']+2)}{boxes_bounds$billion_contribution['end_row']}"),
                               glue::glue("={country_pop_end_year}/1000000")),
                         startRow = start_row + 3,
                         startCol = start_col + 4
  )


  wb <- style_header_hpop_summary_sheet(wb, sheet_name, start_row = start_row, start_col = start_col)

  return(wb)

}
