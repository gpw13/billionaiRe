#' Write UHC summary sheet
#'
#' `write_uhc_summary_sheet()` writes all the content and styling for the UHC
#'  summary sheet. Used within `export_uhc_country_summary_xls()`
#'
#' @inherit export_country_summary_xls
#' @inheritParams write_uhc_summary_sheet
#' @param df Data frame in long format filtered for a specific country, where 1 row corresponds
#'    to a specific year, and indicator.
#' @param ind_df data frame containing the indicators in the correct order and format to be used.
#'
write_uhc_summary_sheet <- function(df, wb, sheet_name,iso,
                                     start_year = 2018,
                                     end_year = 2019:2023,
                                     value = "value",
                                     transform_value = "transform_value",
                                     year = "year",
                                     iso3 = "iso3",
                                     ind = "ind",
                                     population = "population",
                                     type_col = "type",
                                     source_col = "source",
                                     ind_df){

  boxes_bounds <- list(sheet_header = c(start_col = 1,
                                        end_col = 21,
                                        start_row = 2,
                                        end_row = 7),
                       data_header = c(start_col = 1,
                                       end_col = 21,
                                       start_row = 9,
                                       end_row = 11),
                       latest_reported_data = c(start_col = 3,
                                                end_col = 7,
                                                start_row = 9,
                                                end_row = 42),
                       baseline_projection_data = c(
                         start_col = 9,
                         end_col = 21,
                         start_row = 9,
                         end_row = 42
                       ),
                       RMNCH = c(start_col = 1,
                                 end_col = 21,
                                 start_row = 12,
                                 end_row = 17),
                       infec_diseases = c(start_col = 1,
                              end_col = 21,
                              start_row = 18,
                              end_row = 23),
                       ncd = c(start_col = 1,
                               end_col = 21,
                               start_row = 24,
                               end_row = 28),
                       service_cap_access = c(start_col = 1,
                                   end_col = 21,
                                   start_row = 29,
                                   end_row = 35),
                       ASC = c(start_col = 1,
                               end_col = 21,
                               start_row = 36,
                               end_row = 37),
                       UHC_SM = c(start_col = 1,
                                  end_col = 21,
                                  start_row = 38,
                                  end_row = 42),
                       notes = c(start_col = 1,
                                 end_col = 21,
                                 start_row = 44,
                                 end_row = 62)
  )

  wb <- write_sheet_header_uhc_summary(wb, sheet_name, iso, end_year,value, boxes_bounds)

  wb <- write_data_headers_uhc_summary(wb, sheet_name, value, boxes_bounds, start_year, end_year)

  pillars <- c("RMNCH", "infec_diseases", "ncd", "service_cap_access")

  for(i in seq_along(pillars)){
    wb <- write_data_boxes_uhc_summary(df = df,
                                       pillar = pillars[i],
                                       wb = wb,
                                       sheet_name = sheet_name,
                                       value = value,
                                       transform_value = transform_value,
                                       boxes_bounds = boxes_bounds,
                                       start_year = start_year,
                                       end_year = end_year,
                                       ind = ind,
                                       ind_df = ind_df,
                                       year = year,
                                       type_col = type_col,
                                       source_col = source_col,
                                       iso3 = iso3)
  }
  return(wb)
}

#' Write and style UHC summary sheet header
#'
#' @inherit write_sheet_header_hpop_summary

write_sheet_header_uhc_summary <- function(wb, sheet_name, iso, end_year,value, boxes_bounds){
  openxlsx::writeData(wb,
                      sheet = sheet_name,
                      x = glue::glue("Country contribution to GPW13 Universal Health Coverage billion"),
                      startCol = boxes_bounds$sheet_header['start_col'], startRow = boxes_bounds$sheet_header['start_row'], colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  country_pop_end_year <- wppdistro::get_population(iso, year = max(end_year))
  openxlsx::writeData(wb,sheet = sheet_name, x = country_name,
                      startCol = boxes_bounds$sheet_header['start_col'], startRow = boxes_bounds$sheet_header['start_row'] + 2)

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = c(glue::glue("Projected number of persons newly covered by universal healthcare by {max(end_year)}"),
                            glue::glue("% of country population projected to be newly covered by universal healthcare by {max(end_year)}"),
                            glue::glue("{country_name} population in {max(end_year)} (Source: World Population Prospects)")),
                      startCol = boxes_bounds$sheet_header['start_col'], startRow = boxes_bounds$sheet_header['start_row'] + 3)

  openxlsx::writeFormula(wb, sheet = sheet_name,
                         x = c(glue::glue("={openxlsx::int2col(boxes_bounds$UHC_SM['start_col']+12)}{boxes_bounds$UHC_SM['end_row']-1}/1000"),
                               glue::glue("={openxlsx::int2col(boxes_bounds$UHC_SM['start_col']+12)}{boxes_bounds$UHC_SM['end_row']}*100"),
                               glue::glue("={country_pop_end_year}/1000000")),
                         startRow = boxes_bounds$sheet_header['start_row'] + 3,
                         startCol = boxes_bounds$sheet_header['start_col'] + 4
  )


  wb <- style_header_uhc_summary_sheet(wb, sheet_name, boxes_bounds = boxes_bounds)

  return(wb)

}
