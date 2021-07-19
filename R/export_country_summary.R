#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#'
#' @inheritParams summarise_HPOP_cntry_data
#' @param billion Billion to export to return, either "hep", "hpop", "uhc" or "all".
#' @param output_fldr Folder path to where the Excel files should be written
#' @param xls_template Path to the Excel template to be used.
#'
#' @return `openxslx` Workbook object. Output file is in `output_fldr`.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- load_billion_data("hpop")
#' export_country_summary_xls(data, iso = "AFG")
#' }
export_country_summary_xls <- function(df,
                                       iso,
                                       start_year = 2018,
                                       end_year = 2019:2023,
                                       billion = c("hep", "hpop", "uhc", "all"),
                                       output_fldr = "outputs",
                                       xls_template = "data-raw/CountrySummary_template.xlsx",
                                       ...){
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)
  billion <- rlang::arg_match(billion)

  if(billion == "hep"){
    export_hep_country_summary_xls(df, {{iso}}, {{output_fldr}}, {{xls_template}})
  }
  if(billion == "hpop"){
    export_hpop_country_summary_xls(df, {{iso}}, {{output_fldr}}, {{xls_template}})
  }
  if(billion == "uhc"){
    export_uhc_country_summary_xls(df, {{iso}}, {{output_fldr}}, {{xls_template}})
  }
  if(billion == "all"){
    export_all_country_summary_xls(df, iso)
  }
}

#' Export country summary to Excel for HEP billion$
#'
#' `export_hep_country_summary_xls` Export a country-specific for HEP billion.
#'
#' @inherit export_country_summary_xls return details params
#' @export
#'
export_hep_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           ...){
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)

}

write_main_df <- function(df, wb, start_row, start_col,start_year, end_year, sheet_name){

}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#'
#' @inherit export_country_summary_xls return details params
#'
#' @export
#'
export_hpop_country_summary_xls <- function(df,
                                            iso,
                                            start_year = 2018,
                                            end_year = 2019:2023,
                                            output_fldr = "outputs",
                                            xls_template = "data-raw/CountrySummary_template.xlsx",
                                            ...){
  assert_mart_columns(df)

  #get data frame
  data_sheet_df <- summarise_HPOP_cntry_data(df, iso, start_year = start_year,
                                              end_year = end_year,...)
  #load workbook
  wb <- openxlsx::loadWorkbook(xls_template)
  #Write title
  openxlsx::writeData(wb, sheet = "HPOPdata",
                      x = "Country contribution to GPW13 Healthier Populations billion target",
                      startCol = 1, startRow = 2, colNames = FALSE)

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  openxlsx::writeData(wb, sheet = "HPOPdata", x = country_name,
                      startCol = 1, startRow = 4)

  #Write column headers
  openxlsx::writeData(wb, sheet = "HPOPdata",
                      x = glue::glue("{start_year} Baseline, and {end_year} Projection"),
                      startCol = 4, startRow = 6, colNames = FALSE)


  years_header <- c(c(start_year, end_year), "", rep(c(start_year, end_year),3))
  years_df <- data.frame(matrix(ncol = length(years_header), nrow = 0))
  names(years_df) <- years_header
  openxlsx::writeData(wb, sheet = "HPOPdata", x = years_df,
                      startCol = 4, startRow = 8,colNames = TRUE)

  contrib_headers <- c(
    glue::glue("Change in Transformed Values over {start_year}-{end_year} (%)"),
    glue::glue("UN Population {end_year}"),
    glue::glue("Contribution {end_year}"),
    glue::glue("Contribution {end_year} (% Total Population)")
  )
  contrib_headers_df <- data.frame(matrix(ncol = length(contrib_headers), nrow = 0))
  names(contrib_headers_df) <- contrib_headers

  openxlsx::writeData(wb, sheet = "HPOPdata", x = contrib_headers_df,
                      startCol = 14, startRow = 7,colNames = TRUE)

  # Write tables
  empty_column <- data.frame(empty = rep(NA, nrow(data_sheet_df$ind_df)))

  final_table <- data_sheet_df$ind_df %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$df_iso_raw, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$baseline_proj, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$hpop_contrib, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$latest_reported, by = "ind") %>%
    dplyr::select(-ind)


  openxlsx::writeData(wb, sheet = "HPOPdata", x = final_table,
                      startCol = 1, startRow = 9,colNames = FALSE)

  openxlsx::writeData(wb, sheet = "HPOPdata", x = dplyr::select(data_sheet_df$hpop_billion,contribution),
                      startCol = 17, startRow = 9+nrow(final_table)+3, colNames = FALSE)
  openxlsx::writeFormula(wb, sheet = "HPOPdata",
                         glue::glue("=(P{9+nrow(final_table)+5}/{wppdistro::get_population('iso3', year = end_year)})"),
                         startCol = 16,startRow = 9+nrow(final_table)+6)

  notes <- data.frame(`Notes:` =c(
    "values might be slightly different than dashboard values because of rounding.",
    "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
      "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations"
  ))

  openxlsx::writeData(wb, sheet = "HPOPdata", x = notes,
            startCol = 1, startRow = 9+nrow(final_table)+1, colNames = TRUE,
            headerStyle = excel_styles()$bold)


  openxlsx::writeData(wb, sheet = "HPOPdata", x = data.frame(c1 =c("Contribution to Billion", "(All indicators)"),
                                                             c2 = c(NA, NA),
                                                             c3 = c("Corrected for Double Counting", "No"),
                                                             c4 = c(NA, "Yes")),
                      startCol = 14, startRow = 9+nrow(final_table)+1, colNames = FALSE)

  openxlsx::writeData(wb, sheet = "HPOPdata", x = data.frame(c1=c(
      "Newly healthier lives",
      "Newly unhealthier lives",
      "Contribution",
      "% Population with healthier lives"
    )),
    startCol = 14, startRow = 8+nrow(final_table)+4, colNames = FALSE)


  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9+nrow(final_table)+1)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(16:17), rows = 9+nrow(final_table)+1)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9+nrow(final_table)+2)


  openxlsx::writeFormula(wb, sheet = "HPOPdata",
                         x =glue::glue('=SUMIF(P9:P{8+nrow(final_table)},">0")'),
                         startCol = 16, startRow = 8+nrow(final_table)+4)

  openxlsx::writeFormula(wb, sheet = "HPOPdata",
                         x = glue::glue('=SUMIF(P9:P{8+nrow(final_table)},"<0")'),
                         startCol = 16, startRow = 8+nrow(final_table)+5)

  openxlsx::writeFormula(wb, sheet = "HPOPdata",
                         x = glue::glue('=P{8+nrow(final_table)+4}+P{8+nrow(final_table)+5}'),
                         startCol = 16, startRow = 8+nrow(final_table)+6)

  # Style output
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:((9+nrow(final_table)+1+12))),
                     cols = c(1:27), gridExpand = TRUE)


  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$title,
                     rows = c(2), cols = c(1))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$sub_title,
           rows = c(4), cols = c(1))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$dark_blue_header,
                     rows = 6, cols = c(1:2, 4:12, 14:17, 19:25))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$light_blue_header,
                     rows = 7, cols = c(1:2, 4:5, 7:12, 14:17, 19:25))
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$light_blue_header,
                     rows = 8, cols = c(1:2, 4:5, 7:12, 14:17, 19:25))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$dark_blue_header,
                     rows = c(9+nrow(final_table)+1), cols = c(14:17))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$dark_blue_header,
                     rows = c(9+nrow(final_table)+2), cols = c(14:15))
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$light_blue_header,
                     rows = c(9+nrow(final_table)+2), cols = c(16:17))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = openxlsx::createStyle(numFmt = "PERCENTAGE",textDecoration = "bold"),
           rows = 9+nrow(final_table)+6, cols = c(16:17))
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$bold,
                     rows = 9+nrow(final_table)+6, cols = c(14:15))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$normal_data_wrapped,
           rows = c(9:(8+nrow(final_table))), cols = c(1:25), gridExpand = TRUE)

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$normal_data,
           rows = c((9+nrow(final_table)+1+1):(9+nrow(final_table)+1+1+nrow(notes))), cols = c(1))

  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:5),
                     cols = c(2:26), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:(9+nrow(final_table)+1+12)),
                     cols = c(26:27), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c((9+nrow(final_table)+1+7):(9+nrow(final_table)+1+12)),
                     cols = c(1:27), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(9+nrow(final_table)),
                     cols = c(1:27))
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c((9+nrow(final_table)):(9+nrow(final_table)+1+12)),
                     cols = c(2:13), gridExpand = T)
  openxlsx::addStyle(wb, sheet = "HPOPdata", style = excel_styles()$normal_data,
                     rows = c(( 8+nrow(final_table)+4):( 8+nrow(final_table)+6)), cols = c(14:15),
                     gridExpand = TRUE)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9+nrow(final_table)+4)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9+nrow(final_table)+5)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9+nrow(final_table)+6)

  #indicator list
  nice_indicator_list <- dplyr::select(data_sheet_df$ind_df,ind) %>%
    dplyr::left_join(indicator_order, by = c("ind")) %>%
    dplyr::select(sdg, short_name, medium_name, unit_raw, transformed_name, unit_transformed) %>%
    dplyr::rename("Indicator code" = sdg,
           "Short Name" = short_name,
           "Name" = medium_name,
           "Unit pre-tranformation" = unit_raw,
           "Transformed name" = transformed_name,
           "Transformed unit" = unit_transformed
    )

  openxlsx::writeData(wb, sheet = "HPOPIndicator List", x = nice_indicator_list,
            startCol = 2, startRow = 4)

  openxlsx::addStyle(wb, sheet = "HPOPIndicator List", style = excel_styles()$dark_blue_header,
           rows = 4, cols = c(2:7))

  #Inter update
  openxlsx::writeData(wb, sheet = "HPOPInter", x = years_df[1:2],
            startCol = 3, startRow = 2,colNames = TRUE)

  #Time series

  timesseries_df <- data_sheet_df$df_iso_pop %>%
    tidyr::pivot_wider(
      id_cols = c(iso,
                  ind),
      names_from = year,
      values_from = type
    ) %>%
    dplyr::rename_with( ~ paste0("year_", .x),
                        tidyselect::matches("[0-9]{4}")) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::select(-ind)

  timesseries_styles <- timeseries_style(wb, iso, timesseries_df, "HPOPTime Series")
  openxlsx::writeData(wb, sheet = "HPOPTime Series", x = "Values are in bold if reported; normal if estimated; and red if imputed/projected",
                      startCol = 2, startRow = 5+nrow(data_sheet_df$transformed_time_series)+1)

  openxlsx::writeData(wb, sheet = "HPOPTime Series", x = data_sheet_df$transformed_time_series,
            startCol = 2, startRow = 5, headerStyle = excel_styles()$light_blue_header)
  openxlsx::addStyle(wb, sheet = "HPOPTime Series", style = excel_styles()$dark_blue_header,
           rows = 4, cols = 3:26)
  openxlsx::addStyle(wb, sheet = "HPOPTime Series", style = excel_styles()$dark_blue_header,
           rows = 4:5, cols = 2)
  openxlsx::addStyle(wb, sheet = "HPOPTime Series", style = excel_styles()$normal_data,
           rows = c(6:(nrow(data_sheet_df$transformed_time_series)+5+1)), cols = c(2),
           gridExpand = TRUE)

  #Chart
  openxlsx::addStyle(wb, sheet = "HPOPChart", style = excel_styles()$vertical_txt,
                     rows = 22, cols = 3:19)


  #Write workbook
  last_update <-data_sheet_df$latest_reported

  if(!dir.exists(output_fldr)){
    dir.create(output_fldr)
  }

  openxlsx::saveWorkbook(wb, glue::glue("{output_fldr}/GPW13_HPOP_billion_{iso}_CountrySummary_{lubridate::month(lubridate::today(), TRUE)}{lubridate::year(lubridate::today())}.xlsx"), overwrite = TRUE)

  return(wb)
}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_country_summary_xls return details params
#'
#' @export
#'
export_uhc_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           output_fldr = "outputs",
                                           xls_template = "data-raw/CountrySummary_template.xlsx",
                                           ...){
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)

}

#' Export country summary to Excel for all billions.
#' `export_all_country_summary_xls` Export a country-specific for all billions.
#'
#' @inherit export_country_summary_xls return details params
#'
#' @export
#'
export_all_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           output_fldr = "outputs",
                                           xls_template = "data-raw/CountrySummary_template.xlsx",
                                           ...){
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)

}
