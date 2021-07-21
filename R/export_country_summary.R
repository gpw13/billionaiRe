#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#'
#' @inheritParams summarize_hpop_country_data
#' @inheritParams transform_hpop_data
#' @param output_fldr Folder path to where the Excel files should be written
#' @param xls_template Path to the Excel template to be used.
#' @param sheet_prefix Character prefix to add in front of export sheets
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
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       value = "value",
                                       transform_value = "transform_value",
                                       type_col = "type",
                                       source_col = "source",
                                       ind_ids = billion_ind_codes("hpop"),
                                       start_year = 2018,
                                       end_year = 2019:2023,
                                       sheet_prefix = "HPOP",
                                       output_fldr = "outputs",
                                       xls_template = "data-raw/CountrySummary_template.xlsx") {
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)
  billion <- rlang::arg_match(billion)

  if (billion == "hep") {
    export_hep_country_summary_xls(df, {{ iso }}, {{ output_fldr }}, {{ xls_template }})
  }
  if (billion == "hpop") {
    export_hpop_country_summary_xls(df, {{ iso }}, {{ output_fldr }}, {{ xls_template }})
  }
  if (billion == "uhc") {
    export_uhc_country_summary_xls(df, {{ iso }}, {{ output_fldr }}, {{ xls_template }})
  }
  if (billion == "all") {
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
                                           end_year = 2019:2023){
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)

}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#' @param list_df list object with dataframes to be used by function. Typically
#' comes from `summarize_hpop_country_data`. Names of the data frames should follow
#' the
#' @inherit export_country_summary_xls return details params
#'
#' @export
#'
export_hpop_country_summary_xls <- function(list_df,
                                            iso,
                                            iso3 = "iso3",
                                            year = "year",
                                            ind = "ind",
                                            value = "value",
                                            transform_value = "transform_value",
                                            type_col = "type",
                                            source_col = "source",
                                            start_year = 2018,
                                            end_year = 2019:2023,
                                            sheet_prefix = "HPOP",
                                            output_fldr = "outputs",
                                            xls_template = "data-raw/CountrySummary_template.xlsx") {
  assert_summarize(list_df)
  assert_years(start_year, end_year)
  wppdistro:::assert_iso3(iso)

  data_sheet <- glue::glue("{sheet_prefix}data")

  final_table <- list_df$ind_df %>%
    dplyr::left_join(list_df$baseline_proj, by = "ind") %>%
    dplyr::left_join(list_df$hpop_contrib, by = "ind") %>%
    dplyr::left_join(list_df$latest_reported, by = "ind") %>%
    dplyr::select(-.data[[ind]], -.data[[iso3]])


  # load workbook
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, sheetName = data_sheet)

  # White background
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:(8 + nrow(final_table) + 6+5)),
                     cols = c(1:(ncol(final_table)+3)), gridExpand = TRUE
  )

  # Write title
  openxlsx::writeData(wb,
                      sheet = data_sheet,
                      x = "Country contribution to GPW13 Healthier Populations billion target",
                      startCol = 1, startRow = 2, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  openxlsx::writeData(wb,sheet = data_sheet, x = country_name,
                      startCol = 1, startRow = 4)

  # Write tables

  final_table <- list_df$ind_df %>%
    dplyr::left_join(list_df$baseline_proj, by = "ind") %>%
    dplyr::left_join(list_df$hpop_contrib, by = "ind") %>%
    dplyr::left_join(list_df$latest_reported, by = "ind") %>%
    dplyr::select(-.data[[ind]], -.data[[iso3]])

  wb <- write_main_df(final_table, wb,
                start_row = 6, start_col = 1, start_year = start_year,
                end_year = end_year, sheet_name = data_sheet, value = value,
                transform_value = transform_value, type_col = type_col,
                source_col = source_col)

  end_main_table <- 8+nrow(final_table)

  wb <- write_hpop_billion_contrib(dplyr::select(list_df$hpop_billion, -.data[[ind]]),
                             wb, value,
                             start_row =end_main_table+2 , start_col = 7,
                             start_year = start_year, end_year = end_year,
                             sheet_name = data_sheet)

  notes <- data.frame(`Notes:` = c(
    "Values might be slightly different than dashboard values because of rounding.",
    "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
    "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations"
  ))

  wb <- write_notes_data(notes, wb,
                         sheet_name = data_sheet,
                         start_row = end_main_table + 2,
                         start_col = 1,
                         end_col = 5)

  # HERE HERE HERE
  # indicator list
  nice_indicator_list <- dplyr::select(list_df$ind_df, "ind") %>%
    dplyr::left_join(billionaiRe::indicator_df, by = c("ind")) %>%
    dplyr::select("sdg", "short_name", "medium_name", "unit_raw", "transformed_name", "unit_transformed") %>%
    dplyr::rename(
      "Indicator code" = "sdg",
      "Short Name" = "short_name",
      "Name" = "medium_name",
      "Unit pre-tranformation" = "unit_raw",
      "Transformed name" = "transformed_name",
      "Transformed unit" = "unit_transformed"
    )

  openxlsx::writeData(wb,
                      sheet = "HPOPIndicator List", x = nice_indicator_list,
                      startCol = 2, startRow = 4
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPIndicator List", style = excel_styles()$dark_blue_header,
                     rows = 4, cols = c(2:7)
  )

  # Inter update
  openxlsx::writeData(wb,
                      sheet = "HPOPInter", x = c(start_year, end_year),
                      startCol = 3, startRow = 2, colNames = TRUE
  )

  # Time series

  timesseries_df <- list_df$df_iso_pop %>%
    tidyr::pivot_wider(
      id_cols = c(
        iso,
        ind
      ),
      names_from = year,
      values_from = type_col
    ) %>%
    dplyr::rename_with(
      ~ paste0("year_", .x),
      dplyr::matches("[0-9]{4}")
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::select(-ind)

  timesseries_styles <- timeseries_style(wb, iso, timesseries_df, "HPOPTime Series")
  openxlsx::writeData(wb,
                      sheet = "HPOPTime Series", x = "Values are in bold if reported; normal if estimated; and red if imputed/projected",
                      startCol = 2, startRow = 5 + nrow(list_df$transformed_time_series) + 1
  )

  openxlsx::writeData(wb,
                      sheet = "HPOPTime Series", x = list_df$transformed_time_series,
                      startCol = 2, startRow = 5, headerStyle = excel_styles()$light_blue_header
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPTime Series", style = excel_styles()$dark_blue_header,
                     rows = 4, cols = 3:26
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPTime Series", style = excel_styles()$dark_blue_header,
                     rows = 4:5, cols = 2
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPTime Series", style = excel_styles()$normal_data,
                     rows = c(6:(nrow(list_df$transformed_time_series) + 5 + 1)), cols = c(2),
                     gridExpand = TRUE
  )

  # Chart
  openxlsx::addStyle(wb,
                     sheet = "HPOPChart", style = excel_styles()$vertical_txt,
                     rows = 22, cols = 3:19
  )


  # Write workbook
  last_update <- list_df$latest_reported

  if (!dir.exists(output_fldr)) {
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
                                           xls_template = "data-raw/CountrySummary_template.xlsx") {
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
                                           xls_template = "data-raw/CountrySummary_template.xlsx") {
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)
}
