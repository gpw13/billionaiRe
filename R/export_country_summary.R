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
  assert_list(list_df$transformed_time_series)

  # load workbook
  wb <- openxlsx::loadWorkbook(xls_template)

  # data sheet
  data_sheet <- glue::glue("{sheet_prefix}_data")
  openxlsx::renameWorksheet(wb, sheet = "data", newName = data_sheet)

  latest_update_ind <- list_df$df_iso %>%
    dplyr::group_by(.data[[ind]]) %>%
    dplyr::select(.data[[ind]], .data[["upload_date"]]) %>%
    dplyr::filter(.data[["upload_date"]] == max(.data[["upload_date"]])) %>%
    dplyr::distinct()

  main_df <- list_df$ind_df %>%
    dplyr::left_join(list_df$baseline_proj, by = ind) %>%
    dplyr::left_join(latest_update_ind, by = ind) %>%
    dplyr::left_join(list_df$hpop_contrib, by = ind) %>%
    dplyr::left_join(list_df$latest_reported, by = ind) %>%
    dplyr::select(-.data[[ind]], -.data[[iso3]])

  ## White background
  openxlsx::addStyle(wb,
                     sheet = data_sheet, style = excel_styles()$white_bckgrd,
                     rows = c(1:(8 + nrow(main_df) + 6+5)),
                     cols = c(1:(ncol(main_df)+3)), gridExpand = TRUE
  )

  ## Write header
  wb <- write_sheet_header(wb, sheet_name = data_sheet,
                           billion_long = "Healthier Population",
                           iso, start_col = 1, start_row = 2)

  # Write main table

  wb <- write_main_df(main_df, wb,
                start_row = 6, start_col = 1, start_year = start_year,
                end_year = end_year, sheet_name = data_sheet, value = value,
                transform_value = transform_value, type_col = type_col,
                source_col = source_col)

  end_main_table <- 8+nrow(main_df)

  wb <- write_hpop_billion_contrib(dplyr::select(list_df$hpop_billion, -.data[[ind]]),
                             wb, value,
                             start_row =end_main_table+2 , start_col = 7,
                             start_year = start_year, end_year = end_year,
                             sheet_name = data_sheet)
  # Write notes
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

  # indicator list sheet
  indicator_sheet <- glue::glue("{sheet_prefix}_Indicator List")
  openxlsx::addWorksheet(wb, indicator_sheet)
  openxlsx::removeWorksheet(wb, "Indicator List")

  nice_indicator_df <- dplyr::select(list_df$ind_df, .data[[ind]]) %>%
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

  wb <- write_indicator_list_sheet(nice_indicator_df, wb, sheet_name = indicator_sheet,
                                   start_row = 2 , start_col = 2)

  # Time series
  timeseries_sheet <- glue::glue("{sheet_prefix}_Time Series")
  openxlsx::addWorksheet(wb, timeseries_sheet)
  openxlsx::removeWorksheet(wb, "Time Series")
  wb <- write_timeseries_sheet(list_df$df_iso, wb,
                               sheet_name = timeseries_sheet,
                               start_col = 1, start_row = 1,
                               transform_value= transform_value,
                               df_ind = list_df$ind_df, ind = ind,
                               year = year, type_col = type_col)

  openxlsx::removeWorksheet(wb, "Inter")
  openxlsx::removeWorksheet(wb, "Chart")

  # Write workbook
  if (!dir.exists(output_fldr)) {
    dir.create(output_fldr)
  }

  openxlsx::saveWorkbook(wb, glue::glue("{output_fldr}/GPW13_HPOP_billion_{iso}_CountrySummary_{lubridate::month(lubridate::today(), TRUE)}{lubridate::year(lubridate::today())}.xlsx"), overwrite = TRUE)

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
