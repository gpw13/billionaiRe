#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#'
#' @inheritParams summarize_hpop_country_data
#' @inheritParams transform_hpop_data
#' @param iso ISO3 code of country to summarize.
#' @param output_folder Folder path to where the Excel files should be written
#' @param xls_template Path to the Excel template to be used.
#' @param sheet_prefix Character prefix to add in front of export sheets
#'
#' @return `openxslx` Workbook object. Output file is in `output_folder`.
#'
#' @examples
#' \dontrun{
#' data <- load_billion_data("hpop")
#' export_country_summary_xls(data, iso = "AFG")
#' }
export_country_summary_xls <- function(df,
                                       iso,
                                       billion = c("hpop", "hep", "uhc", "all"),
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
                                       output_folder = "outputs",
                                       ...) {
  assert_mart_columns(df)
  billion <- rlang::arg_match(billion)

  if (billion == "hep") {
    export_hep_country_summary_xls(df, {{ iso }}, {{ output_folder }}, {{ xls_template }})
  }
  if (billion == "hpop") {
    export_hpop_country_summary_xls(df, {{ iso }}, {{ output_folder }}, {{ xls_template }})
  }
  if (billion == "uhc") {
    export_uhc_country_summary_xls(df, {{ iso }}, {{ output_folder }}, {{ xls_template }})
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
export_hep_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023){
  assert_mart_columns(df)

}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#' @inherit export_country_summary_xls return details params
#'
#' @export
#'
export_hpop_country_summary_xls <- function(df,
                                            iso,
                                            iso3 = "iso3",
                                            year = "year",
                                            ind = "ind",
                                            value = "value",
                                            transform_value = "transform_value",
                                            scenario = NULL,
                                            type_col = "type",
                                            source_col = "source",
                                            population = "population",
                                            contribution = "contribution",
                                            start_year = 2018,
                                            end_year = 2019:2023,
                                            sheet_prefix = "HPOP",
                                            output_folder = "outputs",
                                            ind_ids = billion_ind_codes("hpop"),
                                            ...) {
  assert_columns(df,year, iso3, ind, value,transform_value, type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)

  df_iso <- df %>%
    dplyr::filter(.data[[iso3]] == iso)

  unique_ind <- unique(df_iso[[ind]])

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(.data[["ind"]] %in% !!unique_ind) %>%
    dplyr::select("ind", "transformed_name", "unit_transformed")


  # load workbook
  wb_file <- system.file("extdata",
                         "country_summary_template.xlsx",
                         package = "billionaiRe")

  wb <- openxlsx::loadWorkbook(wb_file)

  # data sheet
  data_sheet <- glue::glue("{sheet_prefix}_data")
  openxlsx::renameWorksheet(wb, sheet = "data", newName = data_sheet)

  wb <- write_data_sheet_HPOP(df = df_iso, wb, sheet_name = data_sheet, start_year, end_year, value,year,
                         iso3,iso,ind,population,scenario,ind_ids,
                         transform_value, type_col, source_col, contribution)

  # indicator list sheet
  indicator_sheet <- glue::glue("{sheet_prefix}_Indicator List")
  openxlsx::addWorksheet(wb, indicator_sheet)
  openxlsx::removeWorksheet(wb, "Indicator List")

  nice_indicator_df <- dplyr::select(ind_df, .data[[ind]]) %>%
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
  wb <- write_timeseries_sheet(df_iso, wb,
                               sheet_name = timeseries_sheet,
                               start_col = 1, start_row = 1,
                               transform_value= transform_value,
                               df_ind = ind_df, ind = ind,
                               year = year, type_col = type_col)

  openxlsx::removeWorksheet(wb, "Inter")
  openxlsx::removeWorksheet(wb, "Chart")

  # Write workbook
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  openxlsx::saveWorkbook(wb, glue::glue("{output_folder}/GPW13_HPOP_billion_{iso}_CountrySummary_{lubridate::month(lubridate::today(), TRUE)}{lubridate::year(lubridate::today())}.xlsx"), overwrite = TRUE)

}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_country_summary_xls return details params
export_uhc_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           output_folder = "outputs",
                                           xls_template = "data-raw/CountrySummary_template.xlsx") {
  assert_mart_columns(df)
}

#' Export country summary to Excel for all billions.
#' `export_all_country_summary_xls` Export a country-specific for all billions.
#'
#' @inherit export_country_summary_xls return details params
#'
export_all_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           output_folder = "outputs",
                                           xls_template = "data-raw/CountrySummary_template.xlsx") {
  assert_mart_columns(df)
}
