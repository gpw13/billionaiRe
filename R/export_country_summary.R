#' Export all countries summaries to Excel
#'
#' `export_all_countries_summaries_xls` Export summaries of all countries for the three
#' billions or for a specific billion.
#'
#' @inheritParams export_country_summary_xls
#'
#' @return `openxslx` Workbook object. Output file is in `output_folder`.
#'
#' @export
#'
export_all_countries_summaries_xls <- function(df,
                                   billion = c("hpop", "hep", "uhc", "all"),
                                   year = "year",
                                   iso3 = "iso3",
                                   ind = "ind",
                                   value = "value",
                                   transform_value = "transform_value",
                                   scenario = NULL,
                                   type_col = "type",
                                   source_col = "source",
                                   population = "population",
                                   contribution = "contribution",
                                   contribution_pct = paste0(contribution, "_percent"),
                                   contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                   ind_ids = billion_ind_codes("hpop"),
                                   start_year = 2018,
                                   end_year = 2019:2023,
                                   output_folder = "outputs") {

  billion <- rlang::arg_match(billion)

  unique_iso3s <- unique(df[[iso3]])

  if (billion == "hpop") {
    purrr::map(unique_iso3s, ~ export_hpop_country_summary_xls(df = df,
                                                               iso = .x,
                                                               iso3 = iso3,
                                                               year = year,
                                                               ind = ind,
                                                               value = value,
                                                               transform_value = transform_value,
                                                               scenario = scenario,
                                                               type_col = type_col,
                                                               source_col = source_col,
                                                               population = population,
                                                               contribution = contribution,
                                                               contribution_pct = contribution_pct,
                                                               contribution_pct_pop_total = contribution_pct_pop_total,
                                                               start_year = start_year,
                                                               end_year = end_year,
                                                               sheet_prefix = "HPOP",
                                                               output_folder = output_folder,
                                                               ind_ids = ind_ids))
  }
}

#' Export country summary to Excel
#'
#' `export_country_summary_xls` Export a country-specific for all three
#' billions or for a specific billion.
#'
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @param iso ISO3 code of country to summarize.
#' @param billion Billion indicator names to return, either "hep", "hpop", "uhc", or "all".
#' @param output_folder Folder path to where the Excel files should be written
#'
#' @return `openxslx` Workbook object. Output file is in `output_folder`.
#'
#' @export

export_country_summary_xls <- function(df,
                                       iso,
                                       billion = c("hpop", "hep", "uhc", "all"),
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       value = "value",
                                       transform_value = "transform_value",
                                       scenario = NULL,
                                       type_col = "type",
                                       source_col = "source",
                                       population = "population",
                                       contribution = "contribution",
                                       contribution_pct = paste0(contribution, "_percent"),
                                       contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                       ind_ids = billion_ind_codes("hpop"),
                                       start_year = 2018,
                                       end_year = 2019:2023,
                                       output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  #TODO: To be completed as a wrapper function
  # if (billion == "hep") {
  #   export_hep_country_summary_xls()
  # }
  if (billion == "hpop") {
    export_hpop_country_summary_xls(df = df,
                                    iso = iso,
                                    iso3 = iso3,
                                    year = year,
                                    ind = ind,
                                    value = value,
                                    transform_value = transform_value,
                                    scenario = scenario,
                                    type_col = type_col,
                                    source_col = source_col,
                                    population = population,
                                    contribution = contribution,
                                    contribution_pct = contribution_pct,
                                    contribution_pct_pop_total = contribution_pct_pop_total,
                                    start_year = start_year,
                                    end_year = end_year,
                                    sheet_prefix = "HPOP",
                                    output_folder = output_folder,
                                    ind_ids = ind_ids)
  }
  # if (billion == "uhc") {
  #   export_uhc_country_summary_xls()
  # }
  # if (billion == "all") {
  #   export_all_country_summary_xls()
  # }
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

}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#' @inherit export_country_summary_xls return details params
#' @param sheet_prefix Character prefix to add in front of export sheets
#'
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
                                            contribution_pct = paste0(contribution, "_percent"),
                                            contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                            start_year = 2018,
                                            end_year = 2019:2023,
                                            sheet_prefix = "HPOP",
                                            output_folder = "outputs",
                                            ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df,year, iso3, ind, value,transform_value,contribution,scenario,type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)
  assert_same_length(value, transform_value)
  assert_same_length(value, contribution)

  #Get country specific data frame

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::arrange(get_ind_order(.data[[ind]]),
                   .data[[year]])

  water_sanitation_ind <- unlist(unique(stringr::str_extract_all(df_iso[[ind]], "water.*|hpop_sanitation.*")))

  #Indicator data frame to make sure the order of indicators is correct
  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(!!sym("hpop") == TRUE, !is.na(!!sym("ind")), !!sym("ind") %in% remove_unused_wash_ind(.data[["ind"]], water_sanitation_ind))

  # load workbook
  wb_file <- system.file("extdata",
                         "country_summary_template.xlsx",
                         package = "billionaiRe")

  wb <- openxlsx::loadWorkbook(wb_file)

  # data sheet
  data_sheet <- glue::glue("{sheet_prefix}_data")
  openxlsx::renameWorksheet(wb, sheet = "data", newName = data_sheet)

  wb <- write_hpop_summary_sheet(df = df_iso, wb = wb, sheet_name = data_sheet,
                             start_year = start_year, end_year = end_year, value =value,year = year,
                             iso3 = iso3,iso = iso,ind = ind ,population = population ,scenario =scenario,
                             ind_ids = ind_ids, transform_value = transform_value, type_col = type_col,
                             source_col = source_col, contribution = contribution,
                             contribution_pct = contribution_pct,
                             contribution_pct_pop_total = contribution_pct_pop_total,
                             ind_df)

  # indicator list sheet
  indicator_sheet <- glue::glue("{sheet_prefix}_Indicator List")
  openxlsx::addWorksheet(wb, indicator_sheet)
  openxlsx::removeWorksheet(wb, "Indicator List")

  nice_indicator_df <- billionaiRe::indicator_df %>%
    dplyr::filter(!!sym("hpop") == TRUE, !is.na(!!sym("ind")), !!sym("ind") %in% remove_unused_wash_ind(.data[["ind"]], c("water", "hpop_sanitation"))) %>%
    dplyr::mutate("short_name" := dplyr::case_when(
      .data[["short_name"]] == "Safely Managed Water" ~ paste0("Safely Managed Water*"),
      .data[["short_name"]] == "Safely Managed Sanitation" ~ paste0("Safely Managed Sanitation*"),
      TRUE ~ .data[["short_name"]]
    )) %>%
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
  wb <- write_hpop_timeseries_sheet(df_iso, wb,
                               sheet_name = timeseries_sheet,
                               start_col = 1, start_row = 1,
                               value = value,
                               ind_df = ind_df, ind = ind,
                               year = year, type_col = type_col)

  # Inter sheet (data for Chart)
  wb <- write_hpop_inter(wb, sheet_name = "HPOP_Inter", data_sheet_name = data_sheet,
                         ind_df, start_year, end_year, start_col = 1, start_row = 2,
                         start_col_data = 1, start_row_data = 9,
                         transform_value)

  # Flip titles graph
  openxlsx::addStyle(wb, sheet = "HPOP_Chart", rows = 22, cols = (3:(2+nrow(ind_df))),
                     style = excel_styles()$vertical_txt)

  # Write workbook
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  openxlsx::saveWorkbook(wb,
                         glue::glue("{output_folder}/GPW13_HPOP_billion_{iso}_CountrySummary_{lubridate::month(lubridate::today(), TRUE)}{lubridate::year(lubridate::today())}.xlsx"), overwrite = TRUE)

}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_country_summary_xls return details params
#' @export
export_uhc_country_summary_xls <- function(df,
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
                                           contribution_pct = paste0(contribution, "_percent"),
                                           contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           sheet_prefix = "UHC",
                                           output_folder = "outputs",
                                           ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df,year, iso3, ind, value,transform_value,contribution,scenario,type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)
  assert_same_length(value, transform_value)
  assert_same_length(value, contribution)

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::arrange(get_ind_order(.data[[ind]]),
                   .data[[year]])

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(!!sym("uhc") == TRUE, !is.na(!!sym("ind"))) %>%
    dplyr::select("ind", "short_name", "medium_name", "pillar")

  wb_file <- system.file("extdata",
                         "country_summary_template.xlsx",
                         package = "billionaiRe")

  wb <- openxlsx::loadWorkbook(wb_file)

  # data sheet
  data_sheet <- glue::glue("{sheet_prefix}_data")

  wb <- write_uhc_datasheet(df = df_iso, wb = wb, sheet_name = data_sheet,
                             start_year = start_year, end_year = end_year, value =value,year = year,
                             iso3 = iso3,iso = iso,ind = ind ,population = population ,scenario =scenario,
                             ind_ids = ind_ids, transform_value = transform_value, type_col = type_col,
                             source_col = source_col, contribution = contribution,
                             contribution_pct = contribution_pct,
                             contribution_pct_pop_total = contribution_pct_pop_total,
                             ind_df)

}

#' Export country summary to Excel for all billions.
#' `export_all_country_summary_xls` Export a country-specific for all billions.
#'
#' @inherit export_country_summary_xls return details params
#'
export_all_country_summary_xls <- function(df,
                                           iso,
                                           iso3 = "iso3",
                                           start_year = 2018,
                                           end_year = 2019:2023,
                                           output_folder = "outputs") {


}
