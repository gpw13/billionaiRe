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

  wb <- write_permanent_sheets(billion, start_col = 2, start_row = 3)

  if (billion == "hpop") {

    sheets_to_remove <- openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^UHC|^HEP")]
    for(i in seq_along(sheets_to_remove)){
      openxlsx::removeWorksheet(wb, sheets_to_remove[i])
    }

    purrr::map(unique_iso3s, ~ export_hpop_country_summary_xls(df = df,
                                                               wb = wb,
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
                                                               ind_ids = billion_ind_codes("hpop")))
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

  wb <- write_permanent_sheets(billion, start_col = 2, start_row = 3)
  #TODO: To be completed as a wrapper function
  # if (billion == "hep") {
  #   export_hep_country_summary_xls()
  # }
  if (billion == "hpop") {
    sheets_to_remove <- openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^UHC|^HEP")]
    for(i in seq_along(sheets_to_remove)){
      openxlsx::removeWorksheet(wb, sheets_to_remove[i])
    }
    export_hpop_country_summary_xls(df = df,
                                    wb = wb,
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
                                    ind_ids = billion_ind_codes("hpop"))

  }
  if (billion == "uhc") {
    sheets_to_remove <- openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^HPOP|^HEP")]
    for(i in seq_along(sheets_to_remove)){
      openxlsx::removeWorksheet(wb, sheets_to_remove[i])
    }

    export_uhc_country_summary_xls(df,
                                   wb,
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
                                   sheet_prefix = "UHC",
                                   output_folder = "outputs",
                                   ind_ids = billion_ind_codes("uhc"))
  }
  # if (billion == "all") {
  #   export_all_country_summary_xls()
  # }

  sheets_hidden <- grep( "_Inter$", openxlsx::sheets(wb))
  for(i in sheets_hidden){
    openxlsx::sheetVisibility(wb)[sheets_hidden] <- "hidden"
  }

  # Write workbook
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  openxlsx::saveWorkbook(wb,
                         glue::glue("{output_folder}/GPW13_{billion}_billion_{iso}_CountrySummary_{lubridate::month(lubridate::today(), TRUE)}{lubridate::year(lubridate::today())}.xlsx"), overwrite = TRUE)

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
#' @inheritParams export_country_summary_xls
#' @inheritParams write_baseline_projection_hpop_summary
#' @param sheet_prefix Character prefix to add in front of export sheets
#'
export_hpop_country_summary_xls <- function(df,
                                            wb,
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

  #TODO: Big chunks of HPOP export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_hpop_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times"=length(value) == 1 )


  #Get country specific data frame

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::arrange(get_ind_order(.data[[ind]]),
                   .data[[year]]) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~round(.x, digits = 2)))

  water_sanitation_ind <- unlist(unique(stringr::str_extract_all(df_iso[[ind]], "water.*|hpop_sanitation.*")))

  #Indicator data frame to make sure the order of indicators is correct
  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(!!rlang::sym("hpop") == TRUE, !is.na(!!rlang::sym("ind")),
                  !!rlang::sym("ind") %in% remove_unused_wash_ind(.data[["ind"]], water_sanitation_ind))

  # summary sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  wb <- write_hpop_summary_sheet(df = df_iso, wb = wb, sheet_name = summary_sheet,
                             start_year = start_year, end_year = end_year, value =value,year = year,
                             iso3 = iso3,iso = iso,ind = ind ,population = population,
                             transform_value = transform_value, type_col = type_col,
                             source_col = source_col, contribution = contribution,
                             contribution_pct = contribution_pct,
                             contribution_pct_pop_total = contribution_pct_pop_total,
                             ind_df)

  # Time series
  timeseries_sheet <- glue::glue("{sheet_prefix}_Time Series")

  wb <- write_hpop_timeseries_sheet(df = df_iso, wb,
                               sheet_name = timeseries_sheet,
                               start_col = 2, start_row = 4,
                               value = value,
                               ind_df = ind_df, ind = ind,
                               year = year, type_col = type_col)

  # Flip titles graph
  openxlsx::addStyle(wb, sheet = "HPOP_Chart", rows = 22, cols = (3:(2+nrow(ind_df))),
                     style = excel_styles()$vertical_txt)

  return(wb)
}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_hpop_country_summary_xls return details params
export_uhc_country_summary_xls <- function(df,
                                           wb,
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
                                           sheet_prefix = "UHC",
                                           output_folder = "outputs",
                                           ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, year, iso3, ind, value,transform_value,contribution,scenario,type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)
  assert_same_length(value, transform_value)
  assert_same_length(value, contribution)

  #TODO: The whole of UHC export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_uhc_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times"=length(value) == 1 )

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::arrange(get_ind_order(.data[[ind]]),
                   .data[[year]])

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(!!sym("uhc") == TRUE, !is.na(!!sym("ind")))

  # data sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  wb <- write_uhc_summary_sheet(df = df_iso,
                                wb = wb,
                                sheet_name = summary_sheet,
                                iso = iso,
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
