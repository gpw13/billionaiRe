#' Export all countries summaries to Excel
#'
#' `export_all_countries_summaries_xls` Export summaries of all countries for the three
#' billions or for a specific billion.
#'
#' @inheritParams export_country_summary_xls
#' @inheritParams transform_hpop_data
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
                                               contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                               ind_ids = billion_ind_codes("hpop"),
                                               start_year = 2018,
                                               end_year = 2019:2023,
                                               output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  unique_iso3s <- unique(df[[iso3]])

  if (billion == "hpop") {
    purrr::map(unique_iso3s, ~ export_country_summary_xls(
      df = df,
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
      contribution_pct_total_pop = contribution_pct_total_pop,
      start_year = start_year,
      end_year = end_year,
      output_folder = output_folder
    ))
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
                                       contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                       start_year = 2018,
                                       end_year = 2019:2023,
                                       output_folder = "outputs") {
  billion <- rlang::arg_match(billion)

  wb <- write_permanent_sheets(billion, start_col = 2, start_row = 3)
  # TODO: To be completed as a wrapper function
  # if (billion == "hep") {
  #   export_hep_country_summary_xls()
  # }
  if (billion == "hpop") {
    purrr::map(openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^UHC|^HEP")], ~ openxlsx::removeWorksheet(wb, .x))

    export_hpop_country_summary_xls(
      df = df,
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
      contribution_pct_total_pop = contribution_pct_total_pop,
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "HPOP",
      output_folder = output_folder,
      ind_ids = billion_ind_codes("hpop")
    )
  }
  if (billion == "uhc") {
    purrr::map(openxlsx::sheets(wb)[stringr::str_detect(openxlsx::sheets(wb), "^HPOP|^HEP")], ~ openxlsx::removeWorksheet(wb, .x))

    export_uhc_country_summary_xls(df,
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
      start_year = start_year,
      end_year = end_year,
      sheet_prefix = "UHC",
      output_folder = output_folder,
      ind_ids = billion_ind_codes("uhc")
    )
  }
  # if (billion == "all") {
  #   export_all_country_summary_xls()
  # }

  sheets_hidden <- grep("_Inter$", openxlsx::sheets(wb))
  for (i in sheets_hidden) {
    openxlsx::sheetVisibility(wb)[sheets_hidden] <- "hidden"
  }

  # Write workbook
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }

  openxlsx::saveWorkbook(wb,
    glue::glue("{output_folder}/GPW13_{toupper(billion)}_billion_{iso}_CountrySummary_{lubridate::today()}.xlsx"),
    overwrite = TRUE
  )
}

#' Export country summary to Excel for HEP billion$
#'
#' `export_hep_country_summary_xls` Export a country-specific for HEP billion.
#'
#' @inherit export_country_summary_xls return details params
export_hep_country_summary_xls <- function(df,
                                           iso,
                                           start_year = 2018,
                                           end_year = 2019:2023) {

}

#' Export country summary to Excel for HPOP billion
#'
#' `export_hpop_country_summary_xls` Export a country-specific for HPOP billion.
#' @inheritParams export_country_summary_xls
#' @inheritParams write_baseline_projection_hpop_summary
#' @inheritParams transform_hpop_data
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
                                            contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                            start_year = 2018,
                                            end_year = 2019:2023,
                                            sheet_prefix = "HPOP",
                                            output_folder = "outputs",
                                            ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, year, iso3, ind, value, transform_value, contribution, population, contribution_pct, contribution_pct_total_pop, scenario, type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)
  assert_same_length(value, transform_value)
  assert_same_length(value, contribution)
  assert_same_length(contribution, contribution_pct)
  assert_same_length(contribution, contribution_pct_total_pop)

  # TODO: Big chunks of HPOP export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_hpop_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times" = length(value) == 1)

  # Get country specific data frame

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == !!iso) %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    ) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~ round(.x, digits = 2)))

  df_iso <- get_df_one_scenario(df_iso, scenario)

  water_sanitation_ind <- ind_ids[stringr::str_detect(names(ind_ids), "^water|^hpop_sanitation")]

  # indicator data frame to make sure the order of indicators is correct
  # remove wash/sanitation indicators not in data
  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(
      .data[["hpop"]],
      !is.na(.data[["ind"]]),
      !((.data[["ind"]] %in% !!water_sanitation_ind) & !(.data[["ind"]] %in% unique(df_iso[[!!ind]])))
    )

  # summary sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  wb <- write_hpop_summary_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = summary_sheet,
    start_year = start_year,
    end_year = end_year,
    value = value,
    year = year,
    iso3 = iso3,
    iso = iso,
    ind = ind,
    population = population,
    transform_value = transform_value,
    type_col = type_col,
    source_col = source_col,
    contribution = contribution,
    contribution_pct = contribution_pct,
    contribution_pct_total_pop = contribution_pct_total_pop,
    ind_df = ind_df
  )

  # Time series
  timeseries_sheet <- glue::glue("{sheet_prefix}_Time Series")

  wb <- write_hpop_timeseries_sheet(
    df = df_iso, wb,
    sheet_name = timeseries_sheet,
    start_col = 2, start_row = 4,
    value = value,
    ind_df = ind_df, ind = ind,
    year = year, type_col = type_col
  )

  # Flip titles graph
  openxlsx::addStyle(wb,
    sheet = "HPOP_Chart", rows = 22, cols = (3:(2 + nrow(ind_df))),
    style = excel_styles(
      textRotation = 90,
      fontSize = 8,
      fgFill = "white",
      wrapText = TRUE,
      halign = "center",
      valign = "center"
    )
  )
  return(wb)
}

#' Export country summary to Excel for UHC billion
#' `export_uhc_country_summary_xls` Export a country-specific for UHC billion.
#'
#' @inherit export_hpop_country_summary_xls return details params
#' @inheritParams transform_hpop_data
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
  assert_columns(df, year, iso3, ind, value, transform_value, contribution, scenario, type_col, source_col)
  assert_years(start_year, end_year)
  assert_who_iso(iso)
  assert_same_length(value, transform_value)
  assert_same_length(value, contribution)

  # TODO: The whole of UHC export functions are static (length(value) == 1). If required, it would be nice to have it dynamic.
  ## Adding a stop for now to avoid issues for now.
  stopifnot("export_uhc_country_summary_xls:
  value, transform_value, and contribution must be of length 1 at the moment.
  If you need to run with mutliple values, please run function multiple times" = length(value) == 1)

  df_iso <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[iso3]] == iso) %>%
    dplyr::arrange(
      get_ind_order(.data[[ind]]),
      .data[[year]]
    ) %>%
    dplyr::mutate(dplyr::across(c(!!value, !!transform_value), ~ round(.x, digits = 2)))

  df_iso <- get_df_one_scenario(df_iso, scenario)

  ind_df <- billionaiRe::indicator_df %>%
    dplyr::filter(.data[["uhc"]])

  # data sheet
  summary_sheet <- glue::glue("{sheet_prefix}_summary")

  write_uhc_summary_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = summary_sheet,
    iso = iso,
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
    ind_df,
    ind_ids
  )

  write_uhc_timeseries_sheet(
    df = df_iso,
    wb = wb,
    sheet_name = glue::glue("{sheet_prefix}_Time Series"),
    start_row = 4,
    start_col = 2,
    value,
    ind_df,
    ind,
    year,
    type_col,
    ind_ids
  )

  openxlsx::addStyle(wb,
    sheet = "UHC_Chart", rows = 22, cols = (3:(2 + nrow(ind_df))),
    style = excel_styles(
      textRotation = 90,
      fontSize = 8,
      fgFill = "white",
      wrapText = TRUE,
      halign = "center",
      valign = "center"
    )
  )


  return(wb)
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
