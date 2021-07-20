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
#' @param ... Additional arguments passed to `calculate_hpop_contributions` and
#' `calculate_hpop_billion`. Use if you need to provide additional parameters to
#' those functions
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
                                       xls_template = "data-raw/CountrySummary_template.xlsx",
                                       ...) {
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
                                           end_year = 2019:2023,
                                           ...) {
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)
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
                                            xls_template = "data-raw/CountrySummary_template.xlsx",
                                            ...) {
  assert_columns(df,year, iso3, ind, value, type_col, source_col)
  assert_years(start_year, end_year)
  wppdistro:::assert_iso3(iso)

  data_sheet <- glue::glue("{sheet_prefix}data")

  # get data frame
  data_sheet_df <- summarize_hpop_country_data(df,
                                               iso,
                                               year = year,
                                               iso3 = iso3,
                                               ind = ind,
                                               value = value,
                                               transform_value= transform_value,
                                               type_col = type_col,
                                               source_col = source_col,
                                               ind_ids = billion_ind_codes("hpop"),
                                               start_year = start_year,
                                               end_year = end_year,
                                               ...)
  # load workbook
  wb <- openxlsx::loadWorkbook(xls_template)
  # Write title
  openxlsx::writeData(wb,
                      sheet = data_sheet,
                      x = "Country contribution to GPW13 Healthier Populations billion target",
                      startCol = 1, startRow = 2, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type_col = "short", language = "en")
  openxlsx::writeData(wb,
                      sheet = data_sheet, x = country_name,
                      startCol = 1, startRow = 4
  )

  # Write tables
  empty_column <- data.frame(empty = rep(NA, nrow(data_sheet_df$ind_df)))

  final_table <- data_sheet_df$ind_df %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$df_iso_raw, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$baseline_proj, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$hpop_contrib, by = "ind") %>%
    dplyr::bind_cols(empty_column, .name_repair = ~ vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
    dplyr::left_join(data_sheet_df$latest_reported, by = "ind") %>%
    dplyr::select(-ind)

  wb <- write_main_df(final_table, wb,
                start_row = 6, start_col = 4, start_year = start_year, end_year = end_year,
                sheet_name = data_sheet
  )

  end_main_table <- 8+nrow(final_table)

  write_hpop_billion_contrib(dplyr::select(data_sheet_df$hpop_billion, "contribution"),
                             wb,
                             start_row =end_main_table+2 , start_col = 14,
                             start_year = start_year, end_year = end_year,
                             sheet_name = data_sheet)

  # HERE HERE HERE

  notes <- data.frame(`Notes:` = c(
    "values might be slightly different than dashboard values because of rounding.",
    "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
    "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations"
  ))

  openxlsx::writeData(wb,
                      sheet = "HPOPdata", x = notes,
                      startCol = 1, startRow = 9 + nrow(final_table) + 1, colNames = TRUE,
                      headerStyle = excel_styles()$bold
  )

  # Style output
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:((9 + nrow(final_table) + 1 + 12))),
                     cols = c(1:27), gridExpand = TRUE
  )


  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$title,
                     rows = c(2), cols = c(1)
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$sub_title,
                     rows = c(4), cols = c(1)
  )


  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$dark_blue_header,
                     rows = c(9 + nrow(final_table) + 1), cols = c(14:17)
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$dark_blue_header,
                     rows = c(9 + nrow(final_table) + 2), cols = c(14:15)
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$light_blue_header,
                     rows = c(9 + nrow(final_table) + 2), cols = c(16:17)
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = openxlsx::createStyle(numFmt = "PERCENTAGE", textDecoration = "bold"),
                     rows = 9 + nrow(final_table) + 6, cols = c(16:17)
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$bold,
                     rows = 9 + nrow(final_table) + 6, cols = c(14:15)
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$normal_data,
                     rows = c((9 + nrow(final_table) + 1 + 1):(9 + nrow(final_table) + 1 + 1 + nrow(notes))), cols = c(1)
  )

  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:5),
                     cols = c(2:26), gridExpand = TRUE
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(1:(9 + nrow(final_table) + 1 + 12)),
                     cols = c(26:27), gridExpand = TRUE
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c((9 + nrow(final_table) + 1 + 7):(9 + nrow(final_table) + 1 + 12)),
                     cols = c(1:27), gridExpand = TRUE
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c(9 + nrow(final_table)),
                     cols = c(1:27)
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$white_bckgrd,
                     rows = c((9 + nrow(final_table)):(9 + nrow(final_table) + 1 + 12)),
                     cols = c(2:13), gridExpand = T
  )
  openxlsx::addStyle(wb,
                     sheet = "HPOPdata", style = excel_styles()$normal_data,
                     rows = c((8 + nrow(final_table) + 4):(8 + nrow(final_table) + 6)), cols = c(14:15),
                     gridExpand = TRUE
  )
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9 + nrow(final_table) + 4)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9 + nrow(final_table) + 5)
  openxlsx::mergeCells(wb, "HPOPdata", cols = c(14:15), rows = 9 + nrow(final_table) + 6)

  # indicator list
  nice_indicator_list <- dplyr::select(data_sheet_df$ind_df, "ind") %>%
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

  timesseries_df <- data_sheet_df$df_iso_pop %>%
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
                      startCol = 2, startRow = 5 + nrow(data_sheet_df$transformed_time_series) + 1
  )

  openxlsx::writeData(wb,
                      sheet = "HPOPTime Series", x = data_sheet_df$transformed_time_series,
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
                     rows = c(6:(nrow(data_sheet_df$transformed_time_series) + 5 + 1)), cols = c(2),
                     gridExpand = TRUE
  )

  # Chart
  openxlsx::addStyle(wb,
                     sheet = "HPOPChart", style = excel_styles()$vertical_txt,
                     rows = 22, cols = 3:19
  )


  # Write workbook
  last_update <- data_sheet_df$latest_reported

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
                                           xls_template = "data-raw/CountrySummary_template.xlsx",
                                           ...) {
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
                                           ...) {
  requireNamespace("billionaiRe", quietly = TRUE)
  assert_mart_columns(df)
}
