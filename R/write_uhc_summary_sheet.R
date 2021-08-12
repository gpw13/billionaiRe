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
write_uhc_summary_sheet <- function(df, wb, sheet_name, iso,
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
                                    ind_ids) {
  boxes_bounds <- list(
    sheet_header = c(
      start_col = 1,
      end_col = 21,
      start_row = 2,
      end_row = 7
    ),
    data_header = c(
      start_col = 1,
      end_col = 21,
      start_row = 9,
      end_row = 11
    ),
    latest_reported_data = c(
      start_col = 3,
      end_col = 7,
      start_row = 9,
      end_row = 42
    ),
    baseline_projection_data = c(
      start_col = 9,
      end_col = 21,
      start_row = 9,
      end_row = 42
    ),
    RMNCH = c(
      start_col = 1,
      end_col = 21,
      start_row = 12,
      end_row = 17
    ),
    infec_diseases = c(
      start_col = 1,
      end_col = 21,
      start_row = 18,
      end_row = 23
    ),
    ncd = c(
      start_col = 1,
      end_col = 21,
      start_row = 24,
      end_row = 28
    ),
    service_cap_access = c(
      start_col = 1,
      end_col = 21,
      start_row = 29,
      end_row = 35
    ),
    fin_hardship = c(
      start_col = 1,
      end_col = 21,
      start_row = 36,
      end_row = 37
    ),
    summary = c(
      start_col = 1,
      end_col = 21,
      start_row = 38,
      end_row = 42
    ),
    notes = c(
      start_col = 1,
      end_col = 21,
      start_row = 44,
      end_row = 62
    )
  )

  wb <- write_sheet_header_uhc_summary(wb, sheet_name, iso, end_year, value, boxes_bounds)

  wb <- write_data_headers_uhc_summary(wb, sheet_name, value, boxes_bounds, start_year, end_year)

  pillars <- c("RMNCH", "infec_diseases", "ncd", "service_cap_access")

  purrr::walk(
    pillars,
    ~ write_data_boxes_uhc_summary(
      df = df,
      pillar = .x,
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
      iso3 = iso3,
      ind_ids = ind_ids
    )
  )

  write_asc_uhc_data_summary(
    df = df,
    pillar = "fin_hardship",
    data_boxes_pillars = pillars,
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
    iso3 = iso3,
    ind_ids = ind_ids
  )

  write_summary_box_uhc_summary(
    wb,
    sheet_name,
    iso,
    start_year,
    end_year,
    boxes_bounds
  )

  end_notes <- list(
    notes =
      c(
        "* proxy",
        "1 Data refer to married women or in a union for data comparability reasons across countries.",
        "2 The reference year is not the year in which the year was conducted but the middle year of the data coverage period.",
        "3 Data are imputed using linear interpolation between 2020 and 2025 projected values extracted from WHO Global Report on Trends in Tobacco Use 2000-2025.",
        "4 ITN use only applies to malaria endemic countries in sub-Saharan Africa for which data on ITN use are available.",
        "5 The Average Service Coverage value presented here is slightly different to the SDG 3.8.1 value due to the use of nested arithmetic means and small adjustments to the TB treatment, tobacco control, and Health workforce indicators. Please see the Methods Report for more information.",
        "** Projections have been produced jointly with the World Bank based on survey based estimates available to both organizations by July 2019. Modelling relies on the International Monetary Fund World Economic Outlook projections for GDP per capita (2019 released until 2024); the WHO Global expenditure data on household out-of-pocket expenditures (2018 update) and the IMF or the World Bank data on household final private consumption (2019 released). These projections are preliminary, they will be used in the dashboard to compute the UHC billion but won’t be shown. They will be updated to take into account more recent survey-based estimates that will go through country consultation in early 2021 but might not yet capture the impact of COVID-19."
      ),
    methode_transformation =
      c(
        "The prevalence of raised blood pressure is converted into prevalence of non-raised blood pressure and is rescaled using a minimum value of 50% (i.e. rescaled value = (X - 50) / (100 - 50) * 100).",
        "Mean fasting plasma glucose, which is a continuous measure (units of mmol/L), is converted to a scale of 0 to 100 using the minimum theoretical biological risk (5.1 mmol/L) and observed maximum across countries (7.1 mmol/L) (i.e. ° rescaled value  =  (7.1  -  original value)  / (7.1 - 5.1) * 100).",
        "The prevalence of tobacco use is converted into prevalence of tobacco non-use.",
        "Hospital bed density is capped at a maximum threshold, and values above this threshold are held constant at 100. The treshold is based on minimum values observed across Organisation for Economic Co-operation and Development countries (i.e. rescaled hospital beds per 10,000 = minimum (100, original value / 18 * 100)).",
        "Health worker density is the sum of doctors and nurses/midwives densities, is capped at a maximum threshold and values above this threshold are held constant at 100. The treshold is based on the 95th percentile across all national densities from 2000 to 2017 (i.e. rescaled health worker density per 10,000 = minimum (100, original value / 154.7 * 100)).",
        "SDG 3.8.2 is converted into its complement (100 - SDG 3.8.2)",
        "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
        "https://portal.who.int/triplebillions/PowerBIDashboards/UniversalHealthCoverage"
      )
  )

  notes_bounds <- boxes_bounds[["notes"]]
  notes_bounds[["end_row"]] <- boxes_bounds[["notes"]]["start_row"] + length(notes$notes) + 1

  write_notes(end_notes$notes, "Notes:", wb, sheet_name, bounds = notes_bounds)

  methode_transformation_bounds <- boxes_bounds[["notes"]]
  methode_transformation_bounds[["start_row"]] <- notes_bounds[["end_row"]] + 1

  write_notes(end_notes$methode_transformation, "Methode of transformation:", wb, sheet_name, bounds = methode_transformation_bounds)


  return(wb)
}

#' Write and style UHC summary sheet header
#'
#' @inherit write_sheet_header_hpop_summary

write_sheet_header_uhc_summary <- function(wb, sheet_name, iso, end_year, value, boxes_bounds) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = glue::glue("Country contribution to GPW13 Universal Health Coverage billion"),
    startCol = boxes_bounds$sheet_header["start_col"], startRow = boxes_bounds$sheet_header["start_row"], colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  country_pop_end_year <- wppdistro::get_population(iso, year = max(end_year))
  openxlsx::writeData(wb,
    sheet = sheet_name, x = country_name,
    startCol = boxes_bounds$sheet_header["start_col"], startRow = boxes_bounds$sheet_header["start_row"] + 2
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = c(
      glue::glue("Projected number of persons newly covered by universal healthcare by {max(end_year)}"),
      glue::glue("% of country population projected to be newly covered by universal healthcare by {max(end_year)}"),
      glue::glue("{country_name} population in {max(end_year)} (Source: World Population Prospects)")
    ),
    startCol = boxes_bounds$sheet_header["start_col"], startRow = boxes_bounds$sheet_header["start_row"] + 3
  )

  openxlsx::writeFormula(wb,
    sheet = sheet_name,
    x = c(
      as_excel_formula(glue::glue("={openxlsx::int2col(boxes_bounds$summary['start_col']+12)}{boxes_bounds$summary['end_row']-1}/1000")),
      as_excel_formula(glue::glue("={openxlsx::int2col(boxes_bounds$summary['start_col']+12)}{boxes_bounds$summary['end_row']}*100")),
      as_excel_formula(glue::glue("={country_pop_end_year}/1000000"))
    ),
    startRow = boxes_bounds$sheet_header["start_row"] + 3,
    startCol = boxes_bounds$sheet_header["start_col"] + 4
  )

  wb <- style_header_uhc_summary_sheet(wb, sheet_name, boxes_bounds = boxes_bounds)

  return(wb)
}
