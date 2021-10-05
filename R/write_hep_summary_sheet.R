#' Write HEP summary sheet
#'
#' `write_hep_summary_sheet()` writes all the content and styling for the HEP
#'  summary sheet. Used within `export_hep_country_summary_xls()`
#'
#' @inherit export_country_summary_xls
#' @inheritParams write_baseline_projection_hpop_summary
#' @param df Data frame in long format filtered for a specific country, where 1 row corresponds
#'    to a specific year, and indicator.
#' @param ind_df data frame containing the indicators in the correct order and format to be used.
#' @inheritParams  calculate_hpop_contributions
write_hep_summary_sheet <- function(df, wb, sheet_name, iso,
                                    start_year = 2018,
                                    end_year = 2019:2025,
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
      end_col = 19,
      start_row = 2,
      end_row = 7
    ),
    data_header = c(
      start_col = 1,
      end_col = 19,
      start_row = 9,
      end_row = 11
    ),
    latest_reported_data = c(
      start_col = 3,
      end_col = 7,
      start_row = 9,
      end_row = 48
    ),
    baseline_projection_data = c(
      start_col = 9,
      end_col = 19,
      start_row = 9,
      end_row = 48
    ),
    prepare = c(
      start_col = 1,
      end_col = 19,
      start_row = 12,
      end_row = 26
    ),
    prevent = c(
      start_col = 1,
      end_col = 19,
      start_row = 27,
      end_row = 38
    ),
    detect_respond = c(
      start_col = 1,
      end_col = 19,
      start_row = 39,
      end_row = 43
    ),
    summary = c(
      start_col = 1,
      end_col = 21,
      start_row = 44,
      end_row = 48
    ),
    notes = c(
      start_col = 1,
      end_col = 19,
      start_row = 50,
      end_row = 62
    )
  )

  wb <- write_sheet_header_hep_summary(wb, sheet_name, iso, end_year, value, boxes_bounds)

  wb <- write_data_headers_hep_summary(wb, sheet_name, value, boxes_bounds, start_year, end_year)

  pillars <- c("prepare", "prevent", "detect_respond")

  purrr::walk(
    pillars,
    ~ write_data_boxes_hep_summary(
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

  write_summary_box_hep_summary(
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
        "* Yellow fever, Meningitis, and Cholera vaccinations apply only to certain countries. When the vaccination is not relevant, the data is faded.",
        "For more information, please refer to the GPW13 dashboard, section 'Reference', which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:",
        "https://portal.who.int/triplebillions/PowerBIDashboards/UniversalHealthCoverage"
      )
  )

  notes_bounds <- boxes_bounds[["notes"]]
  notes_bounds[["end_row"]] <- boxes_bounds[["notes"]]["start_row"] + length(end_notes$notes) + 1

  write_notes(end_notes$notes, "Notes:", wb, sheet_name, bounds = notes_bounds)

  return(wb)
}

#' Write and style HEP summary sheet header
#'
#' @inherit write_sheet_header_hpop_summary
#' @inheritParams transform_hpop_data
#' @inheritParams export_country_summary_xls
write_sheet_header_hep_summary <- function(wb, sheet_name, iso, end_year, value, boxes_bounds) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = glue::glue("Country contribution to GPW13 Health Emergency Protection billion"),
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
      glue::glue("Projected number of persons newly protected from health emergencies by {max(end_year)}"),
      glue::glue("% of country population projected to be newly protected from health emergencies by {max(end_year)}"),
      glue::glue("{country_name} population in {max(end_year)} (Source: World Population Prospects)")
    ),
    startCol = boxes_bounds$sheet_header["start_col"], startRow = boxes_bounds$sheet_header["start_row"] + 3
  )

  openxlsx::writeFormula(wb,
    sheet = sheet_name,
    x = c(
      as_excel_formula(glue::glue("={openxlsx::int2col(boxes_bounds$summary['start_col']+6)}{boxes_bounds$summary['end_row']-1}/1000")),
      as_excel_formula(glue::glue("={openxlsx::int2col(boxes_bounds$summary['start_col']+6)}{boxes_bounds$summary['end_row']}")),
      as_excel_formula(glue::glue("={country_pop_end_year}/1000000"))
    ),
    startRow = boxes_bounds$sheet_header["start_row"] + 3,
    startCol = boxes_bounds$sheet_header["start_col"] + 6
  )

  wb <- style_header_hep_summary_sheet(wb, sheet_name, boxes_bounds = boxes_bounds)

  return(wb)
}
