#' Write HPOP summary sheet
#'
#' `write_hpop_summary_sheet()` writes all the content and styling for the HPOP
#'  summary sheet. Used within `export_hpop_country_summary_xls()`
#'
#' @inherit export_country_summary_xls
#' @inherit write_baseline_projection_hpop_summary
#' @param df Data frame in long format filtered for a specific country, where 1 row corresponds
#'    to a specific year, and indicator.
#' @param ind_df data frame containing the indicators in the correct order and format to be used.
#' @inheritParams transform_hpop_data
#'
write_hpop_summary_sheet <- function(df, wb, sheet_name, iso,
                                     start_year = 2018,
                                     end_year = 2019:2025,
                                     value = "value",
                                     year = "year",
                                     iso3 = "iso3",
                                     ind = "ind",
                                     population = "population",
                                     transform_value = "transform_value",
                                     type_col = "type",
                                     source_col = "source",
                                     contribution = "contribution",
                                     contribution_pct = paste0(contribution, "_percent"),
                                     contribution_pct_total_pop = paste0(contribution, "_percent_total_pop"),
                                     ind_df,
                                     ind_ids) {
  indicators <- ind_df %>%
    dplyr::select("ind", "sdg", "short_name")

  start_row_data <- 9
  end_row_data <- start_row_data + sum(unique(df[[ind]]) %in% ind_ids) + 2

  # TODO: make dynamic if value or scenario >1
  boxes_bounds <- list(
    indicators = c(
      start_col = 1,
      end_col = 2,
      start_row = start_row_data,
      end_row = end_row_data
    ),
    latest = c(
      start_col = 3,
      end_col = 9,
      start_row = start_row_data,
      end_row = end_row_data
    ),
    baseline_proj = c(
      start_col = 11,
      end_col = 21,
      start_row = start_row_data,
      end_row = end_row_data
    ),
    contribution = c(
      start_col = 23,
      end_col = 26,
      start_row = start_row_data,
      end_row = end_row_data
    ),
    billion_contribution = c(
      start_col = 23,
      end_col = 26,
      start_row = end_row_data + 2,
      end_row = end_row_data + 7
    ),
    notes = c(
      start_col = 1,
      end_col = 5,
      start_row = end_row_data + 2,
      end_row = end_row_data + 7
    )
  )

  # Clean slate

  wb <- write_empty_white_data(
    wb = wb,
    sheet_name = sheet_name,
    bounds = boxes_bounds
  )
  ## Write header
  wb <- write_sheet_header_hpop_summary(wb,
    sheet_name = sheet_name,
    iso, start_col = 1, start_row = 2, end_year = end_year, boxes_bounds = boxes_bounds
  )

  wb <- write_indicators_hpop_summary(ind_df,
    wb,
    sheet_name,
    bounds = boxes_bounds$indicators
  )


  wb <- write_latest_reported_hpop_summary(
    df = df,
    wb = wb,
    ind_df = ind_df,
    sheet_name = sheet_name,
    type_col = type_col,
    iso3 = iso3,
    ind = ind,
    year = year,
    value = value,
    transform_value = transform_value,
    source_col = source_col,
    year_counts = c(2000, 2015),
    bounds = boxes_bounds$latest,
    ind_ids = ind_ids
  )

  wb <- write_baseline_projection_hpop_summary(
    df = df,
    wb = wb,
    sheet_name = sheet_name,
    ind_df = ind_df,
    year = year,
    start_year = start_year,
    end_year = end_year,
    ind = ind,
    value = value,
    transform_value = transform_value,
    type_col = type_col,
    source_col = source_col,
    iso3 = iso3,
    bounds = boxes_bounds$baseline_proj,
    ind_ids = ind_ids
  )

  wb <- write_billion_contrib_ind_hpop_summary(
    df = df,
    wb = wb,
    sheet_name = sheet_name,
    year = year,
    start_year = start_year,
    end_year = end_year,
    ind = ind,
    contribution_pct = contribution_pct,
    population = population,
    contribution = contribution,
    contribution_pct_total_pop = contribution_pct_total_pop,
    ind_df = ind_df,
    boxes_bounds = boxes_bounds,
    ind_ids = ind_ids
  )


  wb <- write_billion_contribution_hpop_summary(
    df = df,
    wb = wb,
    sheet_name = sheet_name,
    contribution = contribution,
    contribution_pct = contribution_pct,
    ind = ind,
    year = year,
    end_year = end_year,
    bounds = boxes_bounds$billion_contribution,
    iso = iso,
    boxes_bounds = boxes_bounds
  )
  # Write notes
  notes <- data.frame(notes = c(
    "Values might be slightly different than dashboard values because of rounding.",
    'For more information, please refer to the GPW13 dashboard, section "Reference", which includes the Impact Measurement Framework, the Methods Report, the Metadata and the Summary of Methods:',
    "https://portal.who.int/triplebillions/PowerBIDashboards/HealthierPopulations",
    "* Values are in bold if there is more than one data source since 2015"
  ))

  wb <- write_notes(notes, wb,
    notes_title = "Notes:",
    sheet_name = sheet_name,
    bounds = boxes_bounds$notes
  )

  # Inter sheet (data for Chart)
  wb <- write_hpop_inter(wb,
    sheet_name = "HPOP_Inter", data_sheet_name = sheet_name,
    ind_df, start_year, end_year, start_col = 1, start_row = 2,
    transform_value, summary_bounds = boxes_bounds
  )
  return(wb)
}

#' Write and style HPOP summary sheet header
#' @inherit write_baseline_projection_hpop_summary
#' @inheritParams transform_hpop_data
#' @inheritParams style_header_hpop_summary_sheet
#' @inheritParams export_country_summary_xls
#' @param boxes_bounds named list of bounds for data frame boxes to be written in sheet.

write_sheet_header_hpop_summary <- function(wb, sheet_name, iso, start_col, start_row, end_year, value, boxes_bounds) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = glue::glue("Country contribution to GPW13 Healthier Population billion"),
    startCol = start_col, startRow = start_row, colNames = FALSE
  )

  country_name <- whoville::iso3_to_names(iso, org = "who", type = "short", language = "en")
  country_pop_end_year <- wppdistro::get_population(iso, year = max(end_year))
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = country_name,
    startCol = start_col,
    startRow = start_row + 2
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = c(
      glue::glue("Projected number of newly healthier lives by {max(end_year)}"),
      glue::glue("% of country population projected to be newly healthier by {max(end_year)}"),
      glue::glue("{country_name} population in {max(end_year)} (Source: World Population Prospects)")
    ),
    startCol = start_col, startRow = start_row + 3
  )

  openxlsx::writeFormula(wb,
    sheet = sheet_name,
    x = c(
      glue::glue("={openxlsx::int2col(boxes_bounds$contribution['end_col'])}{boxes_bounds$billion_contribution['end_row']-1}/1000"),
      glue::glue("={openxlsx::int2col(boxes_bounds$contribution['end_col'])}{boxes_bounds$billion_contribution['end_row']}"),
      glue::glue("={country_pop_end_year}/1000000")
    ),
    startRow = start_row + 3,
    startCol = start_col + 4
  )


  wb <- style_header_hpop_summary_sheet(wb, sheet_name, start_row = start_row, start_col = start_col)

  return(wb)
}
