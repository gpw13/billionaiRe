#' Write indicator list for HPOP summary sheet
#'
#' @inherit write_latest_reported_hpop_summary

write_indicators_hpop_summary <- function(ind_df,
                                          wb,
                                          sheet_name,
                                          bounds) {
  indicators_nice <- ind_df %>%
    dplyr::select("sdg", "short_name")

  openxlsx::writeData(wb, sheet_name,
    x = c("Indicators"),
    startRow = bounds["start_row"],
    startCol = bounds["start_col"]
  )

  openxlsx::writeData(wb, sheet_name,
    x = vec2emptyDF(c("SDG/WHA Number", "Indicator")),
    startRow = bounds["start_row"] + 1,
    startCol = bounds["start_col"], colNames = TRUE
  )

  openxlsx::writeData(wb, sheet_name,
    x = indicators_nice,
    startRow = bounds["start_row"] + 3,
    startCol = bounds["start_col"],
    colNames = FALSE
  )

  wb <- style_hpop_indicators(wb, sheet_name, bounds,
    data_type = get_data_type(indicators_nice)
  )
}

#' Write latest reported data to HPOP summary sheet
#'
#' @inherit write_hpop_summary_sheet
#' @inheritParams style_header_hpop_summary_sheet
#' @param year_counts integer vector with the year(s) to count the number of values available for
#' a specific indicators. Defaults to 2000 and 2015.
#' @param bounds named integer vector identifying integer sheet start and end reference column.
#' All parameters must be numerical. For converting Excel column references to integer references
#' see [openxlsx::int2col()].
#' Must have the following named elements:
#' * `start_col`: start column
#' * `end_col`: end column
#' * `start_row`: start row
#' * `end_row`: end row

write_latest_reported_hpop_summary <- function(df,
                                               wb,
                                               sheet_name,
                                               ind_df,
                                               start_row,
                                               start_col,
                                               type_col,
                                               iso3,
                                               ind,
                                               year,
                                               value,
                                               transform_value,
                                               source_col,
                                               year_counts = c(2000, 2015),
                                               bounds) {
  latest_reported <- df %>%
    dplyr::filter(.data[[type_col]] %in% c("estimated", "reported")) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::filter(.data[[year]] == max(.data[[year]])) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(
      ind, value, transform_value, year,
      type_col, source_col, iso3
    ))) %>%
    dplyr::mutate(!!sym(year) := as.integer(.data[[year]]))

  # Count data points since specified dates
  counts_years <- purrr::map(year_counts, ~ count_since(df, year_specified = .x, year = year, ind = ind, iso3 = iso3, type_col = type_col)) %>%
    purrr::reduce(dplyr::left_join, by = c(iso3, ind))

  # Join counts with latest reported data
  latest_reported <- ind_df[, "ind"] %>%
    dplyr::left_join(latest_reported, by = c("ind" = ind)) %>%
    dplyr::left_join(counts_years, by = c(iso3, ind)) %>%
    dplyr::select(-.data[[ind]], -.data[[iso3]])

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = "Latest Reported/Estimated Data Available",
    startCol = bounds["start_col"], startRow = bounds["start_row"], colNames = FALSE
  )

  sentence_v <- stringr::str_to_title(value)

  latest_rep_headers <- c(
    glue::glue("Raw {sentence_v}"),
    glue::glue("Transformed {sentence_v}"),
    "Year*", "Type", "Source",
    glue::glue("Number of values (since {year_counts})")
  ) %>%
    vec2emptyDF()

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = latest_rep_headers,
    startCol = bounds["start_col"],
    startRow = bounds["start_row"] + 1,
    colNames = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = latest_reported,
    startCol = bounds["start_col"],
    startRow = bounds["start_row"] + 3,
    colNames = FALSE
  )

  wb <- style_hpop_latest(wb, sheet_name, bounds,
    data_type = get_data_type(latest_reported)
  )

  return(wb)
}

#' Write the baseline/projection data frame to the data sheet
#'
#' @param df data frame to be written
#' @param wb [openxlsx::createWorkbook()] workbook to be edited.
#' @param sheet_name character name of the sheet to update
#' @param start_year Base year for contribution calculation, defaults to 2018.
#' @param end_year End year(s) for contribution calculation, defaults to 2019 to 2023.
#' @inheritParams export_hpop_country_summary_xls
#' @inheritParams write_latest_reported_hpop_summary
#'

write_baseline_projection_hpop_summary <- function(df,
                                                   wb,
                                                   sheet_name,
                                                   ind_df,
                                                   year,
                                                   start_year,
                                                   end_year,
                                                   ind,
                                                   value,
                                                   transform_value,
                                                   type_col,
                                                   source_col,
                                                   iso3,
                                                   bounds) {
  baseline_proj <- df %>%
    dplyr::filter(.data[[year]] %in% c(!!start_year, max(!!end_year))) %>%
    dplyr::select(dplyr::all_of(c(
      ind, year, value, transform_value, type_col,
      source_col, iso3
    ))) %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    tidyr::pivot_wider(
      names_from = .data[[year]],
      values_from = c(dplyr::all_of(c(value, transform_value)), .data[[type_col]], .data[[source_col]])
    ) %>%
    dplyr::mutate(empty1 = NA, .after = glue::glue("{value}_{max(end_year)}")) %>%
    dplyr::mutate(empty2 = NA, .after = glue::glue("{transform_value}_{max(end_year)}")) %>%
    dplyr::mutate(empty3 = NA, .after = glue::glue("{type_col}_{max(end_year)}"))


  baseline_proj <- ind_df[, "ind"] %>%
    dplyr::left_join(baseline_proj, by = c("ind" = ind)) %>%
    dplyr::select(.data[[iso3]], -.data[["ind"]])

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = glue::glue("{start_year} Baseline, and {max(end_year)} Projection"),
    startCol = bounds["start_col"], startRow = bounds["start_row"], colNames = FALSE
  )

  baseline_proj_header <- vec2emptyDF(c(
    "Raw Value", rep("", length(value) * 2 - 1), "",
    "Transformed Value", rep("", length(value) * 2 - 1), "",
    "Type", rep("", length(value) * 2 - 1), "",
    "Source"
  ))

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = baseline_proj_header,
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 1, colNames = TRUE
  )

  ### Baseline and proj years sub-header
  start_end_years <- as.character(c(start_year, max(end_year)))
  baseline_proj_subHeader <- vec2emptyDF(c(
    rep(start_end_years, length(value)), "",
    rep(start_end_years, length(value)), "",
    rep(start_end_years, length(value)), "",
    rep(start_end_years, length(value))
  ))

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = baseline_proj_subHeader,
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 2, colNames = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = baseline_proj,
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 3, colNames = FALSE
  )

  wb <- style_hpop_baseline_projection(wb, sheet_name, bounds,
    data_type = get_data_type(baseline_proj)
  )

  return(wb)
}

#' Write the contribution to billion per indicator data frame to the data sheet
#'
#'
#' Used within `write_hpop_summary_sheet()`
#'
#' @param df data frame to be written
#' @param sheet_name character name of the sheet to update
#' @inheritParams export_hpop_country_summary_xls
#' @inheritParams write_latest_reported_hpop_summary
#' @inheritParams write_baseline_projection_hpop_summary
#'

write_bilion_contrib_ind_hpop_summary <- function(df,
                                                  wb,
                                                  sheet_name,
                                                  year,
                                                  start_year,
                                                  end_year,
                                                  ind,
                                                  contribution_pct,
                                                  population,
                                                  contribution,
                                                  contribution_pct_pop_total,
                                                  ind_df,
                                                  bounds) {
  hpop_contrib <- df %>%
    dplyr::filter(.data[[year]] == max(end_year)) %>%
    dplyr::select(dplyr::all_of(c(ind, contribution_pct, population, contribution, contribution_pct_pop_total))) %>%
    dplyr::mutate(dplyr::across(c(!!population, !!contribution), ~ . / 1000))

  hpop_contrib <- ind_df[, "ind"] %>%
    dplyr::left_join(hpop_contrib, by = c("ind" = ind)) %>%
    dplyr::select(-.data[["ind"]])

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = glue::glue("Contribution to the Billion"),
    startCol = bounds["start_col"], startRow = bounds["start_row"], colNames = FALSE
  )

  contrib_subHeader <- vec2emptyDF(c(
    glue::glue("Change in Transformed Value over {start_year}-{max(end_year)} - %"),
    glue::glue("UN Population {max(end_year)} - Thousands"),
    "Contribution - Thousands",
    "Contribution - % Total Population"
  ))

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = contrib_subHeader,
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 1, colNames = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = hpop_contrib,
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 3, colNames = FALSE
  )
  # TODO: Make dynamic
  wb <- style_billion_contrib_ind_hpop(wb, sheet_name, bounds,
    data_type = c("numeric", "integer", "numeric", "numeric")
  )

  return(wb)
}

write_billion_contribution_hpop_summary <- function(df,
                                                    wb,
                                                    sheet_name,
                                                    contribution,
                                                    contribution_pct,
                                                    ind,
                                                    year,
                                                    end_year,
                                                    bounds,
                                                    iso,
                                                    boxes_bounds) {
  hpop_billion_contribution <- df %>%
    dplyr::filter(
      .data[[year]] == max(end_year),
      stringr::str_detect(.data[[ind]], "^hpop_healthier"),
      !stringr::str_detect(.data[[ind]], "_dbl_cntd$")
    ) %>%
    dplyr::select(!!ind, !!contribution) %>%
    dplyr::mutate(dplyr::across(!!contribution, ~ . / 1000))

  hpop_billion_contribution_pct <- df %>%
    dplyr::filter(
      .data[[year]] == max(end_year),
      stringr::str_detect(.data[[ind]], "^hpop_healthier$")
    ) %>%
    dplyr::select(!!ind, !!contribution_pct) %>%
    dplyr::rename(!!sym(contribution) := .data[[contribution_pc]])

  hpop_billion_contribution <- dplyr::bind_rows(hpop_billion_contribution, hpop_billion_contribution_pct) %>%
    dplyr::select(-!!ind)

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = vec2emptyDF(c("Contribution to Billion", "", "Corrected for Double Counting")),
    startCol = bounds["start_col"], startRow = bounds["start_row"], colNames = FALSE
  )

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = (c(
      "(All indicators)",
      "Newly healthier lives",
      "Newly unhealthier lives",
      "Contribution (population in thousands)",
      "% with healthier lives"
    )),
    startCol = bounds["start_col"], startRow = bounds["start_row"] + 1, colNames = FALSE
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = vec2emptyDF(c("Not corrected", "Corrected")),
    colNames = TRUE,
    startCol = bounds["start_col"] + 2,
    startRow = bounds["start_row"] + 1
  )
  contrib_thsd_col <- openxlsx::int2col(boxes_bounds$contribution["start_col"] + 2)
  tot_pop_thousands <- wppdistro::get_population(iso3 = iso, year = max(end_year)) / 1000
  openxlsx::writeFormula(wb,
    sheet = sheet_name,
    x = c(
      glue::glue('=SUMIF({contrib_thsd_col}{boxes_bounds$contribution["start_row"]+3}:{contrib_thsd_col}{boxes_bounds$contribution["end_row"]},">0")'),
      glue::glue('=SUMIF({contrib_thsd_col}{boxes_bounds$contribution["start_row"]+3}:{contrib_thsd_col}{boxes_bounds$contribution["end_row"]},"<0")'),
      glue::glue("={openxlsx::int2col(bounds['start_col']+2)}{bounds['start_row']+2}+{openxlsx::int2col(bounds['start_col']+2)}{bounds['start_row']+3}"),
      glue::glue("={contrib_thsd_col}{bounds['end_row']-1}/{tot_pop_thousands}*100")
    ),
    startCol = bounds["start_col"] + 2,
    startRow = bounds["start_row"] + 2,
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = hpop_billion_contribution,
    startCol = bounds["start_col"] + 3,
    startRow = bounds["start_row"] + 2,
    colNames = FALSE
  )

  wb <- style_hpop_billion_contribution(wb, sheet_name, bounds)

  return(wb)
}
