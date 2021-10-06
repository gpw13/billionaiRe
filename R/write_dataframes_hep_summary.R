#' Write and style headers for data in HEP summary sheets
#'
#' Used in `write_hep_summary_sheet()`
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inheritParams  calculate_hpop_contributions

write_data_headers_hep_summary <- function(wb, sheet_name, value, boxes_bounds,
                                           start_year, end_year) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = vec2emptyDF(c("Indicator", "Sub-indicator")),
    startCol = boxes_bounds$data_header["start_col"],
    startRow = boxes_bounds$data_header["start_row"] + 2
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = "Latest Reported/Estimated Data Available",
    startCol = boxes_bounds$latest_reported_data["start_col"],
    startRow = boxes_bounds$data_header["start_row"]
  )

  sentence_v <- stringr::str_to_title(value)

  latest_rep_headers <- c(
    glue::glue("Raw {sentence_v}"),
    glue::glue("Level (1-5)"),
    "Year", "Type", "Source"
  ) %>%
    vec2emptyDF()

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = latest_rep_headers,
    startCol = boxes_bounds$latest_reported_data["start_col"],
    startRow = boxes_bounds$data_header["start_row"] + 1,
    colNames = TRUE
  )

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = glue::glue("{start_year} Baseline, and {max(end_year)} Projection"),
    startCol = boxes_bounds$baseline_projection_data["start_col"],
    startRow = boxes_bounds$baseline_projection_data["start_row"],
    colNames = FALSE
  )


  baseline_projections_headers <- vec2emptyDF(c(
    "Raw Value", rep("", length(value) * 2 - 1), "",
    "Level (1-5)", rep("", length(value) * 2 - 1), "",
    "Type", rep("", length(value) * 2 - 1), "",
    "Source"
  ))

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = baseline_projections_headers,
    startCol = boxes_bounds$baseline_projection_data["start_col"],
    startRow = boxes_bounds$data_header["start_row"] + 1,
    colNames = TRUE
  )

  openxlsx::writeData(wb, sheet_name,
    x = vec2emptyDF(c(
      rep(c(start_year, max(end_year), ""), 4)
    )),
    startRow = boxes_bounds$baseline_projection_data["start_row"] + 2,
    startCol = boxes_bounds$baseline_projection_data["start_col"],
    colNames = TRUE
  )

  wb <- style_data_headers_hep_summary(wb, sheet_name,
    boxes_bounds = boxes_bounds
  )

  return(wb)
}

#' Write and style data boxes in HEP summary data
#'
#' `write_data_boxes_hep_summary` writes and styles the data box in UHC summary
#' worksheet. Used in `write_hep_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inheritParams  write_sheet_header_hpop_summary
#' @inheritParams  style_uhc_pillar
#' @inheritParams  calculate_hpop_contributions
#' @inheritParams  transform_hpop_data

write_data_boxes_hep_summary <- function(df,
                                         pillar = c(
                                           "prepare",
                                           "prevent",
                                           "detect_respond"
                                         ),
                                         wb,
                                         sheet_name,
                                         value,
                                         transform_value,
                                         boxes_bounds,
                                         start_year,
                                         end_year,
                                         ind,
                                         ind_df,
                                         year,
                                         type_col,
                                         source_col,
                                         iso3,
                                         ind_ids) {
  pillar <- rlang::arg_match(pillar)

  ind_df_pillar <- ind_df %>%
    dplyr::filter(.data[["pillar"]] %in% !!pillar, .data[[ind]] != pillar)

  df_pillar <- df %>%
    dplyr::filter(.data[[ind]] %in% unique(ind_df_pillar[["ind"]]))

  if (nrow(df_pillar) == 0) {
    pillar_latest_reported <- tibble::tibble(
      !!sym(ind) := ind_df_pillar[["ind"]],
      !!sym(value) := NA,
      !!sym("level") := NA,
      !!sym(year) := NA,
      !!sym(type_col) := NA,
      !!sym(source_col) := NA
    )
    pillar_baseline_projection <- tibble::tibble(
      !!sym(ind) := ind_df_pillar[["ind"]],
      !!sym(iso3) := NA,
      !!sym(glue::glue("{value}_{start_year}")) := NA,
      !!sym(glue::glue("{value}_{max(end_year)}")) := NA,
      empty1 = NA,
      !!sym(glue::glue("level_{start_year}")) := NA,
      !!sym(glue::glue("level_{max(end_year)}")) := NA,
      empty2 = NA,
      !!sym(glue::glue("{type_col}_{start_year}")) := NA,
      !!sym(glue::glue("{type_col}_{max(end_year)}")) := NA,
      empty3 = NA,
      !!sym(glue::glue("{source_col}_{start_year}")) := NA,
      !!sym(glue::glue("{source_col}_{max(end_year)}")) := NA
    )
  } else {
    pillar_latest_reported <- df_pillar %>%
      get_latest_reported_df(iso3,
        ind,
        type_col,
        year,
        value,
        transform_value = NULL,
        source_col,
        level = "level",
        ind_df_pillar
      )

    full_df_pillar <- tidyr::expand_grid(
      !!sym(iso3) := unique(df_pillar[[iso3]]),
      !!sym(ind) := unique(df_pillar[[ind]]),
      !!sym(year) := start_year:max(end_year)
    )

    pillar_baseline_projection <- df_pillar %>%
      dplyr::full_join(full_df_pillar, by = c(iso3, ind, year)) %>%
      get_baseline_projection_df(iso3, ind, type_col, year, value,
        transform_value = "level", start_year, end_year,
        source_col, ind_df_pillar
      ) %>%
      dplyr::mutate(empty1 = NA, .after = glue::glue("{value}_{max(end_year)}")) %>%
      dplyr::mutate(empty2 = NA, .after = glue::glue("level_{max(end_year)}")) %>%
      dplyr::mutate(empty3 = NA, .after = glue::glue("{type_col}_{max(end_year)}"))
  }

  if (pillar == "prevent") {
    affected_pathos_iso3 <- billionaiRe::affected_pathogens %>%
      dplyr::filter(.data[[iso3]] %in% unique(df_pillar[[!!iso3]])) %>%
      dplyr::select(-.data[[iso3]])


    if (rowSums(affected_pathos_iso3) < ncol(affected_pathos_iso3)) {
      fade <- TRUE
      pathos_iso3 <- names(affected_pathos_iso3)[affected_pathos_iso3 == FALSE]
      data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):
      (boxes_bounds[[pillar]]["end_row"] - 1)
      fade_row <- data_rows[grep(
        paste0(pathos_iso3, collapse = "|"),
        pillar_latest_reported[[ind]]
      )]
    } else {
      fade <- FALSE
      fade_row <- NA
    }
  } else {
    fade <- FALSE
    fade_row <- NA
  }

  pillar_latest_reported <- dplyr::select(pillar_latest_reported, -.data[[ind]])

  pillar_baseline_projection <- dplyr::select(pillar_baseline_projection, -.data[[ind]], -.data[[iso3]])

  openxlsx::writeData(wb, sheet_name,
    x = pillar_latest_reported,
    startRow = boxes_bounds[[pillar]]["start_row"] + 1,
    startCol = boxes_bounds[[pillar]]["start_col"] + 2,
    colNames = FALSE
  )

  openxlsx::writeData(wb, sheet_name,
    x = pillar_baseline_projection,
    startRow = boxes_bounds[[pillar]]["start_row"] + 1,
    startCol = boxes_bounds$baseline_projection_data["start_col"],
    colNames = FALSE
  )

  raw_value_latest_col <- openxlsx::int2col(boxes_bounds$latest_reported_data["start_col"])
  level_latest_col <- openxlsx::int2col(boxes_bounds$latest_reported_data["start_col"] + 1)

  raw_value_start_year_baseline_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"])
  raw_value_end_year_baseline_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 1)


  if (pillar == "prepare") {
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(ROUND(AVERAGE({raw_value_latest_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_latest_col}{boxes_bounds[[pillar]]["end_row"]-1}),0), "")'),
      startCol = boxes_bounds$latest_reported_data["start_col"],
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(ROUND(AVERAGE({level_latest_col}{boxes_bounds[[pillar]]["start_row"]+1}:{level_latest_col}{boxes_bounds[[pillar]]["end_row"]-1}), 0), "")'),
      startCol = boxes_bounds$latest_reported_data["start_col"] + 1,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )

    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(ROUND(AVERAGE({raw_value_start_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_start_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}), 0), "")'),
      startCol = boxes_bounds$baseline_projection_data["start_col"],
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(ROUND(AVERAGE({raw_value_end_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_end_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}), 0),"")'),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 1,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
  } else {
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(AVERAGE({raw_value_latest_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_latest_col}{boxes_bounds[[pillar]]["end_row"]-1}), "")'),
      startCol = boxes_bounds$latest_reported_data["start_col"],
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(AVERAGE({level_latest_col}{boxes_bounds[[pillar]]["start_row"]+1}:{level_latest_col}{boxes_bounds[[pillar]]["end_row"]-1}), "")'),
      startCol = boxes_bounds$latest_reported_data["start_col"] + 1,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )

    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(AVERAGE({raw_value_start_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_start_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}), "")'),
      startCol = boxes_bounds$baseline_projection_data["start_col"],
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue('=IFERROR(AVERAGE({raw_value_end_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{raw_value_end_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}),"")'),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 1,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
  }

  level_start_year_baseline_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 3)
  level_end_year_baseline_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 4)

  openxlsx::writeFormula(wb, sheet_name,
    x = glue::glue('=IFERROR(AVERAGE({level_start_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{level_start_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}), "")'),
    startCol = boxes_bounds$baseline_projection_data["start_col"] + 3,
    startRow = boxes_bounds[[pillar]]["end_row"]
  )
  openxlsx::writeFormula(wb, sheet_name,
    x = glue::glue('=IFERROR(AVERAGE({level_end_year_baseline_col}{boxes_bounds[[pillar]]["start_row"]+1}:{level_end_year_baseline_col}{boxes_bounds[[pillar]]["end_row"]-1}),"")'),
    startCol = boxes_bounds$baseline_projection_data["start_col"] + 4,
    startRow = boxes_bounds[[pillar]]["end_row"]
  )

  wb <- style_hep_pillar(wb, sheet_name, boxes_bounds,
    data_type = list(
      latest_reported = get_data_type(pillar_latest_reported),
      baseline_projection = get_data_type(pillar_baseline_projection)
    ),
    pillar = pillar,
    fade = fade,
    fade_row = fade_row
  )

  return(wb)
}

#' Write and style summary box in HEP summary sheet
#'
#' `write_summary_box_hep_summary` writes and styles the summary box in HEP
#' summary worksheet. Used in `write_hep_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inherit style_uhc_pillar
#' @inheritParams  calculate_hpop_contributions
#'
write_summary_box_hep_summary <- function(wb,
                                          sheet_name,
                                          iso,
                                          start_year,
                                          end_year,
                                          boxes_bounds) {
  pillars <- c("prepare", "prevent", "detect_respond")
  col_raw_latest <- openxlsx::int2col(boxes_bounds[["latest_reported_data"]]["start_col"])
  col_raw_start_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"])
  col_raw_end_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"] + 1)

  hepi_formulas_baseline_proj <- c(
    glue::glue("=AVERAGE({paste0(purrr::map_chr(pillars, ~paste0(col_raw_start_year,boxes_bounds[[.x]]['end_row'])), collapse = ',')})"),
    glue::glue("=AVERAGE({paste0(purrr::map_chr(pillars, ~paste0(col_raw_end_year,boxes_bounds[[.x]]['end_row'])), collapse = ',')})")
  ) %>%
    as_excel_formula()

  openxlsx::writeData(wb,
    sheet_name,
    x = vec2emptyDF(hepi_formulas_baseline_proj),
    startCol = col_raw_start_year,
    startRow = boxes_bounds[["summary"]]["start_row"],
    colNames = FALSE
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    x = c(
      "HEPI",
      glue::glue("Change in HEPI over {start_year}-{max(end_year)}"),
      glue::glue("UN Population {max(end_year)} (thousand)"),
      "Country contribution to HEP billion target (population - thousand)",
      "% of country population newly protected from Health Emergencies"
    ),
    startCol = boxes_bounds[["summary"]]["start_col"] + 1,
    startRow = boxes_bounds[["summary"]]["start_row"],
    colNames = FALSE
  )

  col_latest_last <- openxlsx::int2col(boxes_bounds[["latest_reported_data"]]["end_col"])

  summary_formulas <- c(
    glue::glue("=AVERAGE({paste0(purrr::map_chr(pillars, ~paste0(col_raw_latest,boxes_bounds[[.x]]['end_row'])), collapse = ',')})"),
    glue::glue("={col_raw_end_year}{boxes_bounds[['summary']]['start_row']} - {col_latest_last}{boxes_bounds[['summary']]['start_row']}"),
    glue::glue("={col_latest_last}{boxes_bounds[['sheet_header']]['end_row']}*1000"),
    glue::glue("={col_latest_last}{boxes_bounds[['summary']]['end_row']-3}*{col_latest_last}{boxes_bounds[['summary']]['end_row']-2}/100"),
    glue::glue("={col_latest_last}{boxes_bounds[['summary']]['end_row']-1}/{col_latest_last}{boxes_bounds[['summary']]['end_row']-2}*100")
  ) %>%
    as_excel_formula()

  openxlsx::writeData(
    wb,
    sheet_name,
    x = summary_formulas,
    startCol = col_latest_last,
    startRow = boxes_bounds[["summary"]]["start_row"],
    colNames = FALSE
  )

  style_summary_box_hep_summary(
    wb,
    sheet_name,
    boxes_bounds
  )

  return(wb)
}
