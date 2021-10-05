#' Write and style headers for data in UHC summary sheets
#'
#' Used in `write_uhc_summary_sheet()`
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inheritParams  calculate_hpop_contributions


write_data_headers_uhc_summary <- function(wb, sheet_name, value, boxes_bounds, start_year, end_year) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = vec2emptyDF(c("Tracer area", "Tracer Indicator")),
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
    glue::glue("Transformed {sentence_v}"),
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
    "Transformed Value", rep("", length(value) * 2 - 1), "",
    "Direction of change", "",
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
      rep(c(start_year, max(end_year), ""), 2),
      "", "",
      rep(c(start_year, max(end_year), ""), 2)
    )),
    startRow = boxes_bounds$baseline_projection_data["start_row"] + 2,
    startCol = boxes_bounds$baseline_projection_data["start_col"],
    colNames = TRUE
  )

  wb <- style_data_headers_uhc_summary(wb, sheet_name, boxes_bounds = boxes_bounds)

  return(wb)
}

#' Write and style data boxes in UHC summary data
#'
#' `write_data_boxes_uhc_summary` writes and styles the data box in UHC summary worksheet.
#' Used in `write_uhc_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inheritParams  write_sheet_header_hpop_summary
#' @inheritParams  style_uhc_pillar
#' @inheritParams  calculate_hpop_contributions
#' @inheritParams  transform_hpop_data

write_data_boxes_uhc_summary <- function(df,
                                         pillar = c("RMNCH", "infec_diseases", "ncd", "service_cap_access"),
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

  this_iso3 <- unique(df[[iso3]])

  ind_df_pillar <- ind_df %>%
    dplyr::filter(.data[["pillar"]] %in% !!pillar)

  df_pillar <- df %>%
    dplyr::filter(.data[[ind]] %in% unique(ind_df_pillar[["ind"]]))

  pillar_latest_reported <- df_pillar %>%
    get_latest_reported_df(
      iso3 = iso3, ind = ind, type_col = type_col,
      year = year, value,
      transform_value = transform_value,
      level = NULL, source_col = source_col,
      ind_df = ind_df_pillar
    )

  pillar_data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):(boxes_bounds[[pillar]]["end_row"] - 1)

  col_raw_end_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 1)
  col_raw_start_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"])

  pillar_baseline_projection <- df_pillar %>%
    get_baseline_projection_df(iso3, ind, type_col, year, value, transform_value, start_year, end_year, source_col, ind_df_pillar) %>%
    dplyr::mutate(dplyr::across(c(.data[[glue::glue("{transform_value}_{start_year}")]], .data[[glue::glue("{transform_value}_{max(end_year)}")]]), ~NA)) %>%
    dplyr::mutate(empty1 = NA, .after = glue::glue("{value}_{max(end_year)}")) %>%
    dplyr::mutate(empty2 = NA, .after = glue::glue("{transform_value}_{max(end_year)}")) %>%
    dplyr::mutate(empty3 = NA, .after = glue::glue("{type_col}_{max(end_year)}")) %>%
    dplyr::mutate(empty21 = NA, .after = "empty2") %>%
    dplyr::mutate(dir_change = as_excel_formula(glue::glue('=IF(OR(ISBLANK({col_raw_end_year}{pillar_data_rows}),ISBLANK({col_raw_start_year}{pillar_data_rows})),"",{col_raw_end_year}{pillar_data_rows}-{col_raw_start_year}{pillar_data_rows})')), .after = "empty2")

  if (pillar == "infec_diseases") {
    no_show <- ifelse(sum(df_pillar[df_pillar[, "ind"] == ind_ids["art"], "use_dash"]) %in% c(0, NA), TRUE, FALSE)
    no_show_row <- grep(ind_ids["art"], pillar_latest_reported[[ind]])
  } else {
    no_show <- FALSE
    no_show_row <- 1
  }

  if (pillar %in% c("RMNCH", "infec_diseases")) {
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

    raw_value_start_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"])
    raw_value_end_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 1)

    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue("=AVERAGE({raw_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{raw_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 3,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue("=AVERAGE({raw_value_end_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{raw_value_end_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 4,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
  } else if (pillar %in% c("ncd", "service_cap_access")) {
    pillar_latest_reported <- pillar_latest_reported %>%
      dplyr::mutate(
        !!sym(glue::glue("{transform_value}")) := as_excel_formula(get_transform_formula(.data[[ind]], boxes_bounds$latest_reported_data["start_col"], pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3))
      ) %>%
      dplyr::select(-.data[[ind]])

    pillar_baseline_projection <- pillar_baseline_projection %>%
      dplyr::mutate(
        !!sym(glue::glue("{transform_value}_{start_year}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data["start_col"], pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3),
        !!sym(glue::glue("{transform_value}_{max(end_year)}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data["start_col"] + 1, pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3)
      ) %>%
      dplyr::select(-.data[[ind]], -.data[[iso3]])

    openxlsx::writeData(wb, sheet_name,
      x = pillar_latest_reported, -.data[[ind]],
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

    transform_value_start_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 3)
    transform_value_end_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 4)

    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue("=AVERAGE({transform_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{transform_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 3,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
    openxlsx::writeFormula(wb, sheet_name,
      x = glue::glue("=AVERAGE({transform_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{transform_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
      startCol = boxes_bounds$baseline_projection_data["start_col"] + 4,
      startRow = boxes_bounds[[pillar]]["end_row"]
    )
  }

  wb <- style_uhc_pillar(wb, sheet_name,
    boxes_bounds = boxes_bounds,
    data_type = list(
      latest_reported = get_data_type(pillar_latest_reported),
      baseline_projection = get_data_type(pillar_baseline_projection)
    ),
    pillar = pillar,
    no_show = no_show,
    no_show_row = no_show_row
  )

  if (!no_show) {
    openxlsx::protectWorksheet(wb, sheet_name, protect = TRUE, password = "GPW13password")
  }

  return(wb)
}

#' Write and style ASC box in UHC summary data
#'
#' `write_asc_uhc_data_summary` writes and styles the ASC box in UHC summary worksheet.
#' Used in `write_uhc_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inherit style_uhc_pillar
#' @inheritParams  calculate_hpop_contributions
write_asc_uhc_data_summary <- function(df,
                                       pillar,
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
  ind_df_pillar <- ind_df %>%
    dplyr::filter(.data[["pillar"]] %in% !!pillar)

  df_pillar <- df %>%
    dplyr::filter(.data[[ind]] %in% unique(ind_df_pillar[["ind"]]))

  this_iso3 <- unique(df[[iso3]])

  col_trans_start_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"] + 3)
  col_trans_end_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"] + 4)

  rows_averages_data_boxes <- purrr::map(
    c("RMNCH", "infec_diseases", "ncd", "service_cap_access"),
    function(x) {
      boxes_bounds[[x]]["end_row"]
    }
  ) %>% unlist()

  averages_cells_start_year <- paste0(col_trans_start_year, rows_averages_data_boxes, collapse = ",")

  openxlsx::writeFormula(wb, sheet_name,
    x = glue::glue("=AVERAGE({averages_cells_start_year})"),
    startCol = col_trans_start_year,
    startRow = boxes_bounds[[pillar]]["start_row"]
  )

  averages_cells_end_year <- paste0(col_trans_end_year, rows_averages_data_boxes, collapse = ",")

  openxlsx::writeFormula(wb, sheet_name,
    x = glue::glue("=AVERAGE({averages_cells_end_year})"),
    startCol = col_trans_end_year,
    startRow = boxes_bounds[[pillar]]["start_row"]
  )
  pillar_data_rows <- (boxes_bounds[[pillar]]["start_row"] + 1):(boxes_bounds[[pillar]]["end_row"])


  pillar_latest_reported <- df_pillar %>%
    get_latest_reported_df(
      iso3 = iso3, ind = ind, type_col = type_col,
      year = year, value,
      transform_value = transform_value,
      level = NULL, source_col = source_col,
      ind_df = ind_df_pillar
    ) %>%
    dplyr::mutate(
      !!sym(glue::glue("{transform_value}")) := get_transform_formula(.data[[ind]], boxes_bounds$latest_reported_data["start_col"], pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3),
    ) %>%
    dplyr::select(-.data[[ind]])

  col_raw_end_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"] + 1)
  col_raw_start_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data["start_col"])


  pillar_baseline_projection <- df_pillar %>%
    get_baseline_projection_df(iso3, ind, type_col, year, value, transform_value, start_year, end_year, source_col, ind_df_pillar) %>%
    dplyr::mutate(
      !!sym(glue::glue("{transform_value}_{start_year}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data["start_col"], pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3),
      !!sym(glue::glue("{transform_value}_{max(end_year)}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data["start_col"] + 1, pillar_data_rows, ind_ids = ind_ids, iso3 = this_iso3)
    ) %>%
    dplyr::mutate(empty1 = NA, .after = glue::glue("{value}_{max(end_year)}")) %>%
    dplyr::mutate(empty2 = NA, .after = glue::glue("{transform_value}_{max(end_year)}")) %>%
    dplyr::mutate(empty3 = NA, .after = glue::glue("{type_col}_{max(end_year)}")) %>%
    dplyr::mutate(empty21 = NA, .after = "empty2") %>%
    dplyr::mutate(dir_change = as_excel_formula(glue::glue('=IF(OR(ISBLANK({col_trans_end_year}{pillar_data_rows}),ISBLANK({col_trans_start_year}{pillar_data_rows})),"",{col_trans_end_year}{pillar_data_rows}-{col_trans_start_year}{pillar_data_rows})')), .after = "empty2") %>%
    dplyr::select(-.data[[iso3]], -.data[[ind]])

  openxlsx::writeData(wb, sheet_name,
    x = pillar_latest_reported,
    startRow = boxes_bounds[[pillar]]["start_row"] + 1,
    startCol = boxes_bounds[["latest_reported_data"]]["start_col"],
    colNames = FALSE
  )
  openxlsx::writeData(wb, sheet_name,
    x = pillar_baseline_projection,
    startRow = boxes_bounds[[pillar]]["start_row"] + 1,
    startCol = boxes_bounds[["baseline_projection_data"]]["start_col"],
    colNames = FALSE
  )

  # TODO: make dynamic
  projected <- grep("projected", pillar_baseline_projection)

  if (length(projected) == 2) {
    no_show_cols <- boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"]
  } else if (projected == 9) {
    no_show_cols <- (boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"])[c(1, 4, 9, 12)]
  } else if (projected == 10) {
    no_show_cols <- (boxes_bounds[["baseline_projection_data"]]["start_col"]:boxes_bounds[["baseline_projection_data"]]["end_col"])[c(2, 5, 10, 13)]
  }

  style_asc_uhc_data_summary(wb,
    sheet_name,
    pillar = pillar,
    boxes_bounds,
    data_type = list(
      latest_reported = get_data_type(pillar_latest_reported),
      baseline_projection = get_data_type(pillar_baseline_projection)
    ),
    no_show = TRUE,
    no_show_cols = no_show_cols,
    no_show_rows = pillar_data_rows
  )


  return(wb)
}



#' Write and style summary box in UHC summary sheet
#'
#' `write_summary_box_uhc_summary` writes and styles the summary box in UHC
#' summary worksheet. Used in `write_uhc_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inherit style_uhc_pillar
#' @inheritParams  calculate_hpop_contributions
#'
write_summary_box_uhc_summary <- function(wb,
                                          sheet_name,
                                          iso,
                                          start_year,
                                          end_year,
                                          boxes_bounds) {
  col_trans_start_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"] + 3)
  col_trans_end_year <- openxlsx::int2col(boxes_bounds[["baseline_projection_data"]]["start_col"] + 4)

  UHC_SM_formulas <- c(
    glue::glue("={col_trans_start_year}{boxes_bounds[['baseline_projection_data']]['start_row']} * {col_trans_start_year}{boxes_bounds[['baseline_projection_data']]['end_row']}/100"),
    glue::glue("={col_trans_end_year}{boxes_bounds[['baseline_projection_data']]['start_row']} * {col_trans_end_year}{boxes_bounds[['baseline_projection_data']]['end_row']}/100")
  ) %>%
    as_excel_formula()

  openxlsx::writeData(wb,
    sheet_name,
    x = vec2emptyDF(UHC_SM_formulas),
    startCol = col_trans_start_year,
    startRow = boxes_bounds[["summary"]]["start_row"],
    colNames = FALSE
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    x = c(
      "UHC single measure",
      glue::glue("Change in UHC single measure over {start_year}-{max(end_year)}"),
      glue::glue("UN Population {max(end_year)} (thousand)"),
      "Country contribution to UHC billion target (population - thousand)",
      "% of country population newly covered by universal healthcare"
    ),
    startCol = boxes_bounds[["summary"]]["start_col"] + 1,
    startRow = boxes_bounds[["summary"]]["start_row"],
    colNames = FALSE
  )

  summary_formulas <- c(
    glue::glue("={col_trans_end_year}{boxes_bounds[['summary']]['start_row']}-{col_trans_start_year}{boxes_bounds[['summary']]['start_row']}"),
    glue::glue("={openxlsx::int2col(boxes_bounds[['sheet_header']]['start_col']+4)}{boxes_bounds[['sheet_header']]['end_row']}*1000"),
    glue::glue("={col_trans_end_year}{boxes_bounds[['summary']]['start_row']+1}*{col_trans_end_year}{boxes_bounds[['summary']]['start_row']+2}/100"),
    glue::glue("={col_trans_end_year}{boxes_bounds[['summary']]['start_row']+3}/{col_trans_end_year}{boxes_bounds[['summary']]['start_row']+2}*100")
  ) %>%
    as_excel_formula()

  openxlsx::writeData(
    wb,
    sheet_name,
    x = summary_formulas,
    startCol = col_trans_end_year,
    startRow = boxes_bounds[["summary"]]["start_row"] + 1,
    colNames = FALSE
  )

  style_summary_box_uhc_summary(
    wb,
    sheet_name,
    boxes_bounds
  )

  return(wb)
}
