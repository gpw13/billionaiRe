#' Write and style headers for data in UHC summary sheets
#'
#' Used in `write_uhc_summary_sheet()`
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary

write_data_headers_uhc_summary <- function(wb, sheet_name, value, boxes_bounds, start_year, end_year){

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = vec2emptyDF(c("Tracer area", "Tracer Indicator")),
                      startCol = boxes_bounds$data_header['start_col'],
                      startRow = boxes_bounds$data_header['start_row']+2)

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = "Latest Reported/Estimated Data Available",
                      startCol = boxes_bounds$latest_reported_data['start_col'],
                      startRow = boxes_bounds$data_header['start_row'])

  sentence_v <- stringr::str_to_title(value)

  latest_rep_headers <-c(
    glue::glue("Raw {sentence_v}"),
    glue::glue("Transformed {sentence_v}"),
    "Year", "Type", "Source") %>%
    vec2emptyDF()

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = latest_rep_headers,
    startCol = boxes_bounds$latest_reported_data['start_col'],
    startRow = boxes_bounds$data_header["start_row"]+1,
    colNames = TRUE
  )

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = glue::glue("{start_year} Baseline, and {max(end_year)} Projection"),
                      startCol = boxes_bounds$baseline_projection_data["start_col"],
                      startRow = boxes_bounds$baseline_projection_data['start_row'],
                      colNames = FALSE)


  baseline_projections_headers <-vec2emptyDF(c("Raw Value", rep("", length(value)*2-1), "",
                                               "Transformed Value", rep("", length(value)*2-1), "",
                                               "Direction of change", "",
                                               "Type",rep("", length(value)*2-1), "",
                                               "Source"))

  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = baseline_projections_headers,
    startCol = boxes_bounds$baseline_projection_data['start_col'],
    startRow = boxes_bounds$data_header["start_row"]+1,
    colNames = TRUE
  )

  openxlsx::writeData(wb, sheet_name,
                      x = vec2emptyDF(c(rep(c(start_year, max(end_year), ""),2),
                            "","",
                            rep(c(start_year, max(end_year), ""),2))),
                      startRow = boxes_bounds$baseline_projection_data['start_row']+2,
                      startCol = boxes_bounds$baseline_projection_data['start_col'],
                      colNames = TRUE
  )

  wb <- style_data_headers_uhc_summary(wb, sheet_name, boxes_bounds = boxes_bounds)

  return(wb)
}

#' Write and style RMNCH box in UHC summary data
#'
#' `write_data_boxes_uhc_summary` writes and styles the RMNCH box in UHC summary worksheet.
#' Used in `write_uhc_summary_sheet()`.
#'
#' @inherit write_latest_reported_hpop_summary
#' @inherit write_sheet_header_hpop_summary
#' @inherit style_uhc_pillar

write_data_boxes_uhc_summary <- function(df, pillar = c("RMNCH", "infec_diseases", "ncd", "service_cap_access"), wb, sheet_name,value, transform_value, boxes_bounds, start_year, end_year,ind, ind_df, year, type_col,source_col, iso3){

  pillar <- rlang::arg_match(pillar)

  ind_df_pillar <- ind_df %>%
    dplyr::filter(!!sym("pillar") %in% !!pillar)

  df_pillar <- df %>%
    dplyr::filter(.data[[ind]] %in% unique(ind_df_pillar[["ind"]]))

  pillar_latest_reported <- df_pillar %>%
    get_latest_reported_df(iso3, ind, type_col, year, value, transform_value = NULL, source_col, ind_df_pillar)%>%
    dplyr::mutate(empty = "", .after = glue::glue("{value}"))

  pillar_data_rows <- (boxes_bounds[[pillar]]["start_row"]+1):(boxes_bounds[[pillar]]["end_row"]-1)

  col_raw_end_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+1)
  col_raw_start_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col'])

  pillar_baseline_projection <- df_pillar %>%
    get_baseline_projection_df(iso3, ind, type_col, year, value, transform_value,start_year, end_year, source_col, ind_df_pillar) %>%
    dplyr::mutate(dplyr::across(c(!!sym(glue::glue("transform_value_{start_year}")),!!sym(glue::glue("transform_value_{max(end_year)}"))) , ~ NA ))%>%
    dplyr::mutate(empty21 = NA, .after = "empty2") %>%
    dplyr::mutate(dir_change = as_excel_formula(glue::glue('=IF(OR(ISBLANK({col_raw_end_year}{pillar_data_rows}),ISBLANK({col_raw_start_year}{pillar_data_rows})),"",{col_raw_end_year}{pillar_data_rows}-{col_raw_start_year}{pillar_data_rows})')), .after = "empty2")

  if(pillar %in% c("RMNCH", "infec_diseases")){
    pillar_latest_reported <- dplyr::select(pillar_latest_reported,-.data[[ind]])
    pillar_baseline_projection <- dplyr::select(pillar_baseline_projection, -.data[[ind]], -.data[[iso3]])
    openxlsx::writeData(wb, sheet_name,
                        x = pillar_latest_reported,
                        startRow = boxes_bounds[[pillar]]["start_row"]+1,
                        startCol = boxes_bounds[[pillar]]['start_col']+2,
                        colNames = FALSE)

    openxlsx::writeData(wb, sheet_name,
                        x = pillar_baseline_projection ,
                        startRow = boxes_bounds[[pillar]]["start_row"]+1,
                        startCol = boxes_bounds$baseline_projection_data['start_col'],
                        colNames = FALSE
    )

    raw_value_start_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col'])
    raw_value_end_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+1)

    openxlsx::writeFormula(wb, sheet_name,
                      x = glue::glue("=AVERAGE({raw_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{raw_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
                      startCol = boxes_bounds$baseline_projection_data['start_col']+3,
                      startRow = boxes_bounds[[pillar]]['end_row']
    )
    openxlsx::writeFormula(wb, sheet_name,
                         x = glue::glue("=AVERAGE({raw_value_end_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{raw_value_end_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
                         startCol = boxes_bounds$baseline_projection_data['start_col']+4,
                         startRow = boxes_bounds[[pillar]]['end_row']
                         )

  }else if(pillar %in% c("ncd", "service_cap_access")){

    pillar_latest_reported <- pillar_latest_reported %>%
      dplyr::mutate(!!sym("empty"):= as_excel_formula(get_transform_formula(.data[[ind]], boxes_bounds$latest_reported_data['start_col'], pillar_data_rows))) %>%
      dplyr::select(-.data[[ind]])

    pillar_baseline_projection <- pillar_baseline_projection %>%
      dplyr::mutate(!!sym(glue::glue("transform_value_{start_year}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data['start_col'], pillar_data_rows),
                    !!sym(glue::glue("transform_value_{max(end_year)}")) := get_transform_formula(.data[[ind]], boxes_bounds$baseline_projection_data['start_col']+1, pillar_data_rows)) %>%
      dplyr::select(-.data[[ind]], -.data[[iso3]])

    openxlsx::writeData(wb, sheet_name,
                        x = pillar_latest_reported,-.data[[ind]],
                        startRow = boxes_bounds[[pillar]]["start_row"]+1,
                        startCol = boxes_bounds[[pillar]]['start_col']+2,
                        colNames = FALSE)

    openxlsx::writeData(wb, sheet_name,
                        x = pillar_baseline_projection,
                        startRow = boxes_bounds[[pillar]]["start_row"]+1,
                        startCol = boxes_bounds$baseline_projection_data['start_col'],
                        colNames = FALSE
    )

    transform_value_start_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+3)
    transform_value_end_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+4)

    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue("=AVERAGE({transform_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{transform_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
                           startCol = boxes_bounds$baseline_projection_data['start_col']+3,
                           startRow = boxes_bounds[[pillar]]['end_row']
    )
    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue("=AVERAGE({transform_value_start_year_col}{boxes_bounds[[pillar]]['start_row']+1}:{transform_value_start_year_col}{boxes_bounds[[pillar]]['end_row']-1})"),
                           startCol = boxes_bounds$baseline_projection_data['start_col']+4,
                           startRow = boxes_bounds[[pillar]]['end_row']
    )
  }
  # HERE HERE HERE function returns text type (numFmt in openxlsx::createStyle)
  # for formula, which breaks their proper formatting in Excel if somebody double click in the cell

  wb <- style_uhc_pillar(wb, sheet_name, boxes_bounds = boxes_bounds,
                         data_type = list(latest_reported = get_data_type(pillar_latest_reported),
                                          baseline_projection = get_data_type(pillar_baseline_projection)),
                         pillar = pillar
  )

  return(wb)
}
