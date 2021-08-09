#' Write and style headers for data in UHC summary sheets
#'
#' Used in `write_uhc_summary_sheet()`

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

#' Write

write_RMNCH_uhc_summary <- function(df, wb, sheet_name,value, transform_value, boxes_bounds, start_year, end_year,ind, ind_df, year, type_col,source_col, iso3){
  #TODO: Styling
  df_ind_rmnch <- ind_df %>%
    dplyr::filter(!!sym("pillar") == "RMNCH")

  df_rmnch <- df %>%
    dplyr::filter(.data[[ind]] %in% unique(df_ind_rmnch[["ind"]]))

  rmnch_latest_reported <- df_rmnch %>%
    get_latest_reported_df(iso3, ind, type_col, year, value, transform_value = NULL, source_col) %>%
    dplyr::mutate(empty = NA, .after = glue::glue("{value}")) %>%
    dplyr::select(-.data[[ind]])

  openxlsx::writeData(wb, sheet_name,
                      x = rmnch_latest_reported,
                      startRow = boxes_bounds$RMHCH["start_row"]+1,
                      startCol = boxes_bounds$RMHCH['start_col']+2,
                      colNames = FALSE)

  rmnch_baseline_projection <- df_rmnch %>%
    get_baseline_projection_df(iso3, ind, type_col, year, value, transform_value,start_year, end_year, source_col) %>%
    dplyr::mutate(dplyr::across(c(!!sym(glue::glue("transform_value_{start_year}")),!!sym(glue::glue("transform_value_{max(end_year)}"))) , ~ NA )) %>%
    dplyr::select(-.data[[ind]], -.data[[iso3]]) %>%
    dplyr::mutate(empty21 = NA, .after = "empty2") %>%
    dplyr::mutate(dir_change = NA, .after = "empty2")

  openxlsx::writeData(wb, sheet_name,
                      x = rmnch_baseline_projection,
                      startRow = boxes_bounds$RMHCH["start_row"]+1,
                      startCol = boxes_bounds$baseline_projection_data['start_col'],
                      colNames = FALSE
  )

  rmnch_rows <- boxes_bounds$RMHCH["start_row"]:(boxes_bounds$RMHCH["end_row"]-2)

  col_raw_end_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+1)
  col_raw_start_year <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col'])

  openxlsx::writeFormula(wb, sheet_name,
                         x = glue::glue("=if(OR(isblank({col_raw_end_year}{rmnch_rows}),isblank({col_raw_start_year}{rmnch_rows})),'',({col_raw_end_year}{rmnch_rows}-{col_raw_start_year}{rmnch_rows}"),
                         startRow = boxes_bounds$RMHCH["start_row"]+1,
                         startCol = boxes_bounds$baseline_projection_data['start_col']+6
  )
  raw_value_start_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col'])
  raw_value_end_year_col <- openxlsx::int2col(boxes_bounds$baseline_projection_data['start_col']+1)

  openxlsx::writeData(wb, sheet_name,
                      x = vec2emptyDF(c(glue::glue("=AVERAGE({raw_value_start_year_col}{boxes_bounds$RMHCH['start_row']+1}:{raw_value_start_year_col}{boxes_bounds$RMHCH['end_row']-1})"),
                               glue::glue("=AVERAGE({raw_value_end_year_col}{boxes_bounds$RMHCH['start_row']+1}:{raw_value_end_year_col}{boxes_bounds$RMHCH['end_row']-1})"))),
                      startCol = boxes_bounds$baseline_projection_data['start_col']+3,
                      startRow = boxes_bounds$RMHCH['end_row'],
                      colNames = FALSE
                      )

  wb <- style_uhc_pillar(wb, sheet_name, boxes_bounds = boxes_bounds,
                                data_type = list(latest_reported = get_data_type(rmnch_latest_reported),
                                              baseline_projection = get_data_type(rmnch_baseline_projection)),
                         pillar = "RMHCH"
  )

  return(wb)

}
