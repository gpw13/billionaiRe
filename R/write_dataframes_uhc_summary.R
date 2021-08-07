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


