#' Write and style notes
#'
#' `write_notes()` writes notes/footnotes provided in `df` to the appropriate
#' sheet.
#'
#' @inherit write_baseline_projection_hpop_summary params return
#' @param notes_title character string with the title (first line) of the notes.

write_notes <- function(df,
                        notes_title = "Notes:",
                        wb,
                        sheet_name,
                        bounds) {
  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = notes_title,
    startCol = bounds["start_col"],
    startRow = bounds["start_row"]
  )


  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = df,
    startCol = bounds["start_col"],
    startRow = bounds["start_row"] + 1,
    colNames = FALSE
  )

  wb <- style_notes(wb, sheet_name, bounds)

  return(wb)
}

#' Styles data worksheet notes
#'
#' @inherit style_hpop_indicators
#' @inherit write_baseline_projection_hpop_summary

style_notes <- function(wb, sheet_name, bounds) {
  for (i in (bounds["start_row"]:bounds["end_row"])) {
    mergeCellForced(wb,
      sheet = sheet_name,
      cols = bounds["start_col"]:bounds["end_col"],
      rows = i
    )
  }

  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(
      style_category = "normal_text",
      textDecoration = "bold"
    ),
    rows = bounds["start_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )

  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(style_category = "normal_text"),
    rows = (bounds["start_row"] + 1):bounds["end_row"],
    cols = bounds["start_col"]:bounds["end_col"],
    gridExpand = TRUE
  )

  return(wb)
}
