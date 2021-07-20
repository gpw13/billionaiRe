#' Write the main data frame to the data sheet
#'
#' @param data_table data frame to be written
#' @param wb openxlsx workbook to be edited.
#' @param start_row numeric start row of the table in the excel sheet
#' @param start_col numeric start row of the table in the excel sheet
#' @param sheet_name character name of the sheet to update
#' @inherit export_country_summary_xls params return
#'
write_main_df <-
  function(data_table,
           wb,
           start_row,
           start_col,
           start_year,
           end_year,
           sheet_name) {
    # Write column headers
    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = glue::glue("{start_year} Baseline, and {end_year} Projection"),
      startCol = start_row,
      startRow = start_col,
      colNames = FALSE
    )


    years_header <-c(c(start_year, end_year), "", rep(c(start_year, end_year), 3))
    years_df <-data.frame(matrix(ncol = length(years_header), nrow = 0))
    names(years_df) <- years_header

    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = years_df,
      startCol = start_row,
      startRow = start_col + 2,
      colNames = TRUE
    )

    contrib_headers <- c(
      glue::glue(
        "Change in Transformed Values over {start_year}-{end_year} (%)"
      ),
      glue::glue("UN Population {end_year}"),
      glue::glue("Contribution {end_year}"),
      glue::glue("Contribution {end_year} (% Total Population)")
    )

    contrib_headers_df <-
      data.frame(matrix(ncol = length(contrib_headers), nrow = 0))
    names(contrib_headers_df) <- contrib_headers

    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = contrib_headers_df,
      startCol = start_col + 10,
      startRow = start_row + 1,
      colNames = TRUE
    )

    # Write headers
    openxlsx::writeData(
      wb,
      sheet = "HPOPdata",
      x = data_table,
      startCol = start_col - 3,
      startRow = start_row + 3,
      colNames = FALSE
    )

    # Style header
    openxlsx::addStyle(
      wb,
      sheet = "HPOPdata",
      style = excel_styles()$dark_blue_header,
      rows = 6,
      cols = c(1:2, 4:12, 14:17, 19:25)
    )

    openxlsx::addStyle(
      wb,
      sheet = "HPOPdata",
      style = excel_styles()$light_blue_header,
      rows = 7,
      cols = c(1:2, 4:5, 7:12, 14:17, 19:25)
    )
    openxlsx::addStyle(
      wb,
      sheet = "HPOPdata",
      style = excel_styles()$light_blue_header,
      rows = 8,
      cols = c(1:2, 4:5, 7:12, 14:17, 19:25)
    )
    # Style data table
    openxlsx::addStyle(
      wb,
      sheet = "HPOPdata",
      style = excel_styles()$normal_data_wrapped,
      rows = c(9:(8 + nrow(data_table))),
      cols = c(1:25),
      gridExpand = TRUE
    )
    return(wb)
  }

#' Write the HPOP billion contribution box
#'
#' @inherit write_main_df

write_hpop_billion_contrib <- function(data_table,
                                       wb,
                                       start_row,
                                       start_col,
                                       start_year,
                                       end_year,
                                       sheet_name) {
  openxlsx::writeData(wb,
                      sheet = sheet_name, x = data_table,
                      startCol = start_col+2, startRow = start_row+2, colNames = FALSE
  )

  openxlsx::writeFormula(wb,
                         sheet = sheet_name,
                         glue::glue("=(P{start_row+4}/{wppdistro::get_population('iso3', year = end_year)})"),
                         startCol = start_col+2, startRow = start_row+5
  )

  openxlsx::writeData(wb,
                      sheet = sheet_name, x = data.frame(
                        c1 = c("Contribution to Billion", "(All indicators)"),
                        c2 = c(NA, NA),
                        c3 = c("Corrected for Double Counting", "No"),
                        c4 = c(NA, "Yes")
                      ),
                      startCol = start_col, startRow = start_row, colNames = FALSE
  )

  openxlsx::writeData(wb,
                      sheet = sheet_name, x = data.frame(c1 = c(
                        "Newly healthier lives",
                        "Newly unhealthier lives",
                        "Contribution",
                        "% Population with healthier lives"
                      )),
                      startCol = start_col, startRow = start_row + 2, colNames = FALSE
  )


  openxlsx::mergeCells(wb, sheet_name, cols = c(start_col:(start_col +1)), rows = start_row)
  openxlsx::mergeCells(wb, sheet_name, cols = c((start_col +2):(start_col +3)), rows = start_row)
  openxlsx::mergeCells(wb, sheet_name, cols = c(start_col:(start_col +1)), rows = start_row + 1)


  openxlsx::writeFormula(wb,
                         sheet = sheet_name,
                         x = glue::glue('=SUMIF(P9:P{start_row-2},">0")'),
                         startCol = start_col+2, startRow = start_row + 2
  )

  openxlsx::writeFormula(wb,
                         sheet = sheet_name,
                         x = glue::glue('=SUMIF(P9:P{start_row - 2},"<0")'),
                         startCol = start_col+2, startRow = start_row + 3
  )

  openxlsx::writeFormula(wb,
                         sheet = sheet_name,
                         x = glue::glue("=P{start_row + 2}+P{start_row + 3}"),
                         startCol = start_col+2, startRow = start_row+4
  )

  return(wb)


}

#' Write notes for data sheet
#'
#' @inherit write_main_df params return

write_notes_data <- function(data_table,
                             wb,
                             start_row,
                             start_col,
                             start_year,
                             end_year,
                             sheet_name){


}
