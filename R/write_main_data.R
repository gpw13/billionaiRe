#' Write the main data frame to the data sheet
#'
#' @param df data frame to be written
#' @param wb openxlsx workbook to be edited.
#' @param start_row numeric start row of the table in the excel sheet
#' @param start_col numeric start row of the table in the excel sheet
#' @param sheet_name character name of the sheet to update
#' @inherit export_country_summary_xls params return
#'
write_main_df <-
  function(df,
           wb,
           start_row,
           start_col,
           start_year,
           end_year,
           sheet_name,
           value,
           transform_value,
           type_col,
           source_col) {


    start_row_subH <- start_row + 1
    start_row_subH_low <- start_row + 2
    start_row_data <- start_row + 3

    n_value <- length(value)

    # Write column headers
    ## Indicators headers
    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Indicators",
      startCol = start_col, startRow = start_row, colNames = FALSE
    )

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = vec2emptyDF(c("Indicator transformed", "Unit transformed")),
      startCol = start_col, startRow = start_row_subH, colNames = TRUE
    )

    ## Baseline and projections headers
    start_col_baseline <- start_col + 2

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = glue::glue("{start_year} Baseline, and {max(end_year)} Projection"),
      startCol = start_col_baseline, startRow = start_row, colNames = FALSE
    )

    baseline_proj_header <- vec2emptyDF(c("Raw Value", rep("", n_value*2-1),
                                          "Transform Value", rep("", n_value*2-1),
                                          "Type", "",
                                          "Source", "",
                                          "Last update"))

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = baseline_proj_header,
      startCol = start_col_baseline, startRow = start_row_subH, colNames = TRUE
    )

    ### Baseline and proj years sub-header
    sentence_v <- stringr::str_to_title(value)
    sentence_v_syear <- glue::glue("{sentence_v} {start_year}")
    sentence_v_eyear <- glue::glue("{sentence_v} {max(end_year)}")
    paired_list_sentences <- unlist(lapply(sentence_v_syear, function(a) lapply(sentence_v_eyear, function (b) c(a, b))), recursive=FALSE)
    paired_list_sentences <- vec2emptyDF(unlist(paired_list_sentences[seq(1, length(paired_list_sentences), n_value+1)]))

    start_col_baseline_trans <- start_col_baseline + length(paired_list_sentences)
    start_col_baseline_type <- start_col_baseline_trans +  length(paired_list_sentences)

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = paired_list_sentences,
      startCol = start_col_baseline, startRow = start_row_subH_low, colNames = TRUE
    )

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = paired_list_sentences,
      startCol = start_col_baseline_trans, startRow = start_row_subH_low, colNames = TRUE
    )

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = vec2emptyDF(as.character(rep(c(start_year, max(end_year)),2))),
      startCol = start_col_baseline_type, startRow = start_row_subH_low, colNames = TRUE
    )

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Last update",
      startCol = start_col_baseline_type + 4, startRow = start_row_subH_low
    )

    ## Billion contribution header
    start_col_contrib <- start_col_baseline_type + 5

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Contribution to the Billion",
      startCol = start_col_contrib, startRow = start_row, colNames = FALSE
    )

    ### Billion contribution sub-header
    contrib_headers <- c(
      glue::glue(
        "Change in Transformed {sentence_v} over {start_year}-{max(end_year)} (%)"
      ),
      glue::glue("UN Population {max(end_year)}"),
      glue::glue("Contribution {sentence_v} {max(end_year)}"),
      glue::glue("Contribution {sentence_v} {max(end_year)} (% Total Population)")
    ) %>% vec2emptyDF()

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = contrib_headers,
      startCol = start_col_contrib, startRow = start_row_subH, colNames = TRUE
    )

    ## Latest reported header
    start_col_latest <- start_col_contrib + length(contrib_headers)

    openxlsx::writeData(
      wb, sheet = sheet_name,
      x = "Latest Reported/Estimated Data Available",
      startCol = start_col_latest, startRow = start_row, colNames = FALSE
    )

    latest_rep_headers <-c(
      glue::glue("Raw {sentence_v}"),
      glue::glue("Transformed {sentence_v}"),
      "Year", "Type", "Source",
      "Number of values reported/estimated (since 2000)",
      "Number of values reported/estimated (since 2012)") %>%
      vec2emptyDF()

    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = latest_rep_headers,
      startCol = start_col_latest,
      startRow = start_row_subH,
      colNames = TRUE
    )

    # Write df
    openxlsx::writeData(
      wb,
      sheet = sheet_name,
      x = df,
      startCol = start_col,
      startRow = start_row_data,
      colNames = FALSE
    )

    style_hpop_main_data(df, wb, sheet_name,
                    start_row,
                    start_col,
                    length_baseline_header = length(paired_list_sentences),
                    length_contrib_header = length(contrib_headers))

    openxlsx::setColWidths(
      wb,
      sheet = sheet_name,
      cols = start_col:ncol(df),
      widths = get_col_width(df, value, transform_value, type_col, source_col,
                             start_year, end_year),
      ignoreMergedCells = FALSE
    )

    return(wb)
  }
