#' Style data based on type of data
#'
#' @inherit write_main_df
#' @inherit export_country_summary_xls

style_data_type_timesseries <- function(df, wb, sheet_name, start_col, start_row, ind, type_col,year) {
  # HERE HERE HERE
  # Change font of time series values based on indicator (Bold:reported, normal:estimated, faded: imputed/projected)
  wide_df <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data[[ind]]) %>%
    tidyr::pivot_wider(-sym("value"), names_from = .data[[year]], values_from = .data[[type_col]]) %>%
    dplyr::select(-sym("transformed_value")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), tidyr::replace_na, ""))

  for (row in seq(nrow(wide_df))) {
    for (col in 2:(ncol(wide_df))) {
      if (wide_df[row, col] == "reported") {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = start_row + row-1,
          cols = start_col + col-1,
          style = excel_styles()$normal_data_wrapped_bold_dec
        )
      } else if (wide_df[row, col] %in% c("projected", "imputed")) {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = start_row + row-1,
          cols = col,
          style = excel_styles()$normal_data_wrapped_faded_dec
        )
      } else if(wide_df[row, col] == "estimated"){
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = start_row + row-1,
          cols = start_col + col-1,
          style = excel_styles()$normal_data_wrapped_dec
        )
      }else{
        wide_df[row, col] <- NA
      }
    }
  }
}


#' Create empty (no rows) data frame from a character vector
#'
#' Used in openxlsx-based function to go around limitation of writing only data
#' frames and thus in long. This allows to write in long.
#'
#' @param vec character vector

vec2emptyDF <- function(vec){
  stopifnot(is.character(vec))
  df <-data.frame(matrix(ncol = length(vec), nrow = 0))
  names(df) <- vec

  return(df)
}

#' Set column width based on type of column in data frame for openxlsx export
#'
#' @inherit write_main_df
#' @inherit transform_hpop_data
#'

get_col_width <- function(df, value, transform_value, type_col, source_col,
                          start_year, end_year){
  medium_width <- 24
  value_width <- 11
  source_width <- 49
  names_df <- names(df)
  value_regex <- c(glue::glue("^{value}_{start_year}$"),
                   glue::glue("^{value}_{max(end_year)}$"),
                   glue::glue("^{value}$"),
                   glue::glue("^{transform_value}_{start_year}$"),
                   glue::glue("^{transform_value}_{max(end_year)}$"),
                   glue::glue("^{transform_value}$"),
                   glue::glue("{type_col}"),
                   "year", "unit_transformed", "upload_date")
  value_cols <- sort(unlist(lapply(value_regex, grep, names_df)))
  names_df[value_cols] <- value_width

  medium_length_regex <- c("transformed_name",
                           glue::glue("^change_{transform_value}"),
                           glue::glue("^ind_contrib_change_{transform_value}"),
                           glue::glue("^ind_contrib_perc_change_{transform_value}"),
                                      "^count_", "population")
  medium_cols <- sort(unlist(lapply(medium_length_regex, grep, names_df)))
  names_df[medium_cols] <- medium_width

  source_cols <-sort(unlist(lapply(source_col, grep, names_df)))
  names_df[source_cols] <- source_width

  names_df <- as.numeric(names_df)

  assert_same_length(names_df[!is.na(names_df)],names(df))
  return(names_df)

}

#' Stores styles to be used in excel outputs
#'
#'@param font character name with the name of the font to use.
#'
excel_styles <- function(font = "Helvetica") {
  dark_blue_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 11,
    fontColour = "white",
    textDecoration = "bold",
    wrapText = T,
    fgFill = "#1E7FB8",
    valign = "center",
    halign = "left",
    border = c("top", "bottom", "left", "right"),
    borderStyle = "thin"
  )


  light_blue_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 9,
    textDecoration = "bold",
    wrapText = T,
    fgFill = "#B0CAD6",
    valign = "center",
    halign = "left",
    border = "bottom",
    borderStyle = "thin"
  )

  wrapped_h <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    wrapText = TRUE
  )

  bold <- openxlsx::createStyle(textDecoration = "bold")

  title <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 16
  )

  sub_title <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 12
  )

  normal_data_wrapped_dec <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_dec <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00",
    wrapText = TRUE
  )

  normal_data_dec <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00"
  )

  normal_data_wrapped_int <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_int <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_int <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0"
  )

  normal_data_wrapped_date <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "DATE",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_date <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "DATE",
    wrapText = TRUE
  )

  normal_data_date <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "DATE"
  )


  white_bckgrd <- openxlsx::createStyle(
    fontName = font,
    fgFill = "white",
    borderColour = "white"
  )

  vertical_txt <- openxlsx::createStyle(
    fontName = font,
    textRotation = 90,
    fontSize = 8,
    fgFill = "white",
    wrapText = TRUE,
    halign = "center",
    valign = "center"
  )
  normal_data_wrapped_faded_dec <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00",
    wrapText = TRUE,
    fontColour = "grey"
  )

  excel_styles <- list(
    title = title,
    sub_title = sub_title,
    bold = bold,
    light_blue_header = light_blue_header,
    dark_blue_header = dark_blue_header,
    wrapped_h = wrapped_h,
    white_bckgrd = white_bckgrd,
    normal_data_dec = normal_data_dec,
    normal_data_wrapped_dec = normal_data_wrapped_dec,
    normal_data_wrapped_bold_dec = normal_data_wrapped_bold_dec,
    normal_data_int = normal_data_int,
    normal_data_wrapped_int = normal_data_wrapped_int,
    normal_data_wrapped_bold_int = normal_data_wrapped_bold_int,
    normal_data_date = normal_data_date,
    normal_data_wrapped_date = normal_data_wrapped_date,
    normal_data_wrapped_bold_date = normal_data_wrapped_bold_date,
    vertical_txt = vertical_txt,
    normal_data_wrapped_faded_dec = normal_data_wrapped_faded_dec
  )

  return(excel_styles)
}
