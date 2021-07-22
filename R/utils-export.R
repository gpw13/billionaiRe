#' Style data based on type of data
#'
#' @inherit write_main_df
#' @inherit export_country_summary_xls

style_data_type <- function(df, wb, iso, sheet_name, start_col, start_row, type_col) {
  # HERE HERE HERE
  # Change font of time series values based on indicator (Bold:reported, normal:estimated, faded: imputed/projected)

  c_font <- df %>% dplyr::filter(.data[["iso3"]] == !!iso)
  col <- colnames(df)


  for (j in 2:length(col)) {
    for (i in 1:dim(c_font)[1]) {
      font <- c_font[i, j]

      col <- j + 1
      row <- i + 5
      if (font == "reported") {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = row,
          cols = col,
          style = openxlsx::createStyle(
            fontSize = 8,
            textDecoration = "bold",
            halign = "left",
            wrapText = TRUE,
            numFmt = "0.00",
            border = "bottom"
          )
        )

      } else if (font == "estimated") {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = row,
          cols = col,
          style = openxlsx::createStyle(
            fontSize = 8,
            halign = "left",
            wrapText = TRUE,
            numFmt = "0.00",
            border = "bottom"
          )
        )

      } else if (font %in% c("projected", "imputed")) {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = row,
          cols = col,
          style = openxlsx::createStyle(
            fontSize = 8,
            fontColour = "red",
            halign = "left",
            wrapText = TRUE,
            numFmt = "0.00",
            border = "bottom"
          )
        )
      } else {
        openxlsx::addStyle(
          wb,
          sheet = sheet_name,
          rows = row,
          cols = col,
          style = openxlsx::createStyle(
            fontSize = 8,
            halign = "left",
            wrapText = TRUE,
            numFmt = "0.00",
            border = "bottom"
          )
        )
      }
    }
  }
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
    vertical_txt = vertical_txt
  )

  return(excel_styles)
}
#' Style data according to its type
#'
#' @inheritParams write_main_df
#' @inheritParams openxlsx::addStyle
#'
style_data <- function(df, wb, sheet_name ,
                       rows,
                       cols){

  for(i in seq(ncol(df))){
    if(dplyr::type_sum(df[[i]]) == "dbl"){
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_dec,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
    }else if(dplyr::type_sum(df[[i]]) == "date"){
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_date,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
    }else{
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = excel_styles()$normal_data_wrapped_int,
        rows = rows,
        cols = cols[i],
        gridExpand = TRUE
      )
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
