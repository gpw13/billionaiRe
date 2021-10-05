#' Gets data type of data frame for styling
#'
#' Gets the type of data for styling purposes. Can be either:
#' * numeric
#' * integer
#' * Date
#' * character
#' * chr_type
#' Used to pass appropriate parameters to `style_data()`
#'
#' @param df data frame to be tested
#' @return character vector with the data_type of data frame columns
get_data_type <- function(df) {
  purrr::map_chr(df, get_data_type_single)
}

#' Gets data type for styling
#'
#' Gets the type of data for styling purposes. Can be either:
#' * numeric
#' * integer
#' * Date
#' * character
#' * chr_type
#' * formula
#' Used to pass appropriate parameters to `style_data()`
#'
#' @param vec vector to be tested
get_data_type_single <- function(vec) {
  if (sum(vec %in% c("imputed", "estimated", "projected", "reported", NA)) == length(vec)) {
    type <- "chr_type"
  } else if (sum(grepl("^=", vec), is.na(vec)) == length(vec)) {
    type <- "formula"
  } else {
    type <- class(vec)
  }
  type[length(type)]
}


#' Style data according to its type
#'
#' @param  data_type character vector of class(es) of the data to be styled.
#' Can be one of "numeric", "integer", "Date", "character", "chr_type",
#' or "formula".
#' @inheritParams write_baseline_projection_hpop_summary
#' @inheritParams openxlsx::addStyle
#' @inheritParams style_data_single
#' @inheritParams style_uhc_pillar
style_data <- function(data_type,
                       wb,
                       sheet_name,
                       rows,
                       cols,
                       no_show = FALSE,
                       no_show_row = 0,
                       fade = FALSE,
                       fade_row = 0) {
  assert_same_length(data_type, cols)

  for (i in seq_along(data_type)) {
    style_data_single(
      data_type = data_type[i], wb, sheet_name, rows,
      col = cols[i],
      no_show = no_show, no_show_row = no_show_row,
      fade = fade, fade_row = fade_row
    )
  }

  return(wb)
}

#' Style data single data column according to its type
#'
#' @param data_type character vector of class(es) of the data to be styled.
#' Can be one of "numeric", "integer", "Date", "character", or "chr_type"
#' @inheritParams write_baseline_projection_hpop_summary
#' @inheritParams openxlsx::addStyle
#' @inheritParams style_uhc_pillar
#' @inheritParams style_hep_pillar
#' @param data_type character vector of class(es) of the data to be styled.
#' Can be one of "numeric", "integer", "Date", "character", or "chr_type"
#' @param col Column to apply style to.
style_data_single <- function(data_type = c("numeric", "integer", "Date", "character", "chr_type", "formula"),
                              wb,
                              sheet_name,
                              rows,
                              col,
                              no_show = FALSE,
                              no_show_row = 0,
                              fade = TRUE,
                              fade_row = 0) {
  data_type <- rlang::arg_match(data_type)

  if (data_type %in% c("numeric", "formula")) {
    row_style <- excel_styles(type_data = "numeric", style_category = "data")
    final_row_style <- excel_styles(type_data = "numeric", style_category = "data", border = "bottom", borderColour = "black")
    row_style_no_show <- excel_styles(type_data = "numeric", style_category = "data", hide = TRUE)
    final_row_style_no_show <- excel_styles(type_data = "numeric", style_category = "data", border = "bottom", borderColour = "black", hide = TRUE)
    row_style_faded <- excel_styles(type_data = "numeric", style_category = "data", fontColour = "grey")
    final_row_style_faded <- excel_styles(type_data = "numeric", style_category = "data", border = "bottom", borderColour = "black", fontColour = "grey")
  } else if (data_type == "Date") {
    row_style <- excel_styles(type_data = "date", style_category = "data")
    final_row_style <- excel_styles(type_data = "date", style_category = "data", border = "bottom", borderColour = "black")
    row_style_no_show <- excel_styles(type_data = "date", style_category = "data", hide = TRUE)
    final_row_style_no_show <- excel_styles(type_data = "date", style_category = "data", border = "bottom", borderColour = "black", hide = TRUE)
    row_style_faded <- excel_styles(type_data = "date", style_category = "data", fontColour = "grey")
    final_row_style_faded <- excel_styles(type_data = "date", style_category = "data", border = "bottom", borderColour = "black", fontColour = "grey")
  } else if (data_type == "integer") {
    row_style <- excel_styles(type_data = "integer", style_category = "data")
    final_row_style <- excel_styles(type_data = "integer", style_category = "data", border = "bottom", borderColour = "black")
    row_style_no_show <- excel_styles(type_data = "integer", style_category = "data", hide = TRUE)
    final_row_style_no_show <- excel_styles(type_data = "integer", style_category = "data", border = "bottom", borderColour = "black", hide = TRUE)
    row_style_faded <- excel_styles(type_data = "integer", style_category = "data", fontColour = "grey")
    final_row_style_faded <- excel_styles(type_data = "integer", style_category = "data", border = "bottom", borderColour = "black", fontColour = "grey")
  } else if (data_type == "character") {
    row_style <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE
    )
    final_row_style <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, border = "bottom", borderColour = "black"
    )
    row_style_no_show <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, hide = TRUE
    )
    final_row_style_no_show <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, borderColour = "black", hide = TRUE
    )
    row_style_faded <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, fontColour = "grey"
    )
    final_row_style_faded <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, border = "bottom", borderColour = "black", fontColour = "grey"
    )
  } else if (data_type == "chr_type") {
    row_style <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, halign = "center"
    )
    final_row_style <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, border = "bottom", borderColour = "black", halign = "center"
    )
    row_style_no_show <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, hide = TRUE, halign = "center"
    )
    final_row_style_no_show <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, border = "bottom", borderColour = "black", hide = TRUE, halign = "center"
    )
    row_style_faded <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, fontColour = "grey", halign = "center"
    )
    final_row_style_faded <- excel_styles(
      type_data = "integer", style_category = "data",
      numFmt = "TEXT", wrapText = FALSE, border = "bottom", borderColour = "black", fontColour = "grey", halign = "center"
    )
  }
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = row_style,
    rows = rows,
    cols = col,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = final_row_style,
    rows = rows[length(rows)],
    cols = col,
    gridExpand = TRUE
  )
  if (no_show) {
    if (no_show_row == length(rows)) {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = final_row_style_no_show,
        rows = rows[no_show_row],
        cols = col,
        gridExpand = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = row_style_no_show,
        rows = rows[no_show_row],
        cols = col,
        gridExpand = TRUE
      )
    }
  }
  if (fade) {
    if (rows[length(rows)] %in% fade_row) {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = final_row_style_faded,
        rows = rows[length(rows)],
        cols = col,
        gridExpand = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb,
        sheet = sheet_name,
        style = row_style_faded,
        rows = fade_row,
        cols = col,
        gridExpand = TRUE
      )
    }
  }

  return(wb)
}
