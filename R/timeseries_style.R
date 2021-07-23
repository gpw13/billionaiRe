#' Change font of time series values
#'
#' `timeseries_style()` changes the font of time series values based on the
#' indicator type it is:
#'
#' * bold: reported
#' * normal: estimated
#' * faded: imputed or projected
#'
#' @param wb
#' @param iso
#' @param font_df
#' @param b_sheet
#'
#' @return
timeseries_style <- function(wb, iso, font_df, b_sheet) {
  c_font <- font_df %>% dplyr::filter(.data[["iso3"]] == !!iso)

  args <- list("type" = list("reported", c("projected", "imputed"), TRUE),
               "fontColour" = list(NULL, "red", NULL),
               "textDecoration" = list("bold", NULL, NULL))

  purrr::pwalk(args,
               type_styler,
               wb = wb,
               c_font = c_font)
}


#' Generate a style with variable color or bolding
#'
#' `add_style_wrapper()` adds a style for use within [timeseries_style()],
#' where the font color and decoration needs to vary based on data type. Wraps
#' around [openxlsx::addStyle()] and [openxlsx::createStyle()].
#'
#' @inheritParams timeseries_style
#' @param rows Rows to apply style to, passed to [openxlsx::add_style()].
#' @param cols Columns to apply style to, passed to [openxlsx::add_style()].
#' @param fontColour Font colour, passed to [openxlsx::createStyle()].
#' @param textDecoration Font decoration, passed to [openxlsx::createStyle()].
add_style_wrapper <- function(wb,
                              b_sheet,
                              rows,
                              cols,
                              fontColour,
                              textDecoration) {
  openxlsx::addStyle(
    wb,
    sheet = b_sheet,
    rows = rows,
    cols = cols,
    style = openxlsx::createStyle(
      fontColour = fontColour,
      textDecoration = textDecoration,
      fontSize = 8,
      halign = "left",
      wrapText = TRUE,
      numFmt = "0.00",
      border = "bottom"
    )
  )
}

#' @inheritParams add_style_wrapper
#' @inheritParams timeseries_style
type_styler <- function(wb,
                        b_sheet,
                        c_font,
                        type,
                        fontColour,
                        textDecoration) {
  # map across matching row/column values
  inds <- which(c_font == type, arr.ind = TRUE)
  inds_list <- split(inds[,2], inds[,1])
  purrr::iwalk(inds_list,
               ~ add_style_wrapper(
                 wb = wb,
                 b_sheet = b_sheet,
                 rows = as.numeric(.y) + 5,
                 cols = .x + 1,
                 fontColour = fontColour,
                 textDecoration = textDecoration
               ))
}
