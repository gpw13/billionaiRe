#' Create basic Excel Styles and provides a way to modify them
#'
#' `excel_styles` creates a series of styles used across the package and provides
#' a way to modify them to increase the performance of the styling functions.
#'
#' @param billion Billion to be used for billion styling when relevant: either
#' "hep", "hpop", "uhc", or NULL when no billion to be applied.
#' @param style_category category of style to be used. Can be either:
#' * "title"
#' * "subtitle"
#' * "datatable_header": for column headers of data tables
#' * "sub_datatable_header": for sub-headers of data tables
#' * "data" : for standard data points
#' * "normal_text"
#' * NULL when no category applies
#' @param type_data type of data. Can be either "date", "numeric", or "integer".
#' @param fontName character name with the name of the font to use.
#' @param billion_fgFill character with the type of billion color to be used. Passed
#' to [openxlsx::createStyle()] fgFill parameter.
#' @param hide bolean. Set fontColor to white and hidden to TRUE in [openxlsx::createStyle()]
#' @param ... additional parameters to be passed to [openxlsx::createStyle()].
#' Overrides any of the parameter passed by previous parameters.
#'
#' @seealso \code{\link[openxlsx]{createStyle}}
excel_styles <- function(billion = NULL,
                         style_category = NULL,
                         type_data = NULL,
                         fontName = "Calibri",
                         billion_fgFill = NULL,
                         hide = FALSE,
                         ...) {
  assert_style_param(...)

  assert_in_list_or_null(billion, c("hpop", "hep", "uhc"))
  assert_in_list_or_null(style_category, c("title", "subtitle", "datatable_header", "sub_datatable_header", "data", "normal_text"))
  assert_in_list_or_null(type_data, c("date", "numeric", "integer"))
  assert_in_list_or_null(billion_fgFill, c("main", "light", "light2"))

  billion_color_list <- list(
    uhc_main = "#00A173", uhc_light = "#66C6AB", uhc_light2 = "#A3DCCC",
    hep_main = "#002D5F", hep_light = "#66819f", hep_light2 = "#D1D9EB",
    hpop_main = "#008DCA", hpop_light = "#B2DCEF", hpop_light2 = "#B2DCEF"
  )

  style <- openxlsx::createStyle(
    fontName = fontName
  )


  if (!is.null(billion) & !is.null(billion_fgFill)) {
    style <- modifyStyle(style,
      fgFill = billion_color_list[[paste(billion, billion_fgFill, sep = "_")]]
    )
  }

  if (!is.null(type_data)) {
    if (type_data %in% "date") {
      numFmt <- "DATE"
    }
    if (type_data == "numeric") {
      style <- modifyStyle(
        style,
        numFmt = "0.0"
      )
    }
    if (type_data == "integer") {
      style <- modifyStyle(
        style,
        numFmt = "0"
      )
    }
  }
  if (!is.null(style_category)) {
    if (style_category == "title") {
      style <- modifyStyle(
        style,
        textDecoration = "bold",
        fontSize = 18
      )
    }
    if (style_category == "subtitle") {
      style <- modifyStyle(
        style,
        textDecoration = "bold",
        fontSize = 16
      )
    }

    if (style_category == "datatable_header") {
      style <- modifyStyle(
        style,
        fontSize = 10,
        fontColour = "white",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        wrapText = TRUE
      )
    }
  }
  if (!is.null(style_category)) {
    if (style_category == "sub_datatable_header") {
      style <- modifyStyle(
        style,
        fontSize = 8,
        fontColour = "black",
        halign = "center",
        valign = "center",
        textDecoration = "bold",
        wrapText = TRUE
      )
    }
    if (style_category == "data") {
      style <- modifyStyle(
        style,
        fontSize = 8,
        fgFill = "white",
        border = "bottom",
        borderColour = "grey",
        wrapText = TRUE
      )
    }

    if (style_category == "normal_text") {
      style <- modifyStyle(
        style,
        fontSize = 8,
        fgFill = "white",
        numFmt = "TEXT",
        halign = "left"
      )
    }
  }

  if (hide) {
    style <- modifyStyle(
      style,
      fontColour = "white",
      hidden = TRUE
    )
  }

  style <- modifyStyle(
    style,
    ...
  )

  return(style)
}
