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
