excel_styles <- function() {
  dark_blue_header <- openxlsx::createStyle(
    fontSize = 11,
    fontColour = "white",
    textDecoration = "bold",
    wrapText = T,
    fgFill = "#1E7FB8",
    valign = "center",
    halign = "center",
    border = c("top", "bottom", "left", "right"),
    borderStyle = "thin"
  )

  light_blue_header <- openxlsx::createStyle(
    fontSize = 10,
    textDecoration = "bold",
    wrapText = T,
    fgFill = "#B0CAD6",
    valign = "center",
    halign = "left",
    border = "bottom",
    borderStyle = "thin"
  )

  wrapped_h <- openxlsx::createStyle(
    textDecoration = "bold",
    wrapText = TRUE
  )

  bold <- openxlsx::createStyle(textDecoration = "bold")

  title <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 16
  )

  sub_title <- openxlsx::createStyle(
    textDecoration = "bold",
    fontSize = 12
  )

  normal_data_wrapped <- openxlsx::createStyle(
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00",
    wrapText = TRUE
  )

  normal_data <- openxlsx::createStyle(
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    numFmt = "0.00"
  )

  white_bckgrd <- openxlsx::createStyle(
    fgFill = "white",
    borderColour = "white"
  )

  vertical_txt <- openxlsx::createStyle(
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
    normal_data = normal_data,
    white_bckgrd = white_bckgrd,
    normal_data_wrapped = normal_data_wrapped,
    vertical_txt = vertical_txt
  )

  return(excel_styles)
}
