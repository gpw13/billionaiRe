#' Stores styles to be used in excel outputs
#'
#'@param font character name with the name of the font to use.
#'

excel_styles <- function(font = "Calibri") {

  #TODO: This is mess... needs to organised/used in a better way.
  ## Maybe pass more arguments to have less objects created, rather than storing
  ## styles independently.

  uhc_main_color <- "#00A173"
  uhc_light_color <- "#66C6AB"
  uhc_light2_color <- "#A3DCCC"
  hep_main_color <- "#002D5F"
  hep_light_color <- "#66819f"
  hep_light2_color <- "#D1D9EB"
  hpop_main_color <- "#008DCA"
  hpop_light_color <- "#B2DCEF"

  title <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 18
  )

  sub_title <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 16
  )

  normal_data_wrapped_dec <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0.0",
    wrapText = TRUE
  )

  normal_data_wrapped_dec_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "0.0",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_dec <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0.0",
    wrapText = TRUE
  )

  normal_data_dec <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    borderColour = "grey",
    border = "bottom",
    numFmt = "0.0"
  )

  normal_data_wrapped_int <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_int_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_int_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_red <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    fontColour = "red",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_red_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    fontColour = "red",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_int <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0",
    wrapText = TRUE
  )

  normal_data_int <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0"
  )
  normal_data_int_faded <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "grey",
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "0"
  )

  normal_data_int_faded_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "grey",
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "0"
  )

  bold_data_dec_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "0.00",
    textDecoration = "bold"
  )

  normal_data_wrapped_date <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "DATE",
    wrapText = TRUE
  )

  normal_data_wrapped_date_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "DATE",
    wrapText = TRUE
  )

  normal_data_wrapped_bold_date <- openxlsx::createStyle(
    fontName = font,
    textDecoration = "bold",
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "DATE",
    wrapText = TRUE
  )

  normal_data_date <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "DATE"
  )

  normal_text_no_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    numFmt = "TEXT",
    halign = "left",
  )

  bold_text_no_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    numFmt = "TEXT",
    halign = "left",
    textDecoration = "bold"
  )

  normal_text_wrapped <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "left",
    wrapText = TRUE
  )
  normal_text_wrapped_vCentered <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "left",
    valign = "center",
    wrapText = TRUE
  )

  normal_text_wrapped_vCentered_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    numFmt = "TEXT",
    halign = "left",
    valign = "center",
    wrapText = TRUE,
    borderStyle = "thin",
    borderColour = "black",
    border = "bottom"
  )

  normal_text_wrapped_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "TEXT",
    halign = "left",
    wrapText = TRUE
  )

  normal_text_10p <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "left"
  )


  normal_text <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "left"
  )
  normal_text_faded <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "grey",
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "left"
  )


  normal_text_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "TEXT",
    halign = "left"
  )

  normal_text_centered <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "grey",
    numFmt = "TEXT",
    halign = "center"
  )

  normal_text_centered_black_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fgFill = "white",
    border = "bottom",
    borderColour = "black",
    numFmt = "TEXT",
    halign = "center"
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

  bold_uhc_hR_2dp <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fgFill = uhc_light_color,
    textDecoration = "bold",
    numFmt = "0.00",
    halign = "right"
  )

  bold_uhc_hL_2dp <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fgFill = uhc_light_color,
    textDecoration = "bold",
    numFmt = "0.00",
    halign = "left"
  )


  bold_hpop_blue_hR_2dp <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fgFill = hpop_light_color,
    textDecoration = "bold",
    numFmt = "0.00",
    halign = "right"
  )

  bold_hpop_blue_hL_2dp <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fgFill = hpop_light_color,
    textDecoration = "bold",
    numFmt = "0.00",
    halign = "left"
  )

  uhc_main_data_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fontColour = "white",
    fgFill = uhc_main_color,
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  uhc_main_data_header_left_align <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fontColour = "white",
    fgFill = uhc_main_color,
    halign = "left",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  uhc_sec_data_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = uhc_light_color,
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  uhc_sec_data_header_border_left_align <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = uhc_light_color,
    halign = "left",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "bottom",
    borderColour = "black",
    borderStyle = "thin"
  )

  uhc_pillar_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fontColour = "black",
    fgFill = uhc_light2_color,
    halign = "left",
    valign = "center",
    textDecoration = "bold",
  )

  hpop_main_data_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 10,
    fontColour = "white",
    fgFill = hpop_main_color,
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  hpop_sec_data_header <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = hpop_light_color,
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  hpop_sec_data_header_border <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = hpop_light_color,
    halign = "left",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "bottom",
    borderColour = "black",
    borderStyle = "thin")

  hpop_sec_data_header_border_right_align <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = hpop_light_color,
    halign = "right",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE,
    border = "bottom",
    borderColour = "black",
    borderStyle = "thin")


  hpop_sec_data_header_left_align <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = hpop_light_color,
    halign = "left",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  uhc_pillar_average_data <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = "#D9D9D9",
    halign = "left",
    valign = "center",
    textDecoration = "bold",
    numFmt = "0.0"
  )

  uhc_pillar_average_text <- openxlsx::createStyle(
    fontName = font,
    fontSize = 8,
    fontColour = "black",
    fgFill = "#D9D9D9",
    halign = "right",
    valign = "center",
    textDecoration = c("bold", "italic"),
    numFmt = "TEXT"
  )

  excel_styles <- list(
    title = title,
    sub_title = sub_title,
    uhc_main_data_header = uhc_main_data_header,
    uhc_main_data_header_left_align = uhc_main_data_header_left_align,
    uhc_sec_data_header = uhc_sec_data_header,
    uhc_sec_data_header_border_left_align = uhc_sec_data_header_border_left_align,
    uhc_pillar_average_data = uhc_pillar_average_data,
    uhc_pillar_average_text = uhc_pillar_average_text,
    hpop_main_data_header = hpop_main_data_header,
    hpop_sec_data_header = hpop_sec_data_header,
    hpop_sec_data_header_border = hpop_sec_data_header_border,
    hpop_sec_data_header_left_align = hpop_sec_data_header_left_align,
    hpop_sec_data_header_border_right_align = hpop_sec_data_header_border_right_align,
    white_bckgrd = white_bckgrd,
    normal_data_dec = normal_data_dec,
    normal_data_wrapped_dec = normal_data_wrapped_dec,
    normal_data_wrapped_dec_black_border = normal_data_wrapped_dec_black_border,
    normal_data_wrapped_bold_dec = normal_data_wrapped_bold_dec,
    normal_data_int = normal_data_int,
    normal_data_wrapped_int = normal_data_wrapped_int,
    normal_data_wrapped_bold_int_black_border = normal_data_wrapped_bold_int_black_border,
    normal_data_wrapped_int_black_border = normal_data_wrapped_int_black_border,
    normal_data_wrapped_bold_int = normal_data_wrapped_bold_int,
    normal_data_date = normal_data_date,
    normal_data_wrapped_date = normal_data_wrapped_date,
    normal_data_wrapped_date_black_border = normal_data_wrapped_date_black_border,
    normal_data_wrapped_bold_date = normal_data_wrapped_bold_date,
    vertical_txt = vertical_txt,
    normal_data_wrapped_faded_dec = normal_data_wrapped_faded_dec,
    normal_text = normal_text,
    normal_text_black_border = normal_text_black_border,
    normal_text_centered = normal_text_centered,
    normal_text_centered_black_border = normal_text_centered_black_border,
    bold_hpop_blue_hR_2dp = bold_hpop_blue_hR_2dp,
    bold_hpop_blue_hL_2dp = bold_hpop_blue_hL_2dp,
    normal_text_wrapped = normal_text_wrapped,
    normal_text_wrapped_black_border = normal_text_wrapped_black_border,
    normal_text_10p = normal_text_10p,
    normal_data_wrapped_red = normal_data_wrapped_red,
    normal_data_wrapped_red_black_border = normal_data_wrapped_red_black_border,
    normal_text_faded = normal_text_faded,
    normal_data_int_faded = normal_data_int_faded,
    normal_text_no_border = normal_text_no_border,
    bold_text_no_border = bold_text_no_border,
    normal_data_int_faded_black_border = normal_data_int_faded_black_border,
    bold_data_dec_black_border = bold_data_dec_black_border,
    bold_uhc_hR_2dp = bold_uhc_hR_2dp,
    bold_uhc_hL_2dp = bold_uhc_hL_2dp,
    uhc_pillar_header = uhc_pillar_header,
    normal_text_wrapped_vCentered = normal_text_wrapped_vCentered,
    normal_text_wrapped_vCentered_black_border = normal_text_wrapped_vCentered_black_border
  )

  return(excel_styles)
}
