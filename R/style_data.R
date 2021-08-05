#' Gets data type of data frame for styling
#'
#' Gets the type of data for styling purposes. Can be either:
#' * numeric
#' * integer
#' * Date
#' * character
#' * character_centered
#' Used to pass appropriate parameters to `style_data()`
#'
#' @param df data frame to be tested
get_data_type <- function(df){
  sapply(df, get_data_type_single)
}

#' Gets data type for styling
#'
#' Gets the type of data for styling purposes. Can be either:
#' * numeric
#' * integer
#' * Date
#' * character
#' * character_centered
#' Used to pass appropriate parameters to `style_data()`
#'
#' @param vec vector to be tested
get_data_type_single <- function(vec){

    if(sum(vec %in% c("imputed", "estimated", "projected", "reported", NA)) == length(vec)){
      type <- "character_centered"
    }else{
      type <- class(vec)
    }
  return(type)
}


#' Style data according to its type
#'
#' @param  data_type character vector of class(es) of the data to be styled.
#' Can be one of "numeric", "integer", "Date", "character", or "character_centered"
#' @inheritParams write_baseline_projection_hpop_summary
#' @inheritParams openxlsx::addStyle
#' @inheritParams style_data_single
#'
style_data <- function(data_type, wb, sheet_name,
                       rows, cols){

  assert_same_length(data_type, cols)

  for(i in seq_along(cols)){
    wb <- style_data_single(data_type[i], wb, sheet_name,rows, cols[i])
  }

  return(wb)
}

#' Style data single data column according to its type
#'
#' @param data_type character vector of class(es) of the data to be styled.
#' Can be one of "numeric", "integer", "Date", "character" or "character_centered"
#' @inheritParams write_baseline_projection_hpop_summary
#' @inheritParams openxlsx::addStyle
#' @param col Column to apply style to.
style_data_single <- function(data_type = c("numeric", "integer", "Date", "character", "character_centered"), wb, sheet_name,
                       rows, col){

  rlang::arg_match(data_type)

  if(data_type == "numeric"){
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_dec,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_dec_black_border,
      rows = rows[length(rows)],
      cols = col,
      gridExpand = TRUE
    )

  }else if(data_type == "Date"){
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_date,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_date_black_border,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )

  }else if(data_type == "integer"){
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_int,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_data_wrapped_int_black_border,
      rows = rows[length(rows)],
      cols = col,
      gridExpand = TRUE
    )
  }else if(data_type == "character"){
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_text,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_text_black_border,
      rows = rows[length(rows)],
      cols = col,
      gridExpand = TRUE
    )
  }else if(data_type == "character_centered"){
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_text_centered,
      rows = rows,
      cols = col,
      gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = sheet_name,
      style = excel_styles()$normal_text_centered_black_border,
      rows = rows[length(rows)],
      cols = col,
      gridExpand = TRUE
    )
  }

  return(wb)

}

