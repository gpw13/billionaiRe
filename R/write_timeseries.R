#' Write times series sheet
#'
#' @inherit write_main_df
#' @param df_ind data frame containing information on indicators
#' @inherit export_hpop_country_summary_xls
#'

write_timeseries_sheet <- function(df, wb, sheet_name,
                                   start_row, start_col, transform_value,
                                   df_ind, ind, year, type_col){

  transformed_time_series <- df %>%
    dplyr::filter(!stringr::str_detect(.data[[ind]], "^hpop_healthier")) %>%
    dplyr::select(.data[[ind]],.data[[year]],.data[[type_col]],!!transform_value) %>%
    dplyr::group_by(.data[[ind]], .data[[year]],.data[[type_col]]) %>%
    tidyr::pivot_longer(c(!!transform_value), names_to = "transformed_value", values_to = "value") %>%
    dplyr::mutate(!!sym("transformed_value") := factor(!!sym("transformed_value"), levels = !!transform_value)) %>%
    dplyr::group_by(!!sym("transformed_value")) %>%
    dplyr::group_split()


  time_series_wide_out <- list()
  for(i in seq(length(transformed_time_series))){
    time_series_wide_out[[i]] <- transformed_time_series[[i]] %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data[[ind]]) %>%
      tidyr::pivot_wider(c(-.data[[type_col]]), names_from = .data[[year]], values_from = !!sym("value"))

    time_series_wide <- df_ind %>%
      dplyr::left_join(time_series_wide_out[[i]], by = ind) %>%
      dplyr::select(-sym("transformed_value"), -sym("unit_transformed"), -ind)

    if(i > 1){
      nrows_sofar <- sum(unlist(lapply(1:(i-1), function(x)nrow(time_series_wide_out[[x]])+2)))
      start_row_new <-  start_row + nrows_sofar + (2*(i-1))
    }else{
      start_row_new <- start_row
    }

    openxlsx::writeData(wb, sheet = sheet_name, x = "Transformed indicator",
                        startCol = start_col, startRow = start_row_new
    )
    openxlsx::mergeCells(wb, sheet = sheet_name,
                         cols = start_col, rows = start_row_new:(start_row_new+1))
    openxlsx::writeData(wb, sheet = sheet_name,
                        x = vec2emptyDF(glue::glue("Time serie: {transform_value[i]}")),
                        startCol = start_col+1, startRow = start_row_new,
                        colNames = TRUE
    )
    openxlsx::mergeCells(wb, sheet = sheet_name,
                         cols = (start_col+1):(ncol(time_series_wide)+1), rows = start_row_new)

    years_list <- names(time_series_wide)[2:ncol(time_series_wide)]
    openxlsx::writeData(wb, sheet = sheet_name,
                        x = vec2emptyDF(years_list),
                        startCol = start_col+1, startRow = start_row_new+1,
                        colNames = TRUE
    )
    openxlsx::writeData(wb,
                        sheet = sheet_name, x = time_series_wide,
                        startCol = start_col, startRow = start_row_new + 2,
                        colNames = FALSE
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$dark_blue_header,
                       rows =start_row_new, cols = c(start_col:(ncol(time_series_wide)+1))
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$dark_blue_header,
                       rows = start_row_new:(start_row_new+1), cols = start_col
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$light_blue_header,
                       rows = c((start_row_new+1)), cols = c((start_col+1):(ncol(time_series_wide)+1)),
                       gridExpand = TRUE
    )
    openxlsx::addStyle(wb,
                       sheet = sheet_name, style = excel_styles()$normal_data_wrapped_dec,
                       rows = c((start_row_new+2):(start_row_new+nrow(time_series_wide)+1)), cols = c(start_col:(ncol(time_series_wide)+1)),
                       gridExpand = TRUE
    )
    style_data_type_timesseries(df = transformed_time_series[[i]], wb, sheet_name, start_col, start_row = start_row_new + 2, ind, type_col, year = year)

  }
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = start_col,
    widths = 23,
    ignoreMergedCells = FALSE
  )
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = (start_col+1):(ncol(time_series_wide)+1),
    widths = 6,
    ignoreMergedCells = FALSE
  )
  nrows_final <- sum(unlist(lapply(1:(length(transformed_time_series)), function(x)nrow(time_series_wide_out[[x]])+2)))
  start_row_final <-  start_row + nrows_final + (2*(length(transformed_time_series)-1))

  openxlsx::writeData(wb, sheet = sheet_name,
                      x = "* Values are in bold if reported; normal if estimated; and faded if imputed/projected",
                      startRow = start_row_final + 2,
                      startCol = start_col)
  openxlsx::addStyle(wb,
                     sheet = sheet_name, style = excel_styles()$normal_data_int,
                     rows = start_row_final + 2,
                     cols = start_col, gridExpand = TRUE
  )
  return(wb)
}
