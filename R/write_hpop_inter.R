#' Write Inter sheet for HPOP
#' @inherit write_main_df
#' @inherit write_hpop_timeseries_sheet
#' @param data_sheet_name character Name of the data sheet
#' @param start_col_data integer Start column of the data frame in the data sheet
#' @param start_row_data integer Start row of the data frame in the data sheet

write_hpop_inter <- function(wb,
                             sheet_name,
                             data_sheet_name,
                             ind_df,
                             start_year,
                             end_year,
                             start_col,
                             start_row,
                             start_col_data = 1,
                             start_row_data = 9,
                             transform_value){

    l_trans_values <- length(transform_value)

    position_first_trans_value <- 2 + (l_trans_values*2) + 1 # 2 indicators columns + raw_values columns + 1 to be at start of columns

    # Write indicators
    openxlsx::writeData(wb, sheet_name, startCol = start_col, startRow = start_row+1,
                        x = data.frame(ind_df[,"transformed_name"]), colNames = FALSE)

    # start_year formulae
    ## start_year header
    openxlsx::writeData(wb, sheet_name, startCol = start_col + 2,
                        startRow = start_row,
                        x = start_year)

    letter_position_start <- openxlsx::int2col(position_first_trans_value)
    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_start}{start_row_data:(start_row_data+nrow(ind_df))}<>"",{data_sheet_name}!{letter_position_start}{start_row_data:(start_row_data+nrow(ind_df))},#N/A)'),
                           startCol = start_col+2, startRow = start_row +1)

    # end_year formulae
    ## end_year header
    openxlsx::writeData(wb, sheet_name, startCol = start_col + 3,
                        startRow = start_row,
                        x = max(end_year))

    letter_position_end <- openxlsx::int2col(position_first_trans_value+1)
    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_end}{start_row_data:(start_row_data+nrow(ind_df))}<>"",{data_sheet_name}!{letter_position_end}{start_row_data:(start_row_data+nrow(ind_df))},#N/A)'),
                           startCol = start_col + 3, startRow = start_row +1)

    # latest available
    position_start_latest_available <- 2 + (l_trans_values*2)*2 + 4 + 1 + l_trans_values + 1 + l_trans_values*2 + l_trans_values*1 + 1
    letter_position_latest <- openxlsx::int2col(position_start_latest_available)

    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_latest}{start_row_data:(start_row_data+nrow(ind_df))}<>"",{data_sheet_name}!{letter_position_latest}{start_row_data:(start_row_data+nrow(ind_df))},#N/A)'),
                           startCol = start_col + 8, startRow = start_row +1)

    #latest year
    position_start_latest_year <- position_start_latest_available + l_trans_values
    letter_position_latest_year <- openxlsx::int2col(position_start_latest_year)

    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_latest_year}{start_row_data:(start_row_data+nrow(ind_df))}<>"",{data_sheet_name}!{letter_position_latest_year}{start_row_data:(start_row_data+nrow(ind_df))},#N/A)'),
                           startCol = start_col + 9, startRow = start_row +1)

    # Clear content below last row
    openxlsx::deleteData(wb, sheet_name,
                        cols = start_col:10,
                        rows = (start_row + nrow(ind_df)+1), gridExpand = TRUE)

    return(wb)
}
