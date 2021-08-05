#' Write Inter sheet for HPOP
#' @inherit write_baseline_projection_hpop_summary
#' @inheritParams write_hpop_timeseries_sheet
#' @inheritParams export_hpop_country_summary_xls
#' @inheritParams style_header_hpop_summary_sheet
#' @param data_sheet_name character Name of the data sheet
#' @param summary_bounds named list with boxes bounds of the different data 'boxes'
#' present in the summary sheet. Used to identify where data sits in Excel sheet
#' dynamically.

write_hpop_inter <- function(wb,
                             sheet_name,
                             data_sheet_name,
                             ind_df,
                             start_year,
                             end_year,
                             start_col,
                             start_row,
                             transform_value,
                             summary_bounds){

    l_trans_values <- length(transform_value)

    start_trans <- l_trans_values*2+1

    # Write indicators
    openxlsx::writeData(wb, sheet_name, startCol = start_col, startRow = start_row+1,
                        x = data.frame(ind_df[,"short_name"]), colNames = FALSE)
    row_range <- (summary_bounds$baseline_proj['start_row']+3):summary_bounds$baseline_proj['end_row']

    # start_year formulae
    ## start_year header
    openxlsx::writeData(wb, sheet_name, startCol = start_col + 2,
                        startRow = start_row,
                        x = start_year)

    letter_position_start <- openxlsx::int2col(summary_bounds$baseline_proj['start_col']+start_trans)
    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_start}{row_range}<>"",{data_sheet_name}!{letter_position_start}{row_range},#N/A)'),
                           startCol = start_col+2, startRow = start_row +1)

    # end_year formulae
    ## end_year header
    openxlsx::writeData(wb, sheet_name, startCol = start_col + 3,
                        startRow = start_row,
                        x = max(end_year))

    letter_position_end <- openxlsx::int2col(summary_bounds$baseline_proj['start_col']+start_trans+1)
    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_end}{row_range}<>"",{data_sheet_name}!{letter_position_end}{row_range},#N/A)'),
                           startCol = start_col + 3, startRow = start_row +1)

    # latest available
    position_start_latest_available <- summary_bounds$latest["start_col"]+l_trans_values
    letter_position_latest <- openxlsx::int2col(position_start_latest_available)

    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_latest}{row_range}<>"",{data_sheet_name}!{letter_position_latest}{row_range},#N/A)'),
                           startCol = start_col + 8, startRow = start_row +1)

    #latest year
    position_start_latest_year <- position_start_latest_available + l_trans_values
    letter_position_latest_year <- openxlsx::int2col(position_start_latest_year)

    openxlsx::writeFormula(wb, sheet_name,
                           x = glue::glue('=IF({data_sheet_name}!{letter_position_latest_year}{row_range}<>"",{data_sheet_name}!{letter_position_latest_year}{row_range},#N/A)'),
                           startCol = start_col + 9, startRow = start_row +1)

    # Clear content below last row
    openxlsx::deleteData(wb, sheet_name,
                        cols = start_col:10,
                        rows = (start_row + nrow(ind_df)+1), gridExpand = TRUE)

    return(wb)
}
