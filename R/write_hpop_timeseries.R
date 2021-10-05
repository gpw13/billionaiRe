#' Write times series sheet
#'
#' @inherit write_baseline_projection_hpop_summary
#' @param ind_df data frame containing information on indicators
#' @inherit export_hpop_country_summary_xls
#' @inheritParams style_header_hpop_summary_sheet
#'

write_hpop_timeseries_sheet <- function(df, wb, sheet_name,
                                        start_row, start_col, value,
                                        ind_df, ind, year, type_col, end_year) {
  openxlsx::writeData(wb, sheet_name,
    x = "Time Series",
    startCol = start_col,
    startRow = 2
  )

  wb <- write_empty_white_data(
    wb,
    sheet_name,
    bounds = c(
      start_row = start_row,
      start_col = start_col,
      end_row = 1000,
      end_col = 100
    )
  )

  # TODO: Simplify function to purrr-like walk rather than looping

  time_series <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(!stringr::str_detect(.data[[ind]], "^hpop_healthier")) %>%
    dplyr::select(.data[[ind]], .data[[year]], .data[[type_col]], !!value) %>%
    dplyr::group_by(.data[[ind]], .data[[year]], .data[[type_col]]) %>%
    tidyr::pivot_longer(c(!!value), names_to = "value_mod", values_to = "value") %>%
    dplyr::filter(.data[[year]] <= max(end_year)) %>%
    dplyr::mutate(!!sym("value_mod") := factor(!!sym("value_mod"), levels = !!value)) %>%
    dplyr::group_by(!!sym("value_mod")) %>%
    dplyr::group_split()

  time_series_wide_out <- list()
  for (i in seq(time_series)) {
    time_series_wide_out[[i]] <- time_series[[i]] %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data[[ind]]) %>%
      tidyr::pivot_wider(c(-.data[[type_col]]), names_from = .data[[year]], values_from = !!sym("value"))

    time_series_wide <- dplyr::select(ind_df, "ind", "short_name") %>%
      dplyr::left_join(time_series_wide_out[[i]], by = ind) %>%
      dplyr::select(-sym("value_mod"), -ind)

    if (i > 1) {
      nrows_sofar <- sum(unlist(lapply(1:(i - 1), function(x) nrow(time_series_wide_out[[x]]) + 2)))
      start_row_new <- start_row + nrows_sofar + (2 * (i - 1))
    } else {
      start_row_new <- start_row
    }

    openxlsx::writeData(wb,
      sheet = sheet_name, x = "Indicator",
      startCol = start_col, startRow = start_row_new
    )
    openxlsx::writeData(wb,
      sheet = sheet_name,
      x = vec2emptyDF(glue::glue("Time serie: Raw {value[i]}*")),
      startCol = start_col + 1, startRow = start_row_new,
      colNames = TRUE
    )
    years_list <- names(time_series_wide)[2:ncol(time_series_wide)]
    openxlsx::writeData(wb,
      sheet = sheet_name,
      x = vec2emptyDF(years_list),
      startCol = start_col + 1, startRow = start_row_new + 1,
      colNames = TRUE
    )
    openxlsx::writeData(wb,
      sheet = sheet_name, x = time_series_wide,
      startCol = start_col, startRow = start_row_new + 2,
      colNames = FALSE
    )
    wb <- style_timeseries(
      df = time_series[[i]], wb, billion = "hpop", sheet_name,
      start_row = start_row_new, start_col = start_col,
      ind, year, type_col, df_wide = time_series_wide, ind_df
    )
  }
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = start_col,
    widths = 27.18,
    ignoreMergedCells = FALSE
  )
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = (start_col + 1):(ncol(time_series_wide) + 1),
    widths = 6,
    ignoreMergedCells = FALSE
  )
  nrows_final <- sum(unlist(lapply(1:(length(time_series)), function(x) nrow(time_series_wide_out[[x]]) + 2)))
  start_row_final <- start_row + nrows_final + (2 * (length(time_series) - 1))

  openxlsx::writeData(wb,
    sheet = sheet_name,
    x = "* Values are in bold if reported; normal if estimated; and faded if imputed/projected",
    startRow = start_row_final + 2,
    startCol = start_col
  )
  openxlsx::addStyle(wb,
    sheet = sheet_name, style = excel_styles(style_category = "normal_text"),
    rows = start_row_final + 2,
    cols = start_col, gridExpand = TRUE
  )
  return(wb)
}
