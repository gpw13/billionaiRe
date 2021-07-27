#' Summarize data for HPOP country summary export
#'
#' `summarize_hpop_data()` summarizes the data for the HPOP country summary
#' Excel file. It is used within [export_hpop_country_summary_xls()].
#'
#' @inheritParams calculate_hpop_contributions
#' @inheritParams calculate_uhc_billion
#' @inheritParams transform_hpop_data
#'
#' @return list of data frames to be used in the 'data' sheet of HPOP country summary
#'
summarize_hpop_data <- function(df,
           year = "year",
           iso3 = "iso3",
           ind = "ind",
           value = "value",
           transform_value = "transform_value",
           contribution = stringr::str_replace(transform_value, "transform_value", "contribution"),
           contribution_pct = paste0(contribution, "_percent"),
           contribution_pct_pop_total = paste0(contribution, "_percent_pop_total"),
           population = "population",
           scenario = NULL,
           type_col = "type",
           source_col = "source",
           ind_ids = billion_ind_codes("hpop"),
           start_year = 2018,
           end_year = 2019:2023) {

    assert_columns(df, year, iso3, ind, value, transform_value, scenario, type_col, source_col, population)
    assert_years(start_year, end_year)
    assert_same_length(value, transform_value)

    ### TODO: Check full scenarios implementation

    # Arrange indicators by order.
    df <- df %>%
      dplyr::arrange(get_ind_order(.data[[ind]]),
                     .data[[year]]) %>%
      dplyr::mutate(dplyr::across(c(!!population, !!year), as.integer))

    # Get unique indicators
    unique_ind <- unique(df[[ind]])

    # Data frame with indicators' order
    ind_df <- df %>%
      dplyr::group_by(.data[[iso3]]) %>%
      dplyr::distinct(.data[[ind]]) %>%
      dplyr::left_join(billionaiRe::indicator_df, by = c(ind)) %>%
      dplyr::select(.data[[iso3]],"ind", "transformed_name", "unit_transformed") %>%
      dplyr::filter(!stringr::str_detect(.data[[ind]], "^hpop_healthier"))


    # Latest reported data
    latest_reported <- df %>%
      dplyr::filter(.data[[type_col]] %in% c("estimated", "reported")) %>%
      dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
      dplyr::filter(.data[[year]] == max(.data[[year]])) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(ind, value, transform_value,year,
                                    type_col, source_col, iso3)))

    # Count data points since specified date
    counts_2012 <- count_since(df, year_specified = 2012, year = year, ind = ind, iso3 = iso3, type_col = type_col)
    counts_2000 <- count_since(df, year_specified = 2000, year = year, ind = ind, iso3 = iso3, type_col = type_col)

    # Join counts with latest reported data
    latest_reported <- latest_reported %>%
      dplyr::left_join(counts_2000, by = c(ind, iso3 )) %>%
      dplyr::left_join(counts_2012, by = c(ind, iso3))

    # Baseline and projected for end date data in wider format
    baseline_proj <- df %>%
      dplyr::filter(.data[[year]] %in% c(!!start_year, max(!!end_year)),
                    .data[[ind]] %in% ind_ids) %>%
      dplyr::select(dplyr::all_of(c(
        ind,
        year,
        value,
        transform_value,
        type_col,
        source_col,
        iso3
      ))) %>%
      dplyr::group_by(!!rlang::sym(ind), !!rlang::sym(iso3)) %>%
      tidyr::pivot_wider(
        names_from = !!rlang::sym(year),
        values_from = c(dplyr::all_of(c(value, transform_value)), .data[[type_col]], .data[[source_col]])
      )

    # Contribution of each indicator to billion
    hpop_contrib <- df %>%
      dplyr::filter(.data[[year]] == max(end_year)) %>%
      dplyr::select(dplyr::all_of(c(iso3, ind , contribution_pct, population, contribution, contribution_pct_pop_total)))

    latest_update_ind <- df %>%
      dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
      dplyr::select(.data[[iso3]],.data[[ind]], .data[["upload_date"]]) %>%
      dplyr::filter(.data[["upload_date"]] == max(.data[["upload_date"]])) %>%
      dplyr::distinct()

    main_df <- ind_df %>%
      dplyr::left_join(baseline_proj, by = c(ind, iso3)) %>%
      dplyr::left_join(latest_update_ind, by = c(ind, iso3)) %>%
      dplyr::left_join(hpop_contrib, by = c(ind, iso3)) %>%
      dplyr::left_join(latest_reported, by = c(ind, iso3)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data[[ind]])

    return(main_df)
  }


#' Summarize HPOP billion contributions for all indicators
#'
#' @inherit summarize_hpop_data
#' @inherit calculate_hpop_contributions
#'
#' @return data frame with summarized data
summarize_hpop_billion_contribution <- function(df,year, end_year,ind, contribution, contribution_pct){

  assert_columns(df, year, ind, contribution)

  hpop_healthier <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[year]] == c(!!max(end_year)),
                  stringr::str_detect(.data[[ind]], "^hpop_healthier")) %>%
    dplyr::select(dplyr::all_of(c(!!ind, !!contribution))) %>%
    dplyr::distinct() %>%
    dplyr::mutate("dbl_cntd" := dplyr::case_when(
      stringr::str_detect(.data[[ind]], "dbl_cntd$") ~ "dbl_cntd",
      TRUE ~ "not_dbl_cntd"),
      !!sym(ind) := stringr::str_remove(.data[[ind]], "_dbl_cntd")) %>%
    dplyr::group_by(!!sym(ind)) %>%
    tidyr::pivot_wider( values_from = !!contribution, names_from = "dbl_cntd",
                        names_glue = "{.value}_{dbl_cntd}") %>%
    dplyr::ungroup()

  hpop_perc_healthier <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate("dbl_cntd" := dplyr::case_when(
      stringr::str_detect(.data[[ind]], "dbl_cntd$") ~ "dbl_cntd",
      TRUE ~ "not_dbl_cntd"),
      !!sym(ind) := stringr::str_remove(.data[[ind]], "_dbl_cntd")) %>%
    dplyr::filter(.data[[year]] == c(!!max(end_year)),
                  stringr::str_detect(.data[[ind]], "^hpop_healthier$")) %>%
    dplyr::select(dplyr::all_of(c(!!ind, !!contribution_pct, "dbl_cntd"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(!!sym(ind)) %>%
    tidyr::pivot_wider( values_from = !!contribution_pct, names_from = "dbl_cntd",
                        names_glue = "{.value}_{dbl_cntd}") %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ names(hpop_healthier))

  dplyr::bind_rows(hpop_healthier, hpop_perc_healthier)

}
