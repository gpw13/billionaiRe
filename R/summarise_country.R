#' Summarize data for HPOP country summary export
#'
#' `summarize_hpop_country_data` summarizes the data for the HPOP country summary.
#'
#' @param iso ISO3 codes of country to summarize.
#' @inheritParams calculate_hpop_contributions
#' @inheritParams calculate_uhc_billion
#' @inheritParams transform_hpop_data
#'
#' @return list of data frames to be used in the 'data' sheet of HPOP country summary
#' @export
#'
summarize_hpop_country_data <-
  function(df,
           iso,
           year = "year",
           iso3 = "iso3",
           ind = "ind",
           value = "value",
           transform_value = "transform_value",
           contribution = "contribution",
           population = "population",
           scenario = NULL,
           type_col = "type",
           source_col = "source",
           ind_ids = billion_ind_codes("hpop"),
           start_year = 2018,
           end_year = 2019:2023) {

    assert_columns(df,year, iso3, ind, value, transform_value,scenario, contribution, type_col, source_col, population)
    assert_mart_columns(df)
    assert_years(start_year, end_year)
    assert_same_length(value, transform_value)
    assert_same_length(value, contribution)
    wppdistro:::assert_iso3(iso)

    ### TODO: Add full scenarios implementation

    # Filter df for country, arrange indicators by order.
    df_iso <- df %>%
      dplyr::filter(.data[[iso3]] == !!iso) %>%
      dplyr::arrange(get_ind_order(.data[[ind]]))

    # Get unique indicators
    unique_ind <- unique(df_iso[[ind]])

    # Data frame with indicators' order
    ind_df <- billionaiRe::indicator_df %>%
      dplyr::filter(.data[["ind"]] %in% !!unique_ind) %>%
      dplyr::select("ind", "transformed_name", "unit_transformed")


    # Latest reported data
    latest_reported <- df_iso %>%
      dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
      dplyr::filter(.data[[type_col]] %in% c("estimated", "reported"),
                    .data[[year]] >= max(.data[[year]])) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(ind, value, transform_value,year,
                                    type_col, source_col, iso3)))

    # Count data points since specified date
    counts_2012 <- count_since(df_iso, year_specified = 2012, year = year, ind = ind, iso3 = iso3)
    counts_2000 <- count_since(df_iso, year_specified = 2000, year = year, ind = ind, iso3 = iso3)

    # Join counts with latest reported data
    latest_reported <- latest_reported %>%
      dplyr::left_join(counts_2000, by = c(ind, iso3 )) %>%
      dplyr::left_join(counts_2012, by = c(ind, iso3))

    # Baseline and projected for end date data in wider format
    baseline_proj <- df_iso %>%
      dplyr::filter(.data[[year]] %in% c(!!start_year, max(!!end_year)),
                    .data[[ind]] %in% ind_ids) %>%
      dplyr::select(dplyr::all_of(c(
        ind,
        year,
        value,
        transform_value,
        type_col,
        source_col
      ))) %>%
      dplyr::group_by(!!rlang::sym(ind)) %>%
      tidyr::pivot_wider(
        names_from = !!rlang::sym(year),
        values_from = c(dplyr::all_of(c(value, transform_value)), .data[[type_col]], .data[[source_col]])
      )

    # Contribution of each indicator to billion
    ### TODO: would benefit from custom function to calculate with double-counting
    ### based on calculate_hpop_billion
    hpop_contrib <- df_iso %>%
      dplyr::filter(.data[[year]] %in% c(!!max(end_year), !!start_year),
                    .data[[ind]] %in% ind_ids) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario, ind)))) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(transform_value),
                                  calculate_hpop_change_vector,
                                  .data[[year]],
                                  !!start_year, .names = "change_{.col}")) %>%
      dplyr::filter(.data[[year]] == !!max(end_year)) %>%
      dplyr::group_by(dplyr::across(c(!!iso3, !!scenario, !!ind))) %>%
      dplyr::mutate(
        tot_pop = wppdistro::get_population(!!iso, year = !!max(end_year)),
        dplyr::across(c(!!glue::glue("change_{transform_value}")),
                      ~ (.x/100)*population,
                      .names = "ind_contrib_{.col}"),
        dplyr::across(c(!!glue::glue("change_{transform_value}")),
             ~((.x/100)*population)/tot_pop*100,
             .names = "ind_contrib_perc_{.col}"
             )) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(!!ind,
                                    !!glue::glue("change_{transform_value}"),
                                    !!population,
                                    !!glue::glue("ind_contrib_change_{transform_value}"),
                                    !!glue::glue("ind_contrib_perc_change_{transform_value}"))))

    # Contribution towards billion (all indicator) with double counting
    dbl_counted_billion <- hpop_contrib %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(glue::glue("ind_contrib_change_{transform_value}")),
                       ~ sum(.x[which(.x >=0)]), .names = "hpop_healthier_dbl_plus_{contribution}"),
                       dplyr::across(dplyr::all_of(glue::glue("ind_contrib_change_{transform_value}")),
                                     ~ sum(.x[which(.x <0)]), .names = "hpop_healthier_dbl_minus_{contribution}")
                       ) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with(!!contribution),
                          names_to = c("ind", "var"), names_pattern = "(hpop_healthier_dbl_plus|hpop_healthier_dbl_minus)_(.*)$") %>%
      dplyr::group_by(dplyr::across(ind)) %>%
      tidyr::pivot_wider(dplyr::everything(), values_from = "value", names_from = "var") %>%
      dplyr::ungroup()

    dbl_healthier <- dbl_counted_billion %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(contribution),
                                     ~ sum(.x))) %>%
      dplyr::mutate("ind" := c("hpop_healthier_dbl","hpop_healthier_dbl_perc"), .before = 1)

    #Binding frames together
    dbl_counted_billion <- dbl_counted_billion %>%
      dplyr::bind_rows(dbl_healthier) %>%
      dplyr::rename_with()

    # Contribution towards overall billion (all indicators)
    hpop_billion <- df_iso %>%
      dplyr::filter(.data[[year]] == c(!!max(end_year)),
                    stringr::str_detect(.data[[ind]], "^hpop_healthier")) %>%
      dplyr::select(dplyr::all_of(c(!!ind, !!contribution))) %>%
      dplyr::bind_rows(dbl_counted_billion)

    # transformed time series
    transformed_time_series <- dplyr::select(ind_df, -"unit_transformed") %>%
      dplyr::left_join(df_iso, by = "ind") %>%
      dplyr::select(c(.data[["transformed_name"]], .data[[year]], !!transform_value)) %>%
      dplyr::arrange(.data[[year]]) %>%
      dplyr::group_by(.data[["transformed_name"]]) %>%
      tidyr::pivot_wider(values_from = !!transform_value, names_from = .data[[year]])

    #Final list of df to be returned
    final_tables <- list(
      "ind_df" = ind_df,
      "latest_reported" = latest_reported,
      "baseline_proj" = baseline_proj,
      "hpop_contrib" = hpop_contrib,
      "hpop_billion" = hpop_billion,
      "df_iso" = df_iso,
      "transformed_time_series" = transformed_time_series
    )

    return(final_tables)
  }
