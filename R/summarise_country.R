#' Summarize Excel sheet 'data' for HPOP country summary
#'
#' `summarize_hpop_country_data` summarises the 'data' sgeet for the HPOP country summary.
#'
#' @param iso ISO3 codes of country to export.
#' @param ... Additional arguments passed to `calculate_hpop_contributions` and
#' `calculate_hpop_billion`. Use if you need to provide additional parameters to
#' those functions.
#' @inherit calculate_hpop_contributions params
#'
#' @return list of data frames to be used in the 'data' sheet of HPOP country summary
#' export
#'
summarize_hpop_country_data <-
  function(df,
           iso,
           year = "year",
           iso3 = "iso3",
           ind = "ind",
           value = "value",
           transform_glue = "transform_{value}",
           type = "type",
           source = "source",
           ind_ids = billion_ind_codes("hpop"),
           start_year = 2018,
           end_year = 2019:2023,
           ...) {
    assert_columns(df,year, iso3, ind, value, type, source)
    assert_mart_columns(df)
    assert_years(start_year, end_year)
    wppdistro:::assert_iso3(iso)

    transform_value <- glue::glue(transform_glue)

    # Filter df for country, arrange indicators by order.
    df_iso <- df %>%
      dplyr::filter(.data[[iso3]] == !!iso) %>%
      dplyr::arrange(get_ind_order(.data[[ind]]))

    # Get unique indicators
    unique_ind <- unique(df_iso[[ind]])

    # Data frame with indicators' order
    ind_df <- billionaiRe::indicator_order %>%
      dplyr::filter(.data[["ind"]] %in% !!unique_ind) %>%
      dplyr::select("ind", "transformed_name", "unit_transformed")

    df_iso_tranf <- df_iso %>%
      transform_hpop_data(iso3 = iso3, ind = ind, value = value,
                          transform_glue = transform_glue, ind_ids = ind_ids)

    # Latest reported data
    latest_reported <- df_iso_tranf %>%
      dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
      dplyr::filter(.data[[type]] %in% c("estimated", "reported")) %>%
      dplyr::filter(.data[[year]] >= max(.data[[year]])) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(!!ind, !!value, !!transform_value,!!year,
                                    !!type, !!source, !!iso3)))

    # Count data points since specified date
    counts_2012 <- count_since(df_iso, year_specified = 2012, year = year, ind = ind, iso3 = iso3)
    counts_2000 <- count_since(df_iso, year_specified = 2000, year = year, ind = ind, iso3 = iso3)

    # Join counts with latest reported data
    latest_reported <- latest_reported %>%
      dplyr::left_join(counts_2000, by = c(ind, iso3 )) %>%
      dplyr::left_join(counts_2012, by = c(ind, iso3))


    # wider raw data (pre-transformation)
    df_iso_raw <- df_iso %>%
      dplyr::filter(.data[[year]] %in% c(!!start_year, !!end_year)) %>%
      dplyr::select(all_of(c(!!ind, !!year, !!value))) %>%
      dplyr::group_by(!!rlang::sym(ind)) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "raw_"
      )

    # df with HPOP pops
    df_iso_pop <- df_iso_tranf %>%
      add_hpop_populations(pop_year = end_year,
                           iso3 = iso3,
                           ind = ind,
                           population = "population",
                           ind_ids = ind_ids)


    # Contribution to HPOP billion in wider format
    baseline_proj <- df_iso_pop %>%
      calculate_hpop_contributions(year = year, start_year = start_year, end_year = end_year, iso3 = iso3, ...) %>%
      dplyr::filter(.data[[year]] %in% c(!!start_year, !!end_year)) %>%
      dplyr::select(
        .data[[ind]],
        .data[[year]],
        .data[[transform_value]],
        .data[["contribution"]],
        .data[["population"]],
        .data[[type]],
        .data[[source]]
      ) %>%
      dplyr::group_by(!!rlang::sym(ind)) %>%
      tidyr::pivot_wider(
        names_from = !!rlang::sym(year),
        values_from = c("transform_value", "contribution", "population", .data[[type]], .data[[source]])
      )

    hpop_contrib <- baseline_proj %>%
      dplyr::mutate(!!sym(glue::glue("change_{transform_value}")) := as.numeric(.data[[glue::glue("{transform_value}_{end_year}")]])
      - as.numeric(.data[[glue::glue("{transform_value}_{start_year}")]])) %>%
      dplyr::rename(
        contribution = glue::glue("contribution_{end_year}"),
        population = glue::glue("population_{end_year}")
      ) %>%
      dplyr::select(
        .data[[ind]],
        glue::glue("change_{transform_value}"),
        population,
        contribution
      ) %>%
      dplyr::mutate(contribution_perc = contribution / population)

    baseline_proj <- baseline_proj %>%
      dplyr::select(
        ind,
        dplyr::starts_with("transform_"),
        dplyr::starts_with("type_"),
        dplyr::starts_with("source")
      )

    # Billions for HPOP
    hpop_billion <- df_iso_pop %>%
      calculate_hpop_billion(year = year, start_year = start_year, end_year = end_year,
                             iso3 = iso3, ind = ind, pop_year = end_year,
                             transform_value =transform_value,
                             ind_ids = ind_ids) %>%
      dplyr::filter(.data[[year]] %in% c(!!start_year, !!end_year)) %>%
      dplyr::group_by(.data[[ind]]) %>%
      dplyr::filter(stringr::str_detect(.data[[ind]], "hpop_healthier")) %>%
      dplyr::select(.data[[ind]], .data[["contribution"]]) %>%
      dplyr::ungroup()

    perc_pop_healthier <- hpop_billion[hpop_billion$ind == "hpop_healthier", "contribution"] / wppdistro::get_population(iso, year = end_year)

    hpop_billion <- hpop_billion %>%
      dplyr::bind_rows(tibble::tibble(ind = "hpop_healthier_perc", contribution = perc_pop_healthier[[1]]))

    # transformed time series
    transformed_time_series <- dplyr::select(ind_df, -"unit_transformed") %>%
      dplyr::left_join(df_iso_pop, by = "ind") %>%
      dplyr::select(transformed_name, .data[[year]], .data[[transform_value]]) %>%
      dplyr::arrange(.data[[year]]) %>%
      dplyr::group_by(transformed_name) %>%
      tidyr::pivot_wider(values_from = transform_value, names_from = .data[[year]])


    final_tables <- list(
      "ind_df" = ind_df,
      "latest_reported" = latest_reported,
      "df_iso_raw" = df_iso_raw,
      "baseline_proj" = baseline_proj,
      "hpop_contrib" = hpop_contrib,
      "hpop_billion" = hpop_billion,
      "df_iso_pop" = df_iso_pop,
      "transformed_time_series" = transformed_time_series
    )

    return(final_tables)
  }
