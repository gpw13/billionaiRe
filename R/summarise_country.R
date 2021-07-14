#' Summarise Excel sheet 'data' for HPOP country summary
#'
#' `summarise_HPOP_cntry_data` summarises the 'data' sgeet for the HPOP country summary.
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
#' @export
summarise_HPOP_cntry_data <-
  function(df,
           iso,
           start_year = 2018,
           end_year = 2019:2023,
           ...) {
    assert_mart_columns(df)
    assert_years(start_year, end_year)

    # Filter df for country, arrange indicators by order.
    df_iso <- df %>%
      dplyr::filter(iso3 == !!iso) %>%
      dplyr::left_join(dplyr::select(indicator_order, ind, order), by = "ind") %>%
      dplyr::arrange(order) %>%
      dplyr::select(-order)


    #Get unique indicators
    unique_ind <- unique(df_iso[["ind"]])

    # Data frame with indicators' order
    ind_df <- indicator_order %>%
      dplyr::filter(ind %in% unique_ind) %>%
      dplyr::arrange(order) %>%
      dplyr::select(ind, transformed_name, unit_transformed)

    # Latest reported data
    latest_reported <- df_iso %>%
      transform_hpop_data() %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::filter(type %in% c("estimated", "reported")) %>%
      dplyr::mutate(maxyear = max(year)) %>%
      dplyr::filter(year >= maxyear) %>%
      dplyr::ungroup() %>%
      dplyr::select(ind,value, transform_value, year, type, source)

    # Count data points since specified date
    counts_2012 <- count_since(df_iso, 2012) %>%
      dplyr::select(-iso3)
    counts_2000 <- count_since(df_iso, 2000)%>%
      dplyr::select(-iso3)

    # Join counts with latest reported data
    latest_reported <- latest_reported %>%
      dplyr::left_join(counts_2000, by = "ind") %>%
      dplyr::left_join(counts_2012, by = "ind")

    # df with HPOP pops
    df_iso_pop <- transform_hpop_data(df_iso) %>%
      add_hpop_populations(pop_year = end_year)

    # wider raw data (pre-transformation)
    df_iso_raw <- df_iso %>%
      dplyr::filter(year %in% c(!!start_year, !!end_year)) %>%
      dplyr::select(ind, year, value) %>%
      dplyr::group_by(ind) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = value,
        names_prefix = "raw_"
      )

    # Contribution to HPOP billion in wider format
    baseline_proj <- df_iso_pop %>%
      dplyr::ungroup() %>%
      calculate_hpop_contributions(year = "year",start_year = start_year, end_year = end_year, iso3 = "iso3",...) %>%
      dplyr::filter(year %in% c(!!start_year, !!end_year)) %>%
      dplyr::select(ind,
             year,
             transform_value,
             contribution,
             population,
             type,
             source) %>%
      dplyr::group_by(ind) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = c(transform_value, contribution, population, type, source)
      )

    hpop_contrib <- baseline_proj %>%
      dplyr::mutate(change_transform_value = as.numeric(.data[[glue::glue("transform_value_{end_year}")]])
                    - as.numeric(.data[[glue::glue("transform_value_{start_year}")]])) %>%
      dplyr::rename(
        contribution = glue::glue("contribution_{end_year}"),
        population = glue::glue("population_{end_year}")
      ) %>%
      dplyr::select(
        ind,
        change_transform_value,
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
      calculate_hpop_billion(start_year = start_year, end_year = end_year) %>%
      dplyr::filter(year %in% c(!!start_year, !!end_year)) %>%
      dplyr::group_by(ind) %>%
      dplyr::filter(stringr::str_detect(ind, "hpop_healthier")) %>%
      dplyr::select(ind, contribution) %>%
      dplyr::ungroup()

    perc_pop_healthier <- hpop_billion[hpop_billion$ind == "hpop_healthier", "contribution"]/wppdistro::get_population(iso, year = end_year)

    hpop_billion <- hpop_billion %>%
      dplyr::bind_rows(tibble::tibble(ind = "hpop_healthier_perc", contribution = perc_pop_healthier[[1]]))

    #transformed time series
    transformed_time_series <- dplyr::select(ind_df, - unit_transformed) %>%
      dplyr::left_join(df_iso_pop, by = "ind") %>%
      dplyr::select(transformed_name, year, transform_value) %>%
      dplyr::arrange(year) %>%
      dplyr::group_by(transformed_name) %>%
      tidyr::pivot_wider(values_from = "transform_value", names_from = "year")


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
