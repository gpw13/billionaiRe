

test_sdg_hep <- function(ind){
    testthat::test_that(paste0(ind, " returns appropriate values"), {
      df <- tibble::tibble(
        value = 60:80,
        year = 2010:2030,
        ind = ind,
        iso3 = "testalia",
        scenario = "default"
      )

      df_add_indicator <- add_scenario_indicator(df,
                                                 indicator = ind,
                                                 scenario_function = "sdg",
      )
      df_add_indicator <- df_add_indicator %>%
        dplyr::filter(scenario == "sdg", year <= 2025, year >= 2018) %>%
        dplyr::pull(value)

      testthat::expect_equal(df_add_indicator, 68:75)

    })
}

purrr::walk(billion_ind_codes("hep", include_subindicators = F), test_sdg_hep)
