test_hpop_scenarios <- function(ind) {
  testthat::test_that(paste0("accelerate_adult_obese returns  accurate values:"), {
    df <- tibble::tibble(
      value = 60:80,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default"
    )

    df_add_indicator <- add_scenario_indicator(df,
      indicator = ind,
      scenario_function = "accelerate"
    )

    df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "accelerate", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_halt_rise_2025, 60)
  })
}
