testthat::test_that("add_scenarios_adult_obese returns appropriate results:", {
  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "adult_obese",
    iso3 = "testalia",
    scenario = "default"
  )

  df_adult_obese <- add_scenario_adult_obese(df,
    scenario_function = "halt_rise",
    baseline_year = 2010
  )

  df_adult_obese_acc_2025 <- df_adult_obese %>%
    dplyr::filter(scenario == "halt_rise", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_adult_obese_acc_2025, 80)

  df_add_indicator <- add_scenario_indicator(df,
    indicator = "adult_obese",
    baseline_year = 2010,
    scenario_function = "halt_rise"
  )

  testthat::expect_equal(df_add_indicator, df_adult_obese)

  df_add_scenario_hpop <- add_scenario(df,
    scenario_function = "halt_rise",
    baseline_year = 2010
  )

  testthat::expect_equal(df_add_scenario_hpop, df_adult_obese)

  df_adult_obese_acc <- add_scenario_adult_obese(df,
    scenario_function =  "halt_rise",
    baseline_year = 2010,
    scenario_name = "acceleration"
  )

  adult_obese_accelerated <- accelerate_adult_obese(df)

  testthat::expect_equal(adult_obese_accelerated, df_adult_obese_acc)
})
