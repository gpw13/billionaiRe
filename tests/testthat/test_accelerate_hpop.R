testthat::test_that(paste0("accelerate_adult_obese returns  accurate values:"), {
  ind <- "adult_obese"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
    indicator = ind,
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 60)
})

testthat::test_that(paste0("accelerate_alcohol returns  accurate values:"), {
  ind <- "alcohol"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
    indicator = ind,
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 54)
})

testthat::test_that(paste0("accelerate_child_obese returns  accurate values:"), {
  ind <- "child_obese"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
    indicator = ind,
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 60)
})

testthat::test_that(paste0("accelerate_child_viol returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_devontrack returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_fuel returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_hpop_sanitation, accelerate_hpop_sanitation_urban, accelerate_hpop_rural  returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_hpop_tobacco returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_ipv returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_overweight returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_pm25 returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_road returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_stunting returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_suicide returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_transfats returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_wasting returns  accurate values:"), {
})

testthat::test_that(paste0("accelerate_water, water_urban and water_rural returns  accurate values:"), {
  ind <- "water"

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
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 75)

  ind <- "water_urban"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
    indicator = ind,
    scenario_function = "accelerate",
    quantile_year = 2010
  )

  df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 75)

  ind <- "water_rural"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
    indicator = ind,
    scenario_function = "accelerate",
    scenario_name = "test"
  )

  df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_halt_rise_2025, 75)
})
