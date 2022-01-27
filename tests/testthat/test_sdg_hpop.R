testthat::test_that("adult_obese returns appropriate values", {
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
                                             scenario_function = "sdg",
  )

  df_add_indicator <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 60)

})

testthat::test_that(paste0("sdg_alcohol returns accurate values:"), {
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
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 54)
})

testthat::test_that("child_obese returns appropriate values", {
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
                                             scenario_function = "sdg",
  )

  df_add_indicator <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 60)

})

testthat::test_that("child_viol returns appropriate values", {
  ind <- "child_viol"

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
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 28.333333)

})

testthat::test_that("devontrack returns appropriate values", {
  ind <- "devontrack"

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
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 75)

})

testthat::test_that(paste0("accelerate_fuel returns accurate values:"), {
  ind <- "fuel"

  df <- tibble::tibble(
    value = c(rep(60:80, 2), seq(40, 80, length.out = 21)),
    year = rep(2010:2030, 3),
    ind = ind,
    iso3 = c(rep("AFG", 21), rep("FIN", 21), rep("COD", 21)),
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, c(81, 70, 75))
})

testthat::test_that(paste0("sdg_hpop_sanitation returns accurate values:"), {
  ind <- "hpop_sanitation"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})

testthat::test_that(paste0("sdg_hpop_sanitation_rural  returns accurate values:"), {

  ind <- "hpop_sanitation_rural"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})

testthat::test_that(paste0("sdg_hpop_sanitation_rural returns accurate values:"), {

  ind <- "hpop_sanitation_urban"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})

testthat::test_that(paste0("sdg_hpop_tobacco returns accurate values:"), {
  ind <- "hpop_tobacco"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    )
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 42)
})

testthat::test_that(paste0("sdg_ipv returns accurate values:"), {
  ind <- "ipv"
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
                                             baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)
})

testthat::test_that(paste0("sdg_overweight returns accurate values:"), {
  ind <- "overweight"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 11.0119977)
})


testthat::test_that(paste0("sdg_pm25 returns accurate values:"), {
  ind <- "pm25"

  df <- tibble::tibble(
    value = 30:10,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 15)
})

testthat::test_that(paste0("sdg_road returns accurate values:"), {
  ind <- "road"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 70 + ((70 / 2) - 70) / (2030 - 2020) * (2025 - 2020))
})

testthat::test_that(paste0("sdg_stunting returns accurate values:"), {
  ind <- "stunting"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 37.582108)
})

testthat::test_that(paste0("accelerate_suicide returns accurate values:"), {
  ind <- "suicide"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 50.55570)
})

testthat::test_that(paste0("sdg_transfats returns accurate values:"), {
  ind <- "transfats"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default"
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 100)
})

testthat::test_that(paste0("sdg_water returns accurate values:"), {
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
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})

testthat::test_that(paste0("sdg_water_rural  returns accurate values:"), {

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
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})

testthat::test_that(paste0("sdg_water_rural returns accurate values:"), {

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
                                             scenario_function = "sdg"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 83.75)
})
