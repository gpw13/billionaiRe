testthat::test_that(paste0("accelerate_adult_obese returns accurate values:"), {
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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)
})

testthat::test_that(paste0("accelerate_alcohol returns accurate values:"), {
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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 54)
})

testthat::test_that(paste0("accelerate_child_obese returns accurate values:"), {
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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)
})

testthat::test_that(paste0("accelerate_child_viol returns accurate values:"), {
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
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)
})

testthat::test_that(paste0("accelerate_devontrack returns accurate values:"), {
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
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, c(81, 70, 75))
})

testthat::test_that(paste0("accelerate_hpop_sanitation, accelerate_hpop_sanitation_urban, accelerate_hpop_rural  returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)
})

testthat::test_that(paste0("accelerate_hpop_tobacco returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 42)
})

testthat::test_that(paste0("accelerate_ipv returns accurate values:"), {
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
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)
})

testthat::test_that(paste0("accelerate_overweight returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 11.0119977)
})

testthat::test_that(paste0("accelerate_pm25 returns accurate values:"), {
  ind <- "pm25"

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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 68 + (68 * -0.02) * (2025 - 2018))
})

testthat::test_that(paste0("accelerate_road returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 70 + ((70 / 2) - 70) / (2030 - 2020) * (2025 - 2020))
})

testthat::test_that(paste0("accelerate_stunting returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 50.55570)
})

testthat::test_that(paste0("accelerate_transfats returns accurate values:"), {
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
    scenario_function = "accelerate"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 100)
})

testthat::test_that(paste0("accelerate_wasting returns accurate values:"), {
  ind <- "wasting"

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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 11.0119977)
})

testthat::test_that(paste0("accelerate_water, water_urban and water_rural returns accurate values:"), {
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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

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

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)
})

testthat::test_that("accelerate can be run on all hpop indicators:", {
  hpop_test_df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    )
  ) %>%
    tidyr::expand_grid(ind = billion_ind_codes("hpop"))

  calculated_test_data <- add_scenario(hpop_test_df, "accelerate")

  testthat::expect_equal(nrow(calculated_test_data), 609)

  testthat::expect_error(load_misc_data("test_data/test_data.parquet") %>%
                           dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
                           make_default_scenario(billion = "uhc") %>%
                           add_scenario("accelerate"),
                         NA)
})

