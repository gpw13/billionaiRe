test_data <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet")

testthat::test_that("scenarios_quantile produces accurate results:", {
  test_data_quantile <- test_data %>%
    dplyr::filter(ind == "water") %>%
    make_default_scenario(billion = "hpop") %>%
    dplyr::filter(scenario == "default")

  df_scenario_quantile <- scenario_quantile(test_data_quantile, n = 5)

  afg_2025 <- df_scenario_quantile %>%
    dplyr::filter(
      iso3 == "AFG", year == 2025,
      scenario == "quantile_5"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(afg_2025, 32.28041, tolerance = 5)

  afg_2025 <- df_scenario_quantile %>%
    dplyr::filter(
      iso3 == "AFG", year == 2025,
      scenario == "quantile_5"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(afg_2025, 32.28041, tolerance = 5)

  uga_2022 <- df_scenario_quantile %>%
    dplyr::filter(
      iso3 == "UGA", year == 2022,
      scenario == "quantile_5"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(uga_2022, 18.67948, tolerance = 5)
})


testthat::test_that("scenario_best_in_region produces accurate results:", {
  test_data_best_in_region <- test_data %>%
    make_default_scenario(billion = "hep") %>%
    dplyr::filter(scenario == "default", ind == "espar")

  df_scenario_best_in_region <- scenario_best_in_region(test_data_best_in_region, keep_better_values = FALSE)

  afg_2025 <- df_scenario_best_in_region %>%
    dplyr::filter(
      iso3 == "AFG", year == 2025,
      scenario == "best_in_region"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(afg_2025, 33.60000, tolerance = 5)

  bol_2025 <- df_scenario_best_in_region %>%
    dplyr::filter(
      iso3 == "BOL", year == 2025,
      scenario == "best_in_region"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(bol_2025, 64.72000, tolerance = 5)

  uga_2022 <- df_scenario_best_in_region %>%
    dplyr::filter(
      iso3 == "UGA", year == 2022,
      scenario == "best_in_region"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(uga_2022, 46.46667, tolerance = 5)
})
