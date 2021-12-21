testthat::test_that("espar returns appropriate values", {
  df <- tidyr::expand_grid(
    iso3 = c("AFG", "BGD", "PAK", "BRN", "CHE", "POL", "SWE", "VUT"),
    year = 2018:2020,
    ind = billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "espar")]
  ) %>%
    dplyr::mutate(
      value = c(
        35, 13, 20, 20, 0, 60, 60, 60, 80, 20, 40, 40, 20, 60, 80, 80, 80, 40, 27, 20, 20, 40, 40, 60, 20, 40, 20, 20, 20, 20, 0, 20, 43, 33, 20, 40, 40, 80, 100, 60, 80, 20, 47, 60, 20, 60, 80, 80,
        80, 40, 33, 20, 40, 40, 53, 40, 40, 80, 20, 30, 40, 20, 20, 20, 47, 33, 20, 40, 40, 90, 80, 100, 80, 20, 60, 60, 60, 60, 70, 60, 80, 40, 80, 80, 80, 80, 53, 40, 40, 80, 20, 30, 20, 40, 20, 20,
        58, 60, 60, 40, 80, 80, 100, 60, 80, 40, 73, 100, 40, 80, 80, 80, 80, 40, 47, 40, 60, 40, 60, 40, 60, 80, 60, 60, 60, 60, 40, 40, 67, 80, 80, 60, 100, 90, 100, 80, 80, 40, 73, 100, 40, 80, 80, 80,
        80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 60, 60, 60, 60, 60, 70, 80, 80, 60, 100, 90, 100, 80, 80, 60, 73, 100, 40, 80, 80, 80, 80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 80, 80, 80, 60, 60,
        51, 27, 20, 20, 40, 80, 60, 100, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 49, 27, 20, 20, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60,
        60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 52, 33, 20, 40, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 60, 40, 60, 80, 47, 40, 40, 60, 20, 40, 40, 40, 40, 100,
        rep(NA_integer_, 96),
        rep(NA_integer_, 32), 95, 93, 100, 100, 80, 100, 100, 100, 100, 80, 100, 100, 100, 100, 90, 100, 80, 80, 100, 100, 100, 100, 87, 100, 100, 60, 100, 100, 100, 100, 100, 100, rep(NA_integer_, 32),
        rep(NA_integer_, 32), 66, 80, 100, 60, 80, 100, 100, 100, 100, 40, 67, 100, 80, 20, 0, 0, 0, 60, 80, 80, 80, 80, 27, 0, 80, 0, 80, 40, 80, 0, 80, 100, 50, 0, 0, 0, 0, 100, 100, 100, 100, 0, 87, 100, 80, 80, 80, 80,
        80, 0, 33, 40, 40, 20, 33, 0, 0, 100, 0, 60, 80, 40, 60, 100,
        92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100,
        100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 91, 100, 100, 100, 100, 100, 100, 100, 100, 80, 93, 100, 80, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100,
        34, 27, 20, 40, 20, 30, 40, 20, 20, 20, 27, 20, 20, 40, 80, 100, 60, 40, 53, 20, 60, 80, 27, 20, 20, 40, 60, 20, 20, 20, 20, 20,
        rep(NA_integer_, 32),
        55, 47, 60, 40, 40, 30, 40, 20, 20, 80, 100, 100, 100, 100, 90, 100, 80, 40, 73, 40, 100, 80, 40, 40, 40, 40, 80, 40, 40, 40, 40, 40
      ),
      type = "reported",
      scenario = "default"
    )

  df_accelerated <- accelerate_espar(df, keep_better_values = FALSE)

  df_accelerated_2025 <- df_accelerated %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_accelerated_2025, c(49.282051282051285, 100, 34.07692307692308, 100, 34.07692307692308, 65.897435897435898, 100, 100))

  df_add_indicator <- add_scenario_indicator(df,
    indicator = "espar",
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, df_accelerated_2025)
})

basic_hep_test <- function(ind) {
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
      scenario_function = "accelerate",
      baseline_year = 2018
    )

    df_add_indicator_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "acceleration", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_2025, 75)

    df_2018 <- df %>%
      dplyr::filter(year <= 2018)

    df_add_indicator <- add_scenario_indicator(df_2018,
      indicator = ind,
      scenario_function = "accelerate",
      baseline_year = 2018
    )

    df_add_indicator_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "acceleration", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_2025, 68)
  })
}

purrr::walk(c("respond", "notify", "detect", "detect_respond"), basic_hep_test)

testthat::test_that("accelerate can be run on all hep indicators:", {
  hep_test_df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    )
  ) %>%
    tidyr::expand_grid(ind = billion_ind_codes("hep"))

  calculated_test_data <- add_scenario(hep_test_df, "accelerate")

  testthat::expect_equal(nrow(calculated_test_data), 609)
})
