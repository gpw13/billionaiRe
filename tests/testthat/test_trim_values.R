testthat::test_that("trim_values trims properly:", {
  df_small_is_best <- tibble::tibble(
    value = 0:20,
    scenario_value = -10:10,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  df_test_small_not_keep_all_years <- df_small_is_best %>%
    dplyr::mutate(value = dplyr::case_when(
      scenario_value %in% 0:100 ~ scenario_value,
      scenario_value < 0 ~ 0L,
      scenario_value > 100 ~ 100L
    )) %>%
    dplyr::select(-"scenario_value")

  df_trim_small_not_keep_all_years <- trim_values(df_small_is_best,
    "scenario_value",
    small_is_best = TRUE,
    keep_better_values = FALSE,
    trim_years = FALSE
  )

  testthat::expect_equal(df_trim_small_not_keep_all_years, df_test_small_not_keep_all_years)

  df_test_small_not_keep <- df_small_is_best %>%
    dplyr::mutate(value = dplyr::case_when(
      scenario_value %in% 0:100 ~ scenario_value,
      scenario_value < 0 ~ 0L,
      scenario_value > 100 ~ 100L
    )) %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::select(-"scenario_value")

  df_trim_small_not_keep <- trim_values(df_small_is_best,
    "scenario_value",
    small_is_best = TRUE,
    keep_better_values = FALSE
  )

  testthat::expect_equal(df_trim_small_not_keep, df_test_small_not_keep)

  df_small_is_best_keep <- df_small_is_best %>%
    dplyr::mutate(
      value = 10:30,
      scenario_value = 0:20
    )

  df_test_small_keep <- df_small_is_best_keep %>%
    dplyr::mutate(value = dplyr::case_when(
      scenario_value %in% 0:100 & scenario_value <= value ~ scenario_value,
      scenario_value > value & value < 0 ~ 0L,
      scenario_value > value & value > 100 ~ 100L,
      TRUE ~ 0L
    )) %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::select(-"scenario_value")


  df_trim_small_keep <- trim_values(df_small_is_best_keep,
    "scenario_value",
    small_is_best = TRUE,
    keep_better_values = TRUE
  )

  testthat::expect_equal(df_trim_small_keep, df_test_small_keep)

  df_high_is_best <- tibble::tibble(
    value = 80:100,
    scenario_value = 90:110,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  df_test_high_not_keep <- df_high_is_best %>%
    dplyr::mutate(value = dplyr::case_when(
      scenario_value %in% 0:100 ~ scenario_value,
      scenario_value < 0 ~ 0L,
      scenario_value > 100 ~ 100L
    )) %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::select(-"scenario_value")

  df_trim_high_not_keep <- trim_values(df_high_is_best,
    "scenario_value",
    small_is_best = FALSE,
    keep_better_values = FALSE
  )

  testthat::expect_equal(df_trim_high_not_keep, df_test_high_not_keep)


  df_high_is_best_keep <- df_high_is_best %>%
    dplyr::mutate(
      value = 90:110,
      scenario_value = 80:100
    )

  df_test_high_keep <- df_high_is_best_keep %>%
    dplyr::mutate(value = dplyr::case_when(
      scenario_value %in% 0:100 & scenario_value >= value ~ scenario_value,
      scenario_value < value & value < 0 ~ 0L,
      scenario_value < value & value > 100 ~ 100L,
      TRUE ~ value
    )) %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::select(-"scenario_value")

  df_trim_high_keep <- trim_values(df_high_is_best_keep,
    "scenario_value",
    small_is_best = FALSE,
    keep_better_values = TRUE
  )

  testthat::expect_equal(df_trim_high_keep, df_test_high_keep)
})
