testthat::test_that("scenario_best_of takes the best and only the best scenario", {
  df <- tibble::tibble(
    value = c(80:100, 10:30, 30:50),
    year = rep(2010:2030, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  )

  df_best_small_is_best_2025 <- scenario_best_of(df, c("a", "b", "c"), small_is_best = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_small_is_best_2025, 25)

  df_best_high_is_best_2025 <- scenario_best_of(df, c("a", "b", "c"), small_is_best = FALSE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_high_is_best_2025, 95)

  two_ind_df <- tibble::tibble(
    value = c(10:30, 80:100, 30:50),
    year = rep(2010:2030, 3),
    ind = "adult_obese",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  ) %>%
    dplyr::bind_rows(df)

  df_best_small_is_best_two_ind_2025 <- scenario_best_of(two_ind_df, c("a", "b", "c"), small_is_best = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_small_is_best_two_ind_2025, c(25, 25))

  df_best_high_is_best_two_ind_2025 <- scenario_best_of(two_ind_df, c("a", "b", "c"), small_is_best = FALSE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_high_is_best_two_ind_2025, c(95, 95))

  two_iso_df <- two_ind_df %>%
    dplyr::mutate(ind = "water", iso3 = c(rep("testalia", 3 * 21), rep("tryalia", 3 * 21)))

  df_best_small_is_best_two_iso_2025 <- scenario_best_of(two_iso_df, c("a", "b", "c"), small_is_best = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_small_is_best_two_iso_2025, c(25, 25))

  df_best_high_is_best_two_iso_2025 <- scenario_best_of(two_iso_df, c("a", "b", "c"), small_is_best = FALSE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_high_is_best_two_iso_2025, c(95, 95))

  df_twice_same_values <- tibble::tibble(
    value = c(10:30, 10:30, 30:50),
    year = rep(2010:2030, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  )

  df_twice_same_values_best_small_is_best_2025 <- scenario_best_of(df_twice_same_values, c("a", "b", "c"), small_is_best = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_twice_same_values_best_small_is_best_2025, 25)

  df_twice_same_values_diff_interval_best_small_is_best <- tibble::tibble(
    value = c(
      seq(0, 30, length.out = length(2018:2025)),
      seq(10, 30, length.out = length(2018:2025)),
      seq(60, 100, length.out = length(2018:2025))
    ),
    year = rep(2018:2025, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", length(2018:2025)), rep("b", length(2018:2025)), rep("c", length(2018:2025)))
  )

  df_twice_same_values_diff_interval_best_small_is_best_2025 <- scenario_best_of(df_twice_same_values_diff_interval_best_small_is_best, c("a", "b", "c"), small_is_best = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c") %>%
    dplyr::pull(value)

  test_results <- df_twice_same_values_diff_interval_best_small_is_best %>%
    dplyr::filter(scenario == "a") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_twice_same_values_diff_interval_best_small_is_best_2025, test_results)

  df_twice_same_values_diff_interval_best <- tibble::tibble(
    value = c(
      seq(0, 30, length.out = length(2018:2025)),
      seq(90, 100, length.out = length(2018:2025)),
      seq(60, 100, length.out = length(2018:2025))
    ),
    year = rep(2018:2025, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", length(2018:2025)), rep("b", length(2018:2025)), rep("c", length(2018:2025)))
  )


  df_twice_same_values_diff_interval_best_2025 <- scenario_best_of(df_twice_same_values_diff_interval_best, c("a", "b", "c"), small_is_best = FALSE) %>%
    dplyr::filter(scenario == "best_of_a_b_c") %>%
    dplyr::pull(value)

  test_results <- df_twice_same_values_diff_interval_best %>%
    dplyr::filter(scenario == "b") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_twice_same_values_diff_interval_best_2025, test_results)



  df_maximize_last_value <- df %>%
    dplyr::mutate(
      value = dplyr::case_when(
        scenario == "c" & year == 2025 ~ 100L,
        TRUE ~ value
      )
    )

  df_best_small_is_best_2025 <- scenario_best_of(df_maximize_last_value, c("a", "b", "c"), small_is_best = FALSE, maximize_end_year = TRUE) %>%
    dplyr::filter(scenario == "best_of_a_b_c", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_best_small_is_best_2025, 100)

})

testthat::test_that("scenario_bau returns accurate values", {
  df <- tibble::tibble(
    value = c(80:100, 10:30, 30:50),
    year = rep(2010:2030, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  )

  df_scenario_bau <- scenario_bau(df,
                                  default_scenario = "a",
                                  bau_scenario = "a") %>%
    dplyr::filter(scenario == "business_as_usual")

  df_test <- df %>%
    dplyr::filter(
      scenario == "a",
      year %in% 2018:2025
    ) %>%
    dplyr::mutate(scenario = "business_as_usual")

  testthat::expect_equal(df_scenario_bau, df_test)

  df_scenario_bau <- df %>%
    dplyr::filter(year <= 2018) %>%
    scenario_bau(default_scenario = "a",
                 bau_scenario = "a") %>%
    dplyr::filter(scenario == "business_as_usual")

  df_test_2018 <- df %>%
    dplyr::filter(
      scenario == "a",
      year %in% 2018:2025
    ) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        year >= 2018 ~ .data[["value"]][.data[["year"]] == 2018]
      ),
      scenario = "business_as_usual"
    )

  testthat::expect_equal(df_scenario_bau, df_test_2018)

  # scenario starts from last value in default_scenario

  df_scenario_bau <- scenario_bau(df,
                                  default_scenario = "a",
                                  bau_scenario = "b") %>%
    dplyr::filter(scenario == "business_as_usual")

  df_scenario_bau_2018 <- df_scenario_bau %>%
    dplyr::filter(year == 2018,
                  scenario == "business_as_usual") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_bau_2018, 88)

  df_scenario_bau_2025 <- df_scenario_bau %>%
    dplyr::filter(year == 2025,
                  scenario == "business_as_usual") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_bau_2025, 25)

})
