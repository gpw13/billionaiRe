test_hpop_scenarios <- function(ind) {
  testthat::test_that(paste0("add_scenarios_indicators returns  values for ", ind, " :"), {
    df <- tibble::tibble(
      value = 60:80,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default"
    )

    df_add_indicator <- add_scenario_indicator(df,
      indicator = ind,
      baseline_year = 2010,
      scenario_function = "halt_rise"
    )

    df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "halt_rise", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_halt_rise_2025, 60)

    df_add_indicator_fixed_target <- add_scenario_indicator(df,
      indicator = ind,
      target_year = 2025,
      scenario_function = "fixed_target",
      target_value = 90,
      small_is_best = TRUE # Test that is replaced by get_small_is_best(ind) value in add_scenario_indicator
    )

    df_add_indicator_90_2025_2025 <- df_add_indicator_fixed_target %>%
      dplyr::filter(scenario == "90_2025", year == 2025) %>%
      dplyr::pull(value)

    expected_value <- dplyr::if_else(get_small_is_best(ind), 68, 90)

    testthat::expect_equal(df_add_indicator_90_2025_2025, expected_value)

    df_small <- tibble::tibble(
      value = 60:40,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default"
    )

    df_add_indicator_fixed_target <- add_scenario_indicator(df_small,
      indicator = ind,
      target_year = 2025,
      scenario_function = "fixed_target",
      target_value = 30,
      small_is_best = TRUE # Test that is replaced by get_small_is_best(ind) value in add_scenario_indicator
    )

    df_add_indicator_30_2025_2025 <- df_add_indicator_fixed_target %>%
      dplyr::filter(scenario == "30_2025", year == 2025) %>%
      dplyr::pull(value)

    expected_value <- dplyr::if_else(get_small_is_best(ind), 30, 52)

    testthat::expect_equal(df_add_indicator_30_2025_2025, expected_value)
  })
}

hpop_ind <- billion_ind_codes("hpop")

purrr::walk(hpop_ind, ~ test_hpop_scenarios(.x))

testthat::test_that("add_scenarios runs properly on hpop_df", {
  hpop_halt_rise <- hpop_df %>%
    dplyr::mutate(scenario = "default") %>%
    add_scenario("halt_rise", baseline_year = 2018)

  hpop_halt_rise_2023 <- hpop_halt_rise %>%
    dplyr::filter(scenario == "halt_rise", year == 2023) %>%
    dplyr::select(-scenario, -year, -type)

  hpop_2018 <- hpop_df %>%
    dplyr::filter(year == 2018) %>%
    dplyr::select(-year, -type)

  testthat::expect_equal(hpop_halt_rise_2023, hpop_2018)
})

testthat::test_that("add_scenarios runs properly on test_data", {
  test_data <- billionaiRe:::load_test_data("test_data") %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
    make_default_scenario(billion = "hpop") %>%
    dplyr::filter(!ind %in% c("child_viol", "devontrack", "ipv"))

  hpop_halt_rise <- test_data %>%
    add_scenario("halt_rise", baseline_year = 2018)

  hpop_halt_rise_2023 <- hpop_halt_rise %>%
    dplyr::filter(scenario == "halt_rise", year == 2025) %>%
    dplyr::select(iso3, ind, value)

  hpop_2018 <- test_data %>%
    dplyr::filter(year == 2018) %>%
    dplyr::select(iso3, ind, value)

  testthat::expect_equal(hpop_halt_rise_2023, hpop_2018)
})
