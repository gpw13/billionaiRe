test_hpop_scenarios <- function(ind) {
  testthat::test_that(paste0("add_scenarios_indicators returns  values for ", ind, " :"), {
    df <- tibble::tibble(
      value = 60:80,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default",
      source = NA_character_,
      type = dplyr::if_else(year <= 2021, "reported", "projected")
    )

    df_add_indicator <- add_scenario_indicator(df,
      indicator = ind,
      baseline_year = 2010,
      scenario_function = "halt_rise",
      start_scenario_last_default = FALSE,
      make_default = FALSE,
      expend_bau = FALSE
    )

    df_add_indicator_halt_rise_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "halt_rise", year == 2025) %>%
      dplyr::pull(value)

    if(get_ind_metadata(ind, "small_is_best")){
      testthat::expect_equal(df_add_indicator_halt_rise_2025, 60)
    }else{
      testthat::expect_equal(df_add_indicator_halt_rise_2025, 68)
    }

    df_add_indicator_fixed_target <- add_scenario_indicator(df,
      indicator = ind,
      target_year = 2025,
      scenario_function = "fixed_target",
      scenario_name = "90_2025",
      target_value = 90,
      start_scenario_last_default = FALSE,
      small_is_best = TRUE, # Test that is replaced by get_ind_metadata(ind, "small_is_best") value in add_scenario_indicator
      make_default = FALSE,
      expend_bau = FALSE
    )

    df_add_indicator_90_2025_2025 <- df_add_indicator_fixed_target %>%
      dplyr::filter(scenario == "90_2025", year == 2025) %>%
      dplyr::pull(value)

    expected_value <- dplyr::if_else(get_ind_metadata(ind, "small_is_best"), 68, 90)

    testthat::expect_equal(df_add_indicator_90_2025_2025, expected_value)

    df_small <- tibble::tibble(
      value = 60:40,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default",
      source = NA_character_,
      type = dplyr::if_else(year <= 2021, "reported", "projected")
    )

    df_add_indicator_fixed_target <- add_scenario_indicator(df_small,
      indicator = ind,
      target_year = 2025,
      scenario_function = "fixed_target",
      scenario_name = "30_2025",
      target_value = 30,
      start_scenario_last_default = FALSE,
      small_is_best = TRUE, # Test that is replaced by get_ind_metadata(ind, "small_is_best") value in add_scenario_indicator
      make_default = FALSE,
      expend_bau = FALSE
    )

    df_add_indicator_30_2025_2025 <- df_add_indicator_fixed_target %>%
      dplyr::filter(scenario == "30_2025", year == 2025) %>%
      dplyr::pull(value)

    expected_value <- dplyr::if_else(get_ind_metadata(ind, "small_is_best"), 30, 52)

    testthat::expect_equal(df_add_indicator_30_2025_2025, expected_value)
  })
}

hpop_ind <- billion_ind_codes("hpop")

purrr::walk(hpop_ind, ~ test_hpop_scenarios(.x))
