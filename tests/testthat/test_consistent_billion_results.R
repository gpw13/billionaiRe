
testthat::test_that("basic billion calculations are consistent", {
  uhc_basic_calculated <- uhc_df %>%
    transform_uhc_data() %>%
    calculate_uhc_billion() %>%
    calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>%
    dplyr::filter(
      ind %in% c("uhc_sm", "asc", "fh"),
      year == 2023
    )

  hpop_basic_calculated <- hpop_df %>%
    transform_hpop_data() %>%
    add_hpop_populations(pop_year = 2023) %>%
    calculate_hpop_billion(end_year = 2023, pop_year = 2023)

  hep_basic_calculated <- hep_df %>%
    transform_hep_data(extrapolate_to = 2023) %>%
    calculate_hep_components() %>%
    calculate_hep_billion(end_year = 2023, pop_year = 2023) %>%
    dplyr::filter(
      ind %in% c(
        "prevent",
        "espar",
        "detect_respond",
        "hep_idx"
      ),
      year == 2023
    )

  all_basic_calculated <- uhc_basic_calculated %>%
    dplyr::bind_rows(hpop_basic_calculated) %>%
    dplyr::bind_rows(hep_basic_calculated)

  testthat::expect_equal(all_basic_calculated, billionaiRe:::basic_test_calculated)
})

testthat::test_that("complexe billion calculations without scenarios are consistent", {
  # test_data <- billionaiRe::load_billion_data("all", "test_data")
  # test_data_calculated <- billionaiRe::load_billion_data("all", "test_data_calculated")
  #
  # test_data_one_scenario_hep <- test_data %>%
  #   recycle_data(billion = "hep") %>%
  #   dplyr::filter(scenario == "pre_covid_bau") %>%
  #   transform_hep_data() %>%
  #   calculate_hep_components() %>%
  #   calculate_hep_billion()
  #
  # test_data_one_scenario_hpop <- test_data %>%
  #   recycle_data(billion = "hpop") %>%
  #   dplyr::filter(scenario == "pre_covid_bau") %>%
  #   transform_hpop_data() %>%
  #   add_hpop_populations() %>%
  #   calculate_hpop_billion()
  #
  # test_data_one_scenario_uhc <- test_data %>%
  #   recycle_data(billion = "uhc") %>%
  #   dplyr::filter(scenario == "pre_covid_bau") %>%
  #   transform_uhc_data() %>%
  #   calculate_uhc_billion() %>%
  #   calculate_uhc_contribution()
  #
  # test_data_one_scenario_all <- test_data_one_scenario_hep %>%
  #   dplyr::bind_rows (test_data_one_scenario_hpop) %>%
  #   dplyr::bind_rows(test_data_one_scenario_uhc) %>%
  #   dplyr::distinct()
  #
  # test_data_calculated_one_scenario <- test_data_calculated %>%
  #   dplyr::filter(scenario == "pre_covid_bau")
  #
  # testthat::expect_equal(all_basic_calculated, test_data_calculated)
})

testthat::test_that("complexe billion calculations with scenarios are consistent", {
  # test_data <- billionaiRe::load_billion_data("all", "test_data")
  # test_data_calculated <- billionaiRe::load_billion_data("all", "test_data_calculated")
  #
  # test_data_scenarios_hep <- test_data %>%
  #   recycle_data(billion = "hep") %>%
  #   transform_hep_data(scenario = "scenario") %>%
  #   calculate_hep_components(scenario = "scenario") %>%
  #   calculate_hep_billion(scenario = "scenario")
  #
  # test_data_scenarios_hpop <- test_data %>%
  #   recycle_data(billion = "hpop") %>%
  #   transform_hpop_data() %>%
  #   add_hpop_populations() %>%
  #   calculate_hpop_billion(scenario = "scenario")
  #
  # test_data_scenarios_uhc <- test_data %>%
  #   recycle_data(billion = "uhc") %>%
  #   transform_uhc_data() %>%
  #   calculate_uhc_billion(scenario = "scenario") %>%
  #   calculate_uhc_contribution(scenario = "scenario")
  #
  # test_data_scenarios_all <- test_data_scenarios_hep %>%
  #   dplyr::bind_rows (test_data_scenarios_hpop) %>%
  #   dplyr::bind_rows(test_data_scenarios_uhc) %>%
  #   dplyr::distinct()
  #
  # test_data_calculated_scenarios <- test_data_calculated
  #
  # testthat::expect_equal(test_data_calculated_scenarios, test_data_scenarios_all)
})
