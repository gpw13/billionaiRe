testthat::test_that("basic billion calculations are consistent", {
  uhc_basic_calculated <- uhc_df %>%
    transform_uhc_data() %>%
    calculate_uhc_billion() %>%
    calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>%
    dplyr::filter(
      ind %in% c("uhc_sm", "asc", "fh"),
      year == 2023
    ) %>%
    dplyr::mutate(source = dplyr::case_when(
      stringr::str_detect(source, "WHO DDI calculation") ~ "WHO DDI calculation, January 2022",
      TRUE ~ source
    ))

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
    ) %>%
    dplyr::mutate(source = dplyr::case_when(
      stringr::str_detect(source, "WHO DDI") ~ "WHO DDI, January 2022",
      TRUE ~ source
    ))

  all_basic_calculated <- uhc_basic_calculated %>%
    dplyr::bind_rows(hpop_basic_calculated) %>%
    dplyr::bind_rows(hep_basic_calculated)

  testthat::expect_equal(all_basic_calculated, billionaiRe:::basic_test_calculated)
})

test_data <- load_misc_data("test_data/test_data.parquet")

test_data_calculated <- load_misc_data("test_data/test_data_calculated.parquet")


testthat::test_that("HEP complexe billion calculations without scenarios are consistent", {

  test_data_calculated_one_scenario <- test_data_calculated %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(scenario, iso3, ind, year)

  test_data_calculated_one_scenario_hep <- test_data_calculated_one_scenario %>%
    dplyr::mutate(transform_value = dplyr::case_when(
      ind == "espar" & is.na(level) ~ NA_real_,
      TRUE ~ transform_value
    )) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, transform_value, level) %>%
    dplyr::arrange(iso3, ind, year)

  # HEP
  test_data_one_scenario_hep <- test_data %>%
    recycle_data(billion = "hep") %>%
    dplyr::filter(scenario == "default") %>%
    transform_hep_data() %>%
    calculate_hep_components() %>%
    calculate_hep_billion() %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, transform_value, level) %>%
    dplyr::arrange(iso3, ind, year)

  testthat::expect_equal(test_data_one_scenario_hep, test_data_calculated_one_scenario_hep)
})


testthat::test_that("HPOP complexe billion calculations without scenarios are consistent", {

  test_data_one_scenario_hpop <- test_data %>%
    recycle_data(billion = "hpop") %>%
    dplyr::filter(scenario == "default") %>%
    transform_hpop_data() %>%
    add_hpop_populations() %>%
    calculate_hpop_billion() %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, transform_value, population) %>%
    dplyr::arrange(iso3, ind, year)

  test_data_calculated_one_scenario <- test_data_calculated %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(scenario, iso3, ind, year)

  test_data_calculated_one_scenario_hpop <- test_data_calculated_one_scenario %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, transform_value, population) %>%
    dplyr::arrange(iso3, ind, year)

  testthat::expect_equal(test_data_one_scenario_hpop, test_data_calculated_one_scenario_hpop)
})

testthat::test_that("UHC complexe billion calculations without scenarios are consistent", {

  test_data_one_scenario_uhc <- test_data %>%
    recycle_data(billion = "uhc") %>%
    dplyr::filter(scenario == "default") %>%
    transform_uhc_data() %>%
    calculate_uhc_billion() %>%
    calculate_uhc_contribution() %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::select(iso3, ind, year, transform_value) %>%
    dplyr::arrange(iso3, ind, year)

  test_data_calculated_one_scenario <- test_data_calculated %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(scenario, iso3, ind, year)

  test_data_calculated_one_scenario_uhc <- test_data_calculated_one_scenario %>%
    dplyr::mutate(transform_value = dplyr::case_when(
      ind == "espar" & !is.na(level) ~ NA_real_,
      TRUE ~ transform_value
    )) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, transform_value) %>%
    dplyr::arrange(iso3, ind, year)


  testthat::expect_equal(test_data_one_scenario_uhc, test_data_calculated_one_scenario_uhc)
})

testthat::test_that("HEP complexe billion calculations with scenarios are consistent", {

  test_data_calculated <- test_data_calculated %>%
    dplyr::mutate(transform_value = dplyr::case_when(
      ind == "espar" & is.na(level) ~ NA_real_,
      TRUE ~ transform_value
    )) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario, transform_value, level) %>%
    dplyr::arrange(scenario, iso3, ind, year)

  # HEP
  test_data_hep <- test_data %>%
    recycle_data(billion = "hep") %>%
    transform_hep_data(scenario = "scenario") %>%
    calculate_hep_components(scenario = "scenario") %>%
    calculate_hep_billion(scenario = "scenario") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario, transform_value, level) %>%
    dplyr::arrange(scenario, iso3, ind, year)

  testthat::expect_equal(test_data_hep, test_data_calculated)
})

testthat::test_that("HPOP complexe billion calculations with scenarios are consistent", {

  test_data_one_scenario_hpop <- test_data %>%
    recycle_data(billion = "hpop") %>%
    transform_hpop_data() %>%
    add_hpop_populations() %>%
    calculate_hpop_billion(scenario = "scenario") %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario, transform_value, population) %>%
    dplyr::arrange(iso3, scenario, ind, year)

  test_data_calculated_one_scenario_hpop <- test_data_calculated %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario, transform_value, population) %>%
    dplyr::arrange(iso3, scenario, ind, year)

  testthat::expect_equal(test_data_one_scenario_hpop, test_data_calculated_one_scenario_hpop)
})

testthat::test_that("UHC complexe billion calculations with scenarios are consistent", {

  test_data_one_scenario_uhc <- test_data %>%
    recycle_data(billion = "uhc") %>%
    transform_uhc_data() %>%
    calculate_uhc_billion(scenario = "scenario") %>%
    calculate_uhc_contribution(scenario = "scenario") %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::select(iso3, scenario, ind, year, transform_value) %>%
    dplyr::arrange(scenario, iso3, ind, year)

  test_data_calculated_one_scenario_uhc <- test_data_calculated %>%
    dplyr::mutate(transform_value = dplyr::case_when(
      ind == "espar" & !is.na(level) ~ NA_real_,
      TRUE ~ transform_value
    )) %>%
    dplyr::filter(!is.na(transform_value)) %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::select(iso3, scenario, ind, year, transform_value) %>%
    dplyr::arrange(scenario, iso3, ind, year)

  testthat::expect_equal(test_data_one_scenario_uhc, test_data_calculated_one_scenario_uhc)
})
