test_data <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet")

test_data_param_exotic_names <- test_data %>%
  dplyr::rename(value_test = value,
         scenario_test = scenario)

test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2022-03-06T09-30-41.parquet")

test_data_calculated_param_exotic_names <- test_data_calculated %>%
  dplyr::rename(value_test = value,
                scenario_test = scenario,
                transform_value_test = transform_value,
                level_test = level)

testthat::test_that("HEP calculations are consistent with exotic parametrized column names", {
  test_data_calculated_param_exotic_names <- test_data_calculated_param_exotic_names %>%
    dplyr::mutate(transform_value_test = dplyr::case_when(
      ind == "espar" & is.na(level_test) ~ NA_real_,
      TRUE ~ transform_value_test
    )) %>%
    dplyr::filter(!is.na(transform_value_test)) %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario_test, transform_value_test, level_test) %>%
    dplyr::arrange(scenario_test, iso3, ind, year)

  # HEP
  test_data_hep <- test_data_param_exotic_names %>%
    recycle_data(value_col = "value_test", scenario_col = "scenario_test", billion = "hep") %>%
    transform_hep_data(value_col = "value_test", scenario_col = "scenario_test") %>%
    calculate_hep_components(scenario_col = "scenario_test", transform_value_col = "transform_value_test") %>%
    calculate_hep_billion(scenario_col = "scenario_test", transform_value_col = "transform_value_test") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario_test, transform_value_test, level_test) %>%
    dplyr::arrange(scenario_test, iso3, ind, year)

  testthat::expect_equal(test_data_hep, test_data_calculated_param_exotic_names)
})

testthat::test_that("HPOP calculations are consistent with exotic parametrized column names", {
  test_data_hpop <- test_data_param_exotic_names %>%
    recycle_data(value_col = "value_test", scenario_col = "scenario_test", billion = "hpop") %>%
    transform_hpop_data(value_col = "value_test") %>%
    add_hpop_populations() %>%
    calculate_hpop_billion(scenario_col = "scenario_test", transform_value_col = "transform_value_test") %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario_test, transform_value_test, population) %>%
    dplyr::arrange(iso3, scenario_test, ind, year)

  test_data_calculated_param_exotic_names_hpop <- test_data_calculated_param_exotic_names %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop", include_calculated = T)) %>%
    dplyr::select(iso3, ind, year, scenario_test, transform_value_test, population) %>%
    dplyr::arrange(iso3, scenario_test, ind, year)

  testthat::expect_equal(test_data_hpop, test_data_calculated_param_exotic_names_hpop)
})

testthat::test_that("UHC calculations are consistent with exotic parametrized column names", {
  test_data_one_scenario_uhc <- test_data_param_exotic_names %>%
    recycle_data(value_col = "value_test", scenario_col = "scenario_test", billion = "uhc", default_scenario = "pre_covid_trajectory") %>%
    transform_uhc_data(value_col = "value_test") %>%
    calculate_uhc_billion(scenario_col = "scenario_test", value_col = "value_test", transform_value_col = "transform_value_test") %>%
    calculate_uhc_contribution(scenario_col = "scenario_test", value_col = "value_test", transform_value_col = "transform_value_test") %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::filter(!is.na(transform_value_test)) %>%
    dplyr::select(iso3, scenario_test, ind, year, transform_value_test) %>%
    dplyr::arrange(scenario_test, iso3, ind, year)

  test_data_calculated_param_exotic_names_one_scenario_uhc <- test_data_calculated_param_exotic_names %>%
    dplyr::mutate(transform_value_test = dplyr::case_when(
      ind == "espar" & !is.na(level_test) ~ NA_real_,
      TRUE ~ transform_value_test
    )) %>%
    dplyr::filter(!is.na(transform_value_test)) %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc", include_calculated = T)) %>%
    dplyr::select(iso3, scenario_test, ind, year, transform_value_test) %>%
    dplyr::arrange(scenario_test, iso3, ind, year)

  testthat::expect_equal(test_data_one_scenario_uhc, test_data_calculated_param_exotic_names_one_scenario_uhc)
})

test_data_non_param_exotic_names <- test_data %>%
  dplyr::rename(iso3_test = iso3,
                ind_test = ind,
                year_test = year)

testthat::test_that("transform_uhc throws error with exotic non-parametrized column names", {
  testthat::expect_error(transform_uhc_data(test_data_non_param_exotic_names))
})

testthat::test_that("calculate_uhc_billion throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year)

  testthat::expect_error(calculate_uhc_billion(test_data_calculated_non_param_exotic_names))
})

testthat::test_that("calculate_uhc_contribution throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year)

  testthat::expect_error(calculate_uhc_contribution(test_data_calculated_non_param_exotic_names))
})

testthat::test_that("transform_hep throws error with exotic non-parametrized column names", {
  testthat::expect_error(transform_hep_data(test_data_non_param_exotic_names))
})

testthat::test_that("calculate_hep_billion throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year)

  testthat::expect_error(calculate_hep_billion(test_data_calculated_non_param_exotic_names))
})

testthat::test_that("calculate_hep_contribution throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year)

  testthat::expect_error(calculate_hep_contribution(test_data_calculated_non_param_exotic_names))
})

testthat::test_that("transform_hpop throws error with exotic non-parametrized column names", {
  testthat::expect_error(transform_hpop_data(test_data_non_param_exotic_names))
})

testthat::test_that("calculate_hpop_billion throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year)

  testthat::expect_error(calculate_hpop_billion(test_data_calculated_non_param_exotic_names))
})

testthat::test_that("add_hpop_populations throws error with exotic non-parametrized column names", {
  test_data_calculated_non_param_exotic_names <- test_data_calculated %>%
    dplyr::rename(iso3_test = iso3,
                  ind_test = ind,
                  year_test = year,
                  pop = population)

  testthat::expect_error(add_hpop_populations(test_data_calculated_non_param_exotic_names))
})
