test_data <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet")

test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2022-10-13T17-10-12.parquet")

testthat::test_that("HEP data recycling returns right number of rows", {
  test_data_calculated_hep <- test_data_calculated %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
    dplyr::filter(!stringr::str_detect(ind, "routine_num$|campaign|surviving_infants")) %>%
    dplyr::select(iso3, year, ind, scenario, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  test_data_hep <- test_data %>%
    recycle_data("hep") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
    dplyr::filter(!stringr::str_detect(ind, "routine_num$|campaign")) %>%
    dplyr::select(iso3, year, ind, scenario, value) %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  testthat::expect_equal(test_data_hep, test_data_calculated_hep)
})

testthat::test_that("HPOP data recycling returns right number of rows", {
  test_data_calculated_hpop <- test_data_calculated %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  test_data_hpop <- test_data %>%
    recycle_data("hpop") %>%
    dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  testthat::expect_equal(test_data_hpop, test_data_calculated_hpop)
})

testthat::test_that("UHC data recycling returns right number of rows", {
  test_data_calculated_uhc <- test_data_calculated %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
    dplyr::filter(!(ind == "espar" & recycled == TRUE & year < 2018)) %>%
    dplyr::select(iso3, ind, year, scenario, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  test_data_uhc <- test_data %>%
    recycle_data("uhc") %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
    dplyr::filter(!(ind == "espar" & year < 2018)) %>%
    dplyr::select(iso3, ind, year, scenario, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  testthat::expect_equal(test_data_uhc, test_data_calculated_uhc)
})


testthat::test_that("recycle_data and transform_(recycle = TRUE) get same results:", {
  hep_recycle <- test_data %>%
    recycle_data("hep") %>%
    transform_hep_data(scenario = "scenario")

  hep_transform <- test_data %>%
    transform_hep_data(scenario = "scenario", recycle = TRUE)

  testthat::expect_equal(hep_transform, hep_recycle)

  hpop_recycle <- test_data %>%
    recycle_data("hpop") %>%
    transform_hpop_data()

  hpop_transform <- test_data %>%
    transform_hpop_data(recycle = TRUE)

  testthat::expect_equal(hpop_transform, hpop_recycle)

  uhc_recycle <- test_data %>%
    recycle_data("uhc") %>%
    transform_uhc_data()

  uhc_transform <- test_data %>%
    transform_uhc_data(recycle = TRUE)

  testthat::expect_equal(uhc_transform, uhc_recycle)
})

testthat::test_that("make_default_scenario adds a default scenario to data frame:", {
  make_default <- test_data %>%
    make_default_scenario(billion = "all", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  make_default_hep <- make_default_scenario(test_data, billion = "hep", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  hep_recycle <- test_data %>%
    recycle_data("hep", trim_years = FALSE) %>%
    dplyr::filter(scenario == "pre_covid_trajectory") %>%
    dplyr::mutate(scenario = "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  testthat::expect_equal(make_default_hep, hep_recycle)

  make_default_uhc <- make_default_scenario(test_data, billion = "uhc", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  uhc_recycle <- test_data %>%
    recycle_data("uhc", trim_years = FALSE) %>%
    dplyr::filter(scenario == "pre_covid_trajectory") %>%
    dplyr::mutate(scenario = "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  testthat::expect_equal(make_default_uhc, uhc_recycle)

  make_default_hpop <- make_default_scenario(test_data, billion = "hpop", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  hpop_recycle <- test_data %>%
    recycle_data("hpop", trim_years = FALSE) %>%
    dplyr::filter(scenario == "pre_covid_trajectory") %>%
    dplyr::mutate(scenario = "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind) %>%
    dplyr::select(-"recycled")

  testthat::expect_equal(make_default_hpop, hpop_recycle)

  all_default <- dplyr::bind_rows(hep_recycle, uhc_recycle) %>%
    dplyr::bind_rows(hpop_recycle) %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind)

  testthat::expect_equal(make_default, all_default)
})

testthat::test_that("make_default_scenario adds a default scenario to data frame, even when `default` is not present in the dateframe:", {
  hpop_default <- test_data %>%
    make_default_scenario(billion = "hpop") %>%
    dplyr::filter(scenario == "default")

  testthat::expect_true(nrow(hpop_default) >0)

  hep_default <- test_data %>%
    make_default_scenario(billion = "hep") %>%
    dplyr::filter(scenario == "default")

  testthat::expect_true(nrow(hep_default) >0)

  # With UHC, not having a default in the case of test_data results into an error as we check for ability to run calculations.
  testthat::expect_error(test_data %>%
                           make_default_scenario(billion = "uhc") %>%
                           dplyr::filter(scenario == "default"))
})

test_df <- tibble::tibble(
  value = 20:27,
  year = 2018:2025,
  iso3 = "AFG",
  ind = "water",
  scenario = c(rep("routine", 2), rep("reference_infilling", 6)),
  type = c(rep("reported", 2), rep("projected", 6))
) %>%
  dplyr::add_row(
    value = 18:19,
    scenario = "covid_shock",
    iso3 = "AFG",
    year = 2020:2021,
    type = rep("reported", 2),
    ind = "water"
  ) %>%
  dplyr::add_row(
    value = 32:35,
    scenario = "default",
    iso3 = "AFG",
    year = 2022:2025,
    type = rep("projected", 4),
    ind = "water"
  )

testthat::test_that("recycling functions manages properly covid_shock", {
  recycled_df <- test_df %>%
    recycle_data("hpop")

  testthat::expect_equal(dplyr::pull(dplyr::filter(recycled_df, scenario == "default", year == 2018), value), 20)
  testthat::expect_equal(dplyr::pull(dplyr::filter(recycled_df, scenario == "default", year == 2020), value), 18)
})

testthat::test_that("recycling functions don't recycle base scenarios", {
  recycled_df <- test_df %>%
    recycle_data("hpop") %>%
    dplyr::filter(scenario %in% c(
      "routine", "covid_shock", "reference_infilling"
    ))

  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "routine"))), 2)
  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "covid_shock"))), 2)
  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "reference_infilling"))), 6)
})

test_df_no_reference_infilling <- tibble::tibble(
  value = 20:27,
  year = 2018:2025,
  iso3 = "AFG",
  ind = "water",
  scenario = c(rep("routine", 8)),
  type = c(rep("reported", 2), rep("projected", 6))
) %>%
  dplyr::add_row(
    value = 18:19,
    scenario = "covid_shock",
    iso3 = "AFG",
    year = 2020:2021,
    type = rep("reported", 2),
    ind = "water"
  ) %>%
  dplyr::add_row(
    value = 32:35,
    scenario = "default",
    iso3 = "AFG",
    year = 2022:2025,
    type = rep("projected", 4),
    ind = "water"
  )

testthat::test_that("recycling functions recycle with missing base scenarios", {
  recycled_df <- test_df_no_reference_infilling %>%
    recycle_data("hpop") %>%
    dplyr::filter(scenario %in% c(
      "routine", "covid_shock"
    ))

  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "routine"))), 8)
  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "covid_shock"))), 2)
  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df, scenario == "reference_infilling"))), 0)
})

testthat::test_that("recycling return one value when two base scenarios are present",{
  duplicated_scenarios <- test_df %>%
    dplyr::add_row(
      value = 34:35,
      scenario = "reference_infilling",
      iso3 = "AFG",
      year = 2018:2019,
      type = rep("projected", 2),
      ind = "water"
    ) %>%
    dplyr::arrange(iso3, ind, year)

  recycled_df <- recycle_data(duplicated_scenarios, billion = "hpop")

  testthat::expect_equal(length(dplyr::pull(dplyr::filter(recycled_df,
                                                          year == 2018,
                                                          scenario == "default"))),
                         1)

})
