test_data <- load_misc_data("test_data/test_data/test_data_2022-01-20T14-13-10.parquet")

test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2022-01-20T14-12-38.parquet")

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
  make_default <- make_default_scenario(test_data, billion = "all") %>%
    dplyr::arrange(iso3, year, ind)

  make_default_hep <- make_default_scenario(test_data, billion = "hep") %>%
    dplyr::arrange(iso3, year, ind)

  hep_recycle <- test_data %>%
    recycle_data("hep", trim_years = FALSE) %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind)

  testthat::expect_equal(make_default_hep, hep_recycle)

  make_default_uhc <- make_default_scenario(test_data, billion = "uhc") %>%
    dplyr::arrange(iso3, year, ind)

  uhc_recycle <- test_data %>%
    recycle_data("uhc", trim_years = FALSE) %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind)

  testthat::expect_equal(make_default_uhc, uhc_recycle)

  make_default_hpop <- make_default_scenario(test_data, billion = "hpop") %>%
    dplyr::arrange(iso3, year, ind)

  hpop_recycle <- test_data %>%
    recycle_data("hpop", trim_years = FALSE) %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind)

  testthat::expect_equal(make_default_hpop, hpop_recycle)

  all_default <- dplyr::bind_rows(hep_recycle, uhc_recycle) %>%
    dplyr::bind_rows(hpop_recycle) %>%
    dplyr::distinct() %>%
    dplyr::arrange(iso3, year, ind)

  testthat::expect_equal(make_default, all_default)
})
