testthat::test_that("data recycling returns right number of rows", {
  # test_data <- billionaiRe::load_billion_data("all", "test_data")
  #
  # nrow_max_calculated <- test_data_calculated %>%
  #   dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
  #   dplyr::group_by(scenario) %>%
  #   dplyr::tally()
  #
  # recycled_data <- test_data %>%
  #   recycle_data("hep") %>%
  #   dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
  #   transform_hep_data(scenario = "scenario") %>%
  #   calculate_hep_components(scenario = "scenario") %>%
  #   calculate_hep_billion(scenario = "scenario") %>%
  #   remove_recycled_data()
  #
  # nrow_recycled <- recycled_data %>%
  #   dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
  #   dplyr::group_by(scenario) %>%
  #   dplyr::tally()
  #
  # testthat::expect_equal(nrow_recycled, nrow_max_calculated)
})
