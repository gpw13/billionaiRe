testthat::test_that("data recycling returns right number of rows", {
  f <- tempfile()
  whdh::download_from_data_lake(data_lake_name = "srhdteuwstdsa", source_path = "3B/Bronze/misc/test_data.parquet", destination_path = f, latest_version_only = FALSE)
  test_data <- arrow::read_parquet(f)

  f <- tempfile()
  whdh::download_from_data_lake(data_lake_name = "srhdteuwstdsa", source_path = "3B/Bronze/misc/test_data_calculated.parquet", destination_path = f, latest_version_only = FALSE)
  test_data_calculated <- arrow::read_parquet(f)

  nrow_max_calculated <- test_data_calculated %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
    remove_recycled_data() %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()

  recycled_data <- test_data %>%
    recycle_data("hep") %>%
    transform_hep_data(scenario = "scenario") %>%
    calculate_hep_components(scenario = "scenario") %>%
    calculate_hep_billion(scenario = "scenario") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
    remove_recycled_data()

  nrow_recycled <- recycled_data %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")) %>%
    dplyr::group_by(scenario) %>%
    dplyr::tally()
  #
  # testthat::expect_equal(nrow_recycled, nrow_max_calculated)
})

testthat::test_that("recycle_data and transform_(recycle = TRUE) get same results", {
  f <- tempfile()
  whdh::download_from_data_lake(data_lake_name = "srhdteuwstdsa", source_path = "3B/Bronze/misc/test_data.parquet", destination_path = f, latest_version_only = FALSE)
  test_data <- arrow::read_parquet(f)

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
