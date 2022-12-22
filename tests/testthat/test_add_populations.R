test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2022-10-13T17-10-12.parquet")


testthat::test_that("add_populations can be run without error",{
  test_add_hep_population <- test_data_calculated %>%
    add_populations(scenario_col = "scenario")

  testthat::expect_error(test_add_hep_population, NA)
})
