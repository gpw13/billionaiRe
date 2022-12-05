test_data <- load_misc_data("test_data/test_data/test_data_2022-11-24T12-07-52.parquet")


testthat::test_that("benchmarking scenarios can be applied to all indicators", {
  benchmarked_df <- test_data %>%
    add_scenario("benchmarking",
                 default_scenario = "bau_2019_then_flat",
                 bau_scenario = "pre_covid_bau",
                 make_default = TRUE)

  testthat::expect_error(benchmarked_df, NA)

})
