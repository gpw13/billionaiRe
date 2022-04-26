
testthat::test_that("impute_covid_shock produces accurate results with 2020 values",{
  df <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2020,
    value = 60:62,
    scenario = c(rep("routine",2), rep("covid_shock", 1)),
    type = "reported",
    ind = "water"
  )

  df_imputed <- impute_covid_shock(df)

  testthat::expect_equal(df_imputed$value, c(60,61,62,62))
})


testthat::test_that("impute_covid_shock produces accurate results with 2021 values",{
  df <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2021,
    value = 60:63,
    scenario = c(rep("routine",2), rep("covid_shock", 2)),
    type = "reported",
    ind = "water"
  )

  df_imputed <- impute_covid_shock(df)

  testthat::expect_equal(df_imputed$value, 60:63)
})

testthat::test_that("impute_covid_shock produces accurate results one indicator, without 2020 values, but regional",{
  df <- load_billion_data("wrangled_data", "hep", "polio_routine", version = "2022-03-10", experiment = "unofficial")

  df_imputed <- impute_covid_shock(df)

  df_imputed_2021_alb <- df_imputed %>%
    dplyr::filter(iso3 == "ALB", year == 2021) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_imputed_2021_alb, 97.777778)

  df_imputed_2021_afg <- df_imputed %>%
    dplyr::filter(iso3 == "AFG", year == 2021) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_imputed_2021_afg, 75)
})

# testthat::test_that("impute_covid_shock runs on many indicator",{
#   df <- load_billion_data("wrangled_data", "hep", version = "2022-03-15", experiment = "unofficial")
#
#   testthat::expect_error(impute_covid_shock(df), NA)
# })
