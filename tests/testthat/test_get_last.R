testthat::test_that("get_last_value gets the last reported value",{
  df <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2020,
    value = 60:62,
    scenario = c(rep("routine",2), rep("covid_shock", 1)),
    type = c("reported", "estimated", rep("projected", 1)),
    ind = "water"
  )

  last_reported_year <- df %>%
    get_last_value("reported") %>%
    dplyr::pull("year")

  testthat::expect_equal(last_reported_year, 2018)
})

testthat::test_that("get_last_value gets the last reported or estimated value",{
  df <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2020,
    value = 60:62,
    scenario = c(rep("routine",2), rep("covid_shock", 1)),
    type = c("reported", "estimated", rep("projected", 1)),
    ind = "water"
  )

  last_reported_year <- df %>%
    get_last_value() %>%
    dplyr::pull("year")

  testthat::expect_equal(last_reported_year, 2019)
})

testthat::test_that("get_last_value gets the last reported value by dplyr::group_by",{
  df_water <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2020,
    value = 60:62,
    scenario = c(rep("routine",2), rep("covid_shock", 1)),
    type = c("reported", "estimated", "reported"),
    ind = "water"
  )

  df_san <- tibble::tibble(
    iso3 = "AFG",
    year = 2018:2020,
    value = 60:62,
    scenario = c(rep("routine",2), rep("covid_shock", 1)),
    type = c("reported", "estimated", "projected"),
    ind = "sanitation"
  )

  last_reported_year <- dplyr::bind_rows(df_water, df_san) %>%
    dplyr::group_by(iso3, ind, scenario) %>%
    get_last_value("reported") %>%
    dplyr::pull("year")

  testthat::expect_equal(last_reported_year, c(2018, 2020, 2018))
})

