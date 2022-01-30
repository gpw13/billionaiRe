testdf <- function(values = 60:80, ind = "water", type = "reported", iso3 = "testalia") {
  tibble::tibble(
    value = values,
    year = 2010:2030,
    ind = ind,
    type = type,
    iso3 = iso3,
    scenario = "default"
  )
}


testthat::test_that("scenario_covid_rapid_return produces accurate results with one iso3", {
  df <- testdf() %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_covid_dip_recover <- scenario_covid_rapid_return(df)

  df_dip_recover_2025_one_iso3 <- df_covid_dip_recover %>%
    dplyr::filter(scenario == "covid_rapid_return", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_2025_one_iso3, 64.411765)
})

testthat::test_that("scenario_covid_rapid_return produces accurate results with two iso3", {

  df <- testdf() %>%
    dplyr::bind_rows(testdf(iso3 = "testistan")) %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_dip_recover <- scenario_covid_rapid_return(df)

  df_dip_recoverr_2025_two_iso3 <- df_dip_recover %>%
    dplyr::filter(scenario == "covid_rapid_return", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(64.411765, 64.411765))
})

testthat::test_that("scenario_covid_never_return produces accurate results with one iso3", {
  df <- testdf() %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_covid_dip_recover <- scenario_covid_never_return(df)

  df_dip_recover_2025_one_iso3 <- df_covid_dip_recover %>%
    dplyr::filter(scenario == "covid_never_return", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_2025_one_iso3, 60)
})

testthat::test_that("scenario_covid_never_return produces accurate results with two iso3", {

  df <- testdf() %>%
    dplyr::bind_rows(testdf(iso3 = "testistan")) %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_dip_recover <- scenario_covid_never_return(df)

  df_dip_recoverr_2025_two_iso3 <- df_dip_recover %>%
    dplyr::filter(scenario == "covid_never_return", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(60, 60))
})

