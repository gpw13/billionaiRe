testthat::test_that("scenario_covid_dip_recover target produces accurate results", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  ) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_covid_dip_recover <- scenario_covid_dip_recover(df)

  df_covid_dip_recover_2025 <- df_covid_dip_recover %>%
    dplyr::filter(scenario == "covid_dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_covid_dip_recover_2025, 64.411765)

  df_projected <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  ) %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2020 ~ 60L,
      TRUE ~ value
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "projected"
    ))

  df_covid_dip_recover_projected <- scenario_covid_dip_recover(df_projected)

  df_covid_dip_recover_projected_2025 <- df_covid_dip_recover_projected %>%
    dplyr::filter(scenario == "covid_dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_covid_dip_recover_projected_2025, 75)
})
