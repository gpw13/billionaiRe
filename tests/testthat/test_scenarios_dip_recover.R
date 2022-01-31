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


testthat::test_that("scenario_dip_recover produces accurate results with one iso3", {
  df <- testdf() %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_covid_dip_recover <- scenario_dip_recover(df)

  df_dip_recover_2025_one_iso3 <- df_covid_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_2025_one_iso3, 64.411765)
})

testthat::test_that("scenario_dip_recover produces accurate results with two iso3", {

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

  df_dip_recover <- scenario_dip_recover(df)

  df_dip_recoverr_2025_two_iso3 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(64.411765, 64.411765))
})

testthat::test_that("scenario_dip_recover produces accurate results without reported values:", {

  df_projected <- testdf() %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2020 ~ 60L,
      TRUE ~ value
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "projected"
    ))

  df_dip_recover_projected <- scenario_dip_recover(df_projected)

  df_dip_recover_projected_2025 <- df_dip_recover_projected %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_projected_2025, 75)
})

testthat::test_that("scenario_dip_recover produces accurate results when the gap between covid_year and is greater than one recovery_year:",{

  df_nas <- testdf() %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2020 ~ 60L,
      year > 2020 ~ NA_integer_,
      TRUE ~ value
    ))

  df_dip_recover_longer <- scenario_dip_recover(df_nas, recovery_year = 2022)

  df_dip_recover_longer_just_scenario <- df_dip_recover_longer %>%
    dplyr::filter(scenario == "dip_recover") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_longer_just_scenario, c(68, 69, 60.00000, 60.00000, 60.882353, 61.764706, 62.647059, 63.529412))


  df_dip_recover_very_long <- scenario_dip_recover(df_nas, recovery_year = 2030)

  df_dip_recover_just_scenario <- df_dip_recover_very_long %>%
    dplyr::filter(scenario == "dip_recover") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_just_scenario, c(68, 69, rep(60.0, 6)))

})

testthat::test_that("scenario_dip_recover produces accurate results with progressive_recovery == TRUE:", {

  df_reported <- testdf() %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2020 ~ 60L,
      TRUE ~ value
    ))



  df_dip_recover_progressive_recovery <- scenario_dip_recover(df_reported, progressive_recovery = TRUE)

  df_dip_recover_progressive_recovery_2025 <- df_dip_recover_progressive_recovery %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_progressive_recovery_2025, 64.411765)
})


