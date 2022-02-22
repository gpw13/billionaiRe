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

  testthat::expect_equal(df_dip_recover_2025_one_iso3, 63.529412)
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

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(63.529412, 63.529412))

  df_neg <- testdf(values = 80:60) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_dip_recover_neg <- scenario_covid_rapid_return(df_neg)

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(63.529412, 63.529412))
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

testthat::test_that("scenario_covid_delayed_return produces accurate results with one iso3", {
  df <- testdf() %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_delayed_return <- scenario_covid_delayed_return(df)

  df_delayed_return_2025_one_iso3 <- df_delayed_return %>%
    dplyr::filter(scenario == "covid_delayed_return", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_delayed_return_2025_one_iso3, 63.529412)

  df_delayed_return_one_iso3 <- df_delayed_return %>%
    dplyr::filter(scenario == "covid_delayed_return", year == 2021, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_delayed_return_one_iso3, 60)
})

testthat::test_that("scenario_covid_delayed_return produces accurate results with two iso3", {
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

  df_delayed_return <- scenario_covid_delayed_return(df)

  df_delayed_return_2025_one_iso3 <- df_delayed_return %>%
    dplyr::filter(scenario == "covid_delayed_return", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_delayed_return_2025_one_iso3, c(62.647059, 62.647059))

  df_delayed_return_2021_two_iso3 <- df_delayed_return %>%
    dplyr::filter(scenario == "covid_delayed_return", year == 2021) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_delayed_return_2021_two_iso3, c(60, 60))
})

testthat::test_that("scenario_covid_sustained_disruption produces accurate results with one iso3", {
  df <- testdf() %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 ~ as.numeric(60 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ), type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_sustained_disruption <- scenario_covid_sustained_disruption(df)

  df_sustained_disruption_2025_one_iso3 <- df_sustained_disruption %>%
    dplyr::filter(scenario == "covid_sustained_disruption", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_sustained_disruption_2025_one_iso3, 63.529412)

  df_sustained_disruption_one_iso3 <- df_sustained_disruption %>%
    dplyr::filter(scenario == "covid_sustained_disruption", year == 2021, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_sustained_disruption_one_iso3, 60)
})

testthat::test_that("scenario_covid_sustained_disruption produces accurate results with two iso3", {
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

  df_sustained_disruption <- scenario_covid_sustained_disruption(df)

  df_sustained_disruption_2025_one_iso3 <- df_sustained_disruption %>%
    dplyr::filter(scenario == "covid_sustained_disruption", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_sustained_disruption_2025_one_iso3, c(63.529412,63.529412))

  df_sustained_disruption_2021_two_iso3 <- df_sustained_disruption %>%
    dplyr::filter(scenario == "covid_sustained_disruption", year == 2021) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_sustained_disruption_2021_two_iso3, c(60, 60))
})
