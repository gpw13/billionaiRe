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

  testthat::expect_equal(df_dip_recover_projected_2025, NA_integer_)
})

testthat::test_that("scenario_dip_recover produces accurate results when the gap between covid_year and is greater than one recovery_year:", {
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

  testthat::expect_equal(df_dip_recover_longer_just_scenario, c(60, 60.882353, 61.764706, 62.647059, 63.529412))


  df_dip_recover_very_long <- scenario_dip_recover(df_nas, recovery_year = 2030)

  df_dip_recover_just_scenario <- df_dip_recover_very_long %>%
    dplyr::filter(scenario == "dip_recover") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_just_scenario, c(rep(60.0, 5)))
})

testthat::test_that("scenario_dip_recover carries over the last reported value when values are not present:", {
  df_no_covid_shock <- testdf() %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2020 ~ NA_integer_,
      TRUE ~ value
    ))

  df_dip_recover_no_covid_shock <- scenario_dip_recover(df_no_covid_shock, recovery_year = 2022)

  df_dip_recover_with_no_covid_shock <- df_dip_recover_no_covid_shock %>%
    dplyr::filter(scenario == "dip_recover") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_with_no_covid_shock, c(70, 71.029412, 72.058824, 73.088235, 74.117647))


  df_no_covid_shock_no_reported <- testdf() %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2015 ~ NA_integer_,
      TRUE ~ value
    ), type = "projected")

  df_dip_recover_no_covid_shock_no_reported <- scenario_dip_recover(df_no_covid_shock_no_reported, recovery_year = 2022)

  df_dip_recover_with_no_covid_shock_no_reported_only <- df_dip_recover_no_covid_shock_no_reported %>%
    dplyr::filter(scenario == "dip_recover") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_with_no_covid_shock_no_reported_only, rep(NA_real_, 6))
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

testthat::test_that("scenario_dip_recover produces accurate results with two ind", {
  df <- testdf() %>%
    dplyr::bind_rows(testdf(ind = "adult_obese")) %>%
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

  df_diff_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year >= 2020 & ind == "adult_obese" ~ as.numeric(61 + (year - 2020)),
      TRUE ~ as.numeric(value)
    ))

  df_dip_recover <- scenario_dip_recover(df_diff_values)

  df_dip_recoverr_2025_two_iso3 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_two_iso3, c(65.4852941, 64.411765))

})

testthat::test_that("scenario_dip_recover carries last reported value when start_year value is missing", {
  df <- testdf() %>%
    dplyr::filter(year >= 2019)

  df_dip_recover <- scenario_dip_recover(df)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 70)
})

testthat::test_that("scenario_dip_recover carries last reported value when start_year value is NA", {
  df <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year == 2018, NA_integer_, value))

  df_dip_recover <- scenario_dip_recover(df)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 70)

  df_dip_recover_2022_recovery <- scenario_dip_recover(df, recovery_year = 2022)

  df_dip_recoverr_2025_2022_recovery <- df_dip_recover_2022_recovery %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_2022_recovery, 71)
})

testthat::test_that("scenario_dip_recover carries last reported value when everything past start_year value is NA", {
  df <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value))

  df_dip_recover <- scenario_dip_recover(df)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 67)

  df_types <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value),
                  type =  "projected")

  df_dip_recover_types <- scenario_dip_recover(df_types, recovery_year = 2022)

  df_dip_recover_types_scenario <- df_dip_recover_types %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_types_scenario, NA_real_)
})

testthat::test_that("scenario_dip_recover returns accurate results when there is only the baseline value.", {
  df <- testdf() %>%
    dplyr::filter(year == 2018)

  df_dip_recover <- scenario_dip_recover(df)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 68)

  df_types <- df %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value),
                  type =  "projected")

  df_dip_recover_types <- scenario_dip_recover(df_types, recovery_year = 2022)

  df_dip_recover_types_scenario <- df_dip_recover_types %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_types_scenario, NA_real_)

})

testthat::test_that("scenario_dip_recover produces accurate results with one iso3 with aroc_type = average_years_in_range", {
  df <- testdf( values = c(70,69,70,69,70,69,70,69,70,69,70,76,80,78,66,75,73,63,62,77,69)) %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_covid_dip_recover <- scenario_dip_recover(df, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recover_2025_one_iso3 <- df_covid_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025, iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_2025_one_iso3, 70.036232)
})


testthat::test_that("scenario_dip_recover carries last reported value when start_year value is missing with aroc_type = average_years_in_range", {
  df <- testdf( values = c(70,69,70,69,70,69,70,69,70,69,70,76,80,78,66,75,73,63,62,77,69)) %>%
    dplyr::filter(year >= 2019)

  df_dip_recover <- scenario_dip_recover(df, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recovery_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recovery_2025_NA_2018, 70)
})

testthat::test_that("scenario_dip_recover carries last reported value when start_year value is NA with aroc_type = average_years_in_range", {
  df <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year == 2018, NA_integer_, value))

  df_dip_recover <- scenario_dip_recover(df, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 75.343823)

  df_dip_recover_2022_recovery <- scenario_dip_recover(df, recovery_year = 2022)

  df_dip_recoverr_2025_2022_recovery <- df_dip_recover_2022_recovery %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_2022_recovery, 71)

})

testthat::test_that("scenario_dip_recover carries last reported value when everything past start_year value is NA with aroc_type = average_years_in_range", {
  df <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value))

  df_dip_recover <- scenario_dip_recover(df, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 72.114802)

  df_types <- testdf() %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value),
                  type =  "projected")

  df_dip_recover_types <- scenario_dip_recover(df_types, recovery_year = 2022, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recover_types_scenario <- df_dip_recover_types %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_types_scenario, NA_real_)

})

testthat::test_that("scenario_dip_recover returns accurate results when there is only the baseline value.", {
  df <- testdf() %>%
    dplyr::filter(year == 2018)

  df_dip_recover <- scenario_dip_recover(df, aroc_type = "average_years_in_range", aroc_start_year = 2015)

  df_dip_recoverr_2025_NA_2018 <- df_dip_recover %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recoverr_2025_NA_2018, 68)

  df_types <- df %>%
    dplyr::mutate(value = dplyr::if_else(year >= 2018, NA_integer_, value),
                  type =  "projected")

  df_dip_recover_types <- scenario_dip_recover(df_types, aroc_type = "average_years_in_range", aroc_start_year = 2015, recovery_year = 2022)

  df_dip_recover_types_scenario <- df_dip_recover_types %>%
    dplyr::filter(scenario == "dip_recover", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_dip_recover_types_scenario, NA_real_)

})
