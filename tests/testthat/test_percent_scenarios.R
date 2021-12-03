testthat::test_that("scenario_percent_baseline gets corrects with position change.", {
  df <- tibble::tibble(
    value = 50:70,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  percent_change <- 40

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2025
  scenario <- "scenario"
  scenario_name <- glue::glue("scenario_{percent_change}perc_{baseline_year}")
  value <- "value"
  ind <- "ind"
  iso3 <- "iso3"
  year <- "year"

  df_test <- df %>%
    dplyr::filter(year %in% (start_year:end_year)) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
      value = c(58:65)
    )

  df_scenario <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario, df_test)
})

testthat::test_that("scenario_percent_baseline gets corrects with negative change.", {
  df <- tibble::tibble(
    value = 50:70,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  percent_change <- -40

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2025
  scenario <- "scenario"
  scenario_name <- glue::glue("{percent_change}perc_{baseline_year}")
  value <- "value"
  ind <- "ind"
  iso3 <- "iso3"
  year <- "year"


  df_test <- df %>%
    dplyr::filter(year %in% (start_year:end_year)) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
      value = 42:35
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario, df_test)
})

testthat::test_that("scenario_percent_baseline sets correct limits.", {
  df <- tibble::tibble(
    value = 20:40,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  percent_change <- -180

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2025
  scenario <- "scenario"
  scenario_name <- glue::glue("scenario_{percent_change}perc_{baseline_year}")
  value <- "value"
  ind <- "ind"
  iso3 <- "iso3"
  year <- "year"

  df_test_neg <- df %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
      value = seq(20, 20 + 20 * percent_change / 100, length.out = length(baseline_year:target_year)),
      value = dplyr::case_when(
        value <= 0 ~ 0,
        TRUE ~ value
      )
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario_neg <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario_neg, df_test_neg)

  df_scenario_neg_setting_limits_explicitely <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = Inf,
    lower_limit = 0
  )

  testthat::expect_equal(df_scenario_neg_setting_limits_explicitely, df_test_neg)


  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  percent_change <- 180

  df_test_pos <- df %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
      value = seq(80, 80 + 80 * percent_change / 100, length.out = length(baseline_year:target_year)),
      value = dplyr::case_when(
        value >= 100 ~ 100,
        TRUE ~ value
      )
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario_pos <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario_pos, df_test_pos)

  df_scenario_pos_setting_limits_explicitely <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = 100,
    lower_limit = -Inf
  )

  testthat::expect_equal(df_scenario_pos_setting_limits_explicitely, df_test_pos)
})

testthat::test_that("scenario_halt_rise returns correct results:", {
  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2025
  scenario <- "scenario"
  scenario_name <- glue::glue("scenario_halt_rise")
  value <- "value"
  ind <- "ind"
  iso3 <- "iso3"
  year <- "year"

  percent_change <- 0

  df_test_halt_rise <- df %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
      value = seq(80, 80, length.out = length(baseline_year:target_year))
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario_halt_rise <- scenario_halt_rise(
    df,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario_halt_rise, df_test_halt_rise)

  df_scenario_halt_rise_through_base_function <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  )

  testthat::expect_equal(df_scenario_halt_rise_through_base_function, df_test_halt_rise)

  df_scenario_halt_rise_through_base_function_with_limits <- scenario_percent_baseline(
    df,
    percent_change = 10000,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = 80,
    lower_limit = 80
  )

  testthat::expect_equal(df_scenario_halt_rise_through_base_function_with_limits, df_test_halt_rise)

  df_scenario_halt_rise_through_base_function_with_limits_neg <- scenario_percent_baseline(
    df,
    percent_change = -10000,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = 80,
    lower_limit = 80
  )

  testthat::expect_equal(df_scenario_halt_rise_through_base_function_with_limits_neg, df_test_halt_rise)
})

testthat::test_that("scenario_linear_percent_change provides accurate results:", {
  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default"
  )

  df_test <- df %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::mutate(scenario = "linear_percent_change")

  df_scenario_linear_percent_change <- scenario_linear_percent_change(df, linear_value = 1)

  testthat::expect_equal(df_scenario_linear_percent_change, df_test)

  df_scenario_linear_percent_change_higer_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2018 ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    scenario_linear_percent_change(linear_value = 1)

  testthat::expect_equal(df_scenario_linear_percent_change_higer_values, df_test)
})

testthat::test_that("scenario_linear_percent_change in vectorized on linear_value:", {
  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default",
    linear_value = 1
  )

  df_test <- df %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::mutate(scenario = "linear_percent_change")

  df_scenario_linear_percent_change <- scenario_linear_percent_change(df, linear_value = df[["linear_value"]])

  testthat::expect_equal(df_scenario_linear_percent_change, df_test)

  df_scenario_linear_percent_change_higer_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2018 ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    scenario_linear_percent_change(linear_value = df[["linear_value"]])

  testthat::expect_equal(df_scenario_linear_percent_change_higer_values, df_test)

  df_scenario_linear_percent_change_col <- df %>%
    scenario_linear_percent_change_col(linear_value = "linear_value")

  testthat::expect_equal(df_scenario_linear_percent_change_col, df_test)
})
