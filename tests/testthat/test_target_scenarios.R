testthat::test_that("scenario_fixed_percent returns accurate values:", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  target_value <- 99

  value_col <- "value"
  start_year <- 2018
  end_year <- 2025
  baseline_year <- start_year
  target_year <- end_year
  scenario_name <- glue::glue("{target_value}_{target_year}")
  small_is_best <- FALSE

  n_years <- target_year - baseline_year
  yearly_change <- (target_value - df[[value_col]][df$year == baseline_year]) / n_years

  df_test <- df %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::mutate(
      value = seq(68, target_value, length.out = length(baseline_year:target_year)),
      scenario = scenario_name
    )

  df_fixed_percent <- scenario_fixed_target(df,
    target_value = target_value,
    value_col = value_col,
    start_year = start_year,
    end_year = end_year,
    baseline_year = start_year,
    target_year = end_year,
    scenario_name = scenario_name,
    small_is_best = small_is_best
  ) %>%
    dplyr::filter(scenario == "99_2025")

  testthat::expect_equal(df_fixed_percent, df_test)

  testthat::expect_equal(df_fixed_percent[[value_col]][df_fixed_percent$year == target_year], target_value)

  df_fixed_percent_higher_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > baseline_year ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    scenario_fixed_target(
      target_value = target_value,
      value_col = value_col,
      start_year = start_year,
      end_year = end_year,
      baseline_year = start_year,
      target_year = end_year,
      scenario_name = scenario_name,
      small_is_best = small_is_best
    ) %>%
    dplyr::filter(scenario == "99_2025")

  df_test_higher_values <- df_test %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2019 ~ 79,
      year == 2020 ~ 80,
      TRUE ~ value
    ))

  testthat::expect_equal(df_fixed_percent_higher_values, df_test_higher_values)
})

testthat::test_that("scenario_fixed_percent is vectorized on target_value:", {
  target_value <- 99

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "test",
    iso3 = "testalia",
    scenario = "default",
    target = target_value,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  value_col <- "value"
  start_year <- 2018
  end_year <- 2025
  baseline_year <- start_year
  target_year <- end_year
  scenario_name <- glue::glue("{target_value}_{target_year}")
  small_is_best <- FALSE

  n_years <- target_year - baseline_year
  yearly_change <- (target_value - df[[value_col]][df$year == baseline_year]) / n_years

  df_test <- df %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::mutate(
      value = seq(68, target_value, length.out = length(baseline_year:target_year)),
      scenario = scenario_name
    )

  df_target_col <- scenario_fixed_target_col(df,
    target_col = "target",
    value_col = value_col,
    start_year = start_year,
    end_year = end_year,
    baseline_year = start_year,
    target_year = end_year,
    scenario_name = scenario_name,
    small_is_best = small_is_best
  ) %>%
    dplyr::filter(scenario == "99_2025")

  testthat::expect_equal(df_target_col, df_test)

  testthat::expect_equal(df_target_col[[value_col]][df_target_col$year == target_year], target_value)
})
