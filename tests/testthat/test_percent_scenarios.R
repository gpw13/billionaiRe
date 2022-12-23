get_df <- function(values = 80:100, years = 2010:2030){
  tibble::tibble(
    value = values,
    year = years,
    ind = "water",
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    )
  )
}

testthat::test_that("scenario_percent_baseline gets corrects with position change.", {
  df <- get_df(50:70)

  percent_change <- 40

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2030
  scenario <- "scenario"
  scenario_name <- glue::glue("scenario_{percent_change}perc_{baseline_year}")
  value <- "value"

  df_scenario <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  ) %>%
    dplyr::filter(scenario == "scenario_40perc_2010")

  df_scenario_2025 <- df_scenario %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_2025, 67)

  df_scenario_2018 <- df %>%
    dplyr::filter(year %in% c(2010, 2018)) %>%
    scenario_percent_baseline(
      percent_change = percent_change,
      value_col = value,
      start_year = start_year,
      end_year = end_year,
      baseline_year = baseline_year,
      target_year = target_year,
      scenario = scenario,
      scenario_name = scenario_name
    ) %>%
    dplyr::filter(scenario == "scenario_40perc_2010")

  df_scenario_2018_2025 <- df_scenario_2018 %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_2018_2025, 67)
})

testthat::test_that("scenario_percent_baseline gets corrects with negative change.", {
  df <- get_df(50:70)

  percent_change <- -40

  baseline_year <- 2010
  target_year <- 2030
  start_year <- 2018
  end_year <- 2025
  scenario <- "scenario"
  scenario_name <- glue::glue("{percent_change}perc_{baseline_year}")
  value <- "value"

  df_scenario <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  ) %>%
    dplyr::filter(scenario == "-40perc_2010")

  df_scenario_2025 <- df_scenario %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_2025, 37)

  df_scenario_2018 <- df %>%
    dplyr::filter(year %in% c(2010, 2018)) %>%
    scenario_percent_baseline(
      percent_change = percent_change,
      value_col = value,
      start_year = start_year,
      end_year = end_year,
      baseline_year = baseline_year,
      target_year = target_year,
      scenario = scenario,
      scenario_name = scenario_name
    ) %>%
    dplyr::filter(scenario == "-40perc_2010")

  df_scenario_2018_2025 <- df_scenario_2018 %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_2018_2025, 37)
})

testthat::test_that("scenario_percent_baseline sets correct limits.", {
  df <- get_df(20:40)

  percent_change <- -180

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

  df_scenario_neg <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  ) %>%
    dplyr::filter(scenario == scenario_name)

  df_scenario_neg_2025 <- df_scenario_neg %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_neg_2025, 0)

  df_scenario_neg_setting_limits_explicitely <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = Inf,
    lower_limit = 0
  ) %>%
    dplyr::filter(scenario == scenario_name)

  df_scenario_neg_setting_limits_explicitely_2025 <- df_scenario_neg_setting_limits_explicitely %>%
    dplyr::filter(year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_scenario_neg_setting_limits_explicitely_2025, 0)

  df <- get_df(80:100)

  percent_change <- 180

  df_test_pos <- df %>%
    dplyr::mutate(!!sym(scenario) := scenario_name,
                  value = seq(80, 80 + 80 * percent_change / 100, length.out = length(baseline_year:target_year)),
                  value = dplyr::case_when(
                    year == start_year ~ 88,
                    value >= 100 ~ 100,
                    TRUE ~ value
                  )
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario_pos <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  ) %>%
    dplyr::filter(scenario == scenario_name)


  testthat::expect_equal(df_scenario_pos, df_test_pos)

  df_scenario_pos_setting_limits_explicitely <- scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = 100,
    lower_limit = -Inf
  ) %>%
    dplyr::filter(scenario == scenario_name)


  testthat::expect_equal(df_scenario_pos_setting_limits_explicitely, df_test_pos)
})

testthat::test_that("scenario_halt_rise returns correct results:", {
  df <- get_df(80:100)

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
                  value = c(80:88, rep(88, 2030-2018))
    ) %>%
    dplyr::filter(year %in% (start_year:end_year))

  df_scenario_halt_rise <- scenario_halt_rise(
    df,
    value_col = value,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name
  ) %>%
    dplyr::filter(scenario == scenario_name) %>%
    dplyr::ungroup()

  testthat::expect_equal(df_scenario_halt_rise, df_test_halt_rise)

  df_scenario_halt_rise_2018 <- df %>%
    dplyr::filter(year %in% c(2010, 2018)) %>%
    scenario_halt_rise(
      value_col = value,
      start_year = start_year,
      end_year = end_year,
      baseline_year = baseline_year,
      target_year = target_year,
      scenario = scenario,
      scenario_name = scenario_name
    ) %>%
    dplyr::filter(scenario == scenario_name)

  testthat::expect_equal(df_scenario_halt_rise_2018, df_test_halt_rise)
})

testthat::test_that("scenario_linear_change provides accurate results:", {
  df <- get_df(80:100)

  df_test <- df %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::mutate(scenario = "linear_change")

  df_scenario_linear_percent_change <- scenario_linear_change(df, linear_value = 1) %>%
    dplyr::filter(scenario == "linear_change")

  testthat::expect_equal(df_scenario_linear_percent_change, df_test)

  df_scenario_linear_percent_change_higer_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2018 ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    scenario_linear_change(linear_value = 1) %>%
    dplyr::filter(scenario == "linear_change")

  testthat::expect_equal(df_scenario_linear_percent_change_higer_values, df_test)

  df_scenario_linear_percent_change_higer_values_2018 <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2018 ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    dplyr::filter(year <= 2018) %>%
    scenario_linear_change(linear_value = 1) %>%
    dplyr::filter(scenario == "linear_change")

  testthat::expect_equal(df_scenario_linear_percent_change_higer_values_2018, df_test)
})

testthat::test_that("scenario_linear_change in vectorized on linear_value:", {
  df <- get_df(80:100) %>%
    dplyr::mutate(linear_value = 1)

  df_test <- df %>%
    dplyr::filter(year >= 2018, year <= 2025) %>%
    dplyr::mutate(scenario = "linear_change")

  df_scenario_linear_percent_change <- scenario_linear_change(df, linear_value = df[["linear_value"]]) %>%
    dplyr::filter(scenario == "linear_change")

  testthat::expect_equal(df_scenario_linear_percent_change, df_test)

  df_scenario_linear_percent_change_higer_values <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year > 2018 ~ as.numeric(value + 10),
      TRUE ~ as.numeric(value)
    )) %>%
    scenario_linear_change(linear_value = df[["linear_value"]]) %>%
    dplyr::filter(scenario == "linear_change")

  testthat::expect_equal(df_scenario_linear_percent_change_higer_values, df_test)

  df_scenario_linear_change_col <- df %>%
    scenario_linear_change_col(linear_value_col = "linear_value") %>%
    dplyr::select(-"linear_value") %>%
    dplyr::filter(scenario == "linear_change", year > 2018)

  df_test <- df_test %>%
    dplyr::filter(year > 2018) %>%
    dplyr::select(-"linear_value")

  testthat::expect_equal(df_scenario_linear_change_col, df_test)

  df_scenario_linear_change_col_2018 <- df %>%
    dplyr::filter(year <= 2018) %>%
    scenario_linear_change_col(linear_value_col = "linear_value") %>%
    dplyr::select(-"linear_value") %>%
    dplyr::filter(scenario == "linear_change", year > 2018)

  testthat::expect_equal(df_scenario_linear_change_col_2018, df_test)
})
