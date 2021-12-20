testthat::test_that("scenario_aroc latest produces accurate results with positive AROC", {
  df <- tibble::tibble(
    value = 80:100,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_latest <- scenario_aroc(df, aroc_type = "latest")

  latest_aroc <- ((df$value[df$year == 2018] / df$value[df$year == 2017])^(1 / (2018 - 2017))) - 1

  df_proj_2025_values <- df$value[df$year == 2018] * (1 + latest_aroc)^(2025 - 2018)

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df_proj_2025_values)

  df_more_gap <- df %>%
    dplyr::mutate(value = seq(20, 60, length.out = length(2010:2030)))

  df_aroc_latest_more_gap <- scenario_aroc(df_more_gap, aroc_type = "latest")

  latest_aroc_more_gap <- ((df_more_gap$value[df_more_gap$year == 2018] / df_more_gap$value[df_more_gap$year == 2017])^(1 / (2018 - 2017))) - 1

  df_proj_2025_values_more_gap <- df_more_gap$value[df_more_gap$year == 2018] * (1 + latest_aroc_more_gap)^(2025 - 2018)

  testthat::expect_equal(df_aroc_latest_more_gap$value[df_aroc_latest_more_gap$year == 2025 & df_aroc_latest_more_gap$scenario == "aroc_latest"], df_proj_2025_values_more_gap)

  df_aroc_latest_more_gap_2018 <- df_more_gap %>%
    dplyr::filter(year <= 2018) %>%
    scenario_aroc(aroc_type = "latest")

  testthat::expect_equal(df_aroc_latest_more_gap_2018$value[df_aroc_latest_more_gap_2018$year == 2025 & df_aroc_latest_more_gap_2018$scenario == "aroc_latest"], df_proj_2025_values_more_gap)
})

testthat::test_that("scenario_aroc latest produces accurate results with negative AROC", {
  df <- tibble::tibble(
    value = 100:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_latest <- scenario_aroc(df, aroc_type = "latest")

  latest_aroc <- ((df$value[df$year == 2018] / df$value[df$year == 2017])^(1 / (2018 - 2017))) - 1

  df_proj_2025_values <- df$value[df$year == 2018] * (1 + latest_aroc)^(2025 - 2018)

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df_proj_2025_values)

  df_more_gap <- df %>%
    dplyr::mutate(value = seq(60, 20, length.out = length(2010:2030)))

  df_aroc_latest_more_gap <- scenario_aroc(df_more_gap, aroc_type = "latest")

  latest_aroc_more_gap <- ((df_more_gap$value[df_more_gap$year == 2018] / df_more_gap$value[df_more_gap$year == 2017])^(1 / (2018 - 2017))) - 1

  df_proj_2025_values_more_gap <- df_more_gap$value[df_more_gap$year == 2018] * (1 + latest_aroc_more_gap)^(2025 - 2018)

  testthat::expect_equal(df_aroc_latest_more_gap$value[df_aroc_latest_more_gap$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df_proj_2025_values_more_gap)

  df_aroc_latest_more_gap_2018 <- df_more_gap %>%
    dplyr::filter(year <= 2018) %>%
    scenario_aroc(aroc_type = "latest")

  testthat::expect_equal(df_aroc_latest_more_gap_2018$value[df_aroc_latest_more_gap_2018$year == 2025 & df_aroc_latest_more_gap_2018$scenario == "aroc_latest"], df_proj_2025_values_more_gap)
})


testthat::test_that("scenario_aroc target produces accurate results with positive AROC", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_target <- scenario_aroc(df, aroc_type = "target", target_value = 99, target_year = 2025)

  testthat::expect_equal(df_aroc_target$value[df_aroc_target$year == 2025 & df_aroc_target$scenario == "aroc_target"], 99)

  df_aroc_target_2018 <- df %>%
    dplyr::filter(year %in% c(2018, 2025)) %>%
    scenario_aroc(aroc_type = "target", target_value = 99, target_year = 2025)

  testthat::expect_equal(df_aroc_target_2018$value[df_aroc_target_2018$year == 2025 & df_aroc_target_2018$scenario == "aroc_target"], 99)
})

testthat::test_that("scenario_aroc target produces accurate results with negative AROC", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_target <- scenario_aroc(df, aroc_type = "target", target_value = 3, target_year = 2025, small_is_best = TRUE)

  testthat::expect_equal(df_aroc_target$value[df_aroc_target$year == 2025 & df_aroc_target$scenario == "aroc_target"], 3)

  df_aroc_target_2018 <- df %>%
    dplyr::filter(year %in% c(2018, 2025)) %>%
    scenario_aroc(aroc_type = "target", target_value = 3, target_year = 2025, small_is_best = TRUE)

  testthat::expect_equal(df_aroc_target_2018$value[df_aroc_target_2018$year == 2025 & df_aroc_target_2018$scenario == "aroc_target"], 3)
})

testthat::test_that("scenario_aroc percent_change produces accurate results with positive percent_change", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_percent_change <- scenario_aroc(df, aroc_type = "percent_change", percent_change = 3, target_year = 2025, small_is_best = TRUE)

  test_percent_change <- df$value[df$year == 2018] * 1.03

  testthat::expect_equal(df_aroc_percent_change$value[df_aroc_percent_change$year == 2025 & df_aroc_percent_change$scenario == "aroc_percent_change"], test_percent_change)
})

testthat::test_that("scenario_aroc percent_change produces accurate results with negative percent_change", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_percent_change <- scenario_aroc(df, aroc_type = "percent_change", percent_change = -3, target_year = 2025, small_is_best = TRUE)

  test_percent_change <- df$value[df$year == 2018] * 0.97

  testthat::expect_equal(df_aroc_percent_change$value[df_aroc_percent_change$year == 2025 & df_aroc_percent_change$scenario == "aroc_percent_change"], test_percent_change)

  df_aroc_percent_change_2018 <- df %>%
    dplyr::filter(year %in% c(2018, 2025)) %>%
    scenario_aroc(aroc_type = "target", target_value = 3, target_year = 2025, small_is_best = TRUE)

  testthat::expect_equal(df_aroc_percent_change_2018$value[df_aroc_percent_change_2018$year == 2025 & df_aroc_percent_change_2018$scenario == "aroc_target"], 3)
})

testthat::test_that("scenario_aroc produces accurate results with limit_aroc_direction positive", {
  df <- tibble::tibble(
    value = seq(60, 70, length.out = length(2017:2027)),
    year = 2017:2027,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_latest <- scenario_aroc(df, aroc_type = "latest", limit_aroc_direction = "positive", small_is_best = TRUE)

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df$value[df$year == 2018])

  df_more_gap <- df %>%
    dplyr::mutate(
      value = seq(10, 200, length.out = length(2017:2027))
    )

  df_proj_2025_values <- df_more_gap$value[df_more_gap$year == 2018] * ((1 + 1.5)^(2025 - 2018))

  df_aroc_latest <- scenario_aroc(df_more_gap,
    aroc_type = "latest",
    limit_aroc_direction = "positive",
    limit_aroc_value = 1.5,
    trim = TRUE,
    upper_limit = Inf,
    small_is_best = FALSE
  )

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df_proj_2025_values)

  df_aroc_latest <- scenario_aroc(df_more_gap,
    aroc_type = "latest",
    limit_aroc_direction = "positive",
    limit_aroc_value = 1.5,
    trim = TRUE
  )

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], 100)
})

testthat::test_that("scenario_aroc produces accurate results with limit_aroc_direction positive", {
  df <- tibble::tibble(
    value = seq(80, 60, length.out = length(2017:2027)),
    year = 2017:2027,
    ind = "water",
    iso3 = "testalia",
    scenario = "default"
  )

  df_aroc_latest <- scenario_aroc(df, aroc_type = "latest", limit_aroc_direction = "negative")

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df$value[df$year == 2018])

  df_more_gap <- df %>%
    dplyr::mutate(
      value = seq(200, 10, length.out = length(2017:2027))
    )

  latest_aroc_more_gap <- ((df_more_gap$value[df_more_gap$year == 2018] / df_more_gap$value[df_more_gap$year == 2017])^(1 / (2018 - 2017))) - 1

  df_proj_2025_values <- df_more_gap$value[df_more_gap$year == 2018] * (1 - .05)^(2025 - 2018)

  df_aroc_latest <- scenario_aroc(df_more_gap, aroc_type = "latest", limit_aroc_direction = "negative", limit_aroc_value = -.05, trim = TRUE, lower_limit = -Inf, upper_limit = Inf)

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], df_proj_2025_values)

  df_aroc_latest <- scenario_aroc(df_more_gap, aroc_type = "latest", limit_aroc_direction = "negative", limit_aroc_value = -.05, trim = TRUE)

  testthat::expect_equal(df_aroc_latest$value[df_aroc_latest$year == 2025 & df_aroc_latest$scenario == "aroc_latest"], 100)
})
