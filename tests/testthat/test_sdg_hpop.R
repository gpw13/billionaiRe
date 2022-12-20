get_fixed_target <- function(target_value, baseline_value, baseline_year = 2018, target_year = 2025) {
  baseline_value + (2025 - baseline_year) * (target_value - baseline_value) / (target_year - baseline_year)
}

get_df <- function(ind, values = 60:80, type = "reported", iso3 = "testalia"){
  tibble::tibble(
    value = values,
    year = 2010:2030,
    ind = ind,
    iso3 = iso3,
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )
}

testthat::test_that("adult_obese returns appropriate values", {
  ind <- "adult_obese"

  df <- get_df(ind) %>%
    dplyr::mutate(type = "reported")

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)
})

testthat::test_that(paste0("sdg_alcohol returns accurate values:"), {
  ind <- "alcohol"
  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 54)
})

testthat::test_that("child_obese returns appropriate values", {
  ind <- "child_obese"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 60)
})

testthat::test_that("child_viol returns appropriate values", {
  ind <- "child_viol"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, 28.333333)
})

testthat::test_that("devontrack returns appropriate values", {
  ind <- "devontrack"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             expend_bau = FALSE
  )

  df_add_indicator <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator, get_fixed_target(100,68, target_year = 2030))
})

testthat::test_that(paste0("sdg_fuel returns accurate values:"), {
  ind <- "fuel"

  df <- tibble::tibble(
    value = c(rep(60:80, 2), seq(40, 80, length.out = 21)),
    year = rep(2010:2030, 3),
    ind = ind,
    iso3 = c(rep("AFG", 21), rep("FIN", 21), rep("COD", 21)),
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, c(rep(get_fixed_target(100, 68, target_year = 2030),2), get_fixed_target(100, 56, target_year = 2030)))
})

testthat::test_that(paste0("sdg_hpop_sanitation returns accurate values:"), {
  ind <- "hpop_sanitation"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_hpop_sanitation_rural  returns accurate values:"), {
  ind <- "hpop_sanitation_rural"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_hpop_sanitation_rural returns accurate values:"), {
  ind <- "hpop_sanitation_urban"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_hpop_tobacco returns accurate values:"), {
  ind <- "hpop_tobacco"

  df <- get_df(ind) %>%
    dplyr::mutate(type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected")
    )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 42)
})

testthat::test_that(paste0("sdg_ipv returns accurate values:"), {
  ind <- "ipv"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             baseline_year = 2018,
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)
})

testthat::test_that(paste0("sdg_overweight returns accurate values:"), {
  ind <- "overweight"

  df <- get_df(ind, type = "reported")

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(0, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_pm25 returns accurate values:"), {
  ind <- "pm25"

  df <- get_df(ind, values = 30:10)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(5, 22, target_year = 2030))
})

testthat::test_that(paste0("sdg_road returns accurate values:"), {
  ind <- "road"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 69 + ((70 / 2) - 70) / (2030 - 2020) * (2025 - 2020))
})

testthat::test_that(paste0("sdg_stunting returns accurate values:"), {
  ind <- "stunting"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(0, 68, target_year = 2030))
})

testthat::test_that(paste0("accelerate_suicide returns accurate values:"), {
  ind <- "suicide"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 51.555556)
})

testthat::test_that(paste0("sdg_transfats returns accurate values:"), {
  ind <- "transfats"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 100)
})

testthat::test_that(paste0("sdg_wasting returns accurate values:"), {
  ind <- "wasting"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(0, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_water returns accurate values:"), {
  ind <- "water"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_water_rural  returns accurate values:"), {
  ind <- "water_rural"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that(paste0("sdg_water_rural returns accurate values:"), {
  ind <- "water_urban"

  df <- get_df(ind)

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "sdg",
                                             start_scenario_last_default = FALSE,
                                             bau_scenario = "default"
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "sdg", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(100, 68, target_year = 2030))
})

testthat::test_that("sdg can be run on all hpop indicators:", {
  hpop_test_df <- get_df(NULL) %>%
    dplyr::mutate(type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    )
  ) %>%
    tidyr::expand_grid(ind = billion_ind_codes("hpop"))

  calculated_test_data <- add_scenario(hpop_test_df, "sdg", start_scenario_last_default = FALSE,
                                       bau_scenario = "default",
                                       expend_bau = FALSE)

  testthat::expect_equal(nrow(calculated_test_data), 576)

  testthat::expect_error(
    load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet") %>%
      dplyr::mutate(source = NA_character_) %>%
      dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
      make_default_scenario(billion = "hpop") %>%
      dplyr::filter(scenario == "default") %>%
      add_scenario("sdg",
                   bau_scenario = "default",
                   expend_bau = FALSE),
    NA)
})
