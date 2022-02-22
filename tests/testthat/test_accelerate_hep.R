testthat::test_that("espar returns appropriate values", {
  df <- tidyr::expand_grid(
    iso3 = c("AFG", "BGD", "PAK", "BRN", "CHE", "POL", "SWE", "VUT"),
    year = 2018:2020,
    ind = billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "espar")]
  ) %>%
    dplyr::mutate(
      value = c(
        35, 13, 20, 20, 0, 60, 60, 60, 80, 20, 40, 40, 20, 60, 80, 80, 80, 40, 27, 20, 20, 40, 40, 60, 20, 40, 20, 20, 20, 20, 0, 20, 43, 33, 20, 40, 40, 80, 100, 60, 80, 20, 47, 60, 20, 60, 80, 80,
        80, 40, 33, 20, 40, 40, 53, 40, 40, 80, 20, 30, 40, 20, 20, 20, 47, 33, 20, 40, 40, 90, 80, 100, 80, 20, 60, 60, 60, 60, 70, 60, 80, 40, 80, 80, 80, 80, 53, 40, 40, 80, 20, 30, 20, 40, 20, 20,
        58, 60, 60, 40, 80, 80, 100, 60, 80, 40, 73, 100, 40, 80, 80, 80, 80, 40, 47, 40, 60, 40, 60, 40, 60, 80, 60, 60, 60, 60, 40, 40, 67, 80, 80, 60, 100, 90, 100, 80, 80, 40, 73, 100, 40, 80, 80, 80,
        80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 60, 60, 60, 60, 60, 70, 80, 80, 60, 100, 90, 100, 80, 80, 60, 73, 100, 40, 80, 80, 80, 80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 80, 80, 80, 60, 60,
        51, 27, 20, 20, 40, 80, 60, 100, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 49, 27, 20, 20, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60,
        60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 52, 33, 20, 40, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 60, 40, 60, 80, 47, 40, 40, 60, 20, 40, 40, 40, 40, 100,
        rep(NA_integer_, 96),
        rep(NA_integer_, 32), 95, 93, 100, 100, 80, 100, 100, 100, 100, 80, 100, 100, 100, 100, 90, 100, 80, 80, 100, 100, 100, 100, 87, 100, 100, 60, 100, 100, 100, 100, 100, 100, rep(NA_integer_, 32),
        rep(NA_integer_, 32), 66, 80, 100, 60, 80, 100, 100, 100, 100, 40, 67, 100, 80, 20, 0, 0, 0, 60, 80, 80, 80, 80, 27, 0, 80, 0, 80, 40, 80, 0, 80, 100, 50, 0, 0, 0, 0, 100, 100, 100, 100, 0, 87, 100, 80, 80, 80, 80,
        80, 0, 33, 40, 40, 20, 33, 0, 0, 100, 0, 60, 80, 40, 60, 100,
        92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100,
        100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 91, 100, 100, 100, 100, 100, 100, 100, 100, 80, 93, 100, 80, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100,
        34, 27, 20, 40, 20, 30, 40, 20, 20, 20, 27, 20, 20, 40, 80, 100, 60, 40, 53, 20, 60, 80, 27, 20, 20, 40, 60, 20, 20, 20, 20, 20,
        rep(NA_integer_, 32),
        55, 47, 60, 40, 40, 30, 40, 20, 20, 80, 100, 100, 100, 100, 90, 100, 80, 40, 73, 40, 100, 80, 40, 40, 40, 40, 80, 40, 40, 40, 40, 40
      ),
      type = "reported",
      scenario = "default"
    )

  df_accelerated <- accelerate_espar(df, keep_better_values = FALSE)

  df_accelerated_2025 <- df_accelerated %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_accelerated_2025, c(49.282051282051285, 100, 34.07692307692308, 100, 34.07692307692308, 65.897435897435898, 100, 100))

  df_add_indicator <- add_scenario_indicator(df,
    indicator = "espar",
    scenario_function = "accelerate",
    baseline_year = 2018
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, df_accelerated_2025)

  df_add_scenario <- add_scenario(df,
    scenario_function = "accelerate"
  )
  df_add_scenario_2025 <- df_add_scenario %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_2025, df_accelerated_2025)
})

basic_hep_test <- function(ind) {
  testthat::test_that(paste0(ind, " returns appropriate values"), {
    df <- tibble::tibble(
      value = 60:80,
      year = 2010:2030,
      ind = ind,
      iso3 = "testalia",
      scenario = "default"
    )

    df_add_indicator <- add_scenario_indicator(df,
      indicator = ind,
      scenario_function = "accelerate",
      baseline_year = 2018
    )

    df_add_indicator_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "acceleration", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_2025, 75)

    df_2018 <- df %>%
      dplyr::filter(year <= 2018)

    df_add_indicator <- add_scenario_indicator(df_2018,
      indicator = ind,
      scenario_function = "accelerate",
      baseline_year = 2018
    )

    df_add_indicator_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == "acceleration", year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_add_indicator_2025, 68)
  })
}

purrr::walk(c("respond", "notify", "detect", "detect_respond"), basic_hep_test)

testthat::test_that("accelerate_cholera_campaign returns accurate results:", {
  hep_test_df <- load_misc_data("test_data/test_data/test_data_2022-02-21T14-36-20.parquet") %>%
    make_default_scenario(billion = "hep") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "cholera_campaign")])

  calculated_test_data <- add_scenario(hep_test_df, "accelerate")

  testthat::expect_equal(nrow(calculated_test_data), 314)

  num_bgd_2025 <- calculated_test_data %>%
    dplyr::filter(
      year == 2025, scenario == "acceleration",
      iso3 == "BGD", ind == "cholera_campaign_num"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(num_bgd_2025, 5833333.33)

  num_tza_2025 <- calculated_test_data %>%
    dplyr::filter(
      year == 2025, scenario == "acceleration",
      iso3 == "TZA", ind == "cholera_campaign_num"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(round(num_tza_2025, 1), round(542710.42 * 0.5333146, 1))
})

testthat::test_that("accelerate_measles_routine returns accurate results:", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "measles_routine",
    iso3 = "testalia",
    scenario = "default"
  )

  aroc_2025 <- scenario_aroc(df, percent_change = 20, aroc_type = "percent_change", baseline_year = 2013) %>%
    dplyr::filter(year == 2025, scenario != "default") %>%
    dplyr::pull(value)


  df_add_scenario <- add_scenario(df, "accelerate")

  df_add_scenario_2025 <- df_add_scenario %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)


  testthat::expect_equal(df_add_scenario_2025, aroc_2025)

  df_add_scenario_indicator <- add_scenario_indicator(df, "accelerate", "measles_routine")

  df_add_scenario_indicator_2025 <- df_add_scenario_indicator %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_2025, aroc_2025)
})

testthat::test_that("accelerate_meningitis_campaign returns accurate results:", {
  hep_test_df <- load_misc_data("test_data/test_data/test_data_2022-02-21T14-36-20.parquet") %>%
    make_default_scenario(billion = "hep") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "meningitis_campaign")])

  calculated_test_data <- add_scenario(hep_test_df, "accelerate")

  testthat::expect_equal(nrow(calculated_test_data), 6)

  num_BDI_2025 <- calculated_test_data %>%
    dplyr::filter(
      year == 2018, scenario == "acceleration",
      iso3 == "BDI", ind == "meningitis_campaign_num"
    ) %>%
    dplyr::pull(value)

  testthat::expect_equal(num_BDI_2025, 7968553)
})

testthat::test_that("accelerate_meningitis_routine returns accurate results:", {
  df <- tibble::tibble(
    value = c(60:80),
    year = 2010:2030,
    ind = "meningitis_routine",
    iso3 = "testalia",
    scenario = "default"
  )

  fixed_target_2025 <- scenario_fixed_target(df, target_value = 90, target_year = 2030) %>%
    dplyr::filter(year == 2025, scenario != "default") %>%
    dplyr::pull(value)

  df_add_scenario <- add_scenario(df, "accelerate")

  df_add_scenario_2025 <- df_add_scenario %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_2025, fixed_target_2025)

  test_data <- load_misc_data("test_data/test_data/test_data_2022-02-21T14-36-20.parquet") %>%
    make_default_scenario(billion = "hep")

  df_add_scenario_indicator <- add_scenario_indicator(test_data, "accelerate", "meningitis_routine")

  df_add_scenario_indicator_2025 <- df_add_scenario_indicator %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_indicator_2025, c(52.5, 52.5))

  df_with_zero <- df %>%
    dplyr::mutate(value = dplyr::case_when(
      year == 2018 ~ 0L,
      TRUE ~ value
    ))

  df_add_scenario_indicator <- add_scenario_indicator(df_with_zero, "accelerate", "meningitis_routine")

  df_add_scenario_indicator_2025 <- df_add_scenario_indicator %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_indicator_2025, 75)
})

testthat::test_that("accelerate_polio_routine returns accurate results:", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "polio_routine",
    iso3 = "testalia",
    scenario = "default"
  )

  aroc_2025 <- scenario_aroc(df, percent_change = 20, aroc_type = "percent_change", baseline_year = 2015) %>%
    dplyr::filter(year == 2025, scenario != "default") %>%
    dplyr::pull(value)


  df_add_scenario <- add_scenario(df, "accelerate")

  df_add_scenario_2025 <- df_add_scenario %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)


  testthat::expect_equal(df_add_scenario_2025, aroc_2025)

  df_add_scenario_indicator <- add_scenario_indicator(df, "accelerate", "polio_routine")

  df_add_scenario_indicator_2025 <- df_add_scenario_indicator %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_2025, aroc_2025)
})

testthat::test_that("accelerate_yellow_fever_campaigns returns accurate results:", {
  hep_test_df <- load_misc_data("scenarios/yellow_fever_campaign/test_data_campaign_yellow_fever.parquet")

  calculated_test_data <- add_scenario(hep_test_df, "accelerate")


  num_UGA_2024 <- calculated_test_data %>%
    dplyr::filter(
      year == 2024, scenario == "acceleration",
      iso3 == "UGA", ind == "yellow_fever_campaign_denom"
    ) %>%
    dplyr::pull(value)

  num_UGA_2024_planned <- load_misc_data("scenarios/yellow_fever_campaign/yellow_fever_campaign_planned.csv") %>%
    dplyr::filter(iso3 == "UGA") %>%
    dplyr::pull("2024_campaign_targeted_population")

  testthat::expect_equal(num_UGA_2024, num_UGA_2024_planned)
})

testthat::test_that("accelerate_yellow_fever_routine returns accurate results:", {
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "yellow_fever_routine",
    iso3 = "testalia",
    scenario = "default"
  )

  aroc_2025 <- scenario_aroc(df, percent_change = 20, aroc_type = "percent_change", baseline_year = 2015) %>%
    dplyr::filter(year == 2025, scenario != "default") %>%
    dplyr::pull(value)


  df_add_scenario <- add_scenario(df, "accelerate")

  df_add_scenario_2025 <- df_add_scenario %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)


  testthat::expect_equal(df_add_scenario_2025, aroc_2025)

  df_add_scenario_indicator <- add_scenario_indicator(df, "accelerate", "yellow_fever_routine")

  df_add_scenario_indicator_2025 <- df_add_scenario_indicator %>%
    dplyr::filter(year == 2025, scenario == "acceleration") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_scenario_2025, aroc_2025)
})

testthat::test_that("accelerate can be run on all hep indicators:", {
  hep_test_df <- load_misc_data("test_data/test_data/test_data_2022-02-21T14-36-20.parquet") %>%
    make_default_scenario(billion = "hep") %>%
    dplyr::filter(ind %in% billion_ind_codes("hep") &
      !.data[["ind"]] %in% billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "espar")])

  testthat::expect_error(add_scenario(hep_test_df, "accelerate"), NA)

  df_espar <- tidyr::expand_grid(
    iso3 = c("AFG", "BGD", "PAK", "BRN", "CHE", "POL", "SWE", "VUT"),
    year = 2018:2020,
    ind = billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "espar")]
  ) %>%
    dplyr::mutate(
      value = c(
        35, 13, 20, 20, 0, 60, 60, 60, 80, 20, 40, 40, 20, 60, 80, 80, 80, 40, 27, 20, 20, 40, 40, 60, 20, 40, 20, 20, 20, 20, 0, 20, 43, 33, 20, 40, 40, 80, 100, 60, 80, 20, 47, 60, 20, 60, 80, 80,
        80, 40, 33, 20, 40, 40, 53, 40, 40, 80, 20, 30, 40, 20, 20, 20, 47, 33, 20, 40, 40, 90, 80, 100, 80, 20, 60, 60, 60, 60, 70, 60, 80, 40, 80, 80, 80, 80, 53, 40, 40, 80, 20, 30, 20, 40, 20, 20,
        58, 60, 60, 40, 80, 80, 100, 60, 80, 40, 73, 100, 40, 80, 80, 80, 80, 40, 47, 40, 60, 40, 60, 40, 60, 80, 60, 60, 60, 60, 40, 40, 67, 80, 80, 60, 100, 90, 100, 80, 80, 40, 73, 100, 40, 80, 80, 80,
        80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 60, 60, 60, 60, 60, 70, 80, 80, 60, 100, 90, 100, 80, 80, 60, 73, 100, 40, 80, 80, 80, 80, 40, 53, 40, 60, 60, 73, 60, 80, 80, 80, 80, 80, 80, 60, 60,
        51, 27, 20, 20, 40, 80, 60, 100, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 49, 27, 20, 20, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60,
        60, 60, 47, 40, 40, 60, 33, 40, 20, 40, 20, 40, 40, 40, 40, 100, 52, 33, 20, 40, 40, 50, 60, 40, 60, 40, 60, 60, 40, 80, 60, 60, 60, 60, 60, 40, 60, 80, 47, 40, 40, 60, 20, 40, 40, 40, 40, 100,
        rep(NA_integer_, 96),
        rep(NA_integer_, 32), 95, 93, 100, 100, 80, 100, 100, 100, 100, 80, 100, 100, 100, 100, 90, 100, 80, 80, 100, 100, 100, 100, 87, 100, 100, 60, 100, 100, 100, 100, 100, 100, rep(NA_integer_, 32),
        rep(NA_integer_, 32), 66, 80, 100, 60, 80, 100, 100, 100, 100, 40, 67, 100, 80, 20, 0, 0, 0, 60, 80, 80, 80, 80, 27, 0, 80, 0, 80, 40, 80, 0, 80, 100, 50, 0, 0, 0, 0, 100, 100, 100, 100, 0, 87, 100, 80, 80, 80, 80,
        80, 0, 33, 40, 40, 20, 33, 0, 0, 100, 0, 60, 80, 40, 60, 100,
        92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 92, 100, 100, 100, 100, 100, 100, 100, 100, 80, 100, 100, 100, 100, 100, 100,
        100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100, 91, 100, 100, 100, 100, 100, 100, 100, 100, 80, 93, 100, 80, 100, 100, 100, 100, 80, 80, 80, 80, 80, 93, 80, 100, 100, 80, 100, 100, 100, 80, 100,
        34, 27, 20, 40, 20, 30, 40, 20, 20, 20, 27, 20, 20, 40, 80, 100, 60, 40, 53, 20, 60, 80, 27, 20, 20, 40, 60, 20, 20, 20, 20, 20,
        rep(NA_integer_, 32),
        55, 47, 60, 40, 40, 30, 40, 20, 20, 80, 100, 100, 100, 100, 90, 100, 80, 40, 73, 40, 100, 80, 40, 40, 40, 40, 80, 40, 40, 40, 40, 40
      ),
      type = "reported",
      scenario = "default"
    )
  testthat::expect_error(add_scenario(df_espar,
    scenario_function = "accelerate"
  ), NA)
})
