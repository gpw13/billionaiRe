get_fixed_target <- function(target_value, baseline_value, baseline_year = 2018, target_year = 2025) {
  baseline_value + (2025 - baseline_year) * (target_value - baseline_value) / (target_year - baseline_year)
}


testthat::test_that(paste0("accelerate_adult_obese returns accurate values:"), {
  ind <- "adult_obese"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)

  df_add_indicator_2018 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2018) %>%
    dplyr::pull(value)


  testthat::expect_equal(df_add_indicator_2018, 68)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)

  df_add_indicator_2018 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2022) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2018, 68.25)
})

testthat::test_that(paste0("accelerate_alcohol returns accurate values:"), {
  ind <- "alcohol"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 54)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 54)
})

testthat::test_that(paste0("accelerate_child_obese returns accurate values:"), {
  ind <- "child_obese"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)


  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60)

})

testthat::test_that(paste0("accelerate_child_viol returns accurate values:"), {
  ind <- "child_viol"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)

})

testthat::test_that(paste0("accelerate_devontrack returns accurate values:"), {
  ind <- "devontrack"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

})

testthat::test_that(paste0("accelerate_fuel returns accurate values:"), {
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
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, c(75, 70, 75))

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "historical",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, c(75, 70, 75))

})

testthat::test_that(paste0("accelerate_hpop_sanitation, accelerate_hpop_sanitation_urban, accelerate_hpop_rural  returns accurate values:"), {
  ind <- "hpop_sanitation"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  ind <- "hpop_sanitation_rural"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  ind <- "hpop_sanitation_urban"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)


  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = "hpop_sanitation",
                                             scenario_function = "accelerate",
                                             bau_scenario = "historical",
                                             start_scenario_last_default = TRUE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

})

testthat::test_that(paste0("accelerate_hpop_tobacco returns accurate values:"), {
  ind <- "hpop_tobacco"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    ),
    source = NA_character_
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 42)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "historical",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 42)

})

testthat::test_that(paste0("accelerate_ipv returns accurate values:"), {
  ind <- "ipv"
  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             baseline_year = 2018,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 28.3333333)


  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "historical",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, get_fixed_target(0, 71, 2021, 2030))

})

testthat::test_that(paste0("accelerate_overweight returns accurate values:"), {
  ind <- "overweight"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 11.0119977)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))


  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 25.087793)

})

testthat::test_that(paste0("accelerate_pm25 returns accurate values:"), {
  ind <- "pm25"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 68 + (68 * -0.02) * (2025 - 2018))

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 71 + (68 * -0.02) * (2025 - 2021))

})

testthat::test_that(paste0("accelerate_road returns accurate values:"), {
  ind <- "road"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 69 + (35 - 70) * (2025 - 2020) / (2030 - 2020))

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 71 + ((35 - 71) * (2025 - 2020) / (2030 - 2020)))

})

testthat::test_that(paste0("accelerate_stunting returns accurate values:"), {
  ind <- "stunting"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE

  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 51.932794)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 60.864323)

})

testthat::test_that(paste0("accelerate_suicide returns accurate values:"), {
  ind <- "suicide"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 66 + ((65 *(100 - 33.333)/100) - 65) * (2025 - 2015) / (2030 - 2015))

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 67 + ((65 *(100 - 33.333)/100) - 65) * (2025 - 2015) / (2030 - 2015))

})

testthat::test_that(paste0("accelerate_transfats returns accurate values:"), {
  ind <- "transfats"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 100)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 100)

})

testthat::test_that(paste0("accelerate_wasting returns accurate values:"), {
  ind <- "wasting"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_
  ) %>%
    dplyr::mutate(type = dplyr::case_when(
      year > 2020 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 11.0119977)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE)

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 71 * ((1 - 0.3266624)^(2025 - 2022)), tolerance = 1e-05)

})

testthat::test_that(paste0("accelerate_water, water_urban and water_rural returns accurate values:"), {
  ind <- "water"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  ind <- "water_urban"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = "water",
                                             scenario_function = "accelerate",
                                             quantile_year = 2010,
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE,
                                             make_default = FALSE,
                                             expend_bau = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  ind <- "water_rural"

  df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = ind,
    iso3 = "testalia",
    scenario = "default",
    source = NA_character_,
    type = dplyr::if_else(year <= 2021, "reported", "projected")
  )

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = FALSE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

  df <- df %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ "reported"
    ))

  df_add_indicator <- add_scenario_indicator(df,
                                             indicator = ind,
                                             scenario_function = "accelerate",
                                             bau_scenario = "default",
                                             start_scenario_last_default = TRUE
  )

  df_add_indicator_2025 <- df_add_indicator %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_add_indicator_2025, 75)

})

testthat::test_that("acceleration can be run on all hpop indicators:", {
  hpop_test_df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    iso3 = "testalia",
    scenario = "default",
    type = dplyr::case_when(
      year <= 2018 ~ "estimated",
      TRUE ~ "projected"
    ),
    source = NA_character_
  ) %>%
    tidyr::expand_grid(ind = billion_ind_codes("hpop", include_subindicators = FALSE))

  calculated_test_data <- add_scenario(hpop_test_df,
                                       "accelerate",
                                       bau_scenario = "default",
                                       start_scenario_last_default = FALSE)

  testthat::expect_equal(nrow(calculated_test_data), 493)

  test_data <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet") %>%
    dplyr::mutate(source = NA_character_)

  testthat::expect_error(
    test_data %>%
      dplyr::filter(ind %in% billion_ind_codes("hpop"),
                    scenario != "default") %>%
      make_default_scenario(billion = "hpop", default_scenario = "pre_covid_trajectory") %>%
      dplyr::filter(scenario == "default") %>%
      add_scenario("accelerate", bau_scenario = "default",
                   start_scenario_last_default = FALSE,
                   make_default = FALSE,
                   expend_bau = FALSE),
    NA
  )

  testthat::expect_error(
    test_data %>%
      dplyr::filter(ind %in% billion_ind_codes("hpop")) %>%
      add_scenario("accelerate",
                   bau_scenario = "pre_covid_trajectory",
                   start_scenario_last_default = TRUE,
                   make_default = TRUE,
                   default_scenario = "default",
                   billion = "hpop",
                   expend_bau = FALSE),
    NA
  )
})
