get_2025_value <- function(values = 60:80, ind, type, iso3 = "testalia") {
  tibble::tibble(
    value = values,
    year = 2010:2030,
    ind = ind,
    type = type,
    iso3 = iso3,
    scenario = "default"
  ) %>%
    add_scenario_indicator("accelerate", ind) %>%
    dplyr::filter(scenario == "acceleration", year == 2025) %>%
    dplyr::pull(value)
}

get_fixed_target <- function(target_value, baseline_value, baseline_year = 2018, target_year = 2025) {
  baseline_value + (2025 - baseline_year) * (target_value - baseline_value) / (target_year - baseline_year)
}

get_linear_change <- function(linear_value, baseline_value = 68, baseline_year = 2018, target_year = 2025) {
  baseline_value + (2025 - baseline_year) * linear_value
}

# anc4 ----------------------------

testthat::test_that(paste0("accelerate_anc4 returns accurate values:"), {
  ind <- "anc4"

  # fixed target, 95 by 2030 = 83.75 & linear change of 2.6/yr to 2025 = 86.2
  # fixed target is easier to achieve so it's selected. This is also better than
  # bau (75) so final value is 83.75 from fixed target.
  testthat::expect_equal(
    get_2025_value(60:80, ind, "reported"),
    get_fixed_target(95, 68, 2018, 2030)
  )

  # No reported data, so bau result is returned
  testthat::expect_equal(get_2025_value(60:80, ind, "imputed"), 75)
})

# art ----------------------------

testthat::test_that(paste0("accelerate_art returns accurate values:"), {
  ind <- "art"

  # Fixed target value of 90.25 in 2025 is better than bau (75)
  testthat::expect_equal(
    get_2025_value(60:80, ind, "reported"),
    get_fixed_target(90.25, 68, 2019, 2025)
  )

  # Fixed target value of 90.25 in 2025 is not better than bau (95), with
  testthat::expect_equal(
    get_2025_value(80:100, ind, "reported"),
    95
  )

  # No reported data, so bau result is returned
  testthat::expect_equal(get_2025_value(60:80, ind, "imputed"), 75)
})

# beds ----------------------------

testthat::test_that(paste0("accelerate_beds returns accurate values:"), {
  ind <- "beds"

  # Beds is > 18 for all years after 2018, so bau is returned
  testthat::expect_equal(get_2025_value(60:80, ind, "reported"), 75)

  # Beds < 18 so linear change of 0.36/yr
  testthat::expect_equal(
    get_2025_value(seq(from = 5, by = 0.25, length.out = 21), ind, "reported"),
    get_linear_change(0.36, 7.0)
  )

  # Beds in 2018 (= 17) < 18 so linear change of 0.36/yr but exceeds 18 by 2025
  # so scenario_linear_change is capped at 18. BAU of 18.75 is better, so BAU is returned.
  testthat::expect_equal(get_2025_value(seq(from = 15, by = 0.25, length.out = 21), ind, "reported"), 18.75)
})

# bp ----------------------------

testthat::test_that("accelerate_bp returns accurate values:", {

  ind <- "bp"

  testthat::expect_equal(
    get_2025_value(60:80, ind, "reported"),
    get_fixed_target(80, 68, 2018, 2030)
  )
})

# doctors ----------------------------

testthat::test_that(paste0("accelerate_doctors returns accurate values:"), {
  ind <- "doctors"

  # Doctors returns BAU in all cases
  testthat::expect_equal(get_2025_value(60:80, ind, "reported"), 75)
})

# nurses ----------------------------

testthat::test_that(paste0("accelerate_nurses returns accurate values:"), {
  ind <- "nurses"

  # Nurses returns BAU in all cases
  testthat::expect_equal(get_2025_value(60:80, ind, "reported"), 75)
})

# hwf ----------------------------

testthat::test_that(paste0("accelerate_hwf returns accurate values:"), {
  ind <- "hwf"

  df_acceleration <- tibble::tibble(
    value = c(20:40, 40:60, 60:80),
    year = rep(2010:2030, 3),
    ind = ind,
    type = "reported",
    iso3 = unlist(purrr::map(c("testalia", "testistan", "testina"), rep, 21)),
    scenario = "default"
  ) %>%
    add_scenario_indicator("accelerate", ind) %>%
    dplyr::filter(scenario == "acceleration")

  # testalia is less than 2018 global median so linear change of 4.54/yr from 2018 to 2025
  testthat::expect_equal(
    dplyr::filter(df_acceleration, iso3 == "testalia", year == 2025) %>% dplyr::pull(value),
    get_linear_change(4.54, 28)
  )

  # testistan is equal to 2018 global median so BAU is returned
  testthat::expect_equal(
    dplyr::filter(df_acceleration, iso3 == "testistan", year == 2025) %>% dplyr::pull(value),
    55
  )

  # testina is greater than 2018 global median so BAU is returned
  testthat::expect_equal(
    dplyr::filter(df_acceleration, iso3 == "testina", year == 2025) %>% dplyr::pull(value),
    75
  )
})

# dtp3 ----------------------------

testthat::test_that(paste0("accelerate_dtp3 returns accurate values:"), {
  # TODO: Difficult to test due to dependencies on external targets

  ind <- "dtp3"

  # Verify that function can be run without errors or messages.
  testthat::expect_error(get_2025_value(60:80, ind, "reported", iso3 = "AFG"), NA)
})

# fh ----------------------------

testthat::test_that(paste0("accelerate_fh returns accurate values:"), {
  ind <- "fh"

  # small_is_best = TRUE so halt_rise stops upward trend at 2018 value (= 68)
  testthat::expect_equal(get_2025_value(60:80, ind, "reported"), 68)

  # small_is_best = TRUE so lower BAU value (= 65) is returned
  testthat::expect_equal(get_2025_value(80:60, ind, "reported"), 65)
})

# fp ----------------------------

testthat::test_that(paste0("accelerate_fp returns accurate values:"), {
  ind <- "fp"

  # TODO: sceanrio_quantile

  # # ASM, BGD, and BTN are in the same quantile, with mean ARC = 0.5
  # # BGD, BTN, and IDN are all in the same region (SEAR).
  # # Using a higher initial value for IDN to ensure regional average is high and does not
  # # prematurely cap BGD and BTN values.
  # df_acceleration <- tibble::tibble(
  #   value = unlist(purrr::map2(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.49, 0.51, 0.5),
  #                              c(20, 20, 20, 20, 20, 20, 20, 70),
  #                              ~ seq(.y, by = .x, length.out = 21))),
  #   year = rep(2010:2030, 8),
  #   ind = ind,
  #   type = "reported",
  #   # Choosing countries from different regions to simplify test
  #   iso3 = unlist(purrr::map(c("ABW", "AFG", "AGO", "ALB", "ASM", "BGD", "BTN", "IDN"), rep, 21)),
  #   scenario = "default"
  # ) %>%
  #   add_scenario_indicator("accelerate", ind) %>%
  #   dplyr::filter(scenario == "acceleration")

  # CYP is one of exclude_countries so BAU is returned
  testthat::expect_equal(get_2025_value(60:80, ind, "reported", "CYP"), 75)
})

# fpg ----------------------------

testthat::test_that(paste0("accelerate_fpg returns accurate values:"), {
  ind <- "fpg"

  testthat::expect_equal(
    get_2025_value(60:80, ind, "reported"),
    60
  )
})

# itn ----------------------------

testthat::test_that(paste0("accelerate_itn returns accurate values:"), {
  ind <- "itn"

  # BAU is better than fixed target value
  testthat::expect_equal(get_2025_value(70:90, ind, "reported"), 85)

  # Fixed target of 80 by 2030 (= 58.3) is better than BAU (= 35)
  testthat::expect_equal(
    get_2025_value(20:40, ind, "reported"),
    get_fixed_target(80, 28, 2018, 2030)
  )
})

# pneumo ----------------------------

testthat::test_that(paste0("accelerate_pneumo returns accurate values:"), {
  ind <- "pneumo"

  # BAU is better than fixed target
  testthat::expect_equal(get_2025_value(80:100, ind, "reported"), 95)

  # Fixed target of 3 by 2025 (= 49) is better  than BAU (= 35)
  testthat::expect_equal(
    get_2025_value(20:40, ind, "reported"),
    get_linear_change(3, 28, 2018, 2025)
  )

  # When BAU starts above 90, BAU is kepts
  testthat::expect_equal(get_2025_value(80:100, ind, "reported"), 95)

  # Cap progress to 90
  testthat::expect_equal(get_2025_value(70:90, ind, "reported"), 90)

  # Returns BAU when all values are 'projected'
  testthat::expect_equal(
    get_2025_value(20:40, ind, "projected"),
    35
  )

})

# tb ----------------------------

testthat::test_that(paste0("accelerate_tb returns accurate values:"), {
  ind <- "tb"

  # Fixed target of 90 by 2025
  testthat::expect_equal(get_2025_value(60:80, ind, "reported"), 90)
})

# uhc_sanitation ----------------------------

testthat::test_that(paste0("accelerate_uhc_sanitation returns accurate values:"), {
  ind <- "uhc_sanitation"
  # TODO: Difficult to test due to dependencies on external targets

  # Verify that function can be run without errors or messages.
  testthat::expect_error(get_2025_value(60:80, ind, "reported"), NA)
})

# uhc_tobacco ----------------------------

testthat::test_that(paste0("accelerate_uhc_tobacco returns accurate values:"), {
  ind <- "uhc_tobacco"

  # No routine (estimated) data so BAU is returned
  testthat::expect_equal(get_2025_value(60:80, ind, "imputed"), 75)

  # TODO: Hard to test for countries with routine (estimated) data due to external dependencies
  # testthat::expect_equal(get_2025_value(60:80, ind, "estimated"), 75)
})

testthat::test_that("accelerate can be run on all UHC indicator:", {
  uhc_test_df <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet") %>%
    dplyr::mutate(scenario = dplyr::case_when(
      .data[["scenario"]] == "default" ~ "pre_covid_trajectory",
      TRUE ~ .data[["scenario"]]
    )) %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
    make_default_scenario(billion = "uhc", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(
      ind %in% billion_ind_codes("uhc"),
      !ind %in% billion_ind_codes("uhc")[stringr::str_detect(billion_ind_codes("uhc"), "espar")],
      scenario == "default"
    )

  testthat::expect_error(add_scenario(uhc_test_df, "accelerate"), NA)
})
