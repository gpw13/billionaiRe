get_value <- function(values = 60:80, ind, type, iso3 = "testalia", end_year = 2030) {
  tibble::tibble(
    value = values,
    year = 2010:2030,
    ind = ind,
    type = type,
    iso3 = iso3,
    scenario = "default",
    source = NA_character_
  ) %>%
    add_scenario_indicator("sdg", ind, bau_scenario = "default",
                           start_scenario_last_default = FALSE, end_year = end_year) %>%
    dplyr::filter(.data[["scenario"]] == "sdg", .data[["year"]] == end_year) %>%
    dplyr::pull(.data[["value"]])
}

get_fixed_target <- function(target_value,
                             baseline_value,
                             baseline_year = 2018,
                             target_year = 2030,
                             end_year = 2030) {
  baseline_value + (end_year - baseline_year) *
    (target_value - baseline_value) /
    (target_year - baseline_year)
}

get_linear_change <- function(linear_value,
                              baseline_value = 68,
                              baseline_year = 2018,
                              target_year = 2030,
                              end_year = 2030) {
  baseline_value + (end_year - baseline_year) * linear_value
}
# anc4 ----------------------------

testthat::test_that(paste0("sdg_anc4 returns accurate values:"), {
  ind <- "anc4"

  # fixed target, 95 by 2030 = 83.75 in 2030
  testthat::expect_equal(
    get_value(60:80, ind, "reported"),
    get_fixed_target(100, 68, 2018, 2030)
  )

  # fixed target, 95 by 2030 = 95 in 2030
  testthat::expect_equal(
    get_value(60:80, ind, "reported", end_year = 2030),
    get_fixed_target(100, 68, 2018, 2030, end_year = 2030)
  )

  # if no reported, bau:
  testthat::expect_equal(
    get_value(60:80, ind, "imputed"),
    80)
})
# art ----------------------------

testthat::test_that(paste0("sdg_art returns accurate values:"), {
  ind <- "art"

  # Fixed target value of 100 in 2030 is better than bau (75)
  testthat::expect_equal(
    get_value(60:80, ind, "reported"),
    get_fixed_target(100, 68, 2018, 2030)
  )

  # Fixed target value of 100 in 2030 is same as bau (95), with
  testthat::expect_equal(
    get_value(80:100, ind, "reported"),
    100
  )

  # No reported data, so bau result is returned
  testthat::expect_equal(get_value(60:80, ind, "imputed"), 80)
})

# beds ----------------------------

testthat::test_that(paste0("sdg_beds returns accurate values:"), {
  ind <- "beds"

  # Beds is > 18 for all years after 2018, so bau is returned
  testthat::expect_equal(get_value(60:80, ind, "reported"), 80)

  # Beds < 18 so targets at 18 by 2030
  testthat::expect_equal(
    get_value(seq(from = 5, by = 0.25, length.out = 21), ind, "reported"),
    18
  )

  # Beds < 18 so targets at 18 by 2030, still 18 in 2030.
  testthat::expect_equal(
    get_value(seq(from = 5, by = 0.25, length.out = 21), ind, "reported", end_year = 2030),
    18
  )
})

# bp ----------------------------

testthat::test_that("sdg_bp returns accurate values:", {
  ind <- "bp"

  testthat::expect_equal(
    get_value(50:70, ind, "reported"),
    get_fixed_target(100, 58, 2018, 2030)
  )

  # Fixed target value of 100 in 2030 is better than bau (75)
  testthat::expect_equal(
    get_value(60:80, ind, "reported"),
    get_fixed_target(100, 68, 2018, 2030)
  )

  # Fixed target value of 90.25 in 2030 is not better than bau (95), with
  testthat::expect_equal(
    get_value(80:100, ind, "reported"),
    100
  )
})

# doctors ----------------------------

testthat::test_that(paste0("sdg_doctors returns accurate values:"), {
  ind <- "doctors"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 154.74)

  testthat::expect_equal(get_value(10:30, ind, "reported", end_year = 2030), 154.74)
})

# nurses ----------------------------

testthat::test_that(paste0("sdg_nurses returns accurate values:"), {
  ind <- "nurses"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 154.74)

  testthat::expect_equal(get_value(10:30, ind, "reported", end_year = 2030), 154.74)
})

# hwf ----------------------------

testthat::test_that(paste0("sdg_hwf returns accurate values:"), {
  ind <- "hwf"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 154.74)

  testthat::expect_equal(get_value(10:30, ind, "reported", end_year = 2030), 154.74)

})

# dtp3 ----------------------------

testthat::test_that(paste0("sdg_dtp3 returns accurate values:"), {

  ind <- "dtp3"

  testthat::expect_equal(
    get_value(
      60:80,
      ind,
      "reported",
      iso3 = "AFG"),
    100)
})

# fh ----------------------------

testthat::test_that(paste0("sdg_fh returns accurate values:"), {
  ind <- "fh"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 0)

  # small_is_best = TRUE so lower BAU value (= 65) is returned
  testthat::expect_equal(get_value(80:60, ind, "reported"), 0)

})

# fp ----------------------------

testthat::test_that(paste0("sdg_fp returns accurate values:"), {
  ind <- "fp"

  testthat::expect_equal(get_value(60:80, ind, "reported", "CYP"), 100)

})

# fpg ----------------------------

testthat::test_that(paste0("sdg_fpg returns accurate values:"), {
  ind <- "fpg"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 0)
})

# itn ----------------------------

testthat::test_that(paste0("sdg_itn returns accurate values:"), {
  ind <- "itn"

  # BAU is better than fixed target value
  testthat::expect_equal(get_value(70:90, ind, "reported"), 100)

  # Fixed target of 80 by 2030 (= 58.3) is better than BAU (= 35)
  testthat::expect_equal(
    get_value(20:40, ind, "reported"),
    get_fixed_target(100, 28, 2018, 2030)
  )
})

# pneumo ----------------------------

testthat::test_that(paste0("sdg_pneumo returns accurate values:"), {
  ind <- "pneumo"

  # BAU is better than fixed target
  testthat::expect_equal(get_value(80:100, ind, "reported"), 100)

  # Fixed target of 90 by 2030 (= 90) is better  than BAU (= 35)
  testthat::expect_equal(
    get_value(20:40, ind, "reported"),
    get_fixed_target(100, 28, 2018, 2030)
  )
})

# tb ----------------------------

testthat::test_that(paste0("sdg_tb returns accurate values:"), {
  ind <- "tb"

  # Fixed target of 90 by 2030
  testthat::expect_equal(get_value(60:80, ind, "reported"), 100)
})

# uhc_sanitation ----------------------------

testthat::test_that(paste0("sdg_uhc_sanitation returns accurate values:"), {
  ind <- "uhc_sanitation"
  testthat::expect_equal(get_value(60:80, ind, "reported"), 100)
})

# uhc_tobacco ----------------------------

testthat::test_that(paste0("sdg_uhc_tobacco returns accurate values:"), {
  ind <- "uhc_tobacco"

  testthat::expect_equal(get_value(60:80, ind, "reported"), 0)
})

testthat::test_that("sdg can be run on all UHC indicator:", {
  uhc_test_df <- load_misc_data("test_data/test_data/test_data_2022-03-06T09-30-41.parquet") %>%
    dplyr::mutate(source = NA_character_) %>%
    dplyr::filter(ind %in% billion_ind_codes("uhc")) %>%
    make_default_scenario(billion = "uhc", default_scenario = "pre_covid_trajectory") %>%
    dplyr::filter(scenario == "default")

  testthat::expect_error(add_scenario(uhc_test_df, "sdg", bau_scenario = "default", start_scenario_last_default = FALSE), NA)
})

