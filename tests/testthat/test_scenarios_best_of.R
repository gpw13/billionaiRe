testthat::test_that("scenario_best_of takes the best and only the best scenario", {
  df <- tibble::tibble(
    value = c(80:100, 10:30, 30:50),
    year = rep(2010:2030, 3),
    ind = "water",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  )

  df_best_small_is_best <- scenario_best_of(df, c("a", "b", "c"), small_is_best = TRUE)

  testthat::expect_equal(df_best_small_is_best[["scenario"]], "b")

  df_best_high_is_best <- scenario_best_of(df, c("a", "b", "c"), small_is_best = FALSE)

  testthat::expect_equal(df_best_high_is_best[["scenario"]], "a")

  two_ind_df <- tibble::tibble(
    value = c(10:30, 80:100, 30:50),
    year = rep(2010:2030, 3),
    ind = "adult_obese",
    iso3 = "testalia",
    scenario = c(rep("a", 21), rep("b", 21), rep("c", 21))
  ) %>%
    dplyr::bind_rows(df)

  df_best_small_is_best_two_ind <- scenario_best_of(two_ind_df, c("a", "b", "c"), small_is_best = TRUE)

  testthat::expect_equal(df_best_small_is_best_two_ind[["scenario"]], c("a", "b"))

  df_best_high_is_best_two_ind <- scenario_best_of(two_ind_df, c("a", "b", "c"), small_is_best = FALSE)

  testthat::expect_equal(df_best_high_is_best_two_ind[["scenario"]], c("b", "a"))

  two_iso_df <- two_ind_df %>%
    dplyr::mutate(ind = "water", iso3 = c(rep("testalia", 3 * 21), rep("tryalia", 3 * 21)))

  df_best_small_is_best_two_iso <- scenario_best_of(two_iso_df, c("a", "b", "c"), small_is_best = TRUE)

  testthat::expect_equal(df_best_small_is_best_two_iso[["scenario"]], c("a", "b"))

  df_best_high_is_best_two_iso <- scenario_best_of(two_iso_df, c("a", "b", "c"), small_is_best = FALSE)

  testthat::expect_equal(df_best_high_is_best_two_iso[["scenario"]], c("b", "a"))
})
