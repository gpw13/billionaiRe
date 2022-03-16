testdf <- function(values = 60:80, years = 2010:2030,scenario = "default", ind = "water", type = "reported", iso3 = "testalia") {
  tibble::tibble(
    value = values,
    year = years,
    ind = ind,
    type = type,
    iso3 = iso3,
    scenario = scenario
  )
}

scenario_test_df <-
  testdf(values = 60:61,
         years = 2018:2019,
         scenario = "routine",
         type = "reported") %>%
  dplyr::bind_rows(
    testdf(values = 54:55,
           years = 2020:2021,
           scenario = "covid_shock",
           type = "reported")
  )%>%
  dplyr::bind_rows(
    testdf(values = 62:67,
           years = 2020:2025,
           scenario = "pre_covid_trajectory",
           type = "projected")
  )

testthat::test_that("scenario_return_previous_trajectory produces accurate results with one iso3", {

  df_return_prev_traj <- scenario_return_previous_trajectory(scenario_test_df)

  df_return_prev_traj_results <- df_return_prev_traj %>%
    dplyr::filter(scenario == "return_previous_trajectory", iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_return_prev_traj_results, dplyr::filter(scenario_test_df, scenario == "pre_covid_trajectory", year >= 2022) %>% dplyr::pull(value))
})

testthat::test_that("scenario_return_previous_trajectory produces accurate results with later recovery", {

  df_return_prev_traj <- scenario_return_previous_trajectory(scenario_test_df, recovery_year = 2023)

  df_return_prev_traj_results <- df_return_prev_traj %>%
    dplyr::filter(scenario == "return_previous_trajectory", iso3 == "testalia") %>%
    dplyr::pull(value)

  testthat::expect_equal(df_return_prev_traj_results, c(55, 65:67))
})


testthat::test_that("scenario_return_previous_trajectory produces accurate results two iso3", {
  df <- scenario_test_df %>%
    dplyr::bind_rows(scenario_test_df %>% dplyr::mutate(
      iso3 = "testistan",
      value = value -10
    ))

  df_return_prev_traj_two_iso3 <- scenario_return_previous_trajectory(df) %>%
    dplyr::filter(scenario == "return_previous_trajectory", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_return_prev_traj_two_iso3, c(67, 57))
})


testthat::test_that("scenario_return_previous_trajectory produces accurate results two ind", {
  df <- scenario_test_df %>%
    dplyr::bind_rows(scenario_test_df %>% dplyr::mutate(
      ind = "adult_obese",
      value = value -10
    ))

  df_return_prev_traj_two_iso3 <- scenario_return_previous_trajectory(df) %>%
    dplyr::filter(scenario == "return_previous_trajectory", year == 2025) %>%
    dplyr::pull(value)

  testthat::expect_equal(df_return_prev_traj_two_iso3, c(67, 57))
})

testthat::test_that("scenario_return_previous_trajectory runs on real data", {
  df <- load_billion_data("projected_data", "hep", "polio_routine", version ="2022-03-10T15-44-07", experiment = "")

  df_with_covid_impact <- scenario_return_previous_trajectory(df)

  df_with_covid_impact_return <- df_with_covid_impact %>%
    dplyr::filter(scenario == "return_previous_trajectory", year >= 2022) %>%
    dplyr::select(iso3,year, ind, value) %>%
    dplyr::arrange(iso3,year, ind, value)

  df_with_covid_impact_return <- df_with_covid_impact %>%
    dplyr::filter(scenario == "pre_covid_trajectory", year >= 2022) %>%
    dplyr::select(iso3,year, ind, value) %>%
    dplyr::arrange(iso3,year, ind, value)

  testthat::expect_equal(df_with_covid_impact_return, df_with_covid_impact_return)

})

