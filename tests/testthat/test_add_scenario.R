test_scenario_function <- function(df, scenario_function, expected_value, scenario_name, ...) {
  testthat::test_that(glue::glue("{scenario_function} returns appropriate results:"), {
    df_add_indicator <- add_scenario_indicator(df,
      indicator = "adult_obese",
      scenario_function = scenario_function,
      ...
    )
    df_adult_obese_halt_rise_2025 <- df_add_indicator %>%
      dplyr::filter(scenario == scenario_name, year == 2025) %>%
      dplyr::pull(value)

    testthat::expect_equal(df_adult_obese_halt_rise_2025, expected_value)

    df_add_scenario_hpop <- add_scenario(df,
      scenario_function = scenario_function,
      ...
    )

    testthat::expect_equal(df_add_scenario_hpop, df_add_indicator)
  })
}

df <- tibble::tibble(
  value = 80:100,
  year = 2010:2030,
  ind = "adult_obese",
  iso3 = "testalia",
  scenario = "default"
)

test_scenario_function(df, "halt_rise", 80, baseline_year = 2010, scenario_name = "halt_rise")

test_scenario_function(df, "aroc", 95.329345, aroc_type = "latest", scenario_name = "aroc_latest", keep_better_values = FALSE)

test_scenario_function(df, "percent_baseline", 100, percent_change = 40, scenario_name = glue::glue("40_2018"))

test_scenario_function(df, "linear_change", 95, linear_value = 1, scenario_name = "linear_change")

df_linear_col <- df %>%
  dplyr::mutate(linear_value = 1)

test_scenario_function(df_linear_col, "linear_change_col", 95, scenario_name = "linear_change", linear_value_col = "linear_value")

test_scenario_function(df, "quantile", 95, n = 5, scenario_name = "quantile_5")

test_scenario_function(df, "best_in_region", 95, scenario_name = "best_in_region")

test_scenario_function(df, "fixed_target", 70, target_value = 70, scenario_name = "70_2025")

df_fixed_target_col <- df %>%
  dplyr::mutate(target = 70)

test_scenario_function(df = df_fixed_target_col, "fixed_target_col", 70, target_col = "target", scenario_name = "target_2025")

df_type_col <- df %>%
  dplyr::mutate(type = dplyr::case_when(
    year >= 2021 ~ "projected",
    TRUE ~ "reported"
  ))

test_scenario_function(df_type_col, "covid_rapid_return", 95.113636, baseline_year = 2010, scenario_name = "covid_rapid_return")

test_scenario_function(df_type_col, "covid_never_return", 90, baseline_year = 2010, scenario_name = "covid_never_return")

test_scenario_function(df_type_col, "covid_delayed_return", 94.090909, baseline_year = 2010, scenario_name = "covid_delayed_return")

test_scenario_function(df_type_col, "covid_sustained_disruption", 95.113636, baseline_year = 2010, scenario_name = "covid_sustained_disruption")
