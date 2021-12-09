accelerate_adult_obese <- function(df,
                                   ...) {
  df %>%
    add_scenario_adult_obese(
      scenario_function = "halt_rise",
      baseline_year = 2010,
      target_year = 2025,
      scenario_name = "acceleration",
      ...
    )
}
