accelerate_adult_obese <- function(df,
                                   ind_ids = billion_ind_codes("hpop"),
                                   start_year = 2018,
                                   end_year = 2025,
                                   scenario = "scenario",
                                   ind = "ind",
                                   ...) {
  this_ind <- ind_ids["adult_obese"]

  params <- list(...)
  params["baseline_year"] <- 2010
  params["scenario_name"] <- "accelerate"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "accelerate")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_water <- function(df,
                             ...) {
  df %>%
    scenario_quantile(
      n = 5,
      quantile_year = 2017,
      baseline_quantile_year = 2018,
      upper_limit = 99,
      scenario_name = "acceleration",
    )
}

accelerate_water_rural <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ...
    )
}

accelerate_water_urban <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ...
    )
}
