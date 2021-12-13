accelerate_adult_obese <- function(df,
                                   ind_ids = billion_ind_codes("hpop"),
                                   end_year = 2025,
                                   scenario = "scenario",
                                   ind = "ind",
                                   ...) {
  this_ind <- ind_ids["adult_obese"]

  params <- list(...)
  params["baseline_year"] <- 2010
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_alcohol <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               end_year = 2025,
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  this_ind <- ind_ids[stringr::str_detect(ind_ids, "alcohol")]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  params <- list(...)
  params_perc_baseline <- params
  params_perc_baseline["baseline_year"] <- 2010
  params_perc_baseline["percent_change"] <- -10
  params_perc_baseline["scenario_name"] <- NULL

  params_halt_rise <- params
  params_halt_rise["baseline_year"] <- 2018
  params_halt_rise["scenario_name"] <- NULL

  df_perc_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind, target_year = end_year), params_perc_baseline)
  ) %>%
    dplyr::filter(scenario == "-10_2010")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params_halt_rise)
  ) %>%
    dplyr::filter(scenario == "halt_rise")

  df_accelerated <- df_perc_baseline %>%
    dplyr::bind_rows(df_halt_rise) %>%
    scenario_best_of(c("-10_2010", "halt_rise"), scenario_name = "acceleration", small_is_best = params[["small_is_best"]]) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_child_obese <- function(df,
                                   ...) {
  accelerate_adult_obese(
    df = df,
    ind_ids = list(adult_obese = "child_obese")
  )
}

accelerate_child_viol <- function(df,
                                  ...) {
  accelerate_adult_obese(
    df = df,
    ...
  )
}

accelerate_water <- function(df,
                             ind_ids = billion_ind_codes("hpop"),
                             end_year = 2025,
                             scenario = "scenario",
                             ind = "ind",
                             ...) {
  this_ind <- ind_ids[stringr::str_detect(ind_ids, "^water")]

  params <- list(...)
  params["n"] <- 5
  params["quantile_year"] <- 2017
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[[ind]], "^water"))

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
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
