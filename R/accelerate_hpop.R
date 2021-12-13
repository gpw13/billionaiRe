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
  params[["small_is_best"]] <- TRUE
  params_bau <- get_right_params(params, scenario_bau)

  params_perc_baseline <- get_right_params(params, scenario_percent_baseline)
  params_perc_baseline["baseline_year"] <- 2010
  params_perc_baseline["percent_change"] <- -10

  params_halt_rise <- get_right_params(params, scenario_halt_rise)
  params_halt_rise["baseline_year"] <- 2018

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(scenario == "business_as_usual")

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
    scenario_best_of(c("-10_2010", "halt_rise", "business_as_usual"), scenario_name = "acceleration", small_is_best = params[["small_is_best"]]) %>%
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
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario = "scenario",
                                  ind = "ind",
                                  ...) {
  this_ind <- ind_ids["child_viol"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  params <- list(...)
  params["target_value"] <- 0
  params["target_year"] <- 2030
  params["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_devontrack <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario = "scenario",
                                  ind = "ind",
                                  ...) {
  this_ind <- ind_ids["devontrack"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  params <- list(...)
  params["target_value"] <- 80
  params["target_year"] <- 2030
  params["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_fuel <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            iso3 = "iso3",
                            ind = "ind",
                            scenario = "scenario",
                            ...) {
  this_ind <- ind_ids["fuel"]

  this_ind_df <- df %>%
    dplyr::filter(.data[[ind]] == this_ind) %>%
    dplyr::mutate(wb_ig = whoville::iso3_to_regions(.data[[iso3]], region = "wb_ig"))

  params <- list(...)

  if ("HIC" %in% unique(this_ind_df$wb_ig)) {
    high_income_df <- this_ind_df %>%
      dplyr::filter(wb_ig == "HIC") %>%
      dplyr::mutate("{scenario}" := "acceleration")

    high_income <- do.call(
      scenario_bau, c(list(df = high_income_df), params)
    ) %>%
      dplyr::filter(scenario == "acceleration")
  } else {
    high_income <- this_ind_df[0, ]
  }

  # for non hic a regional approach is used using years 2018 to 2023.
  if (sum(c("LMC", "LIC", "UMC") %in% unique(this_ind_df$wb_ig)) > 0 | sum(is.na(this_ind_df$wb_ig)) > 0) {
    other_df <- this_ind_df %>%
      dplyr::filter(wb_ig != "HIC" | is.na(wb_ig))

    params_others <- params
    params_others["target_year"] <- 2013
    params_others["baseline_year"] <- 2018
    params_others["scenario_name"] <- "acceleration"

    other <- do.call(
      scenario_best_in_region, c(list(df = other_df), params_others)
    ) %>%
      dplyr::filter(scenario == "acceleration")
  } else {
    other <- this_ind_df[0, ]
  }


  df %>%
    dplyr::bind_rows(other) %>%
    dplyr::bind_rows(high_income) %>%
    dplyr::select(-wb_ig)
}

accelerate_hpop_sanitation <- function(df,
                                       ind_ids = billion_ind_codes("hpop"),
                                       end_year = 2025,
                                       scenario = "scenario",
                                       ind = "ind",
                                       ...) {
  this_ind <- ind_ids["hpop_sanitation"]

  params <- list(...)
  params["n"] <- 5
  params["quantile_year"] <- 2019
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == "hpop_sanitation")

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
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
