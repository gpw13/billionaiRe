#' Accelerate adult_obese
#'
#' Accelerate adult_obese by halting upwards trend in the data to the 2010 value.
#' Runs:
#'   - `scenario_halt_rise(df, baseline_year = 2010, small_is_best = TRUE,...)`.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario` is
#' set to `acceleration`
accelerate_adult_obese <- function(df,
                                   ind_ids = billion_ind_codes("hpop"),
                                   end_year = 2025,
                                   scenario = "scenario",
                                   ind = "ind",
                                   ...) {
  assert_columns(df, scenario, ind)
  this_ind <- ind_ids["adult_obese"]

  params <- get_right_params(list(...), scenario_halt_rise)
  params["baseline_year"] <- 2010
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids["adult_obese"], scenario = scenario
  )

  df_accelerated <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate alcohol
#'
#' Accelerate alcohol by picking the best results between business as usual,
#' halt downwards trend from 2018, and -10 percent from 2010.
#'
#' Runs:
#'
#'  - `scenario_halt_rise(df, baseline_year = 2018, small_is_best = TRUE,...)`,
#'  - `scenario_bau(df, small_is_best = TRUE,...)`, and
#'  - `scenario_percent_baseline(df, percent_change = -10, baseline_year = 2010, small_is_best = TRUE, ...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
#'
accelerate_alcohol <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               end_year = 2025,
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  assert_columns(df, scenario, ind)

  this_ind <- ind_ids[stringr::str_detect(ind_ids, "alcohol")]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  params <- list(...)
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

  df_binded <- df_perc_baseline %>%
    dplyr::bind_rows(df_halt_rise) %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "-10_2020",
      "halt_rise"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}
#' Accelerate child_obese
#'
#' Accelerate child_obese by halting upwards trend in the data to the 2010 value.
#'
#' @inherit accelerate_alcohol
#'
accelerate_child_obese <- function(df,
                                   ...) {
  ind_ids <- "child_obese"
  names(ind_ids) <- "adult_obese"

  accelerate_adult_obese(
    df = df,
    ind_ids = ind_ids,
    ...
  )
}

#' Accelerate child_viol
#'
#' Accelerate child_viol by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
accelerate_child_viol <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario = "scenario",
                                  ind = "ind",
                                  ...) {
  assert_columns(df, scenario, ind)

  this_ind <- ind_ids["child_viol"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  params <- get_right_params(list(...), scenario_fixed_target)
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

#' Accelerate devontrack
#'
#' Accelerate devontrack by targeting 80 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 80, target_year = 2030, small_is_best = FALSE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
accelerate_devontrack <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario = "scenario",
                                  ind = "ind",
                                  ...) {
  assert_columns(df, scenario, ind)

  this_ind <- ind_ids["devontrack"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

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

#' Accelerate fuel
#'
#' Accelerate fuel by keeping business as usual for high income countries
#' (HIC) (according to the World Bank), and by aiming at the best rate of change
#' between 2013 and 2015 in the region for Lower-middle income countries (LMC),
#' Low-income countries (LIC), High-income countries(HIC), and for unclassified
#' countries.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = FALSE,...)` for HIC,
#'  - `scenario_best_in_region(df, target_year = 2018, baseline_year = 2013, small_is_best = FALSE,...)` for other income groups.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#'
accelerate_fuel <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            iso3 = "iso3",
                            ind = "ind",
                            scenario = "scenario",
                            ...) {
  assert_columns(df, scenario, ind, iso3)

  this_ind <- ind_ids["fuel"]

  this_ind_df <- df %>%
    dplyr::filter(.data[[ind]] == this_ind) %>%
    dplyr::mutate(wb_ig = whoville::iso3_to_regions(.data[[iso3]], region = "wb_ig"))

  params <- list(...)

  if ("HIC" %in% unique(this_ind_df$wb_ig)) {
    params_bau <- get_right_params(params, "scenario_bau")
    params_bau["default_scenario"] <- "acceleration"

    high_income_df <- this_ind_df %>%
      dplyr::filter(.data[["wb_ig"]] == "HIC") %>%
      dplyr::mutate("{scenario}" := "acceleration")

    high_income <- do.call(
      scenario_bau, c(list(df = high_income_df), params_bau)
    ) %>%
      dplyr::filter(scenario == "acceleration")
  } else {
    high_income <- this_ind_df[0, ]
  }

  # for non hic a regional approach is used using years 2018 to 2023.
  if (sum(c("LMC", "LIC", "UMC") %in% unique(this_ind_df$wb_ig)) > 0 | sum(is.na(this_ind_df$wb_ig)) > 0) {
    other_df <- this_ind_df %>%
      dplyr::filter(.data[["wb_ig"]] != "HIC" | is.na(.data[["wb_ig"]]))

    params_others <- params
    params_others["baseline_year"] <- 2013
    params_others["target_year"] <- 2018
    params_others["scenario_name"] <- "acceleration"

    assert_ind_start_end_year(other_df,
      start_year = 2013, end_year = 2018,
      ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
    )

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
    dplyr::select(-"wb_ig")
}

#' Accelerate hpop_sanitation
#'
#' Accelerate hpop_sanitation by aiming at best value in quintile.
#'
#' Runs:
#'
#'  - `scenario_quantile(df, n = 5, quantile_year = 2019, baseline_quantile_year = 2018, upper_limit = 99, small_is_best = FALSE,...)`
#'
#' @inherit accelerate_adult_obese
accelerate_hpop_sanitation <- function(df,
                                       ind_ids = billion_ind_codes("hpop"),
                                       end_year = 2025,
                                       scenario = "scenario",
                                       ind = "ind",
                                       ...) {
  assert_columns(df, scenario, ind)

  this_ind <- ind_ids["hpop_sanitation"]

  params <- get_right_params(list(...), scenario_quantile)
  params["n"] <- 5
  params["quantile_year"] <- 2019
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate hpop_sanitation_rural
#'
#' Accelerate hpop_sanitation_rural by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_hpop_sanitation_rural <- function(df,
                                             ...) {
  df %>%
    accelerate_hpop_sanitation(
      ind_ids = c("hpop_sanitation" = "hpop_sanitation_rural"),
      ...
    )
}

#' Accelerate hpop_sanitation_urban
#'
#' Accelerate hpop_sanitation_urban by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_hpop_sanitation_urban <- function(df,
                                             ...) {
  df %>%
    accelerate_hpop_sanitation(
      ind_ids = c("hpop_sanitation" = "hpop_sanitation_urban"),
      ...
    )
}

#' Accelerate hpop_tobacco
#'
#' Accelerate hpop_tobacco by picking the best value between business as usual,
#' halt the rise in 2018, or a custom version of scenario_percent_baseline. The
#' custom `scenario_percent_baseline` is taking similar parameters to
#' `scenario_percent_baseline`'s `percent_change` = -30, `baseline_year` = 2010,
#' but values are added to the `start_year` value, rather than the `baseline_year`
#' values.
#'
#' Runs:
#'
#'  - custom scenario_percent_baseline (see above).
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_halt_rise(df, baseline_year= 2018, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hep_data
accelerate_hpop_tobacco <- function(df,
                                    ind_ids = billion_ind_codes("hpop"),
                                    ind = "ind",
                                    scenario = "scenario",
                                    iso3 = "iso3",
                                    value = "value",
                                    year = "year",
                                    start_year = 2018,
                                    end_year = 2025,
                                    type_col = "type",
                                    ...) {
  this_ind <- ind_ids["hpop_tobacco"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  full_df <- tidyr::expand_grid(
    "{iso3}" := unique(df_this_ind[[iso3]]),
    "{year}" := start_year:end_year,
    "{ind}" := this_ind,
    "{scenario}" := unique(df_this_ind[[scenario]])
  )

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  params <- list(...)

  df_scenario_percent_baseline <- df_this_ind %>%
    dplyr::full_join(full_df, by = (c(iso3, year, ind, scenario))) %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(
      has_estimates = any(.data[[type_col]] == "estimated"),
      baseline_value = .data[[value]][.data[[year]] == start_year],
      old_baseline_value = .data[[value]][.data[[year]] == 2010]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      goalend = .data[["old_baseline_value"]] + ((.data[["old_baseline_value"]] * (100 - 30) / 100) - .data[["old_baseline_value"]]) * (end_year - 2010) / (2025 - 2010),
      "{scenario}" := "-30_2020",
      scenario_value = dplyr::if_else(
        .data[[year]] >= start_year & .data[[year]] <= 2025 & .data[["has_estimates"]],
        .data[["baseline_value"]] + (.data[["goalend"]] - .data[["baseline_value"]]) * (.data[[year]] - 2018) / (end_year - start_year),
        NA_real_
      ),
      "{type_col}" := dplyr::if_else(
        is.na(.data[[type_col]]) & .data[[year]] >= start_year,
        "projected",
        .data[[type_col]])
    ) %>%
    dplyr::select(-c("baseline_value", "goalend", "old_baseline_value", "has_estimates")) %>%
    trim_values(
      col = "scenario_value",
      trim = TRUE,
      small_is_best = params[["small_is_best"]],
      keep_better_values = FALSE,
      upper_limit = 100,
      lower_limit = 0,
      trim_years = TRUE
    )

  params_bau <- get_right_params(params, scenario_bau)

  params_halt_rise <- get_right_params(params, scenario_halt_rise)
  params_halt_rise["baseline_year"] <- 2018

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(scenario == "business_as_usual")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params_halt_rise)
  ) %>%
    dplyr::filter(scenario == "halt_rise")

  df_binded <- df_scenario_percent_baseline %>%
    dplyr::bind_rows(df_halt_rise) %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "-30_2020",
      "halt_rise",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate ipv
#'
#' Accelerate ipv by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
accelerate_ipv <- function(df,
                           ...) {
  ind_ids <- "ipv"
  names(ind_ids) <- "child_viol"

  accelerate_child_viol(
    df = df,
    ind_ids = ind_ids,
    ...
  )
}

#' Accelerate overweight
#'
#' Accelerate overweight by picking the best value between business as usual and
#' AROC of 3 by 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_aroc(df, aroc_type = "target", target_value = 3, target_year = 2030, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_adult_obese
accelerate_overweight <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario = "scenario",
                                  ind = "ind",
                                  ...) {
  this_ind <- ind_ids["overweight"]

  params <- list(...)
  params_aroc <- get_right_params(params, scenario_aroc)
  params_aroc["aroc_type"] <- "target"
  params_aroc["target_year"] <- 2030
  params_aroc["target_value"] <- 3

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario]] == "aroc_target")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "aroc_target",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate pm25
#'
#' Accelerate pm25 by picking the best value between business as usual, and
#' linear change of -2% * 2018 `value` per year.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_linear_change(df, linear_value = df$value[df$year == 2018] * -0.02, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams accelerate_hpop_tobacco
accelerate_pm25 <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario = "scenario",
                            ind = "ind",
                            iso3 = "iso3",
                            value = "value",
                            year = "year",
                            ...) {
  this_ind <- ind_ids["pm25"]

  params <- list(...)

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  linear_value_df <- df_this_ind %>%
    dplyr::filter(.data[[year]] == 2018) %>%
    dplyr::mutate(linear_value = .data[[value]] * -0.02) %>%
    dplyr::select(iso3, "linear_value")

  df_this_ind <- df_this_ind %>%
    dplyr::left_join(linear_value_df, by = iso3)

  params_linear <- get_right_params(params, scenario_linear_change_col)

  params_linear[["linear_value_col"]] <- "linear_value"

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_linear <- do.call(
    scenario_linear_change_col, c(list(df = df_this_ind), params_linear)
  ) %>%
    dplyr::filter(.data[[scenario]] == "linear_change")

  df_binded <- df_bau %>%
    dplyr::bind_rows(df_linear)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "linear_change"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate road
#'
#' Accelerate road by picking the best results between business as usual, and
#' -50 percent from 2020 to 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`, and
#'  - `scenario_percent_baseline(df, percent_change = -50, baseline_year = 2020, target_year = 2030, small_is_best = TRUE, ...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_adult_obese
#'
accelerate_road <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario = "scenario",
                            ind = "ind",
                            ...) {
  this_ind <- ind_ids["road"]

  params <- list(...)
  params_percent_baseline <- get_right_params(params, scenario_percent_baseline)
  params_percent_baseline["percent_change"] <- -50
  params_percent_baseline["target_year"] <- 2030
  params_percent_baseline["baseline_year"] <- 2020

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2018, end_year = 2020,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  df_percent_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind), params_percent_baseline)
  ) %>%
    dplyr::filter(.data[[scenario]] == "-50_2020")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_binded <- df_percent_baseline %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "-50_2020",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate stunting
#'
#' Accelerate stunting by picking the best results between business as usual,
#' halt downwards trend, and AROC of -50% change between 2012 and 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`,
#'  - `scenario_aroc(df, aroc_type = "percent_change", percent_change = -50, baseline_year = 2012, target_year = 2030, small_is_best = TRUE, ...)`
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
accelerate_stunting <- function(df,
                                ind_ids = billion_ind_codes("hpop"),
                                scenario = "scenario",
                                ind = "ind",
                                ...) {
  this_ind <- ind_ids["stunting"]

  params <- list(...)

  params_aroc <- get_right_params(params, scenario_aroc)
  params_aroc["aroc_type"] <- "percent_change"
  params_aroc["percent_change"] <- -50
  params_aroc["baseline_year"] <- 2012
  params_aroc["target_year"] <- 2030

  params_halt <- get_right_params(params, scenario_halt_rise)

  params_bau <- get_right_params(params, scenario_bau)

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2012, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario]] == "aroc_percent_change")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind), params_halt)
  ) %>%
    dplyr::filter(.data[[scenario]] == "halt_rise")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_bau) %>%
    dplyr::bind_rows(df_halt_rise)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "halt_rise",
      "aroc_percent_change"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate suicide
#'
#' Accelerate suicide by picking the best results between business as usual,
#' halt downwards trend, and -33.333% points between 2015 and 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`,
#'  - `scenario_percent_baseline(df, percent_change = -33.333, baseline_year = 2015, target_year = 2030, small_is_best = TRUE, ...)`
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
accelerate_suicide <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  this_ind <- ind_ids["suicide"]

  params <- list(...)

  params_percent_baseline <- get_right_params(params, scenario_percent_baseline)
  params_percent_baseline["percent_change"] <- -33.333
  params_percent_baseline["baseline_year"] <- 2015
  params_percent_baseline["target_year"] <- 2030

  params_halt <- get_right_params(params, scenario_halt_rise)

  params_bau <- get_right_params(params, scenario_bau)

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2015, end_year = 2018,
    ind = ind, ind_ids = ind_ids[this_ind], scenario = scenario
  )

  df_percent_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind), params_percent_baseline)
  ) %>%
    dplyr::filter(.data[[scenario]] == "-33.333_2015")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind), params_halt)
  ) %>%
    dplyr::filter(.data[[scenario]] == "halt_rise")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_binded <- df_percent_baseline %>%
    dplyr::bind_rows(df_bau) %>%
    dplyr::bind_rows(df_halt_rise)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "halt_rise",
      "-33.333_2015"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate transfats
#'
#' Accelerate transfats by targeting 100 by 2025.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 100, target_year = 2025, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
accelerate_transfats <- function(df,
                                 ind_ids = billion_ind_codes("hpop"),
                                 scenario = "scenario",
                                 ind = "ind",
                                 ...) {
  this_ind <- ind_ids["transfats"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  params <- list(...)
  params_target <- get_right_params(params, scenario_fixed_target)
  params_target["target_value"] <- 100
  params_target["target_year"] <- 2025
  params_target["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params_target)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate wasting
#'
#' Accelerate wasting by picking the best results between business as usual,
#' halt downwards trend from 2018, and AROC by 3% by 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'  - `scenario_aroc(df, aroc_type = "target", target_value = 3, target_year = 2030, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
accelerate_wasting <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               end_year = 2025,
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  this_ind <- ind_ids["wasting"]

  params <- list(...)
  params_aroc <- get_right_params(params, scenario_aroc)
  params_aroc["aroc_type"] <- "target"
  params_aroc["target_year"] <- 2030
  params_aroc["target_value"] <- 3

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario]] == "aroc_target")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  params_halt_rise <- get_right_params(params, scenario_halt_rise)

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario]] == "halt_rise")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_halt_rise)
  dplyr::bind_rows(df_bau)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "aroc_target",
      "business_as_usual",
      "halt_rise"
    )), params_best_of)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate water
#'
#' Accelerate water by aiming at best value in quintile.
#'
#' Runs:
#'
#'  - `scenario_quantile(df, n = 5, quantile_year = 2017, baseline_quantile_year = 2018, upper_limit = 99, lower_limit = 0 small_is_best = FALSE,...)`
#'
#' @inherit accelerate_adult_obese
accelerate_water <- function(df,
                             ind_ids = billion_ind_codes("hpop"),
                             scenario = "scenario",
                             ind = "ind",
                             ...) {
  this_ind <- ind_ids["water"]

  params <- list(...)
  params["n"] <- 5
  params["quantile_year"] <- 2017
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["lower_limit"] <- 0
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2017, end_year = 2018,
    ind = ind, ind_ids = ind_ids["water"], scenario = scenario
  )

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate water_rural
#'
#' Accelerate water_rural by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_water_rural <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ind_ids = c("water" = "water_rural"),
      ...
    )
}

#' Accelerate water_urban
#'
#' Accelerate water_urban by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_water_urban <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ind_ids = c("water" = "water_urban"),
      ...
    )
}
