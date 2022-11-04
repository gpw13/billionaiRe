#' Accelerate adult_obese to SDG target
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
#' set to `sdg`
sdg_adult_obese <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario_col = "scenario",
                            ...) {

  assert_columns(df,scenario_col, "ind")

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_adult_obese,
                params
  )

  # accelerate_adult_obese(
  #   df = df,
  #   ind_ids = this_ind,
  #   ...
  # ) %>%
  #   dplyr::mutate("{scenario_col}" := dplyr::case_when(
  #     .data[[scenario_col]] == "acceleration" ~ "sdg",
  #     TRUE ~ as.character(.data[[scenario_col]])
  #   ))
}

#' Accelerate alcohol to SDG target
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
#' @inherit sdg_adult_obese
#'
sdg_alcohol <- function(df,
                        ind_ids = billion_ind_codes("hpop"),
                        scenario_col = "scenario",
                        ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_alcohol,
                params
                )
}

#' Accelerate child_obese to SDG target
#'
#' Accelerate child_obese by halting upwards trend in the data to the 2010 value.
#'
#' @inherit accelerate_alcohol
#'
sdg_child_obese <- function(df,
                            ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("adult_obese" = "child_obese"))

  exec_scenario(df,
                sdg_adult_obese,
                params)
}

#' Accelerate child_viol to SDG target
#'
#' Accelerate child_viol by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
sdg_child_viol <- function(df,
                           ind_ids = billion_ind_codes("hpop"),
                           scenario_col = "scenario",
                           ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_child_viol,
                params)
}

#' Accelerate devontrack to SDG target
#'
#' Accelerate devontrack by targeting 80 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 80, target_year = 2030, small_is_best = FALSE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
sdg_devontrack <- function(df,
                           ind_ids = billion_ind_codes("hpop"),
                           end_year = 2025,
                           scenario_col = "scenario",
                           ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_devontrack,
                params)
}

#' Accelerate fuel to SDG target
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
sdg_fuel <- function(df,
                     ind_ids = billion_ind_codes("hpop"),
                     scenario_col = "scenario",
                     ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_fuel,
                params)
}

#' Accelerate hpop_sanitation to SDG target
#'
#' Accelerate hpop_sanitation by aiming at 95% by 20230
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 95,target_year = 2030, small_is_best = FALSE)`
#'
#' @inherit accelerate_adult_obese
sdg_hpop_sanitation <- function(df,
                                ind_ids = billion_ind_codes("hpop"),
                                end_year = 2025,
                                scenario_col = "scenario",
                                scenario_name = "sdg",
                                ...) {

  this_ind <- ind_ids["hpop_sanitation"]

  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_fixed_target) %>%
    set_parameters(target_year = 2030,
                   target_value = 95)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  exec_scenario(df_this_ind,
                scenario_fixed_target,
                params)
}

#' Accelerate hpop_sanitation_rural to SDG target
#'
#' Accelerate hpop_sanitation_rural by aiming at 95% by 20230
#'
#' @inherit accelerate_water
sdg_hpop_sanitation_rural <- function(df,
                                      ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("hpop_sanitation" = "hpop_sanitation_rural"))

  exec_scenario(df,
                sdg_hpop_sanitation,
                params)
}

#' Accelerate hpop_sanitation_urban to SDG target
#'
#' Accelerate hpop_sanitation_urban by aiming at 95% by 20230
#'
#' @inherit accelerate_water
sdg_hpop_sanitation_urban <- function(df,
                                      ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("hpop_sanitation" = "hpop_sanitation_urban"))

  exec_scenario(df,
                sdg_hpop_sanitation,
                params)
}

#' Accelerate hpop_tobacco to SDG target
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
sdg_hpop_tobacco <- function(df,
                             ind_ids = billion_ind_codes("hpop"),
                             scenario_col = "scenario",
                             start_year = 2018,
                             end_year = 2025,
                             ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_hpop_tobacco,
                params)
}

#' Accelerate ipv to SDG target
#'
#' Accelerate ipv by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
sdg_ipv <- function(df,
                    scenario_col = "scenario",
                    ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_ipv,
                params)
}

#' Accelerate overweight to SDG target
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
sdg_overweight <- function(df,
                           ind_ids = billion_ind_codes("hpop"),
                           scenario_col = "scenario",
                           ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_overweight,
                params)

}

#' Accelerate pm25 to SDG target
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
sdg_pm25 <- function(df,
                     ind_ids = billion_ind_codes("hpop"),
                     scenario_col = "scenario",
                     ...) {

  this_ind <- ind_ids["pm25"]

  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_fixed_target) %>%
    set_parameters(target_value = 10,
                   target_year = 2030)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  exec_scenario(df_this_ind,
                scenario_fixed_target,
                params)
}

#' Accelerate road to SDG target
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
sdg_road <- function(df,
                     ind_ids = billion_ind_codes("hpop"),
                     scenario_col = "scenario",
                     ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_road,
                params)
}

#' Accelerate stunting to SDG target
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
sdg_stunting <- function(df,
                         ind_ids = billion_ind_codes("hpop"),
                         scenario_col = "scenario",
                         ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_stunting,
                params)
}

#' Accelerate suicide to SDG targets
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
sdg_suicide <- function(df,
                        ind_ids = billion_ind_codes("hpop"),
                        scenario_col = "scenario",
                        ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_suicide,
                params)
}

#' Accelerate transfats to SDG targets
#'
#' Accelerate transfats by targeting 100 by 2025.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 100, target_year = 2025, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_adult_obese
#'
sdg_transfats <- function(df,
                          ind_ids = billion_ind_codes("hpop"),
                          scenario_col = "scenario",
                          ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_transfats,
                params)
}

#' Accelerate wasting to SDG targets
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
sdg_wasting <- function(df,
                        ind_ids = billion_ind_codes("hpop"),
                        end_year = 2025,
                        scenario_col = "scenario",
                        ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_wasting,
                params)
}

#' Accelerate water to SDG target
#'
#' Accelerate water by aiming at 95% by 20230
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 95,target_year = 2030, small_is_best = FALSE)`
#'
#' @inherit accelerate_adult_obese
sdg_water <- function(df,
                      ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("hpop_sanitation" = "water"))

  exec_scenario(df,
                sdg_hpop_sanitation,
                params)

}

#' Accelerate water_rural to SDG target
#'
#' Accelerate water_rural by aiming at 95% by 20230
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 95,target_year = 2030, small_is_best = FALSE)`
#'
#' @inherit accelerate_adult_obese
sdg_water_rural <- function(df,
                            ...) {
  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("hpop_sanitation" = "water_rural"))

  exec_scenario(df,
                sdg_hpop_sanitation,
                params)
}

#' Accelerate water_urban to SDG target
#'
#' Accelerate water_urban by aiming at 95% by 20230
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 95,target_year = 2030, small_is_best = FALSE)`
#'
#' @inherit accelerate_adult_obese
sdg_water_urban <- function(df,
                            ...) {
  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("hpop_sanitation" = "water_urban"))

  exec_scenario(df,
                sdg_hpop_sanitation,
                params)
}
