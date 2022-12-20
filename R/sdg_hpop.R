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
}

#' Accelerate alcohol to SDG target
#'
#' Accelerate alcohol by picking the best results between business as usual,
#' halt downwards trend from 2018, and -10 percent from 2010.
#'
#' Runs:
#'  - `scenario_percent_baseline(df, percent_change = -10, baseline_year = 2010, small_is_best = TRUE, ...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit sdg_adult_obese
#' @inheritParams accelerate_alcohol
#'
sdg_alcohol <- function(df,
                        ind_ids = billion_ind_codes("hpop"),
                        scenario_col = "scenario",
                        default_scenario = "default",
                        ...) {

  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_fixed_target_col) %>%
    set_parameters(
      target_col = "target",
    )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == ind_ids["alcohol"])

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  neg_10_targets <- df_this_ind_default %>%
    dplyr::filter(.data[["year"]] == 2010) %>%
    dplyr::mutate(target = .data[["value"]] * (100 - 10) / 100) %>%
    dplyr::select("iso3", "ind", "target")

  df_perc_baseline <- df_this_ind_default %>%
    dplyr::left_join(neg_10_targets, by = c("iso3", "ind"))

  df_sdg <- exec_scenario(df_perc_baseline,
                scenario_fixed_target_col,
                params) %>%
    dplyr::select(-"target") %>%
    dplyr::filter(.data[[scenario_col]] %in% params[["scenario_name"]])

  dplyr::bind_rows(df, df_sdg)
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
#' Accelerate devontrack by targeting 100 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = <, target_year = 2030, small_is_best = FALSE,...)`,
#'
#' @inherit accelerate_adult_obese
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_child_viol
#'
sdg_devontrack <- function(df,
                           ind_ids = billion_ind_codes("hpop"),
                           end_year = 2025,
                           scenario_col = "scenario",
                           start_year = 2018,
                           value_col = "value",
                           default_scenario = "default",
                           ...) {

  assert_columns(df, scenario_col, "ind")

  this_ind <- ind_ids["devontrack"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind[["iso3"]]),
    "year" := start_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind[[scenario_col]])
  )

  latest_values <- df_this_ind %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", scenario_col)))) %>%
    dplyr::filter(.data[["year"]] <= start_year) %>%
    dplyr::filter(max(.data[["year"]]) == .data[["year"]]) %>%
    dplyr::mutate(latest_value = .data[[value_col]]) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(c("iso3", "ind", scenario_col, "latest_value")))

  df_this_ind <- df_this_ind %>%
    dplyr::full_join(full_df, by = c("iso3", "year", "ind", scenario_col)) %>%
    dplyr::left_join(latest_values, by = c("iso3", "ind", scenario_col)) %>%
    dplyr::mutate(
      !!sym(value_col) := dplyr::case_when(
        is.na(.data[[value_col]]) ~ .data[["latest_value"]],
        TRUE ~ .data[[value_col]]
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"latest_value")

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(
      target_value = 100,
      target_year = 2030,
      start_year = start_year
    ) %>%
    get_right_parameters(scenario_fixed_target)

  df_accelerated <- exec_scenario(df_this_ind,
                                  scenario_fixed_target,
                                  params) %>%
    dplyr::filter(.data[[scenario_col]] %in% params[["scenario_name"]])

  dplyr::bind_rows(df, df_accelerated)
}

#' Accelerate fuel to SDG target
#'
#' Accelerate devontrack by targeting 100 by 2030.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#'
sdg_fuel <- function(df,
                     ind_ids = billion_ind_codes("hpop"),
                     scenario_col = "scenario",
                     ...) {
  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_fixed_target) %>%
    set_parameters(target_year = 2030,
                   target_value = 100)

  exec_scenario(df,
                scenario_fixed_target,
                params)
}

#' Accelerate hpop_sanitation to SDG target
#'
#' Accelerate hpop_sanitation by aiming at 100% by 20230
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 95,target_year = 2030, small_is_best = FALSE)`
#'
#' @inherit accelerate_adult_obese
#' @inheritParams calculate_hpop_contributions
#'
sdg_hpop_sanitation <- function(df,
                                ind_ids = billion_ind_codes("hpop"),
                                end_year = 2025,
                                scenario_col = "scenario",
                                scenario_name = "sdg",
                                ...) {

  this_ind <- ind_ids["hpop_sanitation"]

  params <- get_dots_and_call_parameters(...)

  params_95_2030 <- params %>%
    get_right_parameters(scenario_fixed_target) %>%
    set_parameters(target_year = 2030,
                   target_value = 100,
                   scenario_name = "100_2030")

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_95_2030 <- exec_scenario(df_this_ind,
                              scenario_fixed_target,
                              params_95_2030) %>%
    dplyr::filter(.data[[scenario_col]] == "100_2030")


  params_bau <- get_right_parameters(params, scenario_bau) %>%
    set_parameters(scenario_name = "business_as_usual")

  df_bau <- exec_scenario(df_this_ind,
                          scenario_bau,
                          params_bau) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_binded <- df_95_2030 %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_parameters(params, scenario_best_of) %>%
    set_parameters(
      scenario_names = c(
        "100_2030",
        "business_as_usual"
      )
    )

  df_sdg <- exec_scenario(df_binded,
                          scenario_best_of,
                          params_best_of) %>%
    dplyr::filter(.data[[scenario_col]] == params["scenario_name"])

  df %>%
    dplyr::bind_rows(df_sdg)

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
#' Accelerate hpop_tobacco by running a custom version of
#' scenario_percent_baseline. The custom `scenario_percent_baseline` is
#' taking similar parameters to `scenario_percent_baseline`'s
#' `percent_change` = -30, `baseline_year` = 2010, but values are added to the
#' `start_year` value, rather than the `baseline_year` values.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hep_data
#' @inheritParams trim_values

sdg_hpop_tobacco <- function(df,
                             ind_ids = billion_ind_codes("hpop"),
                             scenario_col = "scenario",
                             start_year = 2018,
                             end_year = 2025,
                             start_year_trim = start_year,
                             end_year_trim = end_year,
                             default_scenario = "default",
                             value_col = "value",
                             ...) {

  params <- get_dots_and_call_parameters(...)

  this_ind <- ind_ids["hpop_tobacco"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind_default[["iso3"]]),
    "year" := start_year:end_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind_default[[scenario_col]])
  )

  assert_ind_start_end_year(df_this_ind_default,
                            start_year = 2010, end_year = 2018,
                            ind_ids = ind_ids[this_ind], scenario_col = scenario_col
  )

  df_scenario_percent_baseline <- df_this_ind_default %>%
    dplyr::full_join(full_df, by = (c("iso3", "year", "ind", scenario_col))) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(
      has_estimates = any(.data[["type"]] %in% c("estimated", "reported")),
      baseline_value = .data[[value_col]][.data[["year"]] == start_year],
      old_baseline_value = .data[[value_col]][.data[["year"]] == 2010]
    ) %>%
    dplyr::mutate(
      goalend = .data[["old_baseline_value"]] + ((.data[["old_baseline_value"]] * (100 - 30) / 100) - .data[["old_baseline_value"]]) * (end_year - 2010) / (end_year - 2010),
      "{scenario_col}" := params[["scenario_name"]],
      scenario_value = dplyr::case_when(
        .data[[value_col]][.data[["year"]] == start_year] <= .data[["goalend"]] ~ .data[[value_col]][.data[["year"]] == start_year],
        .data[["year"]] >= start_year & .data[["year"]] <= end_year & .data[["has_estimates"]] ~
        .data[["baseline_value"]] + (.data[["goalend"]] - .data[["baseline_value"]]) * (.data[["year"]] - start_year) / (end_year - start_year),
        TRUE ~ NA_real_
      ),
      "type_" := dplyr::if_else(
        is.na(.data[["type"]]) & .data[["year"]] >= start_year,
        "projected",
        .data[["type"]])
    ) %>%
    dplyr::select(-c("baseline_value", "goalend", "old_baseline_value", "has_estimates", "type_")) %>%
    trim_values(
      col = "scenario_value",
      trim = TRUE,
      small_is_best = params[["small_is_best"]],
      keep_better_values = FALSE,
      upper_limit = 100,
      lower_limit = 0,
      trim_years = TRUE,
      end_year_trim = end_year_trim,
      start_year_trim = start_year_trim
    )

  df %>%
    dplyr::bind_rows(df_scenario_percent_baseline)
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
#' Accelerate overweight by targeting 0 by 2030.
#'
#' @inherit accelerate_adult_obese
sdg_overweight <- function(df,
                           ind_ids = billion_ind_codes("hpop"),
                           scenario_col = "scenario",
                           ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_fh,
                params)
}

#' Accelerate pm25 to SDG target
#'
#' Accelerate pm25 aiming at 5% by 2030.
#'
#' Runs:
#'
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
    set_parameters(target_value = 5,
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

  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_percent_baseline) %>%
    set_parameters(percent_change = -50,
                   target_year = 2030,
                   baseline_year = 2020)

  exec_scenario(df,
                scenario_percent_baseline,
                params)

}

#' Accelerate stunting to SDG target
#'
#' Accelerate stunting by aiming at 0% by 2030.
#'
#' @inherit accelerate_adult_obese
sdg_stunting <- function(df,
                         ind_ids = billion_ind_codes("hpop"),
                         scenario_col = "scenario",
                         default_scenario,
                         ...) {


  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_fh,
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
                        default_scenario = "default",
                        ...) {
  this_ind <- ind_ids["suicide"]

  params <- get_dots_and_call_parameters(...)

  params_percent_baseline <- get_right_parameters(params, scenario_percent_baseline) %>%
    set_parameters(
      percent_change = -1/3*100,
      baseline_year = 2015,
      target_year = 2030
    )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)


  df_percent_baseline <- exec_scenario(df_this_ind_default,
                                       scenario_percent_baseline,
                                       params_percent_baseline)%>%
    dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])

  dplyr::bind_rows(df, df_percent_baseline)
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
#' Accelerate wasting by aiming at 0% by 2030.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams accelerate_child_viol
sdg_wasting <- function(df,
                        ind_ids = billion_ind_codes("hpop"),
                        start_year = 2018,
                        scenario_col = "scenario",
                        default_scenario = "default",
                        ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_fh,
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
