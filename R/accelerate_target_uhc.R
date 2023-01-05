#' Accelerate anc4 to 95% by 2030
#'
#' Accelerate `anc4` by aiming at **fixed target of 95% by 2030** for countries
#' with at least 2 reported values. For others, it is business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams calculate_uhc_billion
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration_target scenario binded to `df`. `scenario_name` is
#' set to `acceleration_target`
#'
#' @family accelerate_target
#'
accelerate_target_anc4 <- function(df,
                                   scenario_col = "scenario",
                                   default_scenario = "default",
                                   bau_scenario = "historical",
                                   scenario_name = "acceleration_target",
                                   ind_ids = billion_ind_codes("uhc"),
                                   ...) {

  this_ind <- ind_ids["anc4"]

  params <- get_dots_and_call_parameters(...)

  params_without_data_bau <- get_right_parameters(params, scenario_bau)

  params_with_data_bau <- params_without_data_bau %>%
    set_parameters(scenario_name = "with_data_bau")

  params_with_data_fixed_target <- get_right_parameters(params, scenario_fixed_target) %>%
    set_parameters(scenario_name = "with_data_fixed_target",
                   target_value = 95,
                   target_year = 2030)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) <= 1)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(df_with_data) > 0) {
    df_with_data_bau <- exec_scenario(df_with_data,
                                      scenario_bau,
                                      params_with_data_bau) %>%
      dplyr::filter(.data[[scenario_col]] == "with_data_bau")

    df_with_data_default <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_with_data_fixed_target <- exec_scenario(df_with_data_default,
                                               scenario_fixed_target,
                                               params_with_data_fixed_target)%>%
      dplyr::filter(.data[[scenario_col]] == "with_data_fixed_target")

    params_scenario_best_of_linear_fixed <- get_right_parameters(params, scenario_best_of) %>%
      set_parameters(
        scenario_names = c("with_data_fixed_target", "with_data_bau"))

    df_with_data_accelerated <- dplyr::bind_rows(df_with_data_fixed_target,
                                                 df_with_data_bau) %>%
      exec_scenario(scenario_best_of,
                    params_scenario_best_of_linear_fixed)%>%
      dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])

  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_without_data) > 0) {
    df_without_data_accelerated <- exec_scenario(df_without_data,
                                                 scenario_bau,
                                                 params_without_data_bau) %>%
      dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_without_data_accelerated, df_with_data_accelerated)

}

#' Accelerate art to 90.25 by 2025
#'
#' Accelerate art by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, business as usual is returned.
#' - For countries with reported data, the best of business as usual and **fixed
#'  target of 90.25% by 2025** is chosen.
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_art <- function(df,
                                  ind_ids = billion_ind_codes("uhc"),
                                  scenario_col = "scenario",
                                  start_year = 2018,
                                  default_scenario = "default",
                                  bau_scenario = "default",
                                  scenario_name = "acceleration_target",
                                  ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_art,
                params)
}

#' Accelerate beds to 18 by 2025
#'
#' Accelerate beds by first dividing countries into two groups:
#' - For countries with 18 or more beds for all years after 2018, business
#' as usual is returned.
#' - For countries which have less than 18 beds for any of the years after 2018 (inclusive),
#' 18 beds is targeted by 2025
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_beds <- function(df,
                                   ind_ids = billion_ind_codes("uhc"),
                                   scenario_col = "scenario",
                                   value_col = "value",
                                   start_year = 2018,
                                   default_scenario = "default",
                                   bau_scenario = "historical",
                                   scenario_name = "acceleration_target",
                                   ...) {

  this_ind <- ind_ids["beds"]

  params <- get_dots_and_call_parameters(...)

  params_no_scenario_bau <- set_parameters(
    get_right_parameters(params, scenario_bau),
    avoid_worstening = TRUE,
    upper_limit = Inf,
    scenario_name = scenario_name
  )

  params_with_scenario_bau <- get_right_parameters(params, scenario_bau) %>%
    set_parameters(scenario_name = "with_scenario_bau",
                   upper_limit = Inf)

  params_with_scenario_target <- get_right_parameters(params, scenario_fixed_target) %>%
    set_parameters(target_value = 18,
                   scenario_name = "fixed_target",
                   target_year = 2025,
                   upper_limit = 18)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_with_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(any((.data[[value_col]] < 18 & .data[["year"]] >= start_year))) %>%
    dplyr::ungroup()

  df_no_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!any((.data[[value_col]] < 18 & .data[["year"]] >= start_year))) %>%
    dplyr::ungroup()

  if (nrow(df_no_scenario) > 0) {

    df_no_scenario_accelerated <- exec_scenario(df_no_scenario,
                                                scenario_bau,
                                                params_no_scenario_bau)%>%
      dplyr::filter(.data[[scenario_col]] == scenario_name)
  } else {
    df_no_scenario_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_scenario) > 0) {

    df_with_scenario_bau <- exec_scenario(df_with_scenario,
                                          scenario_bau,
                                          params_with_scenario_bau) %>%
      dplyr::filter(.data[[scenario_col]] == "with_scenario_bau")

    df_with_scenario_default <- df_with_scenario %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_with_scenario_fixed <- exec_scenario(df_with_scenario_default,
                                            scenario_fixed_target,
                                            params_with_scenario_target) %>%
      dplyr::filter(.data[[scenario_col]] == "fixed_target")

    params_scenario_best_of <- get_right_parameters(params, scenario_best_of) %>%
      set_parameters(scenario_names = c("with_scenario_bau", "fixed_target"))

    df_with_scenario_accelerated <- dplyr::bind_rows(df_with_scenario_bau, df_with_scenario_fixed)

    df_with_scenario_accelerated <- exec_scenario(df_with_scenario_accelerated,
                                                  scenario_best_of,
                                                  params_scenario_best_of) %>%
      dplyr::filter(.data[[scenario_col]] == scenario_name)

  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_no_scenario_accelerated, df_with_scenario_accelerated)
}

#' Accelerate bp
#'
#' Accelerate bp by aiming at reaching 80% by 2030.
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_bp <- function(df,
                                 ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_bp,
                params)
}

#' Accelerate doctors
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inheritParams accelerate_anc4
#'
#' @family accelerate_target
#'
accelerate_target_doctors <- function(df,
                                      ind_ids = billion_ind_codes("uhc"),
                                      scenario_col = "scenario",
                                      bau_scenario = "historical",
                                      scenario_name = "acceleration",
                                      ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_doctors,
                params
  )
}

#' Accelerate nurses
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inheritParams accelerate_alcohol
#'
#' @family accelerate_target
#'
accelerate_target_nurses <- function(df,
                                     ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_nurses,
                params
  )}

#' Accelerate hwf
#'
#' Accelerate hwf by returning to business as usual, as there are no globally agreed target.
#'
#' @inheritParams accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
#' @family accelerate_target
#'
accelerate_target_hwf <- function(df,
                                  ...) {
  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_bau)

  exec_scenario(df,
                scenario_bau,
                params)
}

#' Accelerate dtp3
#'
#' Accelerate dtp3 using a customised version of scenario_fixed_target with the
#' following peculiarities:
#' - baseline_year = 2019;
#' - the 2020 value is kept identical to the 2019 (baseline) value;
#' - the target_year is 2030; and
#' - the scenario is then a straight line to the target_value and target_year
#' - the target values for each country are provided by the technical program.
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_dtp3 <- function(df,
                                   ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_dtp3,
                params)
}


#' Accelerate fh
#'
#' Accelerate fh by taking the best of business as usual and halting upward trends
#' in the data to the 2018 value.
#'
#' @inheritParams accelerate_alcohol
#'
#' @family accelerate_target
#'
accelerate_target_fh <- function(df,
                                 ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_fh,
                params)
}

#' Accelerate fp
#'
#' Accelerate fp by returning to business as usual, as there are no globally agreed target.
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_fp <- function(df,
                                 ...) {
  params <- get_dots_and_call_parameters(...) %>%
    get_right_parameters(scenario_bau)

  exec_scenario(df,
                scenario_bau,
                params)
}

#' Accelerate fpg
#'
#' Accelerate fpg by halting the rise to 2010 value.
#'
#' @inheritParams accelerate_alcohol
#'
#' @family accelerate_target
#'
accelerate_target_fpg <- function(df,
                                  ...) {
  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(ind_ids = c("adult_obese" = "fpg"))

  exec_scenario(df, accelerate_adult_obese, params)
}

#' Accelerate itn
#'
#' Accelerate itn by taking the best of business as usual and a **fixed target of
#' 80 by 2030**.
#'
#' @inheritParams accelerate_anc4
#'
#' @family accelerate_target
#'
accelerate_target_itn <- function(df,
                                  ind_ids = billion_ind_codes("uhc"),
                                  scenario_col = "scenario",
                                  default_scenario = "default",
                                  bau_scenario = "historical",
                                  scenario_name = "acceleration",
                                  ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df, accelerate_itn, params)
}

#' Accelerate pneumo
#'
#' Accelerate pneumo by taking the best of business as usual and a **fixed target
#' of 90 by 2025** for countries with two or more data points since 2000. Otherwise,
#' the business as usual scenario is used.
#'
#' @inheritParams accelerate_alcohol
#'
#' @family accelerate_target
#'
accelerate_target_pneumo <- function(df,
                                     ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_pneumo,
                params)
}

#' Accelerate tb
#'
#' Accelerate tb by using a **fixed target of 90 by 2025**.
#'
#' @inheritParams accelerate_alcohol
#'
#' @family accelerate_target
#'
accelerate_target_tb <- function(df,
                                 ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_tb,
                params)
}

#' Accelerate uhc_sanitation
#'
#' Accelerate uhc_sanitation by targeting to reach 95% by 2030.
#'
#' @inheritParams accelerate_anc4
#' @inheritParams recycle_data
#'
#' @family accelerate_target
#'
accelerate_target_uhc_sanitation <- function(df,
                                             ind_ids = billion_ind_codes("uhc"),
                                             ...) {


  params <- get_dots_and_call_parameters(...)

  this_ind <- ind_ids["uhc_sanitation"]

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
#' Accelerate uhc_tobacco
#'
#' Accelerate uhc_tobacco by first dividing countries into two groups:
#' - For countries without any routine (i.e., estimated) data, business as usual
#' is returned
#' - For countries with routine (i.e., estimated) data, the best of business as
#' usual and a **percent decrease of 30% between 2010 and 2025** is returned. Both
#' scenarios are run on the **crude tobacco usage** values, which are then converted
#' to their age-standardised equivalents using an approximation.
#'
#' @inheritParams accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
#' @family accelerate_target
#'
accelerate_target_uhc_tobacco <- function(df,
                                          ind_ids = billion_ind_codes("uhc"),
                                          scenario_col = "scenario",
                                          value_col = "value",
                                          end_year = 2025,
                                          start_year = 2018,
                                          default_scenario = "default",
                                          bau_scenario = "historical",
                                          scenario_name = "acceleration",
                                          ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                accelerate_uhc_tobacco,
                params)
}
