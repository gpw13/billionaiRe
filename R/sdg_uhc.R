#' Accelerate anc4 to SDG target
#'
#' Accelerate `anc4` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams calculate_uhc_billion
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario` is
#' set to `acceleration`
#'
sdg_anc4 <- function(df,
                     scenario_col = "scenario",
                     default_scenario = "default",
                     bau_scenario = "historical",
                     scenario_name = "sdg",
                     ind_ids = billion_ind_codes("uhc"),
                     end_year = 2030,
                     ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(target_value = 100,
                   target_year = 2030,
                   scenario_function = "scenario_fixed_target")

  exec_scenario(df,
                scenario_with_values,
                params)
}

#' Accelerate art to SDG target
#'
#' Accelerate `art` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#'
sdg_art <- function(df,
                    end_year = 2030,
                    ...) {

  params <- get_dots_and_call_parameters(...)
  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate beds to SDG target
#'
#' Accelerate `beds` by aiming at 18 by 2030 if values are below 18. Otherwise,
#' business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_beds <- function(df,
                     ind_ids = billion_ind_codes("uhc"),
                     scenario_col = "scenario",
                     value_col = "value",
                     start_year = 2018,
                     default_scenario = "default",
                     bau_scenario = "historical",
                     scenario_name = "sdg",
                     ...) {

  this_ind <- ind_ids["beds"]

  params <- get_dots_and_call_parameters(...)

  params_no_scenario_bau <- set_parameters(
    get_right_parameters(params, scenario_bau),
    avoid_worstening = TRUE,
    upper_limit = Inf,
    scenario_name = scenario_name
  )

  params_with_scenario_target <- get_right_parameters(params, scenario_fixed_target) %>%
    set_parameters(target_value = 18,
                   target_year = 2030,
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

    df_no_scenario <- exec_scenario(df_no_scenario,
                                                scenario_bau,
                                                params_no_scenario_bau)%>%
      dplyr::filter(.data[[scenario_col]] == scenario_name)
  } else {
    df_no_scenario <- tibble::tibble()
  }

  if (nrow(df_with_scenario) > 0) {


    df_with_scenario_default <- df_with_scenario %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_with_scenario_fixed <- exec_scenario(df_with_scenario_default,
                                            scenario_fixed_target,
                                            params_with_scenario_target) %>%
      dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])

  } else {
    df_with_scenario_fixed <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_no_scenario, df_with_scenario_fixed)
}

#' Accelerate bp to SDG target
#'
#' Accelerate `bp` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_bp <- function(df,
                   ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate doctors to SDG target
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
sdg_doctors <- function(df,
                        ...) {
  params <- get_dots_and_call_parameters(...)

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(target_value = 154.74,
                   target_year = 2030,
                   upper_limit = 10000,
                   scenario_function = "scenario_fixed_target")

  exec_scenario(df,
                scenario_with_values,
                params)
}

#' Accelerate nurses to SDG target
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inherit accelerate_anc4
#'
sdg_nurses <- function(df,
                       ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_doctors,
                params
  )
}

#' Accelerate hwf to SDG target
#'
#' Accelerate `hwf` by returning to business as usual, as there are no globally
#' agreed target.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_hwf <- function(df,
                    ind_ids = billion_ind_codes("uhc"),
                    scenario_col = "scenario",
                    start_year = 2018,
                    default_scenario = "default",
                    bau_scenario = "historical",
                    ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_doctors,
                params
  )
}

#' Accelerate dtp3 to SDG target
#'
#' Accelerate `dtp3` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_dtp3 <- function(df,
                     ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}


#' Accelerate fh to SDG target
#'
#' Accelerate `fh` by aiming at 0 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_fh <- function(df,
                   ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(target_value = 0,
                   target_year = 2030,
                   scenario_function = "scenario_fixed_target")

  exec_scenario(df,
                scenario_with_values,
                params)
}

#' Accelerate fp to SDG target
#'
#' Accelerate `dtp3` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_fp <- function(df,
                   ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate fpg to SDG target
#'
#' Accelerate `fpg` by aiming at 0 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_fpg <- function(df,
                    ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_fh,
                params)
}

#' Accelerate itn to SDG target
#'
#' Accelerate `itn` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_itn <- function(df,
                    ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}


#' Accelerate pneumo to SDG target
#'
#' Accelerate `pneumo` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_pneumo <- function(df,
                       ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate tb to SDG target
#'
#' Accelerate `pneumo` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_tb <- function(df,
                   ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate uhc_sanitation to SDG target
#'
#' Accelerate `uhc_sanitation` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#'
sdg_uhc_sanitation <- function(df,
                               ind_ids = billion_ind_codes("uhc"),
                               ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_anc4,
                params)
}

#' Accelerate uhc_tobacco to SDG target
#'
#' Accelerate `uhc_tobacco` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_uhc_tobacco <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario_col = "scenario",
                            ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                sdg_fh,
                params)
}
