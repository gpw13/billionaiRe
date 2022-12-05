#' Accelerate anc4 to benchmarking scenario
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
benchmarking_anc4 <- function(df,
                              ...) {

  params <- get_dots_and_call_parameters(...)

  df <- df %>%
    dplyr::mutate(region = whoville::iso3_to_regions(.data[["iso3"]]),
                  wb_ig = whoville::iso3_to_regions(.data[["iso3"]], "wb_ig"))

  params_benchmarking <- list(
    prop = list(NULL, .1, .25, .25, .1),
    use_prop = c(FALSE, rep(TRUE,4)),
    group_col = list(NULL, "region", NULL, "wb_ig", "wb_ig")
  )

  params_expanded <- purrr::pmap(params_benchmarking, set_parameters, parameters = params)

  purrr::map_dfr(params_expanded,
                 exec_scenario,
                 df = df,
                 fn = scenario_top_n_iso3)
}

#' Accelerate art to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#'
benchmarking_art <- function(df,
                             ...) {

  params <- get_dots_and_call_parameters(...)
  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate beds to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_beds <- function(df,
                              ...) {
  params <- get_dots_and_call_parameters(...)
  exec_scenario(df,
                benchmarking_anc4,
                params)

}

#' Accelerate bp to benchmarking scenario
#'
#' Accelerate `bp` by aiming at 100 by 2030 if there are 2 values or more
#' reported. Otherwise, business as usual.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_bp <- function(df,
                            ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate doctors to benchmarking scenario
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
benchmarking_doctors <- function(df,
                                 ...) {
  params <- get_dots_and_call_parameters(...)

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(upper_limit = 10000)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate nurses to benchmarking scenario
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inherit accelerate_anc4
#'
benchmarking_nurses <- function(df,
                                ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_doctors,
                params
  )
}

#' Accelerate hwf to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_hwf <- function(df,
                             ...) {

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(baseline_year = 2012,
                   aroc_end_year = 2017)

  exec_scenario(df,
                benchmarking_doctors,
                params
  )
}

#' Accelerate dtp3 to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_dtp3 <- function(df,
                              ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}


#' Accelerate fh to benchmarking scenario
#'
#' @inherit accelerate_anc4
#'
benchmarking_fh <- function(df,
                            ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate fp to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_fp <- function(df,
                            ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate fpg to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams accelerate_child_viol
#'
benchmarking_fpg <- function(df,
                             start_year = 2018,
                             scenario_col = "scenario",
                             ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)

}

#' Accelerate itn to benchmarking scenarios
#'
#' @inherit accelerate_anc4
#'
benchmarking_itn <- function(df,
                             ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}


#' Accelerate pneumo to benchmarking scenario
#'
#' @inherit accelerate_anc4
#'
benchmarking_pneumo <- function(df,
                                ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate tb to benchmarking scenario
#'
#' @inherit accelerate_anc4
#'
benchmarking_tb <- function(df,
                            ...) {
  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate uhc_sanitation to benchmarking scenario
#'
#' @inherit accelerate_anc4
#'
benchmarking_uhc_sanitation <- function(df,
                                        ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate uhc_tobacco to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_uhc_tobacco <- function(df,
                                     ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}

#' Accelerate espar to benchmarking scenario
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
benchmarking_espar <- function(df,
                               ...) {

  params <- get_dots_and_call_parameters(...)

  exec_scenario(df,
                benchmarking_anc4,
                params)
}
