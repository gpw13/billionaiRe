#' Add scenario to data frame
#'
#' `add_scenario()` wraps around `add_scenario_indicator()` to add the scenario
#' specified in `scenario_function` to all the valid billionaiRe indicators
#' present in `df`.
#'
#' `add_scenario_indicator()` gets the right `add_scenario_` function for the
#' indicator specified and adds the scenario.
#'
#' @param scenario_function character identifier of scenario to use. Can be any
#' of the following:
#'
#' - `aroc`: calls \code{\link{scenario_aroc}}
#' - `halt_rise`: calls \code{\link{scenario_halt_rise}}
#' - `percent_baseline`: calls \code{\link{scenario_percent_baseline}}
#' - `linear_change`: calls \code{\link{scenario_linear_change}}
#' - `linear_change_col`: calls \code{\link{scenario_linear_change_col}}
#' - `quantile`: calls \code{\link{scenario_quantile}}
#' - `best_in_region`: calls \code{\link{scenario_best_in_region}}
#' - `fixed_target`: calls \code{\link{scenario_fixed_target}}
#' - `fixed_target_col`: calls \code{\link{scenario_fixed_target_col}}
#' - `bau`: calls \code{\link{scenario_bau}}
#' - `accelerate`: calls indicator accelerate function.
#' - `accelerate_target`: calls indicator accelerate function to specified
#'   targets.
#' - `sdg`: calls Sustainable Development Goals (SDG) acceleration function.
#' - `benchmarking`: calls benchmarking scenarios.
#' - `covid_rapid_return` calls \code{\link{scenario_covid_rapid_return}}
#' - `covid_delayed_return` calls \code{\link{scenario_covid_delayed_return}}
#' - `covid_sustained_disruption` calls \code{\link{scenario_covid_sustained_disruption}}
#' - `return_previous_trajectory` calls \code{\link{scenario_return_previous_trajectory}}
#' - `top_n_iso3` calls \code{\link{scenario_top_n_iso3}}
#' @param ... additional arguments passed to `add_scenario_indicator()` or
#' indicator level functions (e.g. `add_scenario_adult_obese()`)
#' @param indicator name of indicator to be passed to `scenario_function`
#' @param make_default (Boolean) if `TRUE`, then `make_default_scenario` is used to
#' generate the `default_scenario`
#' @param start_scenario_last_default (Boolean) if `TRUE`, then the last year with values
#' in the `default_scenario` is the starting point of the scenario, and not `start_year`.
#' @param expend_bau (Boolean)if `TRUE`, then `make_default_scenario` is used to
#' expend `bau_scenario` to the start of the `default_scenario`.
#' @param ... additional parameters to be passed to the add_scenario_indicator
#' function (e.g. `add_scenario_adult_obese`).
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_billion
#' @inheritParams add_scenario_indicator
#' @inheritParams accelerate_alcohol
#' @inheritParams scenario_fixed_target
#'
#' @rdname add_scenario
#'
#' @family scenarios
#'
#' @return data frame with additional rows with scenario values.
#'
#' @export
#'
add_scenario <- function(df,
                         scenario_function = c(
                           "aroc",
                           "halt_rise",
                           "percent_baseline",
                           "linear_change",
                           "linear_change_col",
                           "quantile",
                           "best_in_region",
                           "fixed_target",
                           "fixed_target_col",
                           "bau",
                           "accelerate",
                           "accelerate_target",
                           "sdg",
                           "benchmarking",
                           "covid_rapid_return",
                           "covid_delayed_return",
                           "covid_sustained_disruption",
                           "return_previous_trajectory",
                           "top_n_iso3"),
                         ind_ids = billion_ind_codes("all"),
                         scenario_col = "scenario",
                         start_year = 2018,
                         end_year = 2025,
                         default_scenario = "default",
                         make_default = FALSE,
                         start_scenario_last_default = TRUE,
                         ...) {
  assert_columns(df, "ind")
  scenario_function <- rlang::arg_match(scenario_function)

  sub_set_inds <- ind_ids[!stringr::str_detect(ind_ids,"surviving_infants")]

  if(default_scenario %in% unique(df[[scenario_col]])){
    last_year_default_scenario <- get_last_year_scenario(df,
                                                         indicator = NULL,
                                                         scenario = default_scenario,
                                                         scenario_col = scenario_col,
                                                         start_year = start_year)
  }else{
    last_year_default_scenario <- end_year
  }

  params <- get_dots_and_call_parameters(...) %>%
    set_parameters(scenario_function = scenario_function)

  if(make_default){

    params_make_default <- get_right_parameters(params, make_default_scenario) %>%
      set_parameters(end_year = last_year_default_scenario,
                     scenario = default_scenario)

    df <- exec_scenario(df,
                        make_default_scenario,
                        params_make_default)

    params <- set_parameters(params,
                             make_default = FALSE)
  }

  if(start_scenario_last_default){
    params <- set_parameters(params,
                             start_year = last_year_default_scenario)
  }

  those_inds <- sub_set_inds[sub_set_inds %in% unique(df[["ind"]])]

  those_inds <- unique(stringr::str_remove_all(those_inds, "_num$|_denom$|_rural$|_urban$"))

  furrr::future_map_dfr(
    those_inds,
    function(x) {
      rlang::exec("add_scenario_indicator",
                  df = df,
                  indicator = x,
                  !!!params
      )
    }
  ) %>%
    dplyr::distinct()
}

#' @rdname add_scenario
#'
add_scenario_indicator <- function(df,
                                   scenario_function = c(
                                     "aroc",
                                     "halt_rise",
                                     "percent_baseline",
                                     "linear_change",
                                     "linear_change_col",
                                     "quantile",
                                     "best_in_region",
                                     "fixed_target",
                                     "fixed_target_col",
                                     "bau",
                                     "accelerate",
                                     "accelerate_target",
                                     "sdg",
                                     "benchmarking",
                                     "covid_rapid_return",
                                     "covid_sustained_disruption",
                                     "covid_delayed_return",
                                     "covid_never_return",
                                     "return_previous_trajectory",
                                     "top_n_iso3"),
                                   indicator,
                                   start_year = 2018,
                                   ind_ids = billion_ind_codes("all"),
                                   scenario_col = "scenario",
                                   default_scenario = "default",
                                   bau_scenario = "historical",
                                   make_default = FALSE,
                                   expend_bau = FALSE,
                                   start_scenario_last_default = TRUE,
                                   ...) {

  params <-   get_dots_and_call_parameters(...) %>%
    set_parameters(
      "small_is_best" = get_ind_metadata(indicator, "small_is_best")
    )

  this_ind <- ind_ids[indicator]

  this_ind_with_sub <- ind_ids[stringr::str_detect(ind_ids, paste0(c(
    glue::glue("^{this_ind}$"),
    glue::glue("^{this_ind}_num$"),
    glue::glue("^{this_ind}_denom$"),
    glue::glue("^{this_ind}_urban$"),
    glue::glue("^{this_ind}_rural$")
  ),
  collapse = "|"
  ))]

  this_ind_df <- df %>%
    dplyr::filter(.data[["ind"]] %in% this_ind_with_sub)

  if (!scenario_col %in% names(df)) {
    billionaiRe_add_columns(df, scenario_col, NA)
  }

  scenario_function <- rlang::arg_match(scenario_function)

  if(scenario_function %in% c("accelerate", "sdg", "accelerate_target", "benchmarking")){

    if (scenario_function == "sdg" & this_ind %in% ind_ids[billion_ind_codes("hep", include_subindicators = FALSE)]) {

      scenario_fn <- get(as.character(paste0("scenario_bau")), mode = "function")
      params <- get_right_parameters(params, scenario_fn)

      if(stringr::str_detect(this_ind, "_campaign")){

        this_ind_df <- this_ind_df %>%
          dplyr::filter(.data[[scenario_col]] %in% c(default_scenario, bau_scenario)) %>%
          exec_scenario(transform_hep_data,
                        params)

        params <- set_parameters(params, value_col = "transform_value")
      }
    }else if(scenario_function == "benchmarking" & this_ind != "espar" & get_ind_billion(this_ind) %in% c("hep", "hpop")){
      scenario_fn <- get(glue::glue("{scenario_function}_anc4"), mode = "function")

      params <- set_parameters(params, no_data_no_scenario = TRUE)
    }else{
      scenario_fn <- get(glue::glue("{scenario_function}_{this_ind}"), mode = "function")
    }
  }else{
    scenario_fn <- get(glue::glue("scenario_{scenario_function}"), mode = "function")
    params <- get_right_parameters(params, scenario_fn)
  }

  if(!"scenario_name" %in% names(params)){
    scenario_name <- switch(scenario_function,
                            "accelerate" = "acceleration",
                            "accelerate_target" = "acceleration_target",
                            scenario_function
    )
  }else{
    scenario_name <- params[["scenario_name"]]
  }

  params <- set_parameters(params,
                           scenario_name = scenario_name)
  if(make_default){

    params_make_default <- get_right_parameters(params, make_default_scenario) %>%
      set_parameters(end_year = get_last_year_scenario(this_ind_df,
                                                       indicator,
                                                       scenario = default_scenario,
                                                       scenario_col = scenario_col,
                                                       start_year = start_year),
                     billion = get_ind_billion(indicator)
      )

    this_ind_df <- exec_scenario(this_ind_df,
                                 make_default_scenario,
                                 params_make_default)
  }

  if(expend_bau){

    params_expend_bau <- get_right_parameters(params, make_default_scenario) %>%
      set_parameters(end_year = get_last_year_scenario(this_ind_df,
                                                       indicator,
                                                       scenario = bau_scenario,
                                                       scenario_col = scenario_col,
                                                       start_year = start_year),
                     start_year = get_last_year_scenario(this_ind_df,
                                                         indicator,
                                                         scenario = default_scenario,
                                                         scenario_col = scenario_col,
                                                         start_year = start_year),
                     scenario = params[["bau_scenario"]],
                     billion = get_ind_billion(indicator)
      )

    this_ind_df <- exec_scenario(this_ind_df,
                                 make_default_scenario,
                                 params_expend_bau)
  }

  if(start_scenario_last_default){
    params <- set_parameters(params,
                             start_year = get_last_year_scenario(this_ind_df,
                                                                 indicator,
                                                                 scenario = default_scenario,
                                                                 scenario_col = scenario_col,
                                                                 start_year = start_year),
                             start_year_trim = get_last_year_scenario(this_ind_df,
                                                                      indicator,
                                                                      scenario = default_scenario,
                                                                      scenario_col = scenario_col,
                                                                      start_year = start_year))
  }

  df_scenario <- exec_scenario(this_ind_df,
                               scenario_fn,
                               params)

  df_scenario <- fill_cols_scenario(df_scenario, scenario_col = scenario_col)

  unique_final_scenario_names <- unique(df_scenario[[scenario_col]])

  base_scenarios <- c(
    "routine",
    "covid_shock",
    "reference_infilling"
  )

  final_scenario_names <- unique_final_scenario_names[!unique_final_scenario_names %in% c(base_scenarios, default_scenario, bau_scenario)]

  df_scenario <- df_scenario %>%
    dplyr::filter(dplyr::case_when(
      .data[[scenario_col]] %in% final_scenario_names & .data[["year"]] < params[["start_year"]] ~ FALSE,
      TRUE ~ TRUE
    ))

  if("recycled" %in% names(df_scenario)){
    df_scenario <- df_scenario %>%
      dplyr::mutate(recycled = dplyr::case_when(
        .data[[scenario_col]] %in% final_scenario_names ~ FALSE,
        TRUE ~ .data[["recycled"]]
      ))
  }

  if(expend_bau){
    df_scenario <- df_scenario %>%
      dplyr::filter(dplyr::if_else(.data[[scenario_col]] == params[["bau_scenario"]] & .data[["recycled"]], FALSE, TRUE))
  }
  return(df_scenario)
}


