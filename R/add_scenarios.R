#' Add scenario to data frame
#'
#' `add_scenario` wraps around `add_scenario_indicator` to add the scenario
#' specified in `scenario_function` to all the valid billionaiRe indicators
#' present in `df`.
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
#' - `sdg`: calls Sustainable Development Goals (SDG) acceleration function.
#' - `covid_rapid_return` calls \code{\link{scenario_covid_rapid_return}}
#' - `covid_delayed_return` calls \code{\link{scenario_covid_delayed_return}}
#' - `covid_sustained_disruption` calls \code{\link{scenario_covid_sustained_disruption}}
#' @param ... additional arguments passed to `add_scenario_indicator`
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_billion
#'
#' @return data frame with additional rows with scenario values.
#' @export
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
                           "sdg",
                           "covid_rapid_return",
                           "covid_delayed_return",
                           "covid_sustained_disruption"
                         ),
                         ind_ids = billion_ind_codes("all"),
                         ind = "ind",
                         start_year = 2018,
                         end_year = 2025,
                         ...) {
  assert_columns(df, ind)
  scenario_function <- rlang::arg_match(scenario_function)

  sub_set_inds <- ind_ids[!stringr::str_detect(ind_ids, paste0(c(
    "espar[0-9].{0,3}",
    "surviving_infants"
  ),
  collapse = "|"
  ))]

  those_inds <- sub_set_inds[sub_set_inds %in% unique(df[[ind]])]

  those_inds <- unique(stringr::str_remove_all(those_inds, "_num$|_denom$"))

  purrr::map_dfr(
    those_inds,
    function(x) {
      rlang::exec("add_scenario_indicator",
        df = df,
        scenario_function = scenario_function,
        indicator = x,
        ind_ids = ind_ids,
        start_year = start_year,
        end_year = end_year,
        ...
      )
    }
  ) %>%
    dplyr::distinct()
}

#' Add scenario for a specific indicator
#'
#' `add_scenario_indicator` gets the right `add_scenario_` function for the
#' indicator specified and adds the scenario.
#'
#' @param indicator name of indicator to be passed to `scenario_function`
#' @param ... additional parameters to be passed to the add_scenario_indicator
#' function (e.g. `add_scenario_adult_obese`).
#'
#' @inheritParams add_scenario
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_billion
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
                                     "sdg",
                                     "covid_rapid_return",
                                     "covid_sustained_disruption",
                                     "covid_delayed_return",
                                     "covid_never_return"
                                   ),
                                   indicator,
                                   ind_ids = billion_ind_codes("all"),
                                   scenario = "scenario",
                                   ...) {
  this_ind <- ind_ids[indicator]

  if (!scenario %in% names(df)) {
    billionaiRe_add_columns(df, scenario, NA)
  }

  scenario_function <- rlang::arg_match(scenario_function)

  params <- list(...)
  params["small_is_best"] <- get_ind_metadata(indicator, "small_is_best")

  if (scenario_function == "accelerate") {
    accelerate_fn <- get(as.character(paste0("accelerate_", this_ind)), mode = "function")

    do.call(
      accelerate_fn, c(list(df = df), params)
    ) %>%
      dplyr::distinct()
  } else if (scenario_function == "sdg") {
    if (this_ind %in% ind_ids[billion_ind_codes("hep", include_subindicators = FALSE)]) {
      sdg_fn <- get(as.character(paste0("scenario_bau")), mode = "function")
      params["scenario_name"] <- "sdg"
    } else {
      sdg_fn <- get(as.character(paste0("sdg_", this_ind)), mode = "function")
    }

    do.call(
      sdg_fn, c(list(df = df), params)
    ) %>%
      dplyr::distinct()
  } else {
    scenario_fn <- get(as.character(paste0("scenario_", scenario_function)), mode = "function")

    do.call(
      scenario_fn, c(list(df = df), params)
    ) %>%
      dplyr::distinct()
  }
}
