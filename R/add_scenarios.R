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
#' - `linear_percent_change`: calls \code{\link{scenario_linear_percent_change}}
#' - `linear_percent_change_col`: calls \code{\link{scenario_linear_percent_change_col}}
#' - `quantile`: calls \code{\link{scenario_quantile}}
#' - `best_in_region`: calls \code{\link{scenario_best_in_region}}
#' - `fixed_target`: calls \code{\link{scenario_fixed_target}}
#' - `fixed_target_col`: calls \code{\link{scenario_fixed_target_col}}
#' - `accelerate`: calls indicator accelerate function.
#' @param ... additional arguments passed to `add_scenario_indicator`
#' @inheritParams transform_hpop_data
#'
#' @return data frame with additional rows with scenario values.
#' @export
add_scenario <- function(df,
                         scenario_function = c(
                           "aroc",
                           "halt_rise",
                           "percent_baseline",
                           "linear_percent_change",
                           "linear_percent_change_col",
                           "quantile",
                           "best_in_region",
                           "fixed_target",
                           "fixed_target_col",
                           "accelerate"
                         ),
                         ind_ids = billion_ind_codes("all"),
                         ind = "ind",
                         ...) {
  assert_columns(df, ind)
  scenario_function <- rlang::arg_match(scenario_function)

  those_inds <- ind_ids[unique(df[[ind]])]

  purrr::map_dfr(
    those_inds,
    function(x) {
      rlang::exec("add_scenario_indicator",
        df = df,
        scenario_function = scenario_function,
        indicator = x,
        ind_ids = ind_ids,
        ...
      )
    }
  )
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
#'
add_scenario_indicator <- function(df,
                                   scenario_function,
                                   indicator,
                                   ind_ids = billion_ind_codes("all"),
                                   ...) {
  this_ind <- ind_ids[indicator]

  if (scenario_function == "accelerate") {
    indicator_function <- get(as.character(paste0("accelerate_", this_ind)), mode = "function")
  } else {
    indicator_function <- get(as.character(paste0("add_scenario_", this_ind)), mode = "function")
  }

  df %>%
    indicator_function(
      scenario_function = scenario_function,
      ind_ids = ind_ids,
      ...
    )
}


#' Add scenario to the data frame
#'
#' `add_scenario_dispatch` adds the right scenario_function
#'
#' @inherit add_scenario
#' @inheritParams transform_hpop_data
#' @param ... additional parameters passed to `scenario_function`
#'
add_scenario_dispatch <- function(df,
                                  scenario_function = c(
                                    "aroc",
                                    "halt_rise",
                                    "percent_baseline",
                                    "linear_percent_change",
                                    "linear_percent_change_col",
                                    "quantile",
                                    "best_in_region",
                                    "fixed_target",
                                    "fixed_target_col"
                                  ),
                                  ...) {
  scenario_function <- rlang::arg_match(scenario_function)

  scenario_function <- get(as.character(paste0("scenario_", scenario_function)), mode = "function")

  scenario_function(df = df, ...)
}
