add_scenario <- function(df,
                         scenario_function,
                         ind_ids = billion_ind_codes("all"),
                         ind = "ind",
                         ...) {
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

add_scenario_indicator <- function(df,
                                   scenario_function,
                                   indicator,
                                   ind_ids = billion_ind_codes("all"),
                                   ...) {
  this_ind <- ind_ids[indicator]

  indicator_function <- get(as.character(paste0("add_scenario_", this_ind)), mode = "function")

  df %>%
    indicator_function(
      scenario_function = scenario_function,
      ind_ids = ind_ids,
      ...
    )
}

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
