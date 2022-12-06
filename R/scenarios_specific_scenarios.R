#' Scenario to pick the best scenario out of a list of scenarios
#'
#' `scenario_best_of` picks the best value between multiple scenarios specified
#' in `scenario_names`. `small_is_best` allows to pick if higher value or
#' smaller values are best.
#'
#' If multiple scenario are tied for the best scenario, the scenario with the best time
#' series for each `iso3` and `ind` is picked. If they are still tied scenarios for
#' the best value, the first in alphabetical order is picked.
#'
#' @param scenario_names names of the scenario to pick from.
#' @param maximize_end_year (Boolean) if TRUE, the best scenario is
#'    picked on the best value at `end_year`. Default to FALSE.
#'
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_best_of <- function(df,
                             scenario_names,
                             value_col = "value",
                             maximize_end_year = TRUE,
                             start_year = 2018,
                             end_year = 2025,
                             target_year = end_year,
                             scenario_name = glue::glue("best_of_{paste0(scenario_names, collapse = '_')}"),
                             scenario_col = "scenario",
                             trim = TRUE,
                             small_is_best = FALSE,
                             keep_better_values = TRUE,
                             upper_limit = 100,
                             lower_limit = 0,
                             trim_years = TRUE,
                             ind_ids = billion_ind_codes("all")) {

  this_ind <- unique(df[["ind"]])
  assert_columns(df, "year", "iso3", "ind", scenario_col, value_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)
  assert_ind_start_end_year(df, value_col, target_year, target_year, scenario_col, ind_ids = this_ind)

  best <- df %>%
    dplyr::filter(.data[["year"]] == target_year) %>%
    dplyr::mutate(scenario_value = .data[[value_col]])

  if (small_is_best) {
    best <- best %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
      dplyr::filter(.data[["scenario_value"]] == min(.data[["scenario_value"]]))
  } else {
    best <- best %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
      dplyr::filter(.data[["scenario_value"]] == max(.data[["scenario_value"]]))
  }

  if (length(unique(best[[scenario_col]])) > 1) {
    those_scenarios <- unique(best[[scenario_col]])

    if(maximize_end_year){
      best <- df %>%
        dplyr::mutate(scenario_value = .data[[value_col]]) %>%
        dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario_col, "scenario_value"))) %>%
        dplyr::filter(.data[[scenario_col]] %in% those_scenarios,
                      .data[["year"]] == end_year) %>%
        dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", scenario_col)))) %>%
        dplyr::summarise(sum_values = sum(.data[["scenario_value"]], na.rm = T))

    }else{
      best <- df %>%
        dplyr::mutate(scenario_value = .data[[value_col]]) %>%
        dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario_col, "scenario_value"))) %>%
        dplyr::filter(.data[[scenario_col]] %in% those_scenarios) %>%
        dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", scenario_col)))) %>%
        dplyr::summarise(sum_values = sum(.data[["scenario_value"]], na.rm = T))
    }

    if (small_is_best) {
      best <- best %>%
        dplyr::ungroup() %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
        dplyr::filter(.data[["sum_values"]] == min(.data[["sum_values"]], na.rm = T))
    } else {
      best <- best %>%
        dplyr::ungroup() %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
        dplyr::filter(.data[["sum_values"]] == max(.data[["sum_values"]], na.rm = T))
    }

    best <- best %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
      dplyr::group_modify(
        ~ {
          if (length(unique(.x[[scenario_col]])) > 1) {
            one_scenario <- unique(.x[[scenario_col]])[[1]]

            .x %>%
              dplyr::filter(.data[[scenario_col]] == one_scenario)
          } else {
            .x
          }
        }
      )
  }

  best_df <- df %>%
    dplyr::mutate(scenario_value = .data[[value_col]]) %>%
    dplyr::semi_join(best, by = c("iso3", "ind", scenario_col)) %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    ) %>%
    dplyr::mutate(!!sym(scenario_col) := scenario_name)

  df %>%
    dplyr::bind_rows(best_df)
}

get_best_equal_scenarios <- function(df,
                                     scenario_col = "scenario",
                                     value_col = "scenario_value") {
  best <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
    tidyr::pivot_wider(values_from = value_col, names_from = scenario_col)
}

#' Scenario establish a business as usual scenario
#'
#' `scenario_bau` filters for values between start_year and end_year for `bau_scenario` and
#' returns values in value. If values are missing for years between `start_year` and `end_year`,
#' the last available value will be imputed.
#'
#' @param only_reported_estimated (logical) if TRUE only the last `reported` and `estimated`
#'    values are imputed.
#' @param avoid_worstening If TRUE, the value at `start_year` is kept if it is
#' better (see `small_is_best`).
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams accelerate_alcohol

scenario_bau <- function(df,
                         only_reported_estimated = FALSE,
                         value_col = "value",
                         start_year = 2018,
                         end_year = 2025,
                         scenario_name = glue::glue("business_as_usual"),
                         scenario_col = "scenario",
                         avoid_worstening = FALSE,
                         trim = TRUE,
                         small_is_best = FALSE,
                         keep_better_values = TRUE,
                         upper_limit = 100,
                         lower_limit = 0,
                         trim_years = TRUE,
                         ind_ids = billion_ind_codes("all"),
                         bau_scenario = "historical",
                         default_scenario = "default") {

  indicator <- unique(df[["ind"]]) %>%
    stringr::str_remove_all(paste0(c("espar[0-9]{2}_[0-9]{2}$", "espar[0-9]{2}$"), collapse = "|")) %>%
    unique()

  indicator <- indicator[indicator != ""]

  scenario_df <- df %>%
    dplyr::filter(.data[[scenario_col]] %in% c(bau_scenario, default_scenario),
                  !is.na(.data[[value_col]]),
                  .data[["ind"]] %in% indicator)

  assert_columns(scenario_df, scenario_col, value_col)

  full_years <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := indicator,
    "{scenario_col}" := c(bau_scenario, default_scenario)
  ) %>%
    dplyr::distinct()

  if(only_reported_estimated){
    scenario_df <- scenario_df %>%
      dplyr::filter(.data[["type"]] %in% c("reported", "estimated"))
  }


  if(nrow(scenario_df) > 0){
    scenario_df <- scenario_df %>%
      dplyr::full_join(full_years, by = c("year", "iso3", "ind", scenario_col)) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
      dplyr::mutate(start_value = get_last_type_baseline_scenario_value(.data[[value_col]],
                                                                        .data[["year"]],
                                                                        .data[["type"]],
                                                                        .data[[scenario_col]],
                                                                        default_scenario,
                                                                        start_year,
                                                                        type_filter = c("reported", "estimated", "imputed", "projected")))%>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario) %>%
      dplyr::mutate(
        last_year = max(.data[["year"]][!is.na(.data[[value_col]])], na.rm = TRUE),
        last_value = .data[[value_col]][.data[["year"]] == .data[["last_year"]]],
        baseline_value = get_baseline_value(.data[[value_col]], .data[["year"]], start_year),
        "{value_col}" := dplyr::case_when(
          !is.na(.data[["start_value"]]) & .data[["year"]] == start_year ~ .data[["start_value"]],
          is.na(.data[[value_col]]) & .data[["year"]] > .data[["last_year"]] ~ .data[["last_value"]],
          TRUE ~ .data[[value_col]]
        ),
        "{value_col}" := dplyr::case_when(
          avoid_worstening & small_is_best & .data[[value_col]] > baseline_value ~ baseline_value,
          avoid_worstening & !small_is_best & .data[[value_col]] < baseline_value ~ baseline_value,
          TRUE ~ .data[[value_col]]
        )
      ) %>%
      dplyr::select(-c("last_value", "last_year", "baseline_value", "start_value"))
  }else{
    scenario_df <- scenario_df %>%
      dplyr::full_join(full_years, by = c("year", "iso3", "ind", scenario_col))
  }

  bau <- scenario_df %>%
    dplyr::filter(
      .data[["year"]] %in% start_year:end_year,
      .data[[scenario_col]] == bau_scenario
    ) %>%
    dplyr::mutate(scenario_value = .data[[value_col]]) %>%
    dplyr::mutate(!!sym(scenario_col) := scenario_name) %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    )

  df %>%
    dplyr::bind_rows(bau)
}

scenario_with_values <- function(df,
                                 scenario_function,
                                 type_filter = c("reported", "estimated"),
                                 only_reported_estimated = FALSE,
                                 value_col = "value",
                                 start_year = 2018,
                                 end_year = 2025,
                                 scenario_name = scenario_function,
                                 scenario_col = "scenario",
                                 avoid_worstening = FALSE,
                                 trim = TRUE,
                                 small_is_best = FALSE,
                                 keep_better_values = TRUE,
                                 upper_limit = 100,
                                 lower_limit = 0,
                                 trim_years = TRUE,
                                 ind_ids = billion_ind_codes("all"),
                                 bau_scenario = "historical",
                                 default_scenario = "default",
                                 ...){

  indicator <- unique(df[["ind"]]) %>%
    stringr::str_remove_all(paste0(c("espar[0-9]{2}_[0-9]{2}$", "espar[0-9]{2}$"), collapse = "|")) %>%
    unique()

  params <- get_dots_and_call_parameters(...)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == indicator)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% type_filter, na.rm = TRUE) <= 1)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% type_filter, na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(df_with_data) > 0) {

    scenario_fn <- get(as.character(scenario_function), mode = "function")

    params_fn <- get_right_parameters(params, scenario_fn)

    df_with_data_fn <-  exec_scenario(df_with_data,
                                 scenario_fn,
                                 params_fn)%>%
      dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])

  } else {
    df_with_data_fn <- tibble::tibble()
  }

  if (nrow(df_without_data) > 0) {

    params_bau <- get_right_parameters(params, scenario_bau)

    df_without_data_fn <- exec_scenario(df_without_data,
                                                 scenario_bau,
                                                 params_bau) %>%
      dplyr::filter(.data[[scenario_col]] == params[["scenario_name"]])
  } else {
    df_without_data_fn <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_without_data_fn, df_with_data_fn)

}
