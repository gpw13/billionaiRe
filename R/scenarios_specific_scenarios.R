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
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_best_of <- function(df,
                             scenario_names,
                             value = "value",
                             ind = "ind",
                             iso3 = "iso3",
                             year = "year",
                             start_year = 2018,
                             end_year = 2025,
                             target_year = 2025,
                             scenario_name = glue::glue("best_of_{paste0(scenario_names, collapse = '_')}"),
                             scenario = "scenario",
                             trim = TRUE,
                             small_is_best = FALSE,
                             keep_better_values = TRUE,
                             upper_limit = 100,
                             lower_limit = 0,
                             trim_years = TRUE,
                             ind_ids = billion_ind_codes("all")) {
  assert_columns(df, year, iso3, ind, scenario, value)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)
  assert_ind_start_end_year(df, iso3, year, value, target_year, target_year, ind, scenario, ind_ids = ind_ids[unique(df[[ind]])])

  best <- df %>%
    dplyr::filter(.data[[year]] == target_year) %>%
    dplyr::mutate(scenario_value = .data[[value]])

  if (small_is_best) {
    best <- best %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::filter(.data[["scenario_value"]] == min(.data[["scenario_value"]]))
  } else {
    best <- best %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::filter(.data[["scenario_value"]] == max(.data[["scenario_value"]]))
  }

  if (length(unique(best[[scenario]])) > 1) {
    those_scenarios <- unique(best[[scenario]])

    best <- df %>%
      dplyr::mutate(scenario_value = .data[[value]]) %>%
      dplyr::select(c({{ iso3 }}, {{ year }}, {{ ind }}, {{ scenario }}, "scenario_value")) %>%
      dplyr::filter(.data[[scenario]] %in% those_scenarios) %>%
      dplyr::group_by(iso3, ind, scenario) %>%
      dplyr::summarise(sum_values = sum(.data[["scenario_value"]], na.rm = T))

    if (small_is_best) {
      best <- best %>%
        dplyr::ungroup() %>%
        dplyr::group_by(iso3, ind) %>%
        dplyr::filter(.data[["sum_values"]] == min(.data[["sum_values"]], na.rm = T))
    } else {
      best <- best %>%
        dplyr::ungroup() %>%
        dplyr::group_by(iso3, ind) %>%
        dplyr::filter(.data[["sum_values"]] == max(.data[["sum_values"]], na.rm = T))
    }

    best <- best %>%
      dplyr::group_by(iso3, ind) %>%
      dplyr::group_modify(
        ~ {
          if (length(unique(.x[[scenario]])) > 1) {
            one_scenario <- unique(.x[[scenario]])[[1]]

            .x %>%
              dplyr::filter(.data[[scenario]] == one_scenario)
          } else {
            .x
          }
        }
      )
  }

  best_df <- df %>%
    dplyr::mutate(scenario_value = .data[[value]]) %>%
    dplyr::semi_join(best, by = c(iso3, ind, scenario)) %>%
    trim_values(
      col = "scenario_value",
      value = value,
      year = year,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year = start_year,
      end_year = end_year
    ) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name)

  df %>%
    dplyr::bind_rows(best_df)
}

get_best_equal_scenarios <- function(df,
                                     scenario = "scenario",
                                     iso3 = "iso3",
                                     ind = "ind",
                                     value = "scenario_value") {
  best <- df %>%
    dplyr::group_by(iso3, ind) %>%
    tidyr::pivot_wider(values_from = value, names_from = scenario)
}

#' Scenario establish a business as usual scenario
#'
#' `scenario_bau` filters for values between start_year and end_year for `default_scenario` and
#' returns values in value. If values are missing for years between `start_year` and `end_year`,
#' the last available value will be imputed.
#'
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data

scenario_bau <- function(df,
                         value = "value",
                         ind = "ind",
                         iso3 = "iso3",
                         year = "year",
                         start_year = 2018,
                         end_year = 2025,
                         scenario_name = glue::glue("business_as_usual"),
                         scenario = "scenario",
                         trim = TRUE,
                         small_is_best = FALSE,
                         keep_better_values = TRUE,
                         upper_limit = 100,
                         lower_limit = 0,
                         trim_years = TRUE,
                         ind_ids = billion_ind_codes("all"),
                         default_scenario = "default") {
  assert_columns(df, year, iso3, ind, scenario, value)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  full_years <- tidyr::expand_grid(
    "{year}" := start_year:end_year,
    "{iso3}" := unique(df[[iso3]]),
    "{ind}" := unique(df[[ind]]),
    "{scenario}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::filter(.data[[scenario]] == default_scenario,
                  !is.na(.data[[value]]))

  if(nrow(scenario_df) > 0){
    scenario_df <- scenario_df %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, ind)))) %>%
      dplyr::mutate(
        last_year = max(.data[[year]][!is.na(.data[[value]])], na.rm = TRUE),
        last_value = .data[[value]][.data[[year]] == last_year],
        "{value}" := dplyr::case_when(
          is.na(.data[[value]]) & .data[[year]] > last_year ~ last_value,
          TRUE ~ .data[[value]]
        )
      ) %>%
      dplyr::select(-c("last_value", "last_year"))
  }

  bau <- scenario_df %>%
    dplyr::full_join(full_years, by = c(year, iso3, ind, scenario)) %>%
    dplyr::filter(
      .data[[year]] %in% start_year:end_year,
      .data[[scenario]] == default_scenario
    ) %>%
    dplyr::mutate(scenario_value = .data[[value]]) %>%
    dplyr::mutate(!!sym(scenario) := scenario_name) %>%
    trim_values(
      col = "scenario_value",
      value = value,
      year = year,
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
