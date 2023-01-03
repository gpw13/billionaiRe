#' Scenario to reach a fixed target
#'
#' @description
#' Those scenarios allow to reach a target value from a baseline year by target
#' year.
#'
#' `scenario_fixed_target()` provides values for scenarios stated as "Reach XX% in INDICATOR by
#' YEAR" or "Eliminate XX by YEAR".
#'
#' `scenario_fixed_target_col()` wraps around `scenario_fixed_target` to provide
#' targets from a column specified in `target_col` rather than a single value.
#'
#' `scenario_halt_rise()` is a special case of `scenario_fixed_target_col()`
#' where each country aims at a value of a specific year.
#'
#' The returned scenario is a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' If `value_col` has values that are higher or lower than the scenario values, then
#' only `value_col` will be kept, depending on `small_is_best`. For instance,
#' if the scenario value is 80 and the value 75 and small_is_best is TRUE, then
#' 75 will be kept.
#'
#' @inheritParams scenario_percent_baseline
#' @param target_value value to be achieved by scenario by `target_year`
#' @param small_is_best Logical to identify if a lower value is better than a higher
#' one (e.g. lower obesity in a positive public health outcome, so obesity rate
#' should have small_is_best = TRUE).
#' @param default_scenario name of the default scenario to be used.
#' @inheritParams trim_values
#'
#' @rdname fixed_target
#'
#' @examples
#' df <- tibble::tibble(
#'   value = 60:80,
#'   year = 2010:2030,
#'   ind = "test",
#'   iso3 = "testalia",
#'   scenario = "default",
#'   type = dplyr::if_else(year <= 2021, "reported", "projected")
#'   )
#'
#' target_value <- 99
#'
#' df_fixed_percent <- scenario_fixed_target(df,
#'                                           target_value = target_value,
#'                                           baseline_year = 2018,
#'                                           target_year = 2025,
#'                                           scenario_name = glue::glue("{target_value}_{2025}"),
#'                                           small_is_best = FALSE)
#'
#'
#' df_target_col <- df %>%
#'   dplyr::mutate(target = target_value) %>%
#'   scenario_fixed_target_col(
#'     target_col = "target",
#'     baseline_year = 2018,
#'     target_year = 2025,
#'     scenario_name = glue::glue("{target_value}_{2025}"),
#'     small_is_best = FALSE)
#'
#' df_scenario_halt_rise <- scenario_halt_rise(df,
#'                                             baseline_year = 2010,
#'                                             target_year = 2025,
#'                                             scenario_name = glue::glue("{target_value}_{2025}"))
#'

scenario_fixed_target <- function(df,
                                  target_value,
                                  value_col = "value",
                                  scenario_col = "scenario",
                                  start_year = 2018,
                                  end_year = 2025,
                                  baseline_year = start_year,
                                  target_year = end_year,
                                  scenario_name = glue::glue("{target_value}_{target_year}"),
                                  small_is_best = FALSE,
                                  trim = TRUE,
                                  keep_better_values = TRUE,
                                  upper_limit = 100,
                                  lower_limit = 0,
                                  trim_years = TRUE,
                                  start_year_trim = start_year,
                                  end_year_trim = end_year,
                                  ind_ids = billion_ind_codes("all"),
                                  default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  target_value <- as.numeric(target_value)
  target_year <- as.numeric(target_year)

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col)) %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::group_by(.data[["ind"]], .data[["iso3"]]) %>%
    dplyr::mutate(
      "baseline_year_" := get_baseline_year(.data[["year"]], .data[["type"]], baseline_year = !!baseline_year, type_filter = c("reported", "estimated", "imputed", "projected")),
      "baseline_value_" := get_baseline_value(.data[[value_col]],
                                              .data[["year"]],
                                              .data[["type"]],
                                              baseline_year = .data[["baseline_year_"]],
                                              type_filter = c("all"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "baseline_value_" := dplyr::case_when(
        is.na(.data[["baseline_value_"]]) ~ as.numeric(0),
        TRUE ~ as.numeric(.data[["baseline_value_"]])
      ),
      scenario_value = calculate_fixed_target(target_value, small_is_best, .data[["year"]], start_year, target_year, .data[["baseline_value_"]]),
      !!sym(scenario_col) := scenario_name
    ) %>%
    dplyr::select(-c("baseline_value_", "baseline_year_")) %>%
    trim_values(
      col = "scenario_value",
      value_col = value_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim
    ) %>%
    dplyr::mutate(type = dplyr::case_when(
      !is.na(.data[["type"]]) ~ .data[["type"]],
      TRUE ~ "projected"
    ))

  df %>%
    dplyr::bind_rows(scenario_df)
}

#' Calculate fixed target from a baseline year by a target year
#'
#' The returned values are a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' @param target_value vector of values to use as targets
#' @param baseline_value value at baseline_year
#' @param year (vector) vector of years
#'
#' @noRd
calculate_fixed_target <- function(target_value,
                                   small_is_best,
                                   year,
                                   baseline_year,
                                   target_year,
                                   baseline_value) {
  if (small_is_best) {
    dplyr::case_when(
      year >= baseline_year & year <= target_year & baseline_value > target_value ~
        baseline_value + (target_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
      year >= baseline_year & year <= target_year & baseline_value <= target_value ~
        as.numeric(baseline_value),
      year > target_year ~ target_value,
      TRUE ~ NA_real_
    )
  } else {
    dplyr::case_when(
      year >= baseline_year & year <= target_year & baseline_value < target_value ~
        baseline_value + (target_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
      year >= baseline_year & year <= target_year & baseline_value >= target_value ~
        as.numeric(baseline_value),
      year > target_year ~ target_value,
      TRUE ~ NA_real_
    )
  }
}

#' @param target_col name of column with targets
#'
#' @rdname fixed_target
scenario_fixed_target_col <- function(df,
                                      value_col = "value",
                                      scenario_col = "scenario",
                                      start_year = 2018,
                                      end_year = 2025,
                                      baseline_year = start_year,
                                      target_col = "target",
                                      target_year = end_year,
                                      scenario_name = glue::glue("{target_col}_{target_year}"),
                                      small_is_best = FALSE,
                                      trim = TRUE,
                                      keep_better_values = TRUE,
                                      upper_limit = 100,
                                      lower_limit = 0,
                                      trim_years = TRUE,
                                      start_year_trim = start_year,
                                      end_year_trim = end_year,
                                      ind_ids = billion_ind_codes("all"),
                                      default_scenario = "default") {
  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  target_df <- df %>%
    dplyr::select(dplyr::all_of(c("iso3", "ind", target_col))) %>%
    dplyr::distinct()

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col)) %>%
    dplyr::select(-tidyselect::all_of(target_col)) %>%
    dplyr::left_join(target_df, by = c("iso3", "ind")) %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  scenario_df <- scenario_fixed_target(
    df = scenario_df,
    target_value = scenario_df[[target_col]],
    value_col = value_col,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario_name = scenario_name,
    small_is_best = small_is_best,
    trim = trim,
    keep_better_values = keep_better_values,
    upper_limit = upper_limit,
    lower_limit = lower_limit,
    trim_years = trim_years,
    start_year_trim = start_year_trim,
    end_year_trim = end_year_trim,
    ind_ids = ind_ids,
    default_scenario = default_scenario
  ) %>%
    dplyr::filter(!is.na(.data[[value_col]])) %>%
    dplyr::distinct()

  df %>%
    dplyr::bind_rows(scenario_df)
}

#' @rdname fixed_target
scenario_halt_rise <- function(df,
                               value_col = "value",
                               start_year = 2018,
                               end_year = 2025,
                               baseline_year = start_year,
                               target_year = end_year,
                               scenario_col = "scenario",
                               scenario_name = glue::glue("halt_rise"),
                               upper_limit = 100,
                               lower_limit = 0,
                               trim = TRUE,
                               keep_better_values = FALSE,
                               small_is_best = FALSE,
                               trim_years = TRUE,
                               start_year_trim = start_year,
                               end_year_trim = end_year,
                               ind_ids = billion_ind_codes("all"),
                               default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)

  target_df <- df %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::mutate("target" := get_baseline_value(.data[[value_col]],
                                              .data[["year"]],
                                              .data[["type"]],
                                              .data[[scenario_col]],
                                              default_scenario,
                                              baseline_year,
                                              type_filter = c("all"))) %>%
    dplyr::ungroup()

  scenario_fixed_target_col(target_df,
                            value_col = value_col,
                            scenario_col = scenario_col,
                            start_year = start_year,
                            end_year = end_year,
                            baseline_year = start_year,
                            target_col = "target",
                            target_year = target_year,
                            scenario_name = scenario_name,
                            small_is_best = small_is_best,
                            trim = trim,
                            keep_better_values = keep_better_values,
                            upper_limit = upper_limit,
                            lower_limit = lower_limit,
                            trim_years = trim_years,
                            start_year_trim = start_year_trim,
                            end_year_trim = end_year_trim,
                            ind_ids = ind_ids,
                            default_scenario = default_scenario) %>%
    dplyr::select(-"target")
}

