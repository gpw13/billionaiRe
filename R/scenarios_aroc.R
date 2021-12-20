#' Scenario to use the average annual rate of change
#'
#' `scenario_aroc` uses the annual rate of change (AROC) in different cases to
#' calculate the values of the scenario:
#'
#' * `target`: aims at a specific `target_value` by `target_year`
#' * `latest`: takes the AROC between `baseline_year` and the previous year
#' * `percent_change`: applies `percent_change`  for the AROC from
#' `baseline_year`.
#'
#' This function is different than `scenario_percent_baseline` or
#' `scenario_linear_change` as it uses percentage difference and not
#' percentage points difference.
#'
#' AROC use a general [compounded rate of change](https://en.wikipedia.org/wiki/Compound_annual_growth_rate)
#' formula:
#' AROC(Value(year_n), Value(year_0)) = (Value(year_n) / Value(year_0)) ^ (1 / (year_n -  year_0)) - 1
#'
#' `direction_limit_aroc` in combination with `limit_aroc` allows to limit the
#' AROC to a value given a direction. This can be helpful to avoid extreme
#' values if AROC is unknown before being passed to the function.If
#' `direction_limit_aroc` is `positive`, the AROC will be the minimum between
#' the calculated AROC and `limit_aroc`. If `negative`, it will be the maximum
#' between AROC and
#'
#' @param target_value value to be reached. Used when `aroc_type` is `target`.
#' Should be expressed as a percentage point and not a fraction of 100 (e.g. 6%
#' increase = 6, and not 0.06).
#' @param aroc_type String identifying type of AROC. Can be `target`,`latest` or
#' `percent_change`. See details. Defaults to `target`.
#' @param percent_change Numeric with the percent_change. Should be
#' expressed a as percentage point and not a fraction of 100 (e.g. 6% increase = 6,
#' and not 0.06).
#' @param limit_aroc_direction string is identify if positive or negative AROC
#' should be limited by `limit_aroc_value`. If NULL (default), no limitation will be
#' applied
#' @param limit_aroc_value numeric with values at which the AROC should be limited to.
#' If `limit_aroc_direction` is `positive`, will take the maximum between
#' `limit_aroc_value` and AROC. If `negative`, it will take the minimum between 0 and
#' AROC. Defaults to 0. Ignored if `limit_aroc_direction` is NULL (default).
#' @inherit scenario_percent_baseline
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#'
scenario_aroc <- function(df,
                          value = "value",
                          ind = "ind",
                          iso3 = "iso3",
                          year = "year",
                          start_year = 2018,
                          end_year = 2025,
                          baseline_year = start_year,
                          target_year = end_year,
                          target_value = NULL,
                          percent_change = NULL,
                          aroc_type = c("target", "latest", "percent_change"),
                          scenario_name = glue::glue("aroc_{aroc_type}"),
                          scenario = "scenario",
                          limit_aroc_direction = NULL,
                          limit_aroc_value = 0,
                          trim = TRUE,
                          small_is_best = FALSE,
                          keep_better_values = TRUE,
                          upper_limit = 100,
                          lower_limit = 0,
                          trim_years = TRUE,
                          ind_ids = billion_ind_codes("all"),
                          default_scenario = "default") {
  assert_columns(df, year, iso3, ind, value, scenario)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  aroc_type <- rlang::arg_match(aroc_type)
  # limit_aroc_direction <- rlang::arg_match(limit_aroc_direction)

  full_years_df <- tidyr::expand_grid(
    "{year}" := start_year:end_year,
    "{iso3}" := unique(df[[iso3]]),
    "{ind}" := unique(df[[ind]]),
    "{scenario}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c(year, iso3, ind, scenario)) %>%
    dplyr::filter(.data[[scenario]] == default_scenario)

  if (aroc_type == "latest") {
    assert_ind_start_end_year(scenario_df, iso3, year, value, baseline_year - 1, baseline_year, ind, ind_ids[unique(scenario_df[[ind]])])

    aroc <- get_latest_aarc(scenario_df,
      baseline_year = baseline_year,
      value = value,
      year = year,
      iso3 = iso3,
      ind = ind
    )
  } else if (aroc_type == "target") {
    if (is.null(target_value)) {
      stop("target_value must be provided for targeted AROC to be calculated. It was NULL.")
    }
    assert_ind_start_end_year(scenario_df, iso3, year, value, baseline_year, target_year, ind, ind_ids[unique(scenario_df[[ind]])])
    assert_numeric(target_value)
    aroc <- get_target_aarc(scenario_df,
      target_value,
      baseline_year = baseline_year,
      target_year = target_year,
      value = value,
      year = year,
      iso3 = iso3,
      ind = ind
    )
  } else if (aroc_type == "percent_change") {
    if (is.null(percent_change)) {
      stop("percent_change must be provided for percent_change AROC to be calculated. It was NULL.")
    }
    assert_numeric(target_value)
    assert_ind_start_end_year(scenario_df, iso3, year, value, baseline_year, target_year, ind, ind_ids[unique(scenario_df[[ind]])])
    aroc <- get_percent_change_aarc(scenario_df,
      percent_change,
      baseline_year,
      target_year,
      value = value,
      year = year,
      iso3 = iso3,
      ind = ind
    )
  }

  if (!is.null(limit_aroc_direction)) {
    if (limit_aroc_direction == "positive") {
      aroc <- aroc %>%
        dplyr::mutate(aroc = pmin(limit_aroc_value, aroc))
    } else {
      aroc <- aroc %>%
        dplyr::mutate(aroc = pmax(limit_aroc_value, aroc))
    }
  }

  aroc_df <- scenario_df %>%
    dplyr::group_by(iso3, ind) %>%
    dplyr::mutate(baseline_value = get_baseline_value(.data[[value]], .data[[year]], baseline_year)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(aroc, by = c(iso3, ind)) %>%
    dplyr::mutate(
      scenario_value = dplyr::case_when(
        .data[[year]] == baseline_year ~ as.numeric(.data[[value]]),
        .data[[year]] > baseline_year ~ .data[["baseline_value"]] * ((1 + .data[["aroc"]])^(.data[[year]] - baseline_year)),
        TRUE ~ NA_real_
      ),
      !!sym(scenario) := scenario_name
    ) %>%
    dplyr::select(-c("baseline_value", "aroc")) %>%
    trim_values(
      col = "scenario_value", value = value, year = year, trim = trim, small_is_best = small_is_best,
      keep_better_values = keep_better_values, upper_limit = upper_limit,
      lower_limit = lower_limit, trim_years = trim_years, start_year = start_year, end_year = end_year
    )

  df %>%
    dplyr::bind_rows(aroc_df)
}
