#' Scenario to change by a fixed percentage from a baseline value by target year
#'
#' This scenario allows to change a value by a fixed percentage to a provided year
#' from a provided baseline year. It provides values for scenarios stated as
#' "Reduce INDICATOR by XX% by YEAR"
#'
#' The `percent_change` parameter is understood as a percentage change,
#' and not a percentage point change, as this is usually what intended by those
#' formulations. If it is indeed the percentage change that is required, please
#' use `THIS_FUNCTION`. For instance, to calculate the scenario "reduce the 2018
#' value (90%) by 30% by 2025", will results to a 2025 value of 63% and not 60%.
#'
#' The returned scenario is a portion of the straight line drawn from the
#' `baseline_year` value to the `target_year`. Only values for years between
#' `start_year` and `end_year` will be returned.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#'
#' @param percent_change Numeric with the percentage change in **points** that is
#' to be achieved from `value` in `baseline_year` by `target_year`. Should be
#' expressed a percentage point and not a fraction of 100 (e.g. 6% increase = 6,
#' and not 0.06).
#' For an increase, use a positive numeric, and a negative one for a decrease.
#' @param start_year Start year for scenario, defaults to 2018.
#' @param end_year End year for scenario, defaults to 2025
#' @param baseline_year Year from which the scenario is measured.
#' Defaults to `start_year`
#' @param target_year Year by which the scenario should eventually be
#' achieved. Defaults to `end_year`
#' @param scenario_name Name of the scenario. Defaults to scenario_{percent_change}_{baseline_year}
#' @param limit_change Boolean to identify if change should be caped at a certain value.
#' @param upper_limit lower_limit limit at which the indicator should be caped.
#' Can take any of "guess", or any numeric. `guess` (default) will take 100 as
#' the limit if `percent_change` is positive, and 0 if negative.
#' @param lower_limit lower_limit limit at which the indicator should be caped.
#' Can take any of "guess", or 0 to 100. `guess` (default) will take 0 as the
#' limit if `percent_change` is positive, and 100 if negative.
#'
#' @return Dataframe with additional scenario rows
scenario_percent_baseline <- function(df,
                                      percent_change,
                                      value = "value",
                                      ind = "ind",
                                      iso3 = "iso3",
                                      year = "year",
                                      start_year = 2018,
                                      end_year = 2025,
                                      baseline_year = start_year,
                                      target_year = end_year,
                                      scenario = "scenario",
                                      scenario_name = glue::glue("{percent_change}_{baseline_year}"),
                                      limit_change = TRUE,
                                      upper_limit = "guess",
                                      lower_limit = "guess") {
  if (limit_change & upper_limit == "guess") {
    upper_limit <- as.numeric(ifelse(percent_change >= 0, 100, Inf))
  } else {
    upper_limit <- as.numeric(upper_limit)
  }

  if (limit_change & lower_limit == "guess") {
    lower_limit <- ifelse(percent_change >= 0, -Inf, 0)
  } else {
    lower_limit <- as.numeric(lower_limit)
  }

  df %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    dplyr::mutate(
      "_goal_value" := get_goal(.data[[value]], .data[[year]], !!baseline_year, !!percent_change),
      "_baseline_value" := get_baseline_value(.data[[value]], .data[[year]], !!baseline_year)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      !!sym(value) := calculate_percent_change_baseline(
        .data[["_baseline_value"]],
        .data[["_goal_value"]],
        .data[[year]],
        !!baseline_year,
        !!target_year
      ),
      !!sym(value) := dplyr::case_when(
        limit_change & .data[[value]] >= upper_limit ~ upper_limit,
        limit_change & .data[[value]] <= lower_limit ~ lower_limit,
        TRUE ~ .data[[value]]
      ),
      !!sym(scenario) := scenario_name
    ) %>%
    dplyr::select(-c("_goal_value", "_baseline_value")) %>%
    dplyr::filter(.data[[year]] >= start_year & .data[[year]] <= end_year)
}


#' Calculate percent change
#'
#' @inheritParams scenario_percent_baseline
#' @param baseline_value vector with the baseline value to be used
#' @param goal_value vector with the goal value to be used
calculate_percent_change_baseline <- function(baseline_value, goal_value, year, baseline_year, target_year) {
  dplyr::if_else(year >= baseline_year & year <= target_year,
    baseline_value + (goal_value - baseline_value) * (year - baseline_year) / (target_year - baseline_year),
    NA_real_
  )
}

#' Calculate halt rise scenarios
#'
#' Special case of `scenario_percent_baseline` where \code{`percent_change` = 0}.
#' Provided as a convenience function.
#'
#' @inherit scenario_percent_baseline
scenario_halt_rise <- function(df,
                               value = "value",
                               ind = "ind",
                               iso3 = "iso3",
                               year = "year",
                               start_year = 2018,
                               end_year = 2025,
                               baseline_year = start_year,
                               target_year = end_year,
                               scenario = "scenario",
                               scenario_name = glue::glue("halt_rise"),
                               limit_change = TRUE,
                               upper_limit = "guess",
                               lower_limit = "guess") {
  percent_change <- 0

  scenario_percent_baseline(
    df,
    percent_change = percent_change,
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario = scenario,
    scenario_name = scenario_name,
    upper_limit = upper_limit,
    lower_limit = lower_limit
  )
}

#' Scenario to add a linear percentage point change
#'
#' Scenario to add a `linear_value` percentage point change to `baseline_value`
#' from a `baseline_year`. It provides values for scenarios stated as "Increase
#' INDICATOR by XX% points".
#'
#' The calculation is done by taking the `baseline_year` `value` and adding the
#' `linear_value` times the number of years between the baseline year and the
#' current year. For instance, if `baseline_year` is 2018, `linear_value` is 2,
#' and `baseline_year` `value` is 10, then 2019 `value` will be 12, 2020 14,
#' etc.
#'
#' `upper_limit` and `lower_limit` allow to trim values when they are exceeding
#' the bounds after calculations. If values were already exceeding the bounds
#' before calculations, they are kept.
#'
#' @param linear_value vector indicating the increase to apply.
#' @param upper_limit numeric indicating the upper bound of the data after
#' calculation. If `value` is already higher before calculation it will be kept
#' @param lower_limit numeric indicating the lower bound of the data after
#' calculation. If `value` is already lower before calculation it will be kept
#' @inherit scenario_percent_baseline
#'
scenario_linear_percent_change <- function(df,
                                           linear_value,
                                           value = "value",
                                           ind = "ind",
                                           iso3 = "iso3",
                                           year = "year",
                                           start_year = 2018,
                                           end_year = 2025,
                                           baseline_year = start_year,
                                           target_year = end_year,
                                           scenario_name = glue::glue("linear_percent_change"),
                                           upper_limit = 100,
                                           lower_limit = 0) {
  target_year <- ifelse(target_year > max(df[[year]]), max(df[[year]]), target_year)

  target_df <- df %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(baseline_value = get_baseline_value(.data[[value]], .data[[year]], baseline_year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[year]] == target_year) %>%
    dplyr::mutate(
      scenario_target = baseline_value + ((.data[[year]] - baseline_year) * linear_value),
      scenario_target = pmin(
        pmax(pmin(lower_limit, baseline_value), scenario_target),
        pmax(upper_limit, baseline_value)
      ),
      trim_lin_col = (scenario_target - baseline_value) / (.data[[year]] - baseline_year)
    ) %>%
    dplyr::select(iso3, ind, trim_lin_col, baseline_value, scenario_target)

  df %>%
    dplyr::left_join(target_df) %>%
    dplyr::mutate(val = dplyr::if_else(
      .data[[year]] >= baseline_year,
      baseline_value + (.data[[year]] - baseline_year) * trim_lin_col,
      NA_real_
    )) %>%
    dplyr::mutate(!!sym(value) := pmin(
      pmax(pmin(lower_limit, scenario_target), val),
      pmax(upper_limit, scenario_target)
    )) %>%
    dplyr::select(-baseline_value, -trim_lin_col, -val, -scenario_target) %>%
    dplyr::filter(.data[[year]] >= start_year, .data[[year]] <= end_year)
}

#' Scenario to add a linear percentage point change stored in a column
#'
#' `scenario_linear_percent_change_col` wraps around
#' `scenario_linear_percent_change` to provide linear values from a column
#' specified in `linear_value` rather than a single value.
#'
#' @param linear_value name of column with linear values
#' @inherit scenario_fixed_target
scenario_linear_percent_change_col <- function(df,
                                               linear_value,
                                               value = "value",
                                               ind = "ind",
                                               iso3 = "iso3",
                                               year = "year",
                                               start_year = 2018,
                                               end_year = 2025,
                                               baseline_year = start_year,
                                               target_year = end_year,
                                               scenario_name = glue::glue("linear_percent_change"),
                                               upper_limit = 100,
                                               lower_limit = 0) {
  scenario_linear_percent_change(df,
    linear_value = df[[linear_value]],
    value = value,
    ind = ind,
    iso3 = iso3,
    year = year,
    start_year = start_year,
    end_year = end_year,
    baseline_year = baseline_year,
    target_year = target_year,
    scenario_name = scenario_name,
    upper_limit = upper_limit,
    lower_limit = lower_limit
  )
}
