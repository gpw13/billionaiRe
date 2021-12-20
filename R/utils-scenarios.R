#' Trim values
#'
#' @inherit scenario_percent_baseline
#' @inheritParams scenario_fixed_target
#' @param col column to trim values from. Will be removed before returning the
#' data frame.
#' @param value Column name of column with indicator values. This column will be
#' used to return the results.
#' @param trim logical to indicate if the data should be trimmed between
#' `upper_limit` and `lower_limit`.
#' @param keep_better_values logical to indicate if "better" values should be
#' kept from `value` if they are present. Follows the direction set in
#' `small_is_best`.  For instance, if small_is_best is TRUE, then `value` lower
#' than `col` will be kept.
#' @param upper_limit upper limit at which the indicator should be caped.
#' @param lower_limit lower_limit limit at which the indicator should be caped.
#' @param trim_years logical to indicate if years before `start_year` and after
#' `end_year` should be removed
#'
#' @return trimed data frame, removing the `col` column, and putting the trimmed
#' values in `value`
#'
trim_values <- function(df,
                        col,
                        value = "value",
                        year = "year",
                        trim = TRUE,
                        small_is_best = FALSE,
                        keep_better_values = FALSE,
                        upper_limit = 100,
                        lower_limit = 0,
                        trim_years = TRUE,
                        start_year = 2018,
                        end_year = 2025) {
  if (trim) {
    df %>%
      dplyr::mutate(
        better_value = dplyr::case_when(
          keep_better_values & small_is_best ~ pmin(.data[[col]], .data[[value]], na.rm = TRUE),
          keep_better_values & !small_is_best ~ pmax(.data[[col]], .data[[value]], na.rm = TRUE),
          !keep_better_values ~ .data[[col]]
        ),
        !!sym(value) := dplyr::case_when(
          .data[["better_value"]] < lower_limit ~ as.numeric(lower_limit),
          .data[["better_value"]] > upper_limit ~ as.numeric(upper_limit),
          TRUE ~ as.numeric(.data[["better_value"]])
        )
      ) %>%
      dplyr::select(-c("better_value", .data[[col]])) %>%
      trim_years(trim_years, year, start_year, end_year)
  } else {
    return(df)
  }
}

#' Trim values
#'
#' @inherit scenario_percent_baseline
#' @inheritParams scenario_fixed_target
#' @param trim_years logical to indicate if years before `start_year` and after
#' `end_year` should be removed
#'
trim_years <- function(df, trim_years, year, start_year, end_year) {
  if (trim_years) {
    df %>%
      dplyr::filter(.data[[year]] >= start_year, .data[[year]] <= end_year)
  } else {
    return(df)
  }
}

guess_limit <- function(percent_change,
                        limit,
                        limit_type = c("upper_limit", "lower_limit")) {
  if (limit == "guess" & limit_type == "upper_limit") {
    as.numeric(ifelse(percent_change >= 0, 100, Inf))
  } else if (limit == "guess" & limit_type == "lower_limit") {
    as.numeric(ifelse(percent_change >= 0, -Inf, 0))
  } else {
    as.numeric(limit)
  }
}

#' Get goal/end value
#'
#' @param value vector of values
#' @param year vector of years. Must be the same length as `value`
#' @param start_year Year at which the
#'
#' @noRd
get_goal <- function(value, year, start_year, perc_change) {
  start_year <- max(min(year), start_year)

  value[year == start_year] * (100 + perc_change) / 100
}

#' Get baseline value
#'
#' @param value vector of values
#' @param year vector of years. Must be the same length as `value`
#' @param baseline_year Year integer at which to get the baseline_value
#'
#' @noRd
get_baseline_value <- function(value, year, baseline_year) {
  value[year == baseline_year]
}

#' Calcualte Average annual rate of change
#'
#' Calculates annual average rate of change (AARC or AROC) with the compounded
#' formula:
#' ((`end_value` / `baseline_value`))^(1/(`end_year` - `baseline_year`)) - 1
#'
#' @param baseline_year year where the AARC starts
#' @param baseline_value value at start_year
#' @param end_year year where the AARC ends
#' @param end_value value at end_year
#'
#' @return numeric with AARC
calculate_aarc <- function(baseline_year,
                           baseline_value,
                           end_year,
                           end_value) {
  ((end_value / baseline_value))^(1 / (end_year - baseline_year)) - 1
}


#' Get the latest AARC for data frame
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the year immediately preceding for every combination of
#' `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
get_latest_aarc <- function(df,
                            baseline_year,
                            value = "value",
                            year = "year",
                            iso3 = "iso3",
                            ind = "ind") {
  df %>%
    dplyr::filter(year %in% c(baseline_year - 1, baseline_year)) %>%
    dplyr::group_by(iso3, ind) %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = "value",
    ) %>%
    dplyr::mutate(
      aroc = calculate_aarc(
        baseline_year = baseline_year - 1,
        baseline_value = .data[[glue::glue("{baseline_year - 1}")]],
        end_year = baseline_year,
        end_value = .data[[glue::glue("{baseline_year}")]]
      )
    ) %>%
    dplyr::select(iso3, ind, "aroc")
}

#' Get AARC for data frame based on a target
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the target year to reach the target value for every
#' combination of `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
get_target_aarc <- function(df,
                            target_value,
                            baseline_year,
                            target_year,
                            value = "value",
                            year = "year",
                            iso3 = "iso3",
                            ind = "ind") {
  df %>%
    dplyr::group_by(iso3, ind) %>%
    dplyr::filter(.data[[year]] == baseline_year) %>%
    dplyr::mutate(
      aroc = calculate_aarc(
        baseline_year = .data[[year]],
        baseline_value = .data[[value]],
        end_year = target_year,
        end_value = target_value
      )
    ) %>%
    dplyr::select(iso3, ind, "aroc")
}

#' Get AARC for data frame based on a percent change to baseline
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the target year given a percent change between the two
#' years for every combination of `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
get_percent_change_aarc <- function(df,
                                    percent_change,
                                    baseline_year,
                                    target_year,
                                    value = "value",
                                    year = "year",
                                    iso3 = "iso3",
                                    ind = "ind") {
  df %>%
    dplyr::group_by(iso3, ind) %>%
    dplyr::filter(.data[[year]] == baseline_year) %>%
    dplyr::mutate(
      target = .data[[value]] * (100 + percent_change) / 100,
      aroc = calculate_aarc(
        baseline_year = baseline_year,
        baseline_value = .data[[value]],
        end_year = target_year,
        end_value = .data[["target"]]
      )
    ) %>%
    dplyr::select(iso3, ind, "aroc")
}

#' Get Average Annual Rate of Reduction
#'
#' Gets the [Average Annual Rate of Reduction (AARC)](https://data.unicef.org/resources/technical-note-calculate-average-annual-rate-reduction-aarr-underweight-prevalence/)
#' by fitting the log of value by year with a linear regression.
#'
#' @param year vector of years
#' @param value vector of values on which the AARR will be calculated. Must be
#' the same length as `year`
#'
#' @return a numeric being the beta value representing the AARR
get_aarr <- function(year, value) {
  df <- tibble::tibble(
    x = year,
    y = value
  )
  fit <- stats::lm(log(y) ~ x, data = df)
  coef <- fit[["coefficients"]][["x"]]
  100 * (1 - exp(coef))
}


get_quantile <- function(value, n) {
  quantiles_limits <- stats::quantile(value, probs = seq(0, 1, 1 / n))

  findInterval(value, quantiles_limits, rightmost.closed = TRUE)
}
