#' Trim values
#'
#' @inheritParams scenario_percent_baseline
#' @inheritParams scenario_fixed_target
#' @param col column to trim values from. Will be removed before returning the
#' data frame.
#' @param value_col Column name of column with indicator values. This column will be
#' used to return the results.
#' @param baseline_col Column name with baseline values. This is used to ensure
#' that trimmed values do not get worst values than baseline.
#' @param trim logical to indicate if the data should be trimmed between
#' `upper_limit` and `lower_limit`.
#' @param keep_better_values logical to indicate if "better" values should be
#' kept from `value_col` if they are present. Follows the direction set in
#' `small_is_best`.  For instance, if small_is_best is TRUE, then `value_col` lower
#' than `col` will be kept.
#' @param upper_limit upper limit at which the indicator should be caped.
#' @param lower_limit lower_limit limit at which the indicator should be caped.
#' @param trim_years logical to indicate if years before `start_year_trim` and after
#' `end_year_trim` should be removed
#' @param start_year_trim (integer) year to start trimming from.
#' @param end_year_trim (integer) year to end trimming.
#'
#' @return trimed data frame, removing the `col` column, and putting the trimmed
#' values in `value_col`
#'
#' @family scenarios
#'
#' @keywords internal
#'
trim_values <- function(df,
                        col,
                        value_col = "value",
                        baseline_col = value_col,
                        trim = TRUE,
                        small_is_best = FALSE,
                        keep_better_values = FALSE,
                        upper_limit = 100,
                        lower_limit = 0,
                        trim_years = TRUE,
                        start_year_trim = 2018,
                        end_year_trim = 2025) {

  assert_columns(df, value_col, col)

  if (trim) {
    df %>%
      dplyr::mutate(
        better_value = dplyr::case_when(
          is.na(.data[[col]]) & is.na(.data[[value_col]]) ~ NA_real_,
          keep_better_values & small_is_best ~ as.numeric(pmin(.data[[col]], .data[[value_col]], na.rm = TRUE)),
          keep_better_values & !small_is_best ~ as.numeric(pmax(.data[[col]], .data[[value_col]], na.rm = TRUE)),
          !keep_better_values ~ as.numeric(.data[[col]])
        ),
        !!sym(value_col) := dplyr::case_when(
          baseline_col != value_col
          & small_is_best
          & .data[["better_value"]] < lower_limit
          & .data[[baseline_col]] < lower_limit ~ as.numeric(.data[[baseline_col]]),
          baseline_col != value_col
          & !small_is_best
          & (.data[["better_value"]] > upper_limit)
          & .data[[baseline_col]] > upper_limit ~ as.numeric(.data[[baseline_col]]),
          .data[["better_value"]] < lower_limit ~ as.numeric(lower_limit),
          .data[["better_value"]] > upper_limit ~ as.numeric(upper_limit),
          TRUE ~ as.numeric(.data[["better_value"]])
        )
      ) %>%
      dplyr::select(-tidyselect::all_of(c("better_value", col))) %>%
      trim_years(trim_years, start_year_trim, end_year_trim)
  } else {
    return(df)
  }
}

#' Trim values
#'
#' @inheritParams scenario_percent_baseline
#' @inheritParams scenario_fixed_target
#' @param trim_years logical to indicate if years before `start_year` and after
#' `end_year` should be removed
#'
#' @family scenarios
#'
#' @keywords internal
#'
trim_years <- function(df, trim_years, start_year, end_year) {
  if (trim_years) {
    df %>%
      dplyr::filter(.data[["year"]] >= start_year, .data[["year"]] <= end_year)
  } else {
    return(df)
  }
}

#' @noRd
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

#' Get last value from baseline
#'
#' @param value (character vector) of values
#' @param year (character vector) of years. Must be the same length as `value`
#' @param baseline_year Year integer at which to get the baseline_value
#' @param type (character vector) of types. Must be the same length as `value`
#' @param scenario_col (character vector) of scenarios. Must be the same length as `value`
#' @param scenario name of scenario in which to look for the baseline value
#' @param type_filter (character vector) vector of types to be filtered for.
#' @param direction direction in which the last value should be found. `before`
#' (default) looks in years before `baseline_year`, while `after` looks in years
#' following `baseline_year`.
#' @param na_rm (Boolean) indicating if NA should be removed (default) from `type_filter`
#' or not.
#'
#' @family scenarios
#'
#' @keywords internal
get_baseline_value <- function(value,
                               year,
                               type,
                               scenario_col = NULL,
                               scenario = NULL,
                               baseline_year,
                               type_filter = c("all", "reported", "estimated", "projected", "imputed"),
                               direction = c("before", "after"),
                               na_rm = TRUE) {

  direction <- rlang::arg_match(direction)
  type_filter <- rlang::arg_match(type_filter, multiple = TRUE)

  if("all" %in% type_filter){
    type_filter <- c("reported", "estimated", "projected", "imputed")
  }

  if(!na_rm){
    type_filter <- c(type_filter, NA)
  }

  assert_same_length(value, year, type)

  if(!is.null(scenario_col) & !is.null(scenario)){
    year <- year[scenario_col == scenario]
    value <- value[scenario_col == scenario]
    type <- type[scenario_col == scenario ]
    assert_same_length(value, year, type)
  }

  if(direction == "before"){
    years <- year[type %in% type_filter & year <= baseline_year]

    values <- value[type %in% type_filter & year <= baseline_year]
    if(length(years>0)){
      this_year <- max(years)
    }else{
      return(NA_integer_)
    }
  }else{
    years <- year[type %in% type_filter & year >= baseline_year]

    values <- value[type %in% type_filter & year >= baseline_year]

    if(length(years>0)){
      this_year <- min(years)
    }else{
      return(NA_integer_)
    }
  }

  values[years == this_year]
}

#' Get last year from baseline
#'
#' @param year (character vector) of years.
#' @param type (character vector) of years. Must be the same length as `year`
#' @inheritParams get_baseline_value
#'
#' @family scenarios
#'
#' @keywords internal
get_baseline_year <- function(year,
                              type,
                              scenario_col = NULL,
                              scenario = NULL,
                              baseline_year,
                              type_filter = c("all", "reported", "estimated", "projected", "imputed"),
                              direction = c("before", "after"),
                              na_rm = TRUE) {

  direction <- rlang::arg_match(direction)
  type_filter <- rlang::arg_match(type_filter, multiple = TRUE)

  if("all" %in% type_filter){
    type_filter <- c("reported", "estimated", "projected", "imputed")
  }

  if(!na_rm){
    type_filter <- c(type_filter, NA)
  }

  assert_same_length(year, type)

  if(!is.null(scenario_col) & !is.null(scenario)){
    year <- year[scenario_col == scenario]
    type <- type[scenario_col == scenario ]
    assert_same_length(year, type)
  }

  if(direction == "before"){
    years <- year[type %in% type_filter & year <= baseline_year]
    if(length(years>0)){
      this_year <- max(years)
    }else{
      return(NA_integer_)
    }
  }else{
    years <- year[type %in% type_filter & year >= baseline_year]

    if(length(years>0)){
      this_year <- min(years)
    }else{
      return(NA_integer_)
    }
  }

  years[years == this_year]

}

#' Get last value in interval
#'
#' @inheritParams get_baseline_value
#'
#' @noRd
get_last_interval_value <- function(value,
                                    year,
                                    type,
                                    scenario_col = NULL,
                                    scenario = NULL,
                                    start_year,
                                    end_year,
                                    type_filter = c("all", "reported", "estimated", "projected", "imputed"),
                                    direction = c("before", "after"),
                                    na_rm = TRUE) {
  direction <- rlang::arg_match(direction)
  type_filter <- rlang::arg_match(type_filter, multiple = TRUE)

  if("all" %in% type_filter){
    type_filter <- c("reported", "estimated", "projected", "imputed")
  }

  if(!na_rm){
    type_filter <- c(type_filter, NA)
  }

  assert_same_length(value, year, type)

  if(!is.null(scenario_col) & !is.null(scenario)){
    year <- year[scenario_col == scenario]
    value <- value[scenario_col == scenario]
    type <- type[scenario_col == scenario ]
    assert_same_length(value, year, type)
  }

  years <- year[type %in% type_filter & year >= start_year & year <= end_year]
  values <- value[type %in% type_filter & year >= start_year & year <= end_year]

  if(direction == "before"){
    if(length(years>0)){
      this_year <- min(years)
    }else{
      return(NA_integer_)
    }
  }else{
    if(length(years>0)){
      this_year <- max(years)
    }else{
      return(NA_integer_)
    }
  }

  values[years == this_year]
}

#' Get last year in interval
#'
#' @inheritParams get_baseline_value
#'
#' @noRd
get_last_interval_year <- function(year,
                                    type,
                                    scenario_col = NULL,
                                    scenario = NULL,
                                    start_year,
                                    end_year,
                                    type_filter = c("all", "reported", "estimated", "projected", "imputed"),
                                    direction = c("before", "after"),
                                    na_rm = TRUE) {
  direction <- rlang::arg_match(direction)
  type_filter <- rlang::arg_match(type_filter, multiple = TRUE)

  if("all" %in% type_filter){
    type_filter <- c("reported", "estimated", "projected", "imputed")
  }

  if(!na_rm){
    type_filter <- c(type_filter, NA)
  }

  assert_same_length(year, type)

  if(!is.null(scenario_col) & !is.null(scenario)){
    year <- year[scenario_col == scenario]
    type <- type[scenario_col == scenario ]
    assert_same_length(year, type)
  }

  years <- year[type %in% type_filter & year >= start_year & year <= end_year]

  if(direction == "before"){
    if(length(years>0)){
      this_year <- min(years)
    }else{
      return(NA_integer_)
    }
  }else{
    if(length(years>0)){
      this_year <- max(years)
    }else{
      return(NA_integer_)
    }
  }

  years[years == this_year]
}

#' Get last value for the filter
#'
#' @param df (data.frame) containing the data. Needs to have at least the `year` and `type` columns.
#' @param type_filter (character vector) vector of types to be filtered for.
#'
#' @family scenarios
#'
#' @keywords internal
get_last_value <- function(df, type_filter = c("reported", "estimated", "projected", "estimated")){
  assert_columns(df, "type", "year")

  df %>%
    dplyr::filter(.data[["type"]] %in% type_filter) %>%
    dplyr::filter(.data[["year"]] == max(.data[["year"]]))
}

#' Get last value for the filter in default scenario
#'
#' @param df (data.frame) containing the data. Needs to have at least the `year`, `scenario_col` and `type` columns.
#' @inheritParams accelerate_alcohol
#' @param indicator (character) identifying the indicator for which to get the last year of `type_filter` data.
#' @inheritParams recycle_data_scenario_single
#' @inheritParams get_last_value
#'
#' @family scenarios
#'
#' @keywords internal
get_last_year_scenario <- function(df,
                                   indicator,
                                   start_year = 2018,
                                   scenario = "default",
                                   scenario_col = "scenario",
                                   type_filter = c("reported", "estimated", "projected", "estimated")){
  assert_columns(df, "year", scenario_col, "ind", "type")

  if(!is.null(indicator)){
    df <- df %>%
      dplyr::filter(.data[["ind"]] == !!indicator)
  }

  df_filtered <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario,
                  .data[["type"]] %in% !!type_filter)

  if(nrow(df_filtered) >0){
    df_filtered %>%
      dplyr::summarise(max_year = max(.data[["year"]], na.rm = T)) %>%
      dplyr::pull(.data[["max_year"]])
  }else{
    start_year
  }

}

#' Calcualte Average annual rate of change
#'
#' Calculates annual average rate of change (AARC or AROC) *with* the compounded
#' formula:
#' ((`end_value` / `baseline_value`))^(1/(`end_year` - `baseline_year`)) - 1
#'
#' To forecast change the baseline value:
#' `baseline_value` * ((1 + `aroc`)^(`current_year` - `start_year`))
#'
#' @param baseline_year year where the AARC starts
#' @param baseline_value value at start_year
#' @param end_year year where the AARC ends
#' @param end_value value at end_year
#'
#' @return numeric with AARC
#'
#' @family scenarios
#'
#' @keywords internal
calculate_aarc <- function(baseline_year,
                           baseline_value,
                           end_year,
                           end_value) {
  # if(baseline_value == 0){
  #   baseline_value
  # }else{
  ((end_value / baseline_value))^(1 / (end_year - baseline_year)) - 1
  # }
}

#' Calculate Average rate of change
#'
#' Calculates Average rate of change (AARC or AROC) *without* the compounded
#' formula:
#' (`end_value` / `baseline_value`)/(`end_year` - `baseline_year`)
#'
#' This formula provides an absolute value change and not a proportional one
#' like `calculate_aarc`. To forecast change it should be add to the baseline
#' value:
#' `baseline_value` + (`aroc`*(`current_year` - `start_year`))
#'
#' @param baseline_year year where the AARC starts
#' @param baseline_value value at start_year
#' @param end_year year where the AARC ends
#' @param end_value value at end_year
#'
#' @return numeric with AARC
#'
#' @family scenarios
#'
#' @keywords internal
calculate_aroc <- function(baseline_year,
                           baseline_value,
                           end_year,
                           end_value) {
  (end_value - baseline_value)/(end_year - baseline_year)
}

#' Get the latest AARC for data frame
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the year immediately preceding for every combination of
#' `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#'
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
#'
#' @family scenarios
#'
#' @keywords internal
get_latest_aarc <- function(df,
                            baseline_year,
                            value_col = "value") {

  assert_columns(df, value_col, "year", "iso3", "ind")

  df %>%
    dplyr::filter(.data[["year"]] %in% c(baseline_year - 1, baseline_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = !!value_col,
    ) %>%
    dplyr::mutate(
      aroc = calculate_aarc(
        baseline_year = baseline_year - 1,
        baseline_value = .data[[glue::glue("{baseline_year - 1}")]],
        end_year = baseline_year,
        end_value = .data[[glue::glue("{baseline_year}")]]
      )
    ) %>%
    dplyr::select("iso3", "ind", "aroc")
}

#' Get AARC for data frame based on a target
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the target year to reach the target value for every
#' combination of `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#'
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
#'
#' @family scenarios
#'
#' @keywords internal
get_target_aarc <- function(df,
                            target_value,
                            baseline_year,
                            target_year,
                            value_col = "value") {

  assert_columns(df, value_col, "year", "iso3", "ind")

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
    dplyr::filter(.data[["year"]] == baseline_year) %>%
    dplyr::mutate(
      aroc = calculate_aarc(
        baseline_year = .data[["year"]],
        baseline_value = .data[[value_col]],
        end_year = target_year,
        end_value = target_value
      )
    ) %>%
    dplyr::select("iso3", "ind", "aroc")
}

#' Get AARC for data frame based on a percent change to baseline
#'
#' Gets the annual average rate of change (AARC or AROC) between the
#' baseline year and the target year given a percent change between the two
#' years for every combination of `iso3` and `ind` present in `df`.
#'
#' @inheritParams  scenario_aroc
#'
#' @return dataframe with AROC (in `aroc` column) for every combination of
#' `iso3` and `ind`
#'
#' @family scenarios
#'
#' @keywords internal
get_percent_change_aarc <- function(df,
                                    percent_change,
                                    baseline_year,
                                    target_year,
                                    value_col = "value") {

  assert_columns(df, value_col, "year", "iso3", "ind")

  df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind")))) %>%
    dplyr::filter(.data[["year"]] == baseline_year) %>%
    dplyr::mutate(
      target = .data[[value_col]] * (100 + percent_change) / 100,
      aroc = calculate_aarc(
        baseline_year = baseline_year,
        baseline_value = .data[[value_col]],
        end_year = target_year,
        end_value = .data[["target"]]
      )
    ) %>%
    dplyr::select("iso3", "ind", "aroc")
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
#'
#' @family scenarios
#'
#' @keywords internal
get_aarr <- function(year, value) {
  df <- tibble::tibble(
    x = year,
    y = value
  )
  fit <- stats::lm(log(y) ~ x, data = df)
  coef <- fit[["coefficients"]][["x"]]
  100 * (1 - exp(coef))
}

#' @noRd
get_quantile <- function(value, n) {
  quantiles_limits <- stats::quantile(value, probs = seq(0, 1, 1 / n))

  findInterval(value, quantiles_limits, rightmost.closed = TRUE)
}

#' Flat extrapolation
#'
#' @param df Data frame of model data
#' @param col Name of column to extrapolate/interpolate.
#' @param group_col Column name(s) of group(s) to use in [dplyr::group_by()] when
#'     supplying type, calculating mean absolute scaled error on data involving
#'     time series, and if `group_models`, then fitting and predicting models too.
#'     If `NULL`, not used. Defaults to `"iso3"`.
#' @param sort_col Column name(s) to use to [dplyr::arrange()] the data prior to
#'     supplying type and calculating mean absolute scaled error on data involving
#'     time series. If `NULL`, not used. Defaults to `"year"`.
#' @param pred_col Column name to store predicted value.
#' @param sort_descending Logical value on whether the sorted values from `sort_col`
#'     should be sorted in descending order. Defaults to `FALSE`.
#' @param replace_obs Character value specifying how, if at all, observations should
#'     be replaced by fitted values. Defaults to replacing only missing values,
#'     but can be used to replace all values or none.
#'
#' @return a data frame with predicted data,
#'
#' @family scenarios
#'
#' @keywords internal
flat_extrapolation <- function(df,
                               col,
                               group_col = NULL,
                               sort_col = "year",
                               pred_col = "pred",
                               sort_descending = FALSE,
                               replace_obs = c("missing", "none")) {

  assert_columns(df, col, group_col, sort_col)

  replace_obs <- rlang::arg_match(replace_obs)

  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(group_col)))

  if (!is.null(sort_col)) {
    if (sort_descending) {
      fn <- dplyr::desc
    } else {
      fn <- NULL
    }
    df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(sort_col), fn),
                         .by_group = TRUE
    )
  }

  df <- dplyr::mutate(
    df,
    !!sym(pred_col) := .data[[col]]
  )

  df <- dplyr::mutate(df, !!sym(pred_col) := simple_extrap(.data[[pred_col]]))

  if (replace_obs == "missing") {
    df <- df %>% dplyr::mutate(!!sym(col) := dplyr::case_when(
      is.na(.data[[col]]) ~ .data[[pred_col]],
      TRUE ~ .data[[col]]
    ))
  }

  dplyr::ungroup(df)
}


#' Simple extrapolation
#'
#' @param x vector with values sorted
#'
#' @noRd
simple_extrap <- function(x) {
  missing_x <- is.na(x)

  whr <- max(which(!missing_x))
  x[whr:length(x)] <- x[whr]
}


#' Remove unwanted scenarios from df
#'
#' @param df (data.frame) containing the data. Needs to have at least the `scenario_col` column
#' @param unwanted_scenarios character vector of scenarios to be remove
#'
#' @inheritParams recycle_data_scenario_single
#'
#' @family scenarios
#'
#' @keywords internal
remove_unwanted_scenarios <- function(df,
                                      scenario_col = "scenario",
                                      unwanted_scenarios) {

  assert_columns(df, scenario_col)

  scenario_present <- unique(df[[scenario_col]])

  unwanted_scenarios_present <- scenario_present %in% unwanted_scenarios

  if (sum(unwanted_scenarios_present) <= length(scenario_present)) {
    df %>%
      dplyr::filter(!.data[[scenario_col]] %in% unwanted_scenarios)
  } else {
    df
  }
}

#' Execute a scenario
#'
#' Simple wrapper around [rlang::exec] to pass `parameters` and `df` to `fn`
#'
#' @param df (data.frame) containing the data.
#' @param fn function to be executed
#' @param parameters parameters to be passed to `fn`
#'
#' @family scenarios
#'
#' @keywords internal
exec_scenario <- function(df, fn, parameters){
  rlang::exec(
    fn,
    df = df,
    !!!parameters
  ) %>%
    dplyr::distinct()
}

#' Infills scenarios with `values` when `cols` are missing values.
#'
#' @param df (data.frame) containing the data
#' @inheritParams recycle_data_scenario_single
#' @param cols (list) of columns to be infilled
#' @param values (list) of values to infill with.
#'
#' @family scenarios
#'
#' @keywords internal
fill_cols_scenario <- function(df,
                               scenario_col = "scenario",
                               cols = list("type",
                                           "source",
                                           "use_dash",
                                           "use_calc"),
                               values = list("projected",
                                             NULL,
                                             TRUE,
                                             TRUE)){

  assert_same_length(cols, values)

  values <- stats::setNames(values, cols) %>%
    purrr::discard(!unlist(cols) %in% names(df))

  if(is.null(values[["source"]])){
    month_year <- format(Sys.Date(), '%B %Y')
    values[["source"]] <- glue::glue("WHO DDI interim infilling and projections,{month_year}")
  }

  if(length(values) > 0 ){
    for(i in 1:length(values)){

      col <- names(values)[i]
      df <- df %>%
        dplyr::mutate("{col}" := dplyr::case_when(
          is.na(.data[[col]]) ~ values[[i]],
          TRUE ~ .data[[col]])
        )
    }
  }
  return(df)
}
