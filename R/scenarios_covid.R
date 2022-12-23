
#' Scenario COVID rapid return to normal
#'
#' `scenario_covid_rapid_return` creates a scenario where there is a rapid
#' return to the pre-pandemic situation after a dip due to COVID-19.
#'
#' In details, the AROC  between the `start_year`  and `covid_year` - 1 is
#' applied to the last reported value to `recovery_year` onward. If there are
#' missing values between `covid_year` and `recovery_year`, the last value from
#' `covid_year` is carried forward. This applies only to countries where the
#' indicator value for `covid_year` is `reported` or `estimated`. Otherwise,
#' the value is carried with `scenario_bau`.
#'
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inherit scenario_aroc
#' @param covid_year (integer) year where the values are impacted by COVID.
#' @param recovery_year integer year from which the AROC will be applied. Default to 2022.
#' @param scenario name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value_col` with a `scenario_col` column.
scenario_covid_rapid_return <- function(df,
                                 start_year = 2018,
                                 covid_year = 2020,
                                 recovery_year = 2022,
                                 end_year =  2025,
                                 value_col =  "value",
                                 scenario_col = "scenario",
                                 scenario_name = "covid_rapid_return",
                                 ind_ids = billion_ind_codes("all"),
                                 default_scenario = "default",
                                 ...){
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)

  params <- rlang::list2(...)

  rlang::exec(scenario_dip_recover,
    df = df,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    end_year = end_year,
    value_col = value_col,
    scenario_col = scenario_col,
    scenario_name = scenario_name,
    default_scenario = "default",
    !!!params
  )
}


#' Scenario COVID never return to normal
#'
#' `scenario_covid_rapid_return` creates a scenario where there is no
#' return to the pre-pandemic situation.
#'
#' In details, the last value from `covid_year` is carried forward.
#' This applies only to countries where the indicator value for
#' `covid_year` is `reported` or `estimated`. Otherwise, the value
#' is carried with `scenario_bau`.
#'
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inherit scenario_aroc
#' @param covid_year (integer) year where the values are impacted by COVID.
#' @param recovery_year integer year from which the AROC will be applied. Default to 2060.
#' @param scenario_col name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value_col` with a `scenario_col` column.
scenario_covid_never_return <- function(df,
                                        start_year = 2018,
                                        covid_year = 2020,
                                        recovery_year = 2060,
                                        end_year =  2025,
                                        value_col =  "value",
                                        scenario_col = "scenario",
                                        scenario_name = "covid_never_return",
                                        ind_ids = billion_ind_codes("all"),
                                        default_scenario = "default",
                                        ...) {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)

  params <- get_right_parameters(rlang::list2(...), scenario_dip_recover_iso3)

  rlang::exec(scenario_dip_recover,
    df = df,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    end_year = end_year,
    value_col = value_col,
    scenario_col = scenario_col,
    scenario_name = scenario_name,
    default_scenario = "default",
    !!!params
  )
}

#' Scenario COVID delayed return to normal
#'
#' `scenario_covid_delayed_return` creates a scenario where there is a
#' a delayed by number for years between `covid_year` and `recovery_year` before
#' returning to the pre-pandemic situation.
#'
#' In details, the AROC  between the `start_year`  and `covid_year` - 1 is
#' applied to the last reported value to `recovery_year` onward. If there are
#' missing values between `covid_year` and `recovery_year`, the last value from
#' `covid_year` is carried forward. This applies only to countries where the
#' indicator value for `covid_year` is `reported` or `estimated`. Otherwise, the
#' value is carried with `scenario_bau`.
#'
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inherit scenario_aroc
#' @param covid_year (integer) year where the values are impacted by COVID.
#' @param recovery_year integer year from which the AROC will be applied. Default to 2023.
#' @param scenario_col name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value_col` with a `scenario_col` column.
scenario_covid_delayed_return <- function(df,
                                        start_year = 2018,
                                        covid_year = 2020,
                                        recovery_year = 2023,
                                        end_year =  2025,
                                        value_col =  "value",
                                        scenario_col = "scenario",
                                        scenario_name = "covid_delayed_return",
                                        ind_ids = billion_ind_codes("all"),
                                        default_scenario = "default",
                                        ...){
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)

  params <- get_right_parameters(list(...), scenario_dip_recover_iso3)

  rlang::exec(scenario_dip_recover,
    df = df,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    end_year = end_year,
    value_col =  value_col,
    scenario_col = scenario_col,
    scenario_name = scenario_name,
    default_scenario = "default",
    !!!params
  )
}

#' Scenario COVID sustained disruption to normal
#'
#' `scenario_covid_delayed_return` creates a scenario where there is a
#' a delayed by one year before progressivelly returning to the pre-pendemic
#' situation.
#'
#' In details, the AROC  between the `start_year`  and `covid_year` - 1 is
#' applied to the last reported value to `recovery_year` onward in a progressive
#' way: full AROC is only applied at `end_year`; otherwise the recovery is
#' spread between the years between `recovery_year` and `end_year`. For
#' instance, if `recovery_year` is 2021 and `end_year` 2025, then 2021 will have 0%
#' of AROC, 2022 25%, 2023 50%, 2024 75%, and 2025 100%.
#' If there are missing values between `covid_year` and `recovery_year`, the
#' last value from `covid_year` is carried forward. This applies only to
#' countries where the indicator value for `covid_year` is `reported` or
#' `estimated`. Otherwise, the value is carried with `scenario_bau`.
#'
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inherit scenario_aroc
#' @param progressive_recovery (logical) TRUE if the recovery after COVID-19
#' should be progressive.
#' @param covid_year (integer) year where the values are impacted by COVID.
#' @param recovery_year integer year from which the AROC will be applied. Default to 2022.
#' @param scenario_col name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value_col` with a `scenario_col` column.
scenario_covid_sustained_disruption <- function(df,
                                          start_year = 2018,
                                          covid_year = 2020,
                                          recovery_year = 2022,
                                          progressive_recovery = TRUE,
                                          end_year =  2025,
                                          value_col =  "value",
                                          scenario_col = "scenario",
                                          scenario_name = "covid_sustained_disruption",
                                          ind_ids = billion_ind_codes("all"),
                                          default_scenario = "default",
                                          ...){
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)

  params <- get_right_parameters(rlang::list2(...), scenario_dip_recover_iso3)

  rlang::exec(scenario_dip_recover,
    df = df,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    progressive_recovery = progressive_recovery,
    end_year = end_year,
    value_col =  value_col,
    scenario_col = scenario_col,
    scenario_name = scenario_name,
    default_scenario = "default",
    !!!params
  )
}
