
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
#' @param recovery_year integer year from which the AROC will be applied. Default to 2021.
#' @param scenario name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value` with a `scenario` column.
scenario_covid_rapid_return <- function(df,
                                 year = "year",
                                 ind = "ind",
                                 iso3 = "iso3",
                                 start_year = 2018,
                                 covid_year = 2020,
                                 recovery_year = 2021,
                                 end_year =  2025,
                                 value =  "value",
                                 scenario = "scenario",
                                 scenario_name = "covid_rapid_return",
                                 ind_ids = billion_ind_codes("all"),
                                 default_scenario = "default",
                                 ...){

  assert_columns(df, year, iso3, ind, value, scenario)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  params <- get_right_params(list(...), scenario_dip_recover_iso3)

  scenario_dip_recover(
    df = df,
    year = year,
    ind = ind,
    iso3 = iso3,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    end_year = end_year,
    value =  value,
    scenario = scenario,
    scenario_name = scenario_name,
    default_scenario = "default",
    params
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
#' @param recovery_year integer year from which the AROC will be applied. Default to 2021.
#' @param scenario name of scenario column to be created
#' @param scenario_name name of scenario
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value` with a `scenario` column.
scenario_covid_never_return <- function(df,
                                        year = "year",
                                        ind = "ind",
                                        iso3 = "iso3",
                                        start_year = 2018,
                                        covid_year = 2020,
                                        recovery_year = Inf,
                                        end_year =  2025,
                                        value =  "value",
                                        scenario = "scenario",
                                        scenario_name = "covid_never_return",
                                        ind_ids = billion_ind_codes("all"),
                                        default_scenario = "default",
                                        ...){

  assert_columns(df, year, iso3, ind, value, scenario)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  params <- get_right_params(list(...), scenario_dip_recover_iso3)

  scenario_dip_recover(
    df = df,
    year = year,
    ind = ind,
    iso3 = iso3,
    start_year = start_year,
    dip_year = covid_year,
    recovery_year = recovery_year,
    end_year = end_year,
    value =  value,
    scenario = scenario,
    scenario_name = scenario_name,
    default_scenario = "default",
    params
  )
}

#' Adds scenario dip, lag, pick up same aroc
#'
#' Applied to all indicators, irrelevant if they have or not a 2020 observed value.
#'
#' @inheritParams get_aroc
#' @inherit scenario_covid_dip_same_aroc
#' @param year_lag integer year in which the lag starts
#' @param length_lag integer number of year where lag continues
#
# scenario_covid_dip_lag_same_aroc <- function(df,
#                                              year = "year",
#                                              ind = "ind",
#                                              iso3 = "iso3",
#                                              start_year = 2019,
#                                              year_lag = 2020,
#                                              end_year = 2025,
#                                              length_lag = 1,
#                                              value =  "value",
#                                              scenario = "scenario",
#                                              scenario_name = "covid_dip_lag_same_aroc",
#                                              type_col = "type"){
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   lagged_df <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][year == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       ))
#
#   final_df <- scenario_covid_dip_same_aroc(lagged_df,
#                                            year = year,
#                                            ind = ind,
#                                            iso3 = iso3,
#                                            start_year = start_year,
#                                            aroc_from_year = year_lag + length_lag,
#                                            end_year = end_year,
#                                            value =  value,
#                                            scenario = scenario,
#                                            scenario_name = scenario_name,
#                                            type_col = type_col
#   ) %>%
#     dplyr::bind_rows(ind_no_timeseries) %>%
#     mutate(!!sym(scenario) := scenario_name) %>%
#     dplyr::distinct()
#
#   return(final_df)
# }

#' Adds scenario COVID dip, lag, pick up same aroc
#'
#' @inheritParams get_aroc
#' @inherit scenario_covid_dip_same_aroc
#' @param year_lag integer year in which the lag starts
#' @param length_lag integer number of year where lag continues

# scenario_covid_dip_lag_same_aroc_only_2020values <- function(df,
#                                                              year = "year",
#                                                              ind = "ind",
#                                                              iso3 = "iso3",
#                                                              start_year = 2019,
#                                                              year_lag = 2020,
#                                                              end_year = 2025,
#                                                              length_lag = 1,
#                                                              value =  "value",
#                                                              scenario = "scenario",
#                                                              scenario_name = "covid_dip_lag_same_aroc_2020_values",
#                                                              type_col = "type"){
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   has_2020_observed_value <- df %>%
#     dplyr::filter(.data[[year]] == 2020 & .data[[type_col]] %in% c("reported", "estimated")) %>%
#     dplyr::select(.data[[iso3]], .data[[ind]])
#
#   lagged_df_only_2020_values <- df %>%
#     dplyr::semi_join(has_2020_observed_value, by = c(iso3, ind)) %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][year == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       ))
#
#   lagged_df_2020_same_aroc <- scenario_covid_dip_same_aroc(lagged_df_only_2020_values,
#                                                            year = year,
#                                                            ind = ind,
#                                                            iso3 = iso3,
#                                                            start_year = start_year,
#                                                            aroc_from_year = year_lag + length_lag,
#                                                            end_year = end_year,
#                                                            value =  value,
#                                                            scenario = scenario,
#                                                            scenario_name = scenario_name,
#                                                            type_col = type_col
#   )
#
#   no_2020_values <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::anti_join(has_2020_observed_value, by = c(iso3, ind))
#
#   final_df <- dplyr::bind_rows(lagged_df_2020_same_aroc, no_2020_values) %>%
#     mutate(!!sym(scenario) := scenario_name)
#
#   return(final_df)
# }
#
# scenario_progressive_return_bau <- function(df,
#                                             year = "year",
#                                             ind = "ind",
#                                             iso3 = "iso3",
#                                             start_year = 2019,
#                                             end_year = 2025,
#                                             year_lag = 2020,
#                                             length_lag = 1,
#                                             return_bau_progression = c(0, 0.25, 0.5, 0.75, 1),
#                                             value =  "value",
#                                             scenario = "scenario",
#                                             scenario_name = "progressive_return",
#                                             type_col = "type"){
#
#   if(length((year_lag + length_lag):end_year) != length(return_bau_progression)){
#     stop("years range and return_bau_progession must have the same length")
#   }
#
#   aroc_from_year <- year_lag + length_lag
#
#   aroc_df <- get_aroc(df,
#                       year = year,
#                       ind = ind,
#                       iso3 = iso3,
#                       start_year = start_year,
#                       end_year = end_year,
#                       value = value)
#
#   aroc_df_year <- tidyr::expand_grid(aroc_df,
#                                      year = (year_lag + length_lag):end_year
#   ) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       aroc_year = aroc * return_bau_progression[grep(.data[[year]],(year_lag + length_lag):end_year)]
#     )
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   lagged_df <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][year == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       )) %>%
#     dplyr::left_join(aroc_df_year, by = c(iso3, ind,year)) %>%
#     dplyr::mutate(!!sym(value) := dplyr::case_when(
#       .data[[ind]] %in% c("detect_respond", "surviving_infants") ~ .data[[value]],
#       stringr::str_detect(ind, "campaign") ~ .data[[value]],
#       .data[[year]] > aroc_from_year ~ baseline_value + ((.data[["aroc_year"]])*(.data[[year]] - aroc_from_year)),
#       .data[[year]] %in% (start_year+1):aroc_from_year ~ .data[[value]],
#       TRUE ~ NA_real_),
#       !!sym(type_col) := dplyr::case_when(
#         .data[[year]] >= year_lag ~ "scenario",
#         TRUE ~ .data[[type_col]]
#       )
#     ) %>%
#     dplyr::filter(!is.na(.data[[value]]))
#
#   base_df <- df %>%
#     dplyr::filter(.data[[year]] %in% 2018:start_year)
#
#   final_df <- lagged_df %>%
#     dplyr::bind_rows(ind_no_timeseries) %>%
#     dplyr::bind_rows(base_df) %>%
#     mutate(!!sym(scenario) := scenario_name) %>%
#     dplyr::distinct()
#
#   return(final_df)
# }
#
# scenario_progressive_return_bau_only2020_values <- function(df,
#                                                             year = "year",
#                                                             ind = "ind",
#                                                             iso3 = "iso3",
#                                                             start_year = 2019,
#                                                             end_year = 2025,
#                                                             year_lag = 2020,
#                                                             length_lag = 1,
#                                                             return_bau_progression = c(0, 0.25, 0.5, 0.75, 1),
#                                                             value =  "value",
#                                                             scenario = "scenario",
#                                                             scenario_name = "progressive_return_only2020",
#                                                             type_col = "type"){
#
#   if(length((year_lag + length_lag):end_year) != length(return_bau_progression)){
#     stop("years range and return_bau_progession must have the same length")
#   }
#
#   aroc_from_year <- year_lag + length_lag
#
#   aroc_df <- get_aroc(df,
#                       year = year,
#                       ind = ind,
#                       iso3 = iso3,
#                       start_year = start_year,
#                       end_year = end_year,
#                       value = value)
#
#   aroc_df_year <- tidyr::expand_grid(aroc_df,
#                                      year = (year_lag + length_lag):end_year) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(
#       aroc_year = aroc * return_bau_progression[grep(.data[[year]],(year_lag + length_lag):end_year)]
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::mutate(
#       aroc_year_sum = zoo::rollapplyr(aroc_year,
#                                       width = length((year_lag + length_lag):end_year),
#                                       FUN = sum, partial = TRUE)
#     )
#
#   has_2020_observed_value <- df %>%
#     dplyr::filter(.data[[year]] == 2020 & .data[[type_col]] %in% c("reported", "estimated")) %>%
#     dplyr::select(.data[[iso3]], .data[[ind]])
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   lagged_df <- df %>%
#     dplyr::semi_join(has_2020_observed_value, by = c(iso3, ind)) %>%
#     dplyr::filter(.data[[year]] %in% c(year_lag:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][.data[[year]] == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       )) %>%
#     dplyr::left_join(aroc_df_year, by = c(iso3, ind,year)) %>%
#     dplyr::mutate(!!sym(value) := dplyr::case_when(
#       .data[[ind]] %in% c("detect_respond", "surviving_infants") ~ .data[[value]],
#       stringr::str_detect(ind, "campaign") ~ .data[[value]],
#       .data[[year]] > aroc_from_year ~ baseline_value + aroc_year_sum,
#       .data[[year]] %in% (start_year+1):aroc_from_year ~ .data[[value]],
#       TRUE ~ NA_real_),
#       !!sym(type_col) := dplyr::case_when(
#         .data[[year]] >= year_lag ~ "scenario",
#         TRUE ~ .data[[type_col]]
#       )
#     ) %>%
#     dplyr::filter(!is.na(.data[[value]]))
#
#   no_2020_values <- df %>%
#     dplyr::filter(.data[[year]] %in% c(year_lag:end_year)) %>%
#     dplyr::anti_join(has_2020_observed_value, by = c(iso3, ind))
#
#   base_df <- df %>%
#     dplyr::filter(.data[[year]] %in% 2018:start_year)
#
#   final_df <- lagged_df %>%
#     dplyr::bind_rows(no_2020_values) %>%
#     dplyr::bind_rows(base_df) %>%
#     dplyr::bind_rows(ind_no_timeseries) %>%
#     mutate(!!sym(scenario) := scenario_name) %>%
#     dplyr::distinct()
#
#   return(final_df)
# }
#
# scenario_never_return_bau_2020_values <- function(df,
#                                                   year = "year",
#                                                   ind = "ind",
#                                                   iso3 = "iso3",
#                                                   start_year = 2019,
#                                                   end_year = 2025,
#                                                   year_lag = 2020,
#                                                   length_lag = 1,
#                                                   value =  "value",
#                                                   scenario = "scenario",
#                                                   scenario_name = "never_return_2020_values",
#                                                   type_col = "type"){
#
#   aroc_from_year <- year_lag + length_lag
#
#   aroc_df <- get_aroc(df,
#                       year = year,
#                       ind = ind,
#                       iso3 = iso3,
#                       start_year = start_year,
#                       end_year = end_year,
#                       value = value) %>%
#     dplyr::mutate(aroc = dplyr::case_when(
#       aroc > 0 ~ 0,
#       TRUE ~ aroc
#     ))
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   has_2020_observed_value <- df %>%
#     dplyr::filter(.data[[year]] == 2020 & .data[[type_col]] %in% c("reported", "estimated")) %>%
#     dplyr::select(.data[[iso3]], .data[[ind]])
#
#   lagged_df <- df %>%
#     dplyr::semi_join(has_2020_observed_value, by = c(iso3, ind)) %>%
#     dplyr::filter(.data[[year]] %in% c(year_lag:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][year == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       )) %>%
#     dplyr::left_join(aroc_df, by = c(iso3, ind)) %>%
#     dplyr::mutate(!!sym(value) := dplyr::case_when(
#       .data[[ind]] %in% c("detect_respond", "surviving_infants") ~ .data[[value]],
#       stringr::str_detect(ind, "campaign") ~ .data[[value]],
#       .data[[year]] > aroc_from_year ~ baseline_value + ((.data[["aroc"]])*(.data[[year]] - aroc_from_year)),
#       .data[[year]] %in% (start_year+1):aroc_from_year ~ .data[[value]],
#       TRUE ~ NA_real_),
#       !!sym(type_col) := dplyr::case_when(
#         .data[[year]] >= year_lag ~ "scenario",
#         TRUE ~ .data[[type_col]]
#       )
#     )
#
#   no_2020_values <- df %>%
#     dplyr::filter(.data[[year]] %in% c(year_lag:end_year)) %>%
#     dplyr::anti_join(has_2020_observed_value, by = c(iso3, ind))
#
#   base_df <- df %>%
#     dplyr::filter(.data[[year]] %in% 2018:start_year)
#
#   final_df <- lagged_df %>%
#     dplyr::bind_rows(no_2020_values) %>%
#     dplyr::bind_rows(base_df) %>%
#     dplyr::bind_rows(ind_no_timeseries) %>%
#     mutate(!!sym(scenario) := scenario_name) %>%
#     dplyr::distinct()
#
#   return(final_df)
# }
#
# scenario_never_return_bau <- function(df,
#                                       year = "year",
#                                       ind = "ind",
#                                       iso3 = "iso3",
#                                       start_year = 2019,
#                                       end_year = 2025,
#                                       year_lag = 2020,
#                                       length_lag = 1,
#                                       value =  "value",
#                                       scenario = "scenario",
#                                       scenario_name = "never_return",
#                                       type_col = "type"){
#
#   aroc_from_year <- year_lag + length_lag
#
#   aroc_df <- get_aroc(df,
#                       year = year,
#                       ind = ind,
#                       iso3 = iso3,
#                       start_year = start_year,
#                       end_year = end_year,
#                       value = value) %>%
#     dplyr::mutate(aroc = dplyr::case_when(
#       aroc > 0 ~ 0,
#       TRUE ~ aroc
#     ))
#
#   ind_timeseries <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::tally() %>%
#     dplyr::filter(n >= (end_year - 2018)) %>%
#     dplyr::select(-n)
#
#   ind_no_timeseries <- df %>%
#     dplyr::anti_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::filter(stringr::str_detect(ind, "campaign")) %>%
#     dplyr::mutate(!!sym(scenario) := scenario_name)
#
#   lagged_df <- df %>%
#     dplyr::filter(.data[[year]] %in% c(2018:end_year)) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::semi_join(ind_timeseries, by = c(iso3, ind)) %>%
#     dplyr::mutate(
#       baseline_value = .data[[value]][year == year_lag],
#       !!sym(value) := dplyr::case_when(
#         .data[[ind]] == "surviving_infants" ~ .data[[value]],
#         .data[[year]] > year_lag & .data[[year]] <= (year_lag + length_lag) ~ unique(.data[["baseline_value"]]),
#         TRUE ~ .data[[value]]
#       )) %>%
#     dplyr::left_join(aroc_df, by = c(iso3, ind)) %>%
#     dplyr::mutate(!!sym(value) := dplyr::case_when(
#       .data[[ind]] %in% c("detect_respond", "surviving_infants") ~ .data[[value]],
#       stringr::str_detect(ind, "campaign") ~ .data[[value]],
#       .data[[year]] > aroc_from_year ~ baseline_value + ((.data[["aroc"]])*(.data[[year]] - aroc_from_year)),
#       .data[[year]] %in% (start_year+1):aroc_from_year ~ .data[[value]],
#       TRUE ~ NA_real_),
#       !!sym(type_col) := dplyr::case_when(
#         .data[[year]] >= year_lag ~ "scenario",
#         TRUE ~ .data[[type_col]]
#       )
#     )
#
#   base_df <- df %>%
#     dplyr::filter(.data[[year]] %in% 2018:start_year)
#
#   final_df <- lagged_df %>%
#     dplyr::bind_rows(ind_no_timeseries) %>%
#     dplyr::bind_rows(base_df) %>%
#     mutate(!!sym(scenario) := scenario_name) %>%
#     dplyr::distinct()
#
#   return(final_df)
# }
