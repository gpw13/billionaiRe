#' Scenario dip and recover
#'
#' `scenario_dip_recover` creates a scenario where there is a rapid
#' return to the previous situation after a dip. The same annual rate of change (AROC) as
#' before dip is applied from the `recovery_year`.
#'
#' In details, the AROC  between the `start_year`  and the last `reported` or
#' `estimated` value before `dip_year` is applied to the last reported value to
#' `recovery_year` onward. If there are missing values between `dip_year` and
#' `recovery_year`, the last value from `dip_year` is carried forward. This
#' applies only to countries where the indicator value for `dip_year` is
#' `reported` or `estimated`. Otherwise, the value is carried with
#' `scenario_bau`.
#'
#' If `progressive_recovery` is TRUE, then the recovery is
#' spread between the years between `recovery_year` and `end_year`. For
#' instance, if `recovery_year` is 2021 and `end_year` 2025, then 2021 will have 0%
#' of AROC, 2022 25%, 2023 50%, 2024 75%, and 2025 100%.
#'
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inherit scenario_aroc
#' @param dip_year (integer) year where the dip appends
#' @param recovery_year (integer) year from which the AROC will be applied
#' @param progressive_recovery (logical) TRUE if the recovery after dip
#' should be progressive.
#' @param scenario name of scenario column to be created
#' @param scenario_name name of scenario
#' @inheritParams calculate_hep_components
#' @param ... additional parameters to be passed to
#' `scenario_dip_recover_iso3()`
#'
#' @return a data frame with scenario values in `value` with a `scenario` column.
scenario_dip_recover <- function(df,
                                 year = "year",
                                 ind = "ind",
                                 iso3 = "iso3",
                                 start_year = 2018,
                                 dip_year = 2020,
                                 recovery_year = 2021,
                                 progressive_recovery = FALSE,
                                 end_year = 2025,
                                 value = "value",
                                 scenario = "scenario",
                                 scenario_name = "dip_recover",
                                 ind_ids = billion_ind_codes("all"),
                                 default_scenario = "default",
                                 source_col = "source",
                                 source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                 ...) {
  assert_columns(df, year, iso3, ind, value, scenario)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids = ind_ids)

  full_years_df <- tidyr::expand_grid(
    "{year}" := start_year:end_year,
    "{iso3}" := unique(df[[iso3]]),
    "{ind}" := unique(df[[ind]]),
    "{scenario}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c(year, iso3, ind, scenario)) %>%
    dplyr::filter(.data[[scenario]] == default_scenario) %>%
    dplyr::filter(dplyr::case_when(
      is.na(.data[[value]]) & .data[[year]] < dip_year ~ FALSE,
      TRUE ~ TRUE
    ))

  unique_iso3 <- unique(df[[iso3]])

  params <- get_right_params(list(...), scenario_dip_recover_iso3)

  purrr::map_dfr(unique_iso3, ~ rlang::exec(scenario_dip_recover_iso3,
                                            iso3 =  .x,
                                            df = scenario_df,
                                            year = year,
                                            ind = ind,
                                            value = value,
                                            iso3_col = iso3,
                                            start_year = start_year,
                                            end_year = end_year,
                                            dip_year =  dip_year,
                                            recovery_year = recovery_year,
                                            progressive_recovery = progressive_recovery,
                                            scenario = scenario,
                                            ind_ids = ind_ids,
                                            default_scenario = default_scenario,
                                            scenario_name = scenario_name,
                                            source_col = source_col,
                                            source = source,
                                            !!!params
  )) %>%
    dplyr::bind_rows(df) %>%
    dplyr::distinct()
}

#' Scenario dip and recover to specific iso3
#'
#' Applies `scenario_dip_recover()` to a specific iso3.
#'
#' @inherit scenario_dip_recover
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hep_components
#' @inherit scenario_aroc
#' @param iso3_col (character) name of column with the ISO3 country codes
#'
scenario_dip_recover_iso3 <- function(df,
                                      iso3,
                                      year = "year",
                                      ind = "ind",
                                      iso3_col = "iso3",
                                      start_year = 2018,
                                      dip_year = 2020,
                                      recovery_year = 2021,
                                      progressive_recovery = FALSE,
                                      end_year = 2025,
                                      value = "value",
                                      scenario = "scenario",
                                      scenario_name = "dip_recover",
                                      trim = TRUE,
                                      small_is_best = FALSE,
                                      keep_better_values = FALSE,
                                      upper_limit = 100,
                                      lower_limit = 0,
                                      trim_years = TRUE,
                                      ind_ids = billion_ind_codes("all"),
                                      source_col = "source",
                                      source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                      type_col = "type",
                                      default_scenario = "default") {

  scenario_df <- df %>%
    dplyr::filter(.data[[iso3_col]] == !!iso3)

  assert_columns(scenario_df, year, iso3_col, ind, value, scenario)
  assert_unique_rows(scenario_df, ind, iso3_col, year, scenario, ind_ids = ind_ids)

  baseline_year <- scenario_df %>%
    dplyr::filter(.data[[type_col]] %in% c("reported", "estimated"),
                  .data[[year]] >= start_year, .data[[year]] < dip_year)

  if(nrow(baseline_year) > 0){
    baseline_year <- baseline_year %>%
      dplyr::filter(.data[[year]] == min(.data[[year]], na.rm = TRUE)) %>%
      dplyr::pull(.data[[year]], .data[[ind]])

    baseline_year <- baseline_year[order(names(baseline_year))]
  }else{
    baseline_year <- NA
  }

  last_year <- scenario_df %>%
    dplyr::filter(
      .data[[type_col]] %in% c("reported", "estimated"),
      !is.na(.data[[value]]),
      .data[[year]] >= start_year & .data[[year]] < recovery_year
    )

  if(nrow(last_year) > 0){
    last_year <- last_year %>%
      dplyr::filter(.data[[year]] == max(.data[[year]], na.rm = TRUE)) %>%
      dplyr::pull(.data[[year]], .data[[ind]])

    last_year <- last_year[order(names(last_year))]
  }else{
    last_year <- NA
  }

  unique_inds <- sort(unique(scenario_df[[ind]]))

  recover_df <- purrr::pmap_dfr(list(unique_inds, baseline_year,
                                     last_year),
                                ~ scenario_dip_recover_iso3_ind(
                                  df = scenario_df,
                                  iso3 = iso3,
                                  ind = ..1,
                                  year = year,
                                  ind_col = ind,
                                  iso3_col = iso3_col,
                                  dip_year = dip_year,
                                  recovery_year = recovery_year,
                                  progressive_recovery = progressive_recovery,
                                  baseline_year = ..2,
                                  last_year = ..3,
                                  end_year = end_year,
                                  value = value,
                                  scenario = scenario,
                                  scenario_name = scenario_name,
                                  type_col = type_col,
                                  ind_ids = ind_ids,
                                  default_scenario = default_scenario,
                                  trim = trim,
                                  small_is_best = small_is_best,
                                  keep_better_values = keep_better_values,
                                  upper_limit = upper_limit,
                                  lower_limit = lower_limit,
                                  trim_years = trim_years,
                                  source_col = source_col,
                                  source = source
                                ))

  recover_df <- recover_df %>%
    dplyr::filter(.data[[year]] >= dip_year & !.data[[type_col]] %in% c("reported", "estimated")) %>%
    trim_values(
      col = "scenario_value", value = value, year = year, trim = trim, small_is_best = small_is_best,
      keep_better_values = keep_better_values, upper_limit = upper_limit,
      lower_limit = lower_limit, trim_years = trim_years, start_year = start_year, end_year = end_year
    )

  df %>%
    dplyr::bind_rows(recover_df) %>%
    dplyr::distinct()
}


#' Scenario dip and recover to specific `ind` and `iso3`
#'
#' Applies `scenario_dip_recover()` to a specific `ind` and `iso3` combination.
#'
#' @inherit scenario_dip_recover
#' @inheritParams transform_hpop_data
#' @inheritParams recycle_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams trim_values
#' @inherit scenario_aroc
#' @inheritParams scenario_dip_recover_iso3
#' @param ind_col Column name of column with indicator names.
#' @param iso3_col (character) name of column with the ISO3 country codes
#' @param ind (character) name of the indicator on which to calculate the
#'    scenario
#' @param iso3 (character) name of the ISO3 country code on which to calculate
#'    the scenario
#' @param baseline_year (integer) identify baseline year on which the AROC
#'    should be calculated.
#' @param last_year (integer) identify last year where values where `reported`
#'    or `estimated` between `start_year` and `end_year`.
#'
scenario_dip_recover_iso3_ind <- function(df,
                                          iso3,
                                          ind,
                                          year = "year",
                                          ind_col = "ind",
                                          iso3_col = "iso3",
                                          dip_year = 2020,
                                          recovery_year = 2021,
                                          progressive_recovery = FALSE,
                                          baseline_year = 2018,
                                          last_year = NULL,
                                          start_year = 2018,
                                          end_year = 2025,
                                          value = "value",
                                          scenario = "scenario",
                                          scenario_name = "dip_recover",
                                          type_col = "type",
                                          ind_ids = billion_ind_codes("all"),
                                          default_scenario = "default",
                                          trim = TRUE,
                                          small_is_best = FALSE,
                                          keep_better_values = FALSE,
                                          upper_limit = 100,
                                          lower_limit = 0,
                                          trim_years = TRUE,
                                          source_col = "source",
                                          source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y"))){
  ind_df <- df %>%
    dplyr::filter(.data[[ind_col]] == !! ind,
                  .data[[iso3_col]] == !! iso3)

  assert_columns(ind_df, year, iso3_col, ind_col, value, scenario)
  assert_unique_rows(ind_df, ind_col, iso3_col, year, scenario, ind_ids = ind_ids)

  if (is.na(baseline_year) | is.na(last_year)) {
    recover_df <- ind_df %>%
      scenario_bau(
        value = value,
        ind = ind_col,
        iso3 = iso3_col,
        year = year,
        start_year = dip_year,
        end_year = end_year,
        scenario_name = scenario_name,
        scenario = scenario,
        trim = FALSE,
        keep_better_values = keep_better_values,
        upper_limit = upper_limit,
        lower_limit = lower_limit,
        trim_years = trim_years,
        small_is_best = small_is_best,
        ind_ids = ind_ids,
        default_scenario = default_scenario
      ) %>%
      dplyr::filter(.data[[scenario]] == !!scenario_name)
  } else {

    target_value_iso3_ind <- ind_df %>%
      dplyr::filter(.data[[year]] == (dip_year - 1)) %>%
      dplyr::pull(.data[[value]])

    last_value <- ind_df %>%
      dplyr::filter(
        .data[[year]] == last_year
      ) %>%
      dplyr::pull(.data[[value]])

    aroc <- get_target_aarc(ind_df,
                            target_value = target_value_iso3_ind,
                            target_year = dip_year - 1,
                            baseline_year = baseline_year,
                            value = value,
                            year = year,
                            iso3 = iso3_col,
                            ind = ind_col
    ) %>%
      dplyr::pull(.data[["aroc"]])

    aroc_df <- tidyr::expand_grid(
      "{year}" := recovery_year:max(ind_df[[year]]),
      "{iso3_col}" := unique(iso3),
      "{ind_col}" := unique(ind),
      "aroc" := aroc
    )

    if (progressive_recovery) {
      aroc_df <- aroc_df %>%
        dplyr::mutate(aroc = .data[["aroc"]] * ((.data[[year]] - recovery_year) / (end_year - recovery_year)))
    }

    ind_timeseries <- ind_df %>%
      dplyr::filter(.data[[year]] >= dip_year) %>%
      dplyr::group_by(.data[[iso3_col]], .data[[ind_col]]) %>%
      dplyr::tally() %>%
      dplyr::filter(.data[["n"]] >= (end_year - dip_year - 1)) %>%
      dplyr::select(-"n")

    ind_no_timeseries <- ind_df %>%
      dplyr::anti_join(ind_timeseries, by = c(iso3_col, ind_col)) %>%
      dplyr::filter(
        stringr::str_detect(ind_col, "campaign"),
        !.data[[year]] %in% 2018:(dip_year - 1)
      )

    if(!source_col %in% names(ind_df)){
      ind_df <- billionaiRe_add_columns(ind_df, source_col, NA_character_)
    }

    recover_df <- ind_df %>%
      dplyr::group_by(.data[[ind_col]]) %>%
      dplyr::semi_join(ind_timeseries, by = c(iso3_col, ind_col)) %>%
      dplyr::mutate(
        baseline_value = .data[[value]][year == dip_year]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(aroc_df, by = c(iso3_col, ind_col, year)) %>%
      dplyr::mutate(
        scenario_value = dplyr::case_when(
          stringr::str_detect(ind, "campaign") ~ as.numeric(.data[[value]]),
          .data[[year]] < dip_year ~ as.numeric(.data[[value]]),
          .data[[year]] >= recovery_year ~ last_value * (1 + (.data[["aroc"]]) * (.data[[year]] - (recovery_year - 1))),
          .data[[year]] >= dip_year & .data[[year]] < recovery_year & .data[[year]] > last_year ~ as.numeric(last_value),
          .data[[year]] >= dip_year & .data[[year]] < recovery_year & !is.na(.data[[value]]) ~ as.numeric(.data[[value]]),
          TRUE ~ NA_real_
        ),
        scenario_value = dplyr::case_when(
          .data[["scenario_value"]] < 0 ~ 0.0,
          TRUE ~ .data[["scenario_value"]]
        ),
        "{type_col}" := dplyr::case_when(
          (.data[["scenario_value"]] != .data[[value]] | (is.na(.data[[value]] & !is.na(.data[["scenario_value"]])))) & .data[[year]] >= dip_year ~ "projected",
          TRUE ~ .data[[type_col]]
        ),
        "{source_col}" := dplyr::case_when(
          is.na(.data[[source_col]]) ~ source,
          TRUE ~ .data[[source_col]]
        ),
        !!sym(scenario) := scenario_name
      ) %>%
      dplyr::select(-"baseline_value", -"aroc")

  }

  return(recover_df)
}
