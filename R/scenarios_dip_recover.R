#' Scenario dip and recover
#'
#' `scenario_dip_recover` creates a scenario where there is a rapid
#' return to the previous situation after a dip. The same annual rate of change (AROC) as
#' before dip is applied from the `recovery_year`.
#'
#' In details, the AROC  between the `start_year`  and `dip_year` - 1 is
#' applied to the last reported value to `recovery_year` onward. If there are
#' missing values between `dip_year` and `recovery_year`, the last value from
#' `dip_year` is carried forward. This applies only to countries where the
#' indicator value for `dip_year` is `reported` or `estimated`. Otherwise,
#' the value is carried with `scenario_bau`.
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
    dplyr::filter(.data[[scenario]] == default_scenario)

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
    !!!params
  ))
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
                                      type_col = "type",
                                      trim = TRUE,
                                      small_is_best = FALSE,
                                      keep_better_values = FALSE,
                                      upper_limit = 100,
                                      lower_limit = 0,
                                      trim_years = TRUE,
                                      ind_ids = billion_ind_codes("all"),
                                      default_scenario = "default") {
  assert_columns(df, year, iso3_col, ind, value, scenario)
  assert_unique_rows(df, ind, iso3_col, year, scenario, ind_ids = ind_ids)

  scenario_df <- df %>%
    dplyr::filter(.data[[iso3_col]] == !!iso3)

  reported_estimated <- scenario_df %>%
    dplyr::group_by(.data[[iso3_col]]) %>%
    dplyr::filter(
      .data[[type_col]] %in% c("reported", "estimated"),
      .data[[year]] == dip_year
    )

  last_value <- scenario_df %>%
    dplyr::filter(
      .data[[type_col]] %in% c("reported", "estimated"),
      !is.na(.data[[value]]),
      .data[[year]] >= dip_year
    )

  last_value <- ifelse(nrow(last_value) == 0,
    NA_real_,
    dplyr::pull(last_value, .data[[value]])
  )

  if (nrow(reported_estimated) == 0) {
    recover_df <- scenario_df %>%
      dplyr::filter(.data[[year]] >= dip_year) %>%
      scenario_bau(
        value = value,
        ind = ind,
        iso3 = iso3_col,
        year = year,
        start_year = start_year,
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
    target_value_iso3_ind <- scenario_df %>%
      dplyr::filter(.data[[year]] == (dip_year - 1)) %>%
      dplyr::pull(.data[[value]], .data[[ind]])

    aroc_df <- purrr::map2_dfr(target_value_iso3_ind, names(target_value_iso3_ind),
                               ~get_target_aarc(scenario_df %>% dplyr::filter(.data[[ind]] == .y),
                                                target_value = .x,
                                                target_year = dip_year - 1,
                                                baseline_year = start_year,
                                                value = value,
                                                year = year,
                                                iso3 = iso3_col,
                                                ind = ind
                               ))

    full_recovery_df <- tidyr::expand_grid(
      "{year}" := recovery_year:max(scenario_df[[year]]),
      "{iso3_col}" := unique(aroc_df[[iso3_col]]),
      "{ind}" := unique(aroc_df[[ind]]),
    )

    aroc_df <- aroc_df %>%
      dplyr::full_join(full_recovery_df, by = c(iso3_col, ind))

    if (progressive_recovery) {
      aroc_df <- aroc_df %>%
        dplyr::mutate(aroc = .data[["aroc"]] * ((.data[[year]] - recovery_year) / (end_year - recovery_year)))
    }

    ind_timeseries <- scenario_df %>%
      dplyr::filter(.data[[year]] >= dip_year) %>%
      dplyr::group_by(.data[[iso3_col]], .data[[ind]]) %>%
      dplyr::tally() %>%
      dplyr::filter(.data[["n"]] >= (end_year - dip_year - 1)) %>%
      dplyr::select(-"n")

    ind_no_timeseries <- scenario_df %>%
      dplyr::anti_join(ind_timeseries, by = c(iso3_col, ind)) %>%
      dplyr::filter(
        stringr::str_detect(ind, "campaign"),
        !.data[[year]] %in% 2018:(dip_year - 1)
      )

    recover_df <- scenario_df %>%
      dplyr::group_by(.data[[ind]]) %>%
      dplyr::semi_join(ind_timeseries, by = c(iso3_col, ind)) %>%
      dplyr::mutate(
        valtemp = .data[[value]],
        baseline_value = .data[["valtemp"]][year == dip_year]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(aroc_df, by = c(iso3_col, ind, year)) %>%
      dplyr::mutate(
        scenario_value = dplyr::case_when(
          stringr::str_detect(ind, "campaign") ~ as.numeric(.data[[value]]),
          .data[[year]] < dip_year ~ as.numeric(.data[[value]]),
          .data[[year]] >= recovery_year ~ .data[["baseline_value"]] * (1 + (.data[["aroc"]]) * (.data[[year]] - (recovery_year - 1))),
          .data[[year]] >= dip_year ~ as.numeric(last_value),
          TRUE ~ NA_real_
        ),
        scenario_value = dplyr::case_when(
          .data[["scenario_value"]] < 0 ~ 0.0,
          TRUE ~ .data[["scenario_value"]]
        ),
        !!sym(scenario) := scenario_name
      ) %>%
      dplyr::select(-"baseline_value", -"aroc", -"valtemp")
  }

  recover_df <- recover_df %>%
    trim_values(
      col = "scenario_value", value = value, year = year, trim = trim, small_is_best = small_is_best,
      keep_better_values = keep_better_values, upper_limit = upper_limit,
      lower_limit = lower_limit, trim_years = trim_years, start_year = start_year, end_year = end_year
    )

  df %>%
    dplyr::bind_rows(recover_df) %>%
    dplyr::distinct()
}
