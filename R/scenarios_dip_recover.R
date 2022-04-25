#' Scenario dip and recover
#'
#' `scenario_dip_recover` creates a scenario where there is a rapid
#' return to the previous situation after a dip. The same annual rate of change (AROC) as
#' before dip is applied from the `recovery_year`.
#'
#' Two types of AROC are supported:
#'
#'    - `lastest_year`: the AROC is calculated the values between the `start_year` and the last `reported` or
#' `estimated` value before `dip_year` is applied to the last reported value to
#' `recovery_year` onward.
#'    - `average_years_in_range`: all AROC between the `aroc_start_year` and the last `reported` or
#' `estimated` value before `dip_year` are calculated. The average AROC for that period is then
#' applied to the last reported value to `recovery_year` onward.
#'
#' If there are missing values between `dip_year` and
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
#' @param aroc_type (character) name of the type of AROC to be used. Can be either:
#'   - `lastest_year`: only the latest `reported`/`estimated` year between `start_year`
#'   and `dip_year` is used to calculate
#'   - `average_years_in_range`:
#' @param aroc_start_year (integer) year
#'
#' @return a data frame with scenario values in `value_col` with a `scenario` column.
scenario_dip_recover <- function(df,
                                 start_year = 2018,
                                 dip_year = 2020,
                                 recovery_year = 2021,
                                 progressive_recovery = FALSE,
                                 aroc_type = c("lastest_year", "average_years_in_range"),
                                 aroc_start_year = start_year,
                                 end_year = 2025,
                                 value_col = "value",
                                 scenario_col = "scenario",
                                 scenario_name = "dip_recover",
                                 ind_ids = billion_ind_codes("all"),
                                 default_scenario = "default",
                                 source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                 ...) {

  aroc_type <- rlang::arg_match(aroc_type)
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  scenario_df <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", scenario_col)) %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::filter(dplyr::case_when(
      is.na(.data[[value_col]]) & .data[["year"]] < dip_year ~ FALSE,
      TRUE ~ TRUE
    ))

  unique_iso3 <- unique(df[["iso3"]])

  params <- get_right_params(list(...), scenario_dip_recover_iso3)

  furrr::future_map_dfr(unique_iso3, ~ rlang::exec(scenario_dip_recover_iso3,
                                                   iso3 =  .x,
                                                   df = scenario_df,
                                                   value_col = value_col,
                                                   start_year = start_year,
                                                   end_year = end_year,
                                                   dip_year =  dip_year,
                                                   recovery_year = recovery_year,
                                                   progressive_recovery = progressive_recovery,
                                                   aroc_start_year = aroc_start_year,
                                                   aroc_type = aroc_type,
                                                   scenario_col = scenario_col,
                                                   ind_ids = ind_ids,
                                                   default_scenario = default_scenario,
                                                   scenario_name = scenario_name,
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
#' @param iso3 (character) ISO3 code of country to scenario
#'
scenario_dip_recover_iso3 <- function(df,
                                      iso3,
                                      start_year = 2018,
                                      dip_year = 2020,
                                      recovery_year = 2021,
                                      progressive_recovery = FALSE,
                                      aroc_type = c("lastest_year", "average_years_in_range"),
                                      aroc_start_year = start_year,
                                      end_year = 2025,
                                      value_col = "value",
                                      scenario_col = "scenario",
                                      scenario_name = "dip_recover",
                                      trim = TRUE,
                                      small_is_best = FALSE,
                                      keep_better_values = FALSE,
                                      upper_limit = 100,
                                      lower_limit = 0,
                                      trim_years = TRUE,
                                      ind_ids = billion_ind_codes("all"),
                                      source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                      default_scenario = "default") {

  scenario_df <- df %>%
    dplyr::filter(.data[["iso3"]] == !!iso3)

  assert_columns(scenario_df, "year", "iso3", "ind", value_col, scenario_col)

  baseline_year <- scenario_df %>%
    dplyr::filter(.data[["type"]] %in% c("reported", "estimated"),
                  .data[["year"]] >= aroc_start_year, .data[["year"]] < dip_year)

  if(nrow(baseline_year) > 0){
    baseline_year <- baseline_year %>%
      dplyr::filter(.data[["year"]] == min(.data[["year"]], na.rm = TRUE)) %>%
      dplyr::pull(.data[["year"]], .data[["ind"]])

    baseline_year <- baseline_year[order(names(baseline_year))]
  }else{
    baseline_year <- NA
  }

  last_year <- scenario_df %>%
    dplyr::filter(
      .data[["type"]] %in% c("reported", "estimated"),
      !is.na(.data[[value_col]]),
      .data[["year"]] >= aroc_start_year & .data[["year"]] < recovery_year
    )

  if(nrow(last_year) > 0){
    last_year <- last_year %>%
      dplyr::filter(.data[["year"]] == max(.data[["year"]], na.rm = TRUE)) %>%
      dplyr::pull(.data[["year"]], .data[["ind"]])

    last_year <- last_year[order(names(last_year))]
  }else{
    last_year <- NA
  }

  unique_inds <- sort(unique(scenario_df[["ind"]]))

  recover_df <- furrr::future_pmap_dfr(list(unique_inds, baseline_year,
                                            last_year),
                                       ~ scenario_dip_recover_iso3_ind(
                                         df = scenario_df,
                                         iso3 = iso3,
                                         ind = ..1,
                                         dip_year = dip_year,
                                         recovery_year = recovery_year,
                                         progressive_recovery = progressive_recovery,
                                         aroc_type = aroc_type,
                                         aroc_start_year = aroc_start_year,
                                         baseline_year = ..2,
                                         last_year = ..3,
                                         end_year = end_year,
                                         value_col = value_col,
                                         scenario_col = scenario_col,
                                         scenario_name = scenario_name,
                                         ind_ids = ind_ids,
                                         default_scenario = default_scenario,
                                         trim = trim,
                                         small_is_best = small_is_best,
                                         keep_better_values = keep_better_values,
                                         upper_limit = upper_limit,
                                         lower_limit = lower_limit,
                                         trim_years = trim_years,
                                         source = source
                                       ))

  recover_df <- recover_df %>%
    dplyr::filter(.data[["year"]] >= dip_year & !.data[["type"]] %in% c("reported", "estimated")) %>%
    trim_values(
      col = "scenario_value", value_col = value_col, trim = trim, small_is_best = small_is_best,
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
#' @param ind (character) name of the indicator on which to calculate the
#'    scenario
#' @param iso3 (character) name of the ISO3 country code on which to calculate
#'    the scenario
#' @param baseline_year (integer) identify baseline year on which the AROC
#'    should be calculated.
#' @param last_year (integer) identify last year where values were `reported`
#'    or `estimated` between `start_year` and `end_year`.
#'
scenario_dip_recover_iso3_ind <- function(df,
                                          iso3,
                                          ind,
                                          dip_year = 2020,
                                          recovery_year = 2021,
                                          progressive_recovery = FALSE,
                                          aroc_type = c("lastest_year", "average_years_in_range"),
                                          aroc_start_year = start_year,
                                          baseline_year = 2018,
                                          last_year = NULL,
                                          start_year = 2018,
                                          end_year = 2025,
                                          value_col = "value",
                                          scenario_col = "scenario",
                                          scenario_name = "dip_recover",
                                          ind_ids = billion_ind_codes("all"),
                                          default_scenario = "default",
                                          trim = TRUE,
                                          small_is_best = FALSE,
                                          keep_better_values = FALSE,
                                          upper_limit = 100,
                                          lower_limit = 0,
                                          trim_years = TRUE,
                                          source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y"))){
  ind_df <- df %>%
    dplyr::filter(.data[["ind"]] == !! ind,
                  .data[["iso3"]] == !! iso3) %>%
    dplyr::ungroup()

  assert_columns(ind_df, "year", "iso3", "ind", value_col, scenario_col)

  if(!"source" %in% names(ind_df)){
    ind_df <- billionaiRe_add_columns(ind_df, "source", NA_character_)
  }

  if (is.na(baseline_year) | is.na(last_year) | sum(!is.na(ind_df[[value_col]])) <= 1) {
    recover_df <- ind_df %>%
      scenario_bau(
        only_reported_estimated = TRUE,
        value_col = value_col,
        start_year = dip_year,
        end_year = end_year,
        scenario_name = scenario_name,
        scenario_col = scenario_col,
        trim = FALSE,
        keep_better_values = keep_better_values,
        upper_limit = upper_limit,
        lower_limit = lower_limit,
        trim_years = trim_years,
        small_is_best = small_is_best,
        ind_ids = ind_ids,
        default_scenario = default_scenario
      ) %>%
      dplyr::filter(.data[[scenario_col]] == !!scenario_name) %>%
      dplyr::mutate(
        "type" := dplyr::case_when(
          is.na(.data[["type"]]) & .data[["year"]] >= dip_year ~ "projected",
          TRUE ~ .data[["type"]]
        ),
        "source" := dplyr::case_when(
          is.na(.data[["source"]]) ~ !!source,
          TRUE ~ .data[["source"]]
        ))

  } else {

    if(aroc_type == "lastest_year"){
      target_value_iso3_ind <- ind_df %>%
        dplyr::filter(.data[["year"]] == (dip_year - 1)) %>%
        dplyr::pull(.data[[value_col]])

      aroc <- get_target_aarc(ind_df,
                              target_value = target_value_iso3_ind,
                              target_year = dip_year - 1,
                              baseline_year = baseline_year,
                              value_col = value_col
      ) %>%
        dplyr::pull(.data[["aroc"]])
    }else if(aroc_type == "average_years_in_range"){
      target_value_iso3_ind <- ind_df %>%
        dplyr::filter(.data[["year"]] %in% (aroc_start_year + 1):(dip_year - 1)) %>%
        dplyr::mutate("baseline_year" := dplyr::case_when(
          .data[["year"]]-1  <= baseline_year ~ as.integer(baseline_year),
          TRUE ~ as.integer(.data[["year"]] - 1))) %>%
        dplyr::select(target_value = !!value_col, target_year = "year", "baseline_year")

      aroc <- furrr::future_pmap_dfr(target_value_iso3_ind,
                                     get_target_aarc,
                                     df = ind_df,
                                     value_col = value_col) %>%
        dplyr::summarise(aroc = mean(.data[["aroc"]])) %>%
        dplyr::pull(.data[["aroc"]])

    }

    last_value <- ind_df %>%
      dplyr::filter(
        .data[["year"]] == last_year
      ) %>%
      dplyr::pull(.data[[value_col]])

    aroc_df <- tidyr::expand_grid(
      "year" := recovery_year:max(ind_df[["year"]]),
      "iso3" := unique(iso3),
      "ind" := unique(ind),
      "aroc" := aroc
    )

    if (progressive_recovery) {
      aroc_df <- aroc_df %>%
        dplyr::mutate(aroc = .data[["aroc"]] * ((.data[["year"]] - recovery_year) / (end_year - recovery_year)))
    }

    ind_timeseries <- ind_df %>%
      dplyr::filter(.data[["year"]] >= dip_year) %>%
      dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
      dplyr::tally() %>%
      dplyr::filter(.data[["n"]] >= (end_year - dip_year - 1)) %>%
      dplyr::select(-"n")

    ind_no_timeseries <- ind_df %>%
      dplyr::anti_join(ind_timeseries, by = c("iso3", "ind")) %>%
      dplyr::filter(
        stringr::str_detect("ind", "campaign"),
        !.data[["year"]] %in% 2018:(dip_year - 1)
      )

    recover_df <- ind_df %>%
      dplyr::group_by(.data[["ind"]]) %>%
      dplyr::semi_join(ind_timeseries, by = c("iso3", "ind")) %>%
      dplyr::mutate(
        baseline_value = .data[[value_col]][.data[["year"]] == dip_year]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(aroc_df, by = c("iso3", "ind", "year")) %>%
      dplyr::mutate(
        scenario_value = dplyr::case_when(
          stringr::str_detect(ind, "campaign") ~ as.numeric(.data[[value_col]]),
          .data[["year"]] < dip_year ~ as.numeric(.data[[value_col]]),
          .data[["year"]] >= recovery_year ~ last_value * (1 + (.data[["aroc"]]) * (.data[["year"]] - (recovery_year - 1))),
          .data[["year"]] >= dip_year & .data[["year"]] < recovery_year & .data[["year"]] > last_year ~ as.numeric(last_value),
          .data[["year"]] >= dip_year & .data[["year"]] < recovery_year & !is.na(.data[[value_col]]) ~ as.numeric(.data[[value_col]]),
          TRUE ~ NA_real_
        ),
        scenario_value = dplyr::case_when(
          .data[["scenario_value"]] < 0 ~ 0.0,
          TRUE ~ .data[["scenario_value"]]
        ),
        "type" := dplyr::case_when(
          (.data[["scenario_value"]] != .data[[value_col]] | (is.na(.data[[value_col]] & !is.na(.data[["scenario_value"]])))) & .data[["year"]] >= dip_year ~ "projected",
          TRUE ~ .data[["type"]]
        ),
        "source" := dplyr::case_when(
          is.na(.data[["source"]]) ~ !!source,
          TRUE ~ .data[["source"]]
        ),
        !!sym(scenario_col) := scenario_name
      ) %>%
      dplyr::select(-"baseline_value", -"aroc")

  }

  return(recover_df)
}
