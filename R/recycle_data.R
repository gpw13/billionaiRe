#' Recycle data between scenarios
#'
#' `recycle_data` recycles data between the scenarios present in `df` to reduce
#' size of tables stored.
#'
#' This function wraps around `recycle_data_scenario_single` for all the
#' scenarios present in the `scenario` column. `recycle_data_scenario_single `
#' reuses values present in the specified  scenarios in `default_scenario`,
#' `scenario_reported_estimated`, and `scenario_tp` for the specified scenarios.
#'
#' To do so, it looks at:
#'
#' 1. values in `default_scenario` but not in the scenario specified
#' 2. values in `scenario_reported_estimated` but not in the scenario specified
#' or `scenario_tp`
#' 3. values in `scenario_tp` but not in the scenario specified,
#' `scenario_reported_estimated` or `scenario_tp`
#'
#' For more information see:
#'
#' \code{vignette("scenarios", package = "billionaiRe")}
#'
#' @param billion name of billion to recycle data for.
#' @param type Column name of column with types
#' @param default_scenario name of the default scenario.
#' @param scenario_reported_estimated name of the reported/estimated scenario.
#' @param scenario_tp name of the WHO technical programs projections/imputations scenario.
#' @param include_projection Boolean to include or not projections in recycling
#' @param recycle_campaigns Boolean to include or not campaigns in recycling
#'
#' @inherit transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_billion
#'
#' @return Data frame in long format.
#' @export
recycle_data <- function(df,
                         billion = c("hep", "hpop", "uhc"),
                         iso3 = "iso3",
                         ind = "ind",
                         value = "value",
                         year = "year",
                         type = "type",
                         start_year = 2018,
                         end_year = 2025,
                         scenario = "scenario",
                         default_scenario = "pre_covid_bau",
                         scenario_reported_estimated = "none",
                         scenario_tp = "tp",
                         include_projection = TRUE,
                         recycle_campaigns = TRUE,
                         ind_ids = NULL) {
  assert_columns(df, iso3, ind, value, year, scenario, type)
  assert_unique_rows(df, ind, iso3, year, scenario = scenario, ind_ids)
  assert_scenario_in_df(df, c(scenario_reported_estimated, scenario_tp), scenario)

  billion <- rlang::arg_match(billion)
  if (is.null(ind_ids)) {
    ind_ids <- billion_ind_codes(billion)
  }

  scenarios_recycle <- unique(df[[scenario]])

  purrr::map_dfr(
    scenarios_recycle,
    ~ recycle_data_scenario_single(
      df = df,
      scenario = .x,
      billion = billion,
      iso3 = iso3,
      ind = ind,
      value = value,
      year = year,
      type = type,
      start_year = start_year,
      end_year = end_year,
      scenario_col = scenario,
      default_scenario = default_scenario,
      scenario_reported_estimated = scenario_reported_estimated,
      scenario_tp = scenario_tp,
      include_projection = include_projection,
      recycle_campaigns = recycle_campaigns,
      ind_ids = ind_ids
    )
  )
}
#' Recycle data between scenarios for a single scenario
#'
#' `recycle_data_scenario_single ` reuses values present in the specified
#' scenarios in `default_scenario`, `scenario_reported_estimated`, and
#' `scenario_tp` for the specified scenarios.
#'
#' To do so, it looks at:
#'
#' 1. values in `default_scenario` but not in the scenario specified
#' 2. values in `scenario_reported_estimated` but not in the scenario specified
#' or `scenario_tp`
#' 3. values in `scenario_tp` but not in the scenario specified,
#' `scenario_reported_estimated` or `scenario_tp`
#'
#' For more information see:
#'
#' \code{vignette("scenarios", package = "billionaiRe")}
#'
#' @param scenario name of scenario to recycle for.
#' @param scenario_col Column name of column with scenario identifiers.
#' @param billion name of billion to recycle data for.
#' @param type Column name of column with types
#' @param default_scenario name of the default scenario.
#' @param scenario_reported_estimated name of the reported/estimated scenario.
#' @param scenario_tp name of the WHO technical programs projections/imputations scenario.
#' @param include_projection Boolean to include or not projections in recycling
#' @param recycle_campaigns Boolean to include or not campaigns in recycling
#'
#' @inherit transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_billion
#'
#' @return Data frame in long format.
recycle_data_scenario_single <- function(df,
                                         scenario,
                                         billion = c("hep", "hpop", "uhc"),
                                         iso3 = "iso3",
                                         ind = "ind",
                                         value = "value",
                                         year = "year",
                                         type = "type",
                                         start_year = 2018,
                                         end_year = 2025,
                                         scenario_col = "scenario",
                                         default_scenario = "pre_covid_bau",
                                         scenario_reported_estimated = "none",
                                         scenario_tp = "tp",
                                         include_projection = TRUE,
                                         recycle_campaigns = TRUE,
                                         ind_ids = NULL) {
  assert_columns(df, iso3, ind, value, year, scenario_col, type)
  assert_unique_rows(df, ind, iso3, year, scenario = scenario_col, ind_ids)
  assert_scenario_in_df(df, c(scenario, scenario_reported_estimated, scenario_tp), scenario_col)

  billion <- rlang::arg_match(billion)
  if (is.null(ind_ids)) {
    ind_ids <- billion_ind_codes(billion)
  }

  assert_ind_ids(ind_ids, billion)

  default_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!default_scenario)

  reported_estimated_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario_reported_estimated)

  tp_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario_tp)

  scenario_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario)

  default_not_in_scenario <- dplyr::anti_join(default_df, scenario_df,
    by = c(iso3, ind, year)
  )

  reported_not_in_scenario <- dplyr::anti_join(reported_estimated_df, scenario_df,
    by = c(iso3, ind, year)
  )

  reported_not_in_default <- dplyr::anti_join(reported_not_in_scenario, default_not_in_scenario,
    by = c(iso3, ind, year)
  )

  tp_not_in_scenario <- dplyr::anti_join(tp_df, scenario_df,
    by = c(iso3, ind, year)
  )

  tp_not_in_default <- dplyr::anti_join(tp_not_in_scenario, default_not_in_scenario,
    by = c(iso3, ind, year)
  )

  not_in_scenario <- dplyr::bind_rows(default_not_in_scenario, reported_not_in_default) %>%
    dplyr::bind_rows(tp_not_in_default) %>%
    dplyr::mutate(recycled = TRUE)

  if (!include_projection) {
    not_in_scenario_projs <- default_not_in_scenario %>%
      dplyr::filter(!.data[[type]] %in% c("imputed", "projected"))

    not_in_scenario <- dplyr::bind_rows(not_in_scenario_projs, reported_not_in_default) %>%
      dplyr::bind_rows(tp_not_in_default) %>%
      dplyr::mutate(
        recycled = TRUE,
        !!sym(scenario_col) := scenario
      )
  }

  if (recycle_campaigns & billion == "hep") {
    not_in_scenario_campaigns <- not_in_scenario %>%
      dplyr::filter(
        stringr::str_detect(ind, "campaign"),
        .data[[type]] %in% c("reported", "estimated")
      )

    not_in_scenario_surviving_infants <- not_in_scenario %>%
      dplyr::filter(stringr::str_detect(ind, "surviving_infants")) %>%
      dplyr::anti_join(scenario_df,
        by = c(iso3, ind, year)
      )

    not_in_scenario_no_campaigns_no_surviving <- not_in_scenario %>%
      dplyr::anti_join(not_in_scenario_campaigns,
        by = c(iso3, ind, year)
      ) %>%
      dplyr::anti_join(not_in_scenario_surviving_infants,
        by = c(iso3, ind, year)
      )

    scenario_df_final <- scenario_df %>%
      dplyr::mutate(recycled = FALSE) %>%
      dplyr::bind_rows(not_in_scenario) %>%
      dplyr::filter(
        .data[[year]] >= start_year,
        .data[[ind]] %in% ind_ids
      ) %>%
      dplyr::bind_rows(not_in_scenario_campaigns) %>%
      dplyr::bind_rows(not_in_scenario_surviving_infants) %>%
      dplyr::distinct() %>%
      dplyr::mutate(!!sym(scenario_col) := !!scenario) %>%
      dplyr::arrange(iso3, ind, year)
  } else {
    scenario_df_final <- scenario_df %>%
      dplyr::mutate(recycled = FALSE) %>%
      dplyr::bind_rows(not_in_scenario) %>%
      dplyr::filter(
        .data[[year]] >= start_year,
        .data[[ind]] %in% ind_ids
      ) %>%
      dplyr::mutate(!!sym(scenario_col) := !!scenario) %>%
      dplyr::arrange(iso3, ind, year)
  }

  if (billion == "hpop") {
    assert_data_calculation_hpop(scenario_df_final,
      ind = ind,
      year = year,
      iso3 = iso3,
      value = value,
      scenario = scenario_col,
      start_year = start_year,
      end_year = end_year,
      ind_ids = ind_ids
    )
  } else if (billion == "uhc") {
    assert_data_calculation_uhc(scenario_df_final,
      ind = ind,
      year = year,
      iso3 = iso3,
      value = value,
      scenario = scenario_col,
      start_year = start_year,
      end_year = end_year,
      ind_ids = ind_ids
    )
  } else {
    assert_data_calculation_hep(scenario_df_final,
      ind = ind,
      year = year,
      iso3 = iso3,
      value = value,
      scenario = scenario_col,
      start_year = start_year,
      end_year = end_year,
      ind_ids = ind_ids
    )
  }

  return(scenario_df_final)
}
