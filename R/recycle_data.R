#' Recycle data between scenarios
#'
#' `recycle_data()` recycles data between the scenarios present in `df` to reduce
#' size of tables stored. The function wraps around
#' `recycle_data_scenario_single()` for all the scenarios present in the
#' `scenario_col` column.
#'
#' `make_default_scenario()` wraps around `recycle_data_scenario_single()` to
#' create a default scenario based on the parameters passed to the function.
#'
#' `recycle_data_scenario_single()` reuses values present in the specified
#' scenarios in `default_scenario`, `scenario_reported_estimated`,
#' `scenario_covid_shock` and `scenario_reference_infilling` for the specified
#' scenarios.
#'
#' To do so, it looks at:
#'
#' 1. values in `default_scenario` but not in the scenario specified
#' 2. values in `scenario_reported_estimated` or `scenario_covid_shock` but not
#' in the scenario specified or `default_scenario`.
#' 3. values in `scenario_reference_infilling` but not in the scenario specified,
#' `scenario_reported_estimated`, `scenario_covid_shock`, or
#' `scenario_reference_infilling`
#'
#' For more information see:
#'
#' \code{vignette("scenarios", package = "billionaiRe")}
#'
#' @param scenario name of scenario to recycle for.
#' @param scenario_col Column name of column with scenario identifiers.
#' @param billion name of billion to recycle data for.
#' @param default_scenario name of the default scenario.
#' @param scenario_reported_estimated name of the reported/estimated scenario.
#' @param scenario_reference_infilling name of the WHO technical programs projections/imputations scenario.
#' @param scenario_covid_shock name of the scenario with the COVID-19 shock years.
#' @param include_projection Boolean to include or not projections in recycling
#' @param recycle_campaigns Boolean to include or not campaigns in recycling
#' @param assert_data_calculations Boolean if true then output data frame will
#' be tested to see if it contains the minimal required data to run the
#' calculations.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_billion
#' @inheritParams trim_years
#' @inheritParams trim_values
#'
#' @rdname recycle_data
#'
#' @family recycle_data
#'
#' @export
recycle_data <- function(df,
                         billion = c("hep", "hpop", "uhc"),
                         value_col = "value",
                         start_year = 2018,
                         end_year = 2025,
                         scenario_col = "scenario",
                         default_scenario = "default",
                         scenario_reported_estimated = "routine",
                         scenario_covid_shock = "covid_shock",
                         scenario_reference_infilling = "reference_infilling",
                         include_projection = TRUE,
                         recycle_campaigns = TRUE,
                         ind_ids = NULL,
                         trim_years = TRUE,
                         start_year_trim = start_year,
                         end_year_trim = end_year) {
  assert_columns(df, "iso3", "ind", "year",value_col, scenario_col, "type")
  assert_unique_rows(df, scenario_col = scenario_col, ind_ids)

  billion <- rlang::arg_match(billion)
  if (is.null(ind_ids)) {
    ind_ids <- billion_ind_codes(billion)
  }

  scenarios_recycle <- unique(df[[scenario_col]])

  purrr::map_dfr(
    scenarios_recycle,
    ~ recycle_data_scenario_single(
      df = df,
      scenario = .x,
      billion = billion,
      value_col = value_col,
      start_year = start_year,
      end_year = end_year,
      scenario_col = scenario_col,
      default_scenario = default_scenario,
      scenario_reported_estimated = scenario_reported_estimated,
      scenario_covid_shock = scenario_covid_shock,
      scenario_reference_infilling = scenario_reference_infilling,
      include_projection = include_projection,
      recycle_campaigns = recycle_campaigns,
      ind_ids = ind_ids,
      trim_years = trim_years,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim
    )
  )
}

#' @rdname recycle_data
#'
recycle_data_scenario_single <- function(df,
                                         scenario,
                                         billion = c("hep", "hpop", "uhc"),
                                         value_col = "value",
                                         start_year = 2018,
                                         end_year = 2025,
                                         scenario_col = "scenario",
                                         default_scenario = "default",
                                         scenario_reported_estimated = "routine",
                                         scenario_covid_shock = "covid_shock",
                                         scenario_reference_infilling = "reference_infilling",
                                         include_projection = TRUE,
                                         recycle_campaigns = TRUE,
                                         ind_ids = NULL,
                                         trim_years = FALSE,
                                         start_year_trim = start_year,
                                         end_year_trim = end_year,
                                         assert_data_calculations = TRUE) {
  assert_columns(df, scenario_col, value_col, "iso3", "ind", "year", "type")
  assert_unique_rows(df, scenario_col = scenario_col, ind_ids)

  billion <- rlang::arg_match(billion)
  if (is.null(ind_ids)) {
    ind_ids <- billion_ind_codes(billion)
  }

  if (scenario %in% c(scenario_reported_estimated, scenario_covid_shock, scenario_reference_infilling)) {
    df_no_recycling <- df %>%
      dplyr::filter(.data[[scenario_col]] == !!scenario)
    return(df_no_recycling)
  }

  assert_ind_ids(ind_ids, billion)

  scenario_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario)

  # Get unique default

  default_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!default_scenario)

  default_not_in_scenario <- dplyr::anti_join(default_df, scenario_df,
                                              by = c("iso3", "ind", "year")
  )

  # Get unique reported

  reported_estimated_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario_reported_estimated)

  reported_not_in_scenario <- dplyr::anti_join(reported_estimated_df, scenario_df,
                                               by = c("iso3", "ind", "year")
  )

  reported_not_in_default <- dplyr::anti_join(reported_not_in_scenario, default_not_in_scenario,
                                              by = c("iso3", "ind", "year")
  )

  # Get unique covid

  covid_shock_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario_covid_shock)

  covid_shock_not_in_scenario <- dplyr::anti_join(covid_shock_df, scenario_df,
                                                  by = c("iso3", "ind", "year")
  )

  covid_shock_not_in_default <- dplyr::anti_join(covid_shock_not_in_scenario, default_not_in_scenario,
                                                 by = c("iso3", "ind", "year")
  )

  covid_shock_not_in_reported <- dplyr::anti_join(covid_shock_not_in_default, reported_not_in_default,
                                                 by = c("iso3", "ind", "year")
  )

  # Get unique reference

  reference_infilling_df <- df %>%
    dplyr::filter(.data[[scenario_col]] == !!scenario_reference_infilling)

  reference_infilling_not_in_scenario <- dplyr::anti_join(
    reference_infilling_df, scenario_df,
    by = c("iso3", "ind", "year")
  )

  reference_infilling_not_in_default <- dplyr::anti_join(reference_infilling_not_in_scenario, default_not_in_scenario,
                                                         by = c("iso3", "ind", "year")
  )

  reference_infilling_not_in_reported <- dplyr::anti_join(reference_infilling_not_in_default, reported_not_in_default,
                                                          by = c("iso3", "ind", "year")
  )

  reference_infilling_not_in_covid_shock <- dplyr::anti_join(
    reference_infilling_not_in_reported, covid_shock_not_in_reported,
    by = c("iso3", "ind", "year")
  )

  not_in_scenario <- dplyr::bind_rows(default_not_in_scenario,
                                      reported_not_in_default,
                                      reference_infilling_not_in_covid_shock,
                                      covid_shock_not_in_reported
  ) %>%
    dplyr::mutate(recycled = TRUE)

  if (!include_projection) {
    not_in_scenario_projs <- default_not_in_scenario %>%
      dplyr::filter(!.data[["type"]] %in% c("imputed", "projected"))

    not_in_scenario <- dplyr::bind_rows(not_in_scenario_projs, reported_not_in_default) %>%
      dplyr::bind_rows(reference_infilling_not_in_covid_shock, covid_shock_not_in_default) %>%
      dplyr::mutate(
        recycled = TRUE,
        !!sym(scenario_col) := scenario
      )
  }

  if (recycle_campaigns & billion == "hep") {
    not_in_scenario_campaigns <- not_in_scenario %>%
      dplyr::filter(
        stringr::str_detect(.data[["ind"]], "campaign"),
        .data[["type"]] %in% c("reported", "estimated")
      )

    not_in_scenario_no_campaigns <- not_in_scenario %>%
      dplyr::anti_join(not_in_scenario_campaigns,
                       by = c("iso3", "ind", "year")
      )

    if (trim_years) {
      not_in_scenario <- not_in_scenario %>%
        trim_years(trim_years, start_year_trim, end_year_trim)
    }

    scenario_df_final <- scenario_df %>%
      dplyr::mutate(recycled = FALSE) %>%
      dplyr::bind_rows(not_in_scenario) %>%
      dplyr::bind_rows(not_in_scenario_campaigns) %>%
      dplyr::bind_rows(not_in_scenario_no_campaigns) %>%
      dplyr::distinct() %>%
      dplyr::mutate(!!sym(scenario_col) := !!scenario) %>%
      dplyr::arrange("iso3", "ind", "year") %>%
      dplyr::filter(.data[["ind"]] %in% ind_ids)
  } else {
    if (trim_years) {
      not_in_scenario <- not_in_scenario %>%
        dplyr::filter(
          .data[["year"]] >= start_year
        )
    }

    scenario_df_final <- scenario_df %>%
      dplyr::mutate(recycled = FALSE) %>%
      dplyr::bind_rows(not_in_scenario) %>%
      dplyr::mutate(!!sym(scenario_col) := !!scenario) %>%
      dplyr::arrange("iso3", "ind", "year") %>%
      dplyr::filter(.data[["ind"]] %in% ind_ids)
  }

  if(assert_data_calculations){
    if (billion == "hpop") {
      assert_data_calculation_hpop(scenario_df_final,
                                   value_col = value_col,
                                   scenario_col = scenario_col
      )
    } else if (billion == "uhc") {
      assert_data_calculation_uhc(scenario_df_final,
                                  value_col = value_col,
                                  scenario_col = scenario_col,
                                  start_year = start_year,
                                  end_year = end_year,
                                  ind_ids = ind_ids
      )
    } else {
      assert_data_calculation_hep(scenario_df_final,
                                  value_col = value_col,
                                  scenario_col = scenario_col,
                                  start_year = start_year,
                                  end_year = end_year,
                                  ind_ids = ind_ids
      )
    }
  }


  return(scenario_df_final)
}

#' @rdname recycle_data
#'
#' @export
make_default_scenario <- function(df,
                                  scenario = "default",
                                  billion = c("all", "hep", "hpop", "uhc"),
                                  value_col = "value",
                                  start_year = 2018,
                                  end_year = 2025,
                                  scenario_col = "scenario",
                                  default_scenario = "default",
                                  scenario_reported_estimated = "routine",
                                  scenario_covid_shock = "covid_shock",
                                  scenario_reference_infilling = "reference_infilling",
                                  include_projection = TRUE,
                                  recycle_campaigns = TRUE,
                                  ind_ids = NULL,
                                  trim_years = FALSE,
                                  start_year_trim = start_year,
                                  end_year_trim = end_year,
                                  assert_data_calculations = TRUE) {
  assert_columns(df, "iso3", "ind", value_col, "year", scenario_col, "type")
  assert_unique_rows(df, scenario_col, ind_ids)

  base_scenarios <- c(scenario_reported_estimated, scenario_reference_infilling)

  if (is.null(billion)) {
    billion <- "all"
  }

  billion <- rlang::arg_match(billion)

  if (billion == "all") {
    billion <- c("hep", "hpop", "uhc")
  }

  ind_ids <- purrr::map(billion, billion_ind_codes) %>%
    stats::setNames(billion)

  purrr::map_dfr(
    billion,
    ~ recycle_data_scenario_single(
      df = df,
      scenario = scenario,
      billion = .x,
      value_col = value_col,
      start_year = start_year,
      end_year = end_year,
      scenario_col = scenario_col,
      default_scenario = default_scenario,
      scenario_reported_estimated = scenario_reported_estimated,
      scenario_covid_shock = scenario_covid_shock,
      scenario_reference_infilling = scenario_reference_infilling,
      include_projection = include_projection,
      recycle_campaigns = recycle_campaigns,
      ind_ids = ind_ids[[.x]],
      trim_years = trim_years,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim,
      assert_data_calculations = assert_data_calculations
    )
  ) %>%
    dplyr::bind_rows(df) %>%
    dplyr::mutate(recycled = dplyr::case_when(
      is.na(.data[["recycled"]]) | .data[["recycled"]] == FALSE ~ FALSE,
      TRUE ~ TRUE
    )) %>%
    dplyr::distinct()
}
