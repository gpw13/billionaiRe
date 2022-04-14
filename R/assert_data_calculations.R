#' Assert presence of minimum HEP data
#'
#' `assert_data_calculation_hep` asserts that the minimum data required to run
#' HEP calculations is present. This means checking that `surviving_infants` is
#' present for all years where routine prevent indicators are present, and that
#' `espar` `start_year` is not NA. Otherwise, errors will be returned by
#' calculation functions.
#'
#' The function returns warnings
#' if `espar` and `detect_respond` `start_year` and `end_year` are not present
#' in the data frame. Not having those values will significantly impact the
#' billion calculations as `hep_idx` will not include them at all.
#'
#' Warnings are also returned if some country (and scenario if provided) have
#' only NAs.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams calculate_hpop_contributions
#'
#' @return Data frame in long format.
assert_data_calculation_hep <- function(df,
                                        value = "value",
                                        scenario = NULL,
                                        start_year = 2018,
                                        end_year = 2025,
                                        ind_ids = billion_ind_codes("hep")) {
  assert_iso3_not_empty(df, "iso3", scenario, value)

  # Prevent

  pathogens <- c("meningitis", "yellow_fever", "cholera", "polio", "measles", "covid", "ebola")

  patho_ind <- ind_ids[stringr::str_detect(ind_ids, paste0(pathogens, collapse = "|"))]

  patho_df <- df %>%
    dplyr::filter(.data[["ind"]] %in% patho_ind)

  if (nrow(patho_df) == 0) {
    warning("No pathogens indicator was provided. Prevent indicator will not be calculated.")
  }

  # eSPAR

  espar_ind <- ind_ids["espar"]

  espar_df_2018 <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "year", scenario)))) %>%
    dplyr::filter(
      .data[["ind"]] == espar_ind,
      .data[["year"]] %in% start_year,
      is.na(.data[[value]])
    ) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario)))

  if (nrow(espar_df_2018) > 0) {
    stop(sprintf(
      "%s must be present in %s for at least the start_year, for each country (and scenario when provided)
      Missing values in:\n",
      espar_ind[espar_ind %in% espar_df_2018[["ind"]]], paste("df", collapse = ", ")
    ), paste(utils::capture.output(print(espar_df_2018)), collapse = "\n"),
    call. = FALSE
    )
  }

  espar_df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "year", scenario)))) %>%
    dplyr::filter(
      .data[["ind"]] == espar_ind,
      .data[["year"]] %in% end_year,
      is.na(.data[[value]])
    ) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario)))

  if (nrow(espar_df) > 0) {
    warning(sprintf(
      "%s must be present in %s for at least the start_year and end_year, for each country (and scenario when provided)
      Missing values in:\n",
      espar_ind[espar_ind %in% espar_df[["ind"]]], paste("df", collapse = ", ")
    ), paste(utils::capture.output(print(espar_df)), collapse = "\n"),
    call. = FALSE
    )
  }

  # Detect and Respond

  detect_respond_ind <- ind_ids[stringr::str_detect(ind_ids, "detect_respond$")]

  detect_respond_df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "year", scenario)))) %>%
    dplyr::filter(
      .data[["ind"]] == detect_respond_ind,
      .data[["year"]] == end_year,
      is.na(.data[[value]])
    ) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario)))

  if (nrow(detect_respond_df) > 0) {
    warning(sprintf(
      "%s must be present in %s for at least the start_year and end_year, for each country (and scenario when provided)
      Missing values in %s:\n",
      detect_respond_ind[detect_respond_ind %in% detect_respond_df[["ind"]]],
      paste("df", collapse = ", "),
      paste(unique(detect_respond_df[["iso3"]]), collapse = ",")
    ), paste(utils::capture.output(print(detect_respond_df)), collapse = "\n"),
    call. = FALSE
    )
  }

  return(df)
}

#' Assert presence of minimum HPOP data
#'
#' Assert that the minimum data required to run HPOP calculations is
#' present.
#'
#' The function returns warnings
#' if `start_year` and `end_year` are not present in `year` column
#' in the data frame. Not having those values will significantly impact the
#' billion calculations as `hpop_healthier` will return 0.
#'
#' Warnings are also returned if some country (and scenario if provided) have
#' only NAs.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams calculate_hpop_contributions
#'
#' @return Data frame in long format.

assert_data_calculation_hpop <- function(df,
                                         value = "value",
                                         scenario = NULL) {
  assert_iso3_not_empty(df, "iso3", scenario, value)

  return(df)
}

#' Assert minimum data for UHC calculations
#'
#' `assert_data_calculation_uhc` asserts that the minimum data required to run UHC calculations is
#' present.
#'
#' The function returns warnings
#' if `start_year` and `end_year` are not present in `year` column
#' in the data frame. Not having those values will significantly impact the
#' billion calculations as `hpop_healthier` will return 0.
#'
#' Warnings are also returned if some country (and scenario if provided) have
#' only NAs.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams calculate_hpop_contributions
#'
#' @return Data frame in long format.

assert_data_calculation_uhc <- function(df,
                                        value = "value",
                                        scenario = NULL,
                                        start_year = 2018,
                                        end_year = 2025,
                                        ind_ids = billion_ind_codes("uhc")) {
  assert_iso3_not_empty(df, "iso3", scenario, value)

  necessary_inds <- ind_ids[!ind_ids %in% c(ind_ids["nurses"], ind_ids["doctors"], ind_ids["itn"])]

  those_necessary_inds <- necessary_inds[necessary_inds %in% unique(df[["ind"]])]

  only_full <- df %>%
    dplyr::filter(.data[["ind"]] %in% those_necessary_inds) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", scenario, "ind")))) %>%
    dplyr::filter(is.na(.data[[value]]))

  if (nrow(only_full) > 0) {
    stop(sprintf(
      "%s have missing values in at least one `iso3`, `year`, and `ind` (and `scenario`, if provided).
UHC requires full time series.
Missing values in:\n",
      paste(unique(only_full[["iso3"]]), collapse = ", ")
    ),
    paste(utils::capture.output(print(only_full)), collapse = "\n"),
    call. = FALSE
    )
  }

  assert_ind_start_end_year(df, value, start_year, end_year, "ind",
    ind_ids = those_necessary_inds, scenario
  )

  return(df)
}

assert_data_contributions <- function(df,
                                      ind = "ind",
                                      year = "year",
                                      iso3 = "iso3",
                                      value = "value",
                                      scenario = NULL,
                                      start_year = 2018,
                                      end_year = 2025,
                                      billion = c("hep", "hpop", "uhc"),
                                      ind_ids = billion_ind_codes(billion, include_calculated = TRUE)) {
  billion <- rlang::arg_match(billion)

  df_ind <- df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids)

  df_ind_start_end_years <- df_ind %>%
    dplyr::filter(.data[[year]] %in% c(start_year, end_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario, ind)))) %>%
    dplyr::tally() %>%
    dplyr::filter(.data[["n"]] < 2)

  if (nrow(df_ind_start_end_years) > 1) {
    stop(sprintf(
      "%s have missing values at `start_year` or `end_year` in at least one `iso3` and `ind` (and `scenario`, if provided).
                    Contributions cannot be calculated",
      paste(unique(df_ind_start_end_years[[iso3]]), collapse = ",")
    ),
    call. = FALSE
    )
  }
}
