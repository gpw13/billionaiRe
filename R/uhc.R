#' Transform Raw Indicator Values for UHC Billion
#'
#' `transform_uhc_data()` applies transformations on UHC Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report. Values in the transform column, if it already exists,
#' are replaced for UHC indicators that have data in the value column, otherwise
#' the column keeps its original data.
#'
#' For more details on the UHC Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/uhc.html}{\code{vignette("uhc", package = "billionaiRe")}}
#'
#' @inherit transform_hpop_data return params
#'
#' @export
transform_uhc_data <- function(df,
                               ind = "ind",
                               value = "value",
                               transform_glue = "transform_{value}",
                               ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, ind, value)
  assert_ind_ids(ind_ids, billion = "uhc")

  # get transform column names
  transform_value <- glue::glue(transform_glue)

  # transform each
  for (i in 1:length(value)) {
    df <- transform_uhc_single(df, ind, value[i], transform_value[i], ind_ids)
  }

  # generate Billions groups
  df %>%
    dplyr::mutate(billion_group = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("fp", "dtp3", "anc4", "pneumo")] ~ "RMNCH",
                    .data[[ind]] %in% ind_ids[c("tb", "art", "itn", "uhc_sanitation")] ~ "CDs",
                    .data[[ind]] %in% ind_ids[c("uhc_tobacco", "bp", "fpg")] ~ "NCDs",
                    .data[[ind]] %in% ind_ids[c("beds", "hwf", "espar")] ~ "SCA",
                    .data[[ind]] == ind_ids["fh"] ~ "FH"))
}

#' Perform a transformation on a single column
#'
#' This function is used within [transform_uhc_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams transform_uhc_data
#' @inheritParams transform_hpop_single
#'
#' @return A single column data frame of transformed values.
transform_uhc_single <- function(df,
                                 ind,
                                 value,
                                 transform_col,
                                 ind_ids) {

  # check if transform column in data and create if not
  if (!(transform_col %in% names(df))) {
    df[[transform_col]] <- NA_real_
  }
  dplyr::mutate(df,
                !!sym(transform_col) := dplyr::case_when(
                  is.na(.data[[value]]) ~ .data[[transform_col]],
                  .data[[ind]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "espar", "fh", "itn")] ~ trim_transforms(.data[[value]]),
                  .data[[ind]] == ind_ids["bp"] ~ transform_bp(.data[[value]]),
                  .data[[ind]] == ind_ids["fpg"] ~ transform_glucose(.data[[value]]),
                  .data[[ind]] == ind_ids["beds"] ~ transform_hosp_beds(.data[[value]]),
                  .data[[ind]] == ind_ids["uhc_tobacco"] ~ transform_inversion(.data[[value]]),
                  .data[[ind]] == ind_ids["hwf"] ~ transform_hwf(.data[[value]]),
                  TRUE ~ .data[[transform_col]]
                ))
}

#' Untransform Indicator Values for UHC Billion
#'
#' `untransform_uhc_data()` reverses transformations on UHC Billion indicators to
#' return raw indicator values. Details on the specific transformations applied
#' can be found within the Billions methods report.
#'
#' For more details on the UHC Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/uhc.html}{\code{vignette("uhc", package = "billionaiRe")}}
#'
#' @inherit untransform_hpop_data params return
#'
#' @export
untransform_uhc_data <- function(df,
                                 ind = "ind",
                                 transform_value = "transform_value",
                                 value = stringr::str_remove(transform_value, "transform_"),
                                 ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, ind, transform_value)
  assert_string(value, length(transform_value))
  assert_ind_ids(ind_ids, "uhc")

  for (i in 1:length(value)) {
    df <- untransform_uhc_single(df, ind, transform_value[i], value[i], ind_ids)
  }

  df
}

#' Perform a transformation on a single column
#'
#' This function is used within [untransform_uhc_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams untransform_uhc_data
#'
#' @return A single column data frame of transformed values.
untransform_uhc_single <- function(df,
                                   ind,
                                   transform_value,
                                   value,
                                   ind_ids) {

  # check if transform column in data and create if not
  if (!(value %in% names(df))) {
    df[[value]] <- NA_real_
  }

  df %>%
    dplyr::mutate(!!sym(value) := dplyr::case_when(
      is.na(.data[[transform_value]]) ~ .data[[value]],
      .data[[ind]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "espar", "fh", "itn")] ~ .data[[transform_value]],
      .data[[ind]] %in% ind_ids["uhc_tobacco"] ~ transform_inversion(.data[[value]]),
      .data[[ind]] == ind_ids["bp"] ~ untransform_bp(.data[[transform_value]]),
      .data[[ind]] == ind_ids["fpg"] ~ untransform_glucose(.data[[transform_value]]),
      .data[[ind]] == ind_ids["beds"] ~ untransform_hosp_beds(.data[[transform_value]]),
      .data[[ind]] == ind_ids["hwf"] ~ untransform_hwf(.data[[transform_value]]),
      TRUE ~ .data[[value]]
    ))
}


#' Calculate UHC Billion
#'
#' `calculate_uhc_billion()` calculates country-level UHC Billion based on
#' indicator level data. Calculates it for each country-year combination in the provided data.
#'
#' @param billion_group Column name of column indicating UHC Billion group that
#'    each indicator belongs to.
#'
#' @inherit calculate_hpop_contributions params
#' @inherit transform_uhc_data return details params
#'
#' @export
calculate_uhc_billion <- function(df,
                                  year = "year",
                                  iso3 = "iso3",
                                  ind = "ind",
                                  billion_group = "billion_group",
                                  transform_value = "transform_value",
                                  ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, year, iso3, ind, billion_group, transform_value)
  assert_ind_ids(ind_ids, "uhc")
  assert_unique_rows(df, ind, iso3, year, ind_ids)

  df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[!(ind_ids %in% c(ind_ids["nurses"], ind_ids["doctors"]))]) %>% # nurses doctors already aggregated to hwf
    dplyr::group_by(.data[[year]], .data[[iso3]], .data[[billion_group]]) %>%
    dplyr::summarize(!!sym(transform_value) := mean(.data[[transform_value]], na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = billion_group,
                       values_from = transform_value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate("ASC" := mean(dplyr::c_across(c("CDs", "NCDs", "RMNCH", "SCA")),
                             na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("UHC" := .data[["ASC"]] * (100 - .data[["FH"]]) / 100) %>%
    dplyr::select(-dplyr::any_of(c("CDs", "NCDs", "RMNCH", "SCA"))) %>%
    tidyr::pivot_longer(c("FH", "ASC", "UHC"),
                        names_to = "ind")
}

#' Calculate UHC Billion
#'
#' `calculate_uhc_billion()` calculates country-level UHC Billion based on
#' indicator level data. Calculates it for each country-year combination in the provided data.
#'
#' @inherit calculate_uhc_billion return details params
#' @inherit calculate_hpop_contributions params
#'
#' @export
calculate_uhc_contribution <- function(df,
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       start_year = 2018,
                                       end_year = 2023) {
  assert_columns(df, year, iso3, ind)

  df %>%
    dplyr::filter(.data[[year]] %in% c(start_year, end_year)) %>%
    tidyr::pivot_wider(names_from = year) %>%
    dplyr::mutate(population = wppdistro::get_population(.data[[iso3]], year = end_year),
                  contribution = (.data[[as.character(end_year)]] - .data[[as.character(start_year)]]) * .data[["population"]] / 100)
}
