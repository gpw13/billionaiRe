#' Transform Raw Indicator Values for UHC Billion
#'
#' `transform_uhc_data()` applies transformations on UHC Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report.
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
                               ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, ind, value)

  df %>%
    dplyr::mutate(transform_value = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "ihr", "fh", "itn")] ~ .data[[value]],
                    .data[[ind]] == ind_ids["bp"] ~ transform_bp(.data[[value]]),
                    .data[[ind]] == ind_ids["fpg"] ~ transform_glucose(.data[[value]]),
                    .data[[ind]] == ind_ids["beds"] ~ transform_hosp_beds(.data[[value]]),
                    .data[[ind]] == ind_ids["uhc_tobacco"] ~ reverse_ind(.data[[value]]),
                    .data[[ind]] == ind_ids["hwf"] ~ transform_hwf(.data[[value]])
                  ),
                  billion_group = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("fp", "dtp3", "anc4", "pneumo")] ~ "RMNCH",
                    .data[[ind]] %in% ind_ids[c("tb", "art", "itn", "uhc_sanitation")] ~ "CDs",
                    .data[[ind]] %in% ind_ids[c("uhc_tobacco", "bp", "fpg")] ~ "NCDs",
                    .data[[ind]] %in% ind_ids[c("beds", "hwf", "ihr")] ~ "SCA",
                    .data[[ind]] == ind_ids["fh"] ~ "FH"))
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

  df %>%
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
                  contribution = (.data[[as.character(end_year)]] - .data[[as.character(start_year)]]) * .data[["population"]] / 100,
                  contribution = ifelse(.data[[ind]] == "FH", -.data[["contribution"]], .data[["contribution"]]))
}
