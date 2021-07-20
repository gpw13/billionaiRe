#' Calculate HEP component indicators
#'
#' Currently, the Prepare and DNR indicators are already calculated in the input
#' data, so only Prevent is calculated in this function. It takes the numerator
#' and denominator data from the inputs and calculates the vaccination coverage
#' per year and country for each relevant pathogen, as well as the overall Prevent
#' score. For DNR and its components, the level is calculated in this function.
#'
#' @inherit transform_hep_data return params
#' @inheritParams calculate_uhc_billion
#'
#' @param level Column name(s) to create to hold levels data. Should be same length
#'     as `transform_value`.
#' @param hepi_start_year First year to calculate HEP index for.
#' @param source Source to use if no unique source available for the calculation.
#'
#' @export
calculate_hep_components <- function(df,
                                     iso3 = "iso3",
                                     year = "year",
                                     ind = "ind",
                                     scenario = NULL,
                                     transform_value = "transform_value",
                                     type_col = "type",
                                     source_col = "source",
                                     source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                     level = stringr::str_replace(transform_value, "transform_value", "level"),
                                     hepi_start_year = 2018,
                                     ind_ids = billion_ind_codes("hep")) {
  assert_columns(df, iso3, ind, year, transform_value, scenario)
  assert_same_length(transform_value, level)
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)

  df <- billionaiRe_add_columns(df, c(type_col, source_col), NA_character_)

  new_df <- df %>%
    dplyr::bind_rows(prevent_calculations(.,
                                          iso3,
                                          year,
                                          scenario,
                                          ind,
                                          type_col,
                                          source_col,
                                          source,
                                          transform_value,
                                          ind_ids)) %>%
    dplyr::bind_rows(calculate_hepi(.,
                                    iso3,
                                    year,
                                    scenario,
                                    ind,
                                    type_col,
                                    source_col,
                                    source,
                                    transform_value,
                                    hepi_start_year,
                                    ind_ids))
  for (i in 1:length(level)) {
    new_df <- new_df %>%
      dplyr::mutate(!!sym(level[i]) := dplyr::case_when(
        !(.data[[ind]] %in% ind_ids[c("detect_respond",
                                      "detect",
                                      "notify",
                                      "respond",
                                      "prevent",
                                      "espar",
                                      "hep_idx")]) ~ NA_real_,
        .data[[transform_value[i]]] < 30 ~ 1,
        .data[[transform_value[i]]] < 50 ~ 2,
        .data[[transform_value[i]]] < 70 ~ 3,
        .data[[transform_value[i]]] < 90 ~ 4,
        .data[[transform_value[i]]] >= 90 ~ 5
      ))
  }

  new_df
}

#' Calculate prevent indicators
#'
#' This function calculates the overall vaccine coverage scores for each pathogen,
#' including the overall prevent indicator. It is used within `calculate_hep_components()`
#' and primarily relies on using `purrr::pmap_dfr()` to apply the `pathogen_calc()`
#' function for each prevent component and overall score.
#'
#' @inheritParams calculate_hep_components
prevent_calculations <- function(df,
                                 iso3,
                                 year,
                                 scenario,
                                 ind,
                                 type_col,
                                 source_col,
                                 source,
                                 transform_value,
                                 ind_ids) {

  df <- dplyr::group_by(df, dplyr::across(c(!!iso3, !!year, !!scenario)))

  args <- list(name = ind_ids[c("meningitis", "meningitis_campaign", "yellow_fever", "yellow_fever_campaign", "cholera",
                                "cholera_campaign", "polio", "measles", "measles_campaign", "covid",
                                "covid_campaign", "ebola", "ebola_campaign", "prevent")],
               numerator = list(ind_ids[c("meningitis_campaign_num", "meningitis_routine_num")],
                                ind_ids[c("meningitis_campaign_num")],
                                ind_ids[c("yellow_fever_campaign_num", "yellow_fever_routine_num")],
                                ind_ids[c("yellow_fever_campaign_num")],
                                ind_ids[c("cholera_campaign_num")],
                                ind_ids[c("cholera_campaign_num")],
                                ind_ids[c("polio_routine_num")],
                                ind_ids[c("measles_routine_num", "measles_campaign_num")],
                                ind_ids[c("measles_campaign_num")],
                                ind_ids[c("covid_campaign_num")],
                                ind_ids[c("covid_campaign_num")],
                                ind_ids[c("ebola_campaign_num")],
                                ind_ids[c("ebola_campaign_num")],
                                ind_ids[c("meningitis_campaign_num", "meningitis_routine_num", "yellow_fever_campaign_num", "yellow_fever_routine_num",
                                          "cholera_campaign_num", "polio_routine_num", "measles_routine_num", "measles_campaign_num", "covid_campaign_num",
                                          "ebola_campaign_num")]),
               denominator = list(ind_ids[c("meningitis_campaign_denom", "surviving_infants")],
                                  ind_ids[c("meningitis_campaign_denom")],
                                  ind_ids[c("yellow_fever_campaign_denom", "surviving_infants")],
                                  ind_ids[c("yellow_fever_campaign_denom")],
                                  ind_ids[c("cholera_campaign_denom")],
                                  ind_ids[c("cholera_campaign_denom")],
                                  ind_ids[c("surviving_infants")],
                                  ind_ids[c("surviving_infants", "measles_campaign_denom")],
                                  ind_ids[c("measles_campaign_denom")],
                                  ind_ids[c("covid_campaign_denom")],
                                  ind_ids[c("covid_campaign_denom")],
                                  ind_ids[c("ebola_campaign_denom")],
                                  ind_ids[c("ebola_campaign_denom")],
                                  ind_ids[c("meningitis_campaign_denom", "yellow_fever_campaign_denom", "cholera_campaign_denom",
                                            "measles_campaign_denom", "ebola_campaign_denom", "covid_campaign_denom", "surviving_infants")]),
               multiply_surviving_infs = c(rep(FALSE, 13), TRUE),
               max_value = c(rep(Inf, 13), 100))

  purrr::pmap_dfr(args,
                  pathogen_calc,
                  df = df,
                  ind = ind,
                  iso3 = iso3,
                  year = year,
                  transform_value = transform_value,
                  type_col = type_col,
                  source_col = source_col,
                  source = source,
                  ind_ids = ind_ids)
}

#' Calculate the vaccine coverage for a specific pathogen
#'
#' Using numerators and denominators pre-supplied for each ISO3 and year,
#' the overall vaccine coverage for a pathogen is calculated. This function
#' currently counts the number of routine vaccinations included in the numerator,
#' and multiplies the surviving infants denominator by that number.
#'
#' This function is currently called from the `prevent_calculations()` function
#' that sits within `calculate_hep_components()`.
#'
#' @inheritParams calculate_hep_components
#' @param name Name of pathogen to provide in the data frame.
#' @param numerators Indicator names for numerators.
#' @param denominators Indicator names for denominators.
#' @param multiply_surviving_infs Logical, multiple surviving infant population by
#'     number of routine vaccines in numerator.
#' @param max_value Maximum value the calculated pathogen value can take.
#'
pathogen_calc <- function(df,
                          name,
                          ind,
                          iso3,
                          year,
                          numerators,
                          denominators,
                          transform_value,
                          type_col,
                          source_col,
                          source,
                          ind_ids,
                          multiply_surviving_infs,
                          max_value) {
  df <- dplyr::filter(df,
                      .data[[ind]] %in% c(numerators, denominators),
                      any(numerators %in% .data[[ind]]))

  if (multiply_surviving_infs) {
    df <- dplyr::mutate(df,
                        dplyr::across(!!transform_value,
                                      ~ dplyr::case_when(
                                        .data[[ind]] == ind_ids["surviving_infants"] ~ .x * sum(unique(.data[[ind]][!is.na(.x)]) %in% ind_ids[c("meningitis_routine_num", "yellow_fever_routine_num", "polio_routine_num", "measles_routine_num")]),
                                        TRUE ~ .x
                                      )))
  }

  df %>%
    dplyr::summarize(
      !!sym(type_col) := reduce_type(.data[[transform_value[1]]], .data[[type_col]]),
      dplyr::across(!!transform_value,
                    ~dplyr::case_when(
                      all(is.na(.x[.data[[ind]] %in% ind_ids[numerators]])) ~ NA_real_,
                      TRUE ~ 100 * sum(.x[.data[[ind]] %in% ind_ids[numerators]], na.rm = TRUE) /
                        sum(.x[.data[[ind]] %in% ind_ids[denominators]], na.rm = TRUE)
                    )),
      !!sym(ind) := !!name,
      !!sym(source_col) := ifelse(length(unique(.data[[source_col]][!is.na(.data[[source_col]])])) == 1,
                                  unique(.data[[source_col]][!is.na(.data[[source_col]])]),
                                  !!source),
      .groups = "drop") %>%
    dplyr::mutate(dplyr::across(!!transform_value,
                                pmin,
                                !!max_value))
}

#' Calculate HEPI
#'
#' Function to calculate HEPI as the average of DNR, Prepare, and Prevent.
#' Used within `calculate_hep_components()`.
#'
#' @inheritParams calculate_hep_components
#' @param earliest_year Earliest year for HEPI calculation.
calculate_hepi <- function(df,
                           iso3,
                           year,
                           scenario,
                           ind,
                           type_col,
                           source_col,
                           source,
                           transform_value,
                           earliest_year,
                           ind_ids) {
  df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[c("detect_respond",
                                              "prevent",
                                              "espar")],
                  .data[[year]] >= earliest_year) %>%
    dplyr::group_by(dplyr::across(c(iso3, year, scenario))) %>%
    dplyr::summarize(dplyr::across(!!transform_value,
                                   ~ mean(.x, na.rm = TRUE)),
                     !!sym(type_col) := ifelse(length(unique(.data[[type_col]][!is.na(.data[[type_col]])])) == 1,
                                           unique(.data[[type_col]][!is.na(.data[[type_col]])]),
                                           "projected"),
                     !!sym(source_col) := !!source,
                     !!sym(ind) := "hep_idx",
                     .groups = "drop")
}
