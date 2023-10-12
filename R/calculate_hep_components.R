#' Calculate HEP component indicators
#'
#' Currently, the Prepare and DNR indicators are already calculated in the input
#' data, so only Prevent is calculated in this function. It takes the numerator
#' and denominator data from the inputs and calculates the vaccination coverage
#' per year and country for each relevant pathogen, as well as the overall Prevent
#' score. For DNR and its components, the level_col is calculated in this function.
#'
#' @inherit transform_hep_data return params
#' @inheritParams calculate_uhc_billion
#'
#' @param level_col Column name(s) to create to hold levels data. Should be same length
#'     as `transform_value_col`.
#' @param hepi_start_year First year to calculate HEP index for.
#' @param source Source to use if no unique source available for the calculation.
#'
#' @family hep
#'
#' @export
#'
calculate_hep_components <- function(df,
                                     scenario_col = NULL,
                                     transform_value_col = "transform_value",
                                     source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                     level_col = stringr::str_replace(transform_value_col, "transform_value", "level"),
                                     hepi_start_year = 2018,
                                     ind_ids = billion_ind_codes("hep", include_calculated = TRUE)) {
  assert_columns(df, "iso3", "ind", "year", transform_value_col, scenario_col)
  assert_same_length(transform_value_col, level_col)
  assert_unique_rows(df, scenario_col, ind_ids)

  df <- billionaiRe_add_columns(df, c("type", "source"), NA_character_)

  new_df <- df %>%
    dplyr::bind_rows(calculate_hepi(
      .,
      scenario_col,
      source,
      transform_value_col,
      hepi_start_year,
      ind_ids
    ))
  for (i in 1:length(level_col)) {
    new_df <- new_df %>%
      dplyr::mutate(!!sym(level_col[i]) := dplyr::case_when(
        !(stringr::str_detect(.data[["ind"]], paste0(ind_ids[c(
          "detect_respond",
          "detect",
          "notify",
          "respond",
          "prevent",
          "espar",
          "hep_idx"
        )], collapse = "|"))) ~ NA_real_,
        .data[[transform_value_col[i]]] < 30 ~ 1,
        .data[[transform_value_col[i]]] < 50 ~ 2,
        .data[[transform_value_col[i]]] < 70 ~ 3,
        .data[[transform_value_col[i]]] < 90 ~ 4,
        .data[[transform_value_col[i]]] >= 90 ~ 5
      ))
  }

  new_df
}

#' Calculate prevent indicators
#'
#' This function calculates the overall vaccine coverage scores for each pathogen,
#' including the overall prevent indicator. It primarily relies on using
#' `purrr::pmap_dfr()` to apply the `pathogen_calc()` function for each prevent
#' component and overall score.
#'
#' @inheritParams calculate_hep_components
#'
#' @export
calculate_hep_prevent_ind <- function(df,
                                      scenario_col = NULL,
                                      transform_value_col = "transform_value",
                                      source = sprintf("WHO DDI, %s", format(Sys.Date(), "%B %Y")),
                                      level_col = stringr::str_replace(transform_value_col, "transform_value", "level"),
                                      ind_ids = billion_ind_codes("hep", include_calculated = TRUE)) {

  assert_columns(df, "iso3", "ind", "year", transform_value_col, scenario_col)
  assert_same_length(transform_value_col, level_col)
  assert_unique_rows(df, scenario_col, ind_ids)

  df <- billionaiRe_add_columns(df, c("type", "source"), NA_character_)

  df <- dplyr::group_by(df, dplyr::across(c("iso3", "year", !!scenario_col)))

  args <- list(
    name = ind_ids[c(
      "meningitis", "meningitis_campaign", "yellow_fever", "yellow_fever_campaign", "cholera",
      "cholera_campaign", "polio", "measles", "measles_campaign", "covid",
      "covid_campaign", "ebola", "ebola_campaign", "prevent"
    )],
    numerator = list(
      ind_ids[c("meningitis_campaign_num", "meningitis_routine_num")],
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
      ind_ids[c(
        "meningitis_campaign_num", "meningitis_routine_num", "yellow_fever_campaign_num", "yellow_fever_routine_num",
        "cholera_campaign_num", "polio_routine_num", "measles_routine_num", "measles_campaign_num", "covid_campaign_num",
        "ebola_campaign_num"
      )]
    ),
    denominator = list(
      ind_ids[c("meningitis_campaign_denom", "surviving_infants")],
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
      ind_ids[c(
        "meningitis_campaign_denom", "yellow_fever_campaign_denom", "cholera_campaign_denom",
        "measles_campaign_denom", "ebola_campaign_denom", "covid_campaign_denom", "surviving_infants"
      )]
    ),
    multiply_surviving_infs = c(rep(FALSE, 13), TRUE),
    max_value = c(rep(Inf, 13), 100)
  )

  furrr::future_pmap_dfr(args,
    pathogen_calc,
    df = df,
    transform_value_col = transform_value_col,
    source = source,
    ind_ids = ind_ids
  )
}

#' Calculate the vaccine coverage for a specific pathogen
#'
#' Using numerators and denominators pre-supplied for each ISO3 and year,
#' the overall vaccine coverage for a pathogen is calculated. This function
#' currently counts the number of routine vaccinations included in the numerator,
#' and multiplies the surviving infants denominator by that number.
#'
#' This function is currently called from the `calculate_hep_prevent_ind()` function
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
#' @keywords internal
#'
pathogen_calc <- function(df,
                          name,
                          numerators,
                          denominators,
                          transform_value_col,
                          source,
                          ind_ids,
                          multiply_surviving_infs,
                          max_value) {
  df <- dplyr::filter(
    df,
    .data[["ind"]] %in% c(numerators, denominators),
    any(numerators %in% .data[["ind"]])
  )

  if (multiply_surviving_infs) {
    df <- dplyr::mutate(
      df,
      dplyr::across(
        !!transform_value_col,
        ~ dplyr::case_when(
          .data[["ind"]] == ind_ids["surviving_infants"] ~ .x * sum(unique(.data[["ind"]][!is.na(.x)]) %in% ind_ids[c("meningitis_routine_num", "yellow_fever_routine_num", "polio_routine_num", "measles_routine_num")]),
          TRUE ~ .x
        )
      )
    )
  }

  df %>%
    dplyr::summarize(
      !!sym("type") := reduce_type(.data[[transform_value_col[1]]], .data[["type"]]),
      dplyr::across(
        !!transform_value_col,
        ~ dplyr::case_when(
          all(is.na(.x[.data[["ind"]] %in% ind_ids[numerators]])) ~ NA_real_,
          TRUE ~ 100 * sum(.x[.data[["ind"]] %in% ind_ids[numerators]], na.rm = TRUE) /
            sum(.x[.data[["ind"]] %in% ind_ids[denominators]], na.rm = TRUE)
        )
      ),
      !!sym("ind") := !!name,
      !!sym("source") := ifelse(length(unique(.data[["source"]][!is.na(.data[["source"]])])) == 1,
        unique(.data[["source"]][!is.na(.data[["source"]])]),
        !!source
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(
      !!transform_value_col,
      pmin,
      !!max_value
    ))
}

#' Calculate HEPI
#'
#' Function to calculate HEPI as the average of DNR, Prepare, and Prevent.
#' Used within `calculate_hep_components()`.
#'
#' @inheritParams calculate_hep_components
#' @param earliest_year Earliest year for HEPI calculation.
#'
#' @keywords internal
#'
calculate_hepi <- function(df,
                           scenario_col,
                           source,
                           transform_value_col,
                           earliest_year,
                           ind_ids) {
  df %>%
    dplyr::filter(
      .data[["ind"]] %in% ind_ids[c(
        "detect_respond",
        "prevent",
        "espar"
      )],
      .data[["year"]] >= earliest_year
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "year", scenario_col)))) %>%
    dplyr::summarize(dplyr::across(
      !!transform_value_col,
      ~ mean(.x, na.rm = TRUE)
    ),
    !!sym("type") := ifelse(length(unique(.data[["type"]][!is.na(.data[["type"]])])) == 1,
      unique(.data[["type"]][!is.na(.data[["type"]])]),
      "projected"
    ),
    !!sym("source") := !!source,
    !!sym("ind") := "hep_idx",
    .groups = "drop"
    )
}
