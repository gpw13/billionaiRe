#' Calculate global/regional billions contributions
#'
#' Calculates global or regional sums for billions contributions for the UHC, HPOP, and HEP billions.
#'
#' full_data is expected to have the values of the billions calculations per
#' country already computed.
#' In this example, we're calculating the global contributions for the HPOP
#' billion for 2025.
#'
#' @param df A data frame
#' @param billion One of uhc, hpop, and hep. The billion for which we want to find
#'   global sums.
#' @param sum_years The year(s) for which we want to sum the billions contributions.
#' @param sum_type Allows you to select between global and regional sums.
#' @param source The source for the calculated billions. `WHO DDI calculations`,
#'   with `month, year` timestamp by default.
#' @param contribution Column name of column(s) to store contribution (population) values.
#' @inheritParams calculate_hpop_contributions
#'
#' @return A data frame with the global sums for the relevant billion. Does not
#'   include rows from the original data frame.
#' @export
calculate_contribution_sums = function(df,
                                       billion = c("uhc", "hpop", "hep"),
                                       sum_years,
                                       sum_type = c("global", "regional"),
                                       source = sprintf("WHO DDI calculation, %s", format(Sys.Date(), '%B %Y')),
                                       scenario = "scenario",
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       contribution = "contribution") {
  # Checks and assertions
  billion <- rlang::arg_match(billion)
  sum_type <- rlang::arg_match(sum_type)

  ind_code <- switch(billion,
                    uhc = "uhc_sm",
                    hpop = "hpop_healthier",
                    hep = "hep_idx")

  # Add WHO region to the grouping columns for regional sums
  if(sum_type == "global"){
    group_cols <- c(scenario, year)
  }else{
    group_cols <- c(scenario, year, "who_region")
  }

  df %>%
    dplyr::filter(.data[[ind]] == ind_code, .data[[year]] %in% sum_years) %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]], "who_region")) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) %>%
    dplyr::summarise(
      "{iso3}" := ifelse(sum_type == "global", "GLOBAL", .data[["who_region"]]),
      "{ind}" := ind_code,
      "{contribution}" := sum(.data[["contribution"]], na.rm = TRUE),
      "source" := .env$source,
      "type" := dplyr::case_when("projected" %in% .data[["type"]] ~ "projected",
                          "reported" %in% .data[["type"]] ~ "reported",
                          "estimated" %in% .data[["type"]] ~ "estimated",
                          TRUE ~ NA_character_),
      .groups = "drop"
    ) %>%
    dplyr::mutate("who_region" := NULL)
}
