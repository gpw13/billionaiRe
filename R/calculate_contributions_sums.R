#' Calculate global/regional billions contributions
#'
#' Calculates global or regional sums for billions contributions for the UHC, HPOP, and HEP billions.
#'
#' @param df A data frame
#' @param billion One of uhc, hpop, and hep. The billion for which we want to find
#'   global sums.
#' @param sum_year The year for which we want to sum the billions contributions.
#' @param sum_type Allows you to select between global and regional sums.
#' @param source The source for the calculated billions. `WHO DDI calculations`,
#'   with `month, year` timestamp by default.
#'
#' @return A data frame with the global sums for the relevant billion. Does not
#'   include rows from the original data frame.
#' @export
#'
#' @examples
#' # full_data is expected to have the values of the billions calculations per
#' # country already computed.
#' # In this example, we're calculating the global contributions for the HPOP billion for 2025.
#' # calculate_contribution_sums(full_data, "hpop", 2025, "global")
calculate_contribution_sums = function(df,
                                       billion = c("uhc", "hpop", "hep"),
                                       sum_year,
                                       sum_type = c("global", "regional"),
                                       source = sprintf("WHO DDI calculation, %s", format(Sys.Date(), '%B %Y'))) {
  # Checks and assertions
  billion <- rlang::arg_match(billion)
  sum_type <- rlang::arg_match(sum_type)

  ind_code = switch(billion,
                    uhc = "uhc_sm",
                    hpop = "hpop_healthier",
                    hep = "hep_idx")

  # Add WHO region to the grouping columns for regional sums
  group_cols = if (sum_type == "global") c("scenario") else c("scenario", "who_region")

  df %>%
    dplyr::filter(.data[["ind"]] == ind_code, .data[["year"]] == sum_year) %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[["iso3"]], "who_region")) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_cols))) %>%
    dplyr::summarise(
      "iso3" := ifelse(sum_type == "global", "GLOBAL", .data[["who_region"]]),
      "year" := sum_year,
      "ind" := ind_code,
      "contribution" := sum(.data[["contribution"]], na.rm = TRUE),
      "source" := !!source,
      "type" := dplyr::case_when("projected" %in% .data[["type"]] ~ "projected",
                          "reported" %in% .data[["type"]] ~ "reported",
                          "estimated" %in% .data[["type"]] ~ "estimated",
                          TRUE ~ NA_character_)
    ) %>%
    dplyr::mutate("who_region" := NULL)
}
