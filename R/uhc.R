transform_uhc_data <- function(df,
                               ind = "ind",
                               value = "value",
                               ind_ids = billion_ind_codes("uhc")) {
  df %>%
    dplyr::mutate(raw_value = .data[[value]],
                  transform_value = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "ihr", "fh", "itn")] ~ .data[[value]],
                    .data[[ind]] == ind_ids["bp"] ~ transform_bp(.data[[value]]),
                    .data[[ind]] == ind_ids["fpg"] ~ transform_glucose(.data[[value]]),
                    .data[[ind]] == ind_ids["beds"] ~ transform_hosp_beds(.data[[value]]),
                    .data[[ind]] == ind_ids["uhc_tobacco"] ~ transform_tobacco(.data[[value]]),
                    .data[[ind]] == ind_ids["hwf"] ~ transform_hwf(.data[[value]])
                  ),
                  billion_group = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("fp", "dtp3", "anc4", "pneumo")] ~ "RMNCH",
                    .data[[ind]] %in% ind_ids[c("tb", "art", "itn", "uhc_sanitation")] ~ "CDs",
                    .data[[ind]] %in% ind_ids[c("uhc_tobacco", "bp", "fpg")] ~ "NCDs",
                    .data[[ind]] %in% ind_ids[c("beds", "hwf", "ihr")] ~ "SCA",
                    .data[[ind]] == ind_ids["fh"] ~ "FH"))
}

calculate_uhc_billion <- function(df,
                                  year = "year",
                                  iso3 = "iso3",
                                  ind = "ind",
                                  billion_group = "billion_group",
                                  transform_value = "transform_value",
                                  ind_ids = billion_ind_codes("uhc")) {
  df %>%
    dplyr::group_by(.data[[year]], .data[[iso3]], .data[[billion_group]]) %>%
    dplyr::summarize(!!sym(transform_value) := mean(.data[[transform_value]], na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = billion_group,
                       values_from = transform_value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ASC = mean(dplyr::c_across(c("CDs", "NCDs", "RMNCH", "SCA")),
                             na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(UHC = .data[["ASC"]] * (100 - .data[["FH"]]) / 100) %>%
    dplyr::select(-dplyr::any_of(c("CDs", "NCDs", "RMNCH", "SCA"))) %>%
    tidyr::pivot_longer(c("FH", "ASC", "UHC"),
                        names_to = "ind")
}

calculate_uhc_contribution <- function(df,
                                       year = "year",
                                       iso3 = "iso3",
                                       ind = "ind",
                                       year_start = 2018,
                                       year_end = 2023) {
  df %>%
    dplyr::filter(.data[[year]] %in% c(year_start, year_end)) %>%
    tidyr::pivot_wider(names_from = year) %>%
    dplyr::mutate(population = wppdistro::get_population(.data[[iso3]], year = year_end),
                  contribution = (.data[[as.character(year_end)]] - .data[[as.character(year_start)]]) * .data[["population"]] / 100,
                  contribution = ifelse(.data[[ind]] == "FH", -.data[["contribution"]], .data[["contribution"]]))
}
