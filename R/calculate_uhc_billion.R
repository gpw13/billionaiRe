#' Calculate UHC Billion
#'
#' `calculate_uhc_billion()` calculates country-level UHC Billion based on
#' indicator level data. Calculates it for each country-year combination in the provided data.
#'
#' @inherit calculate_hpop_contributions params
#' @inherit transform_uhc_data return details params
#'
#' @param source_col Column name of column to store source information.
#' @param source Source to provide for calculated average service coverage and
#'     single measure.
#' @param type_col Column name of column to store type.
#' @param projected_year Year that calculated data will be regarded as projected.
#'     Years prior reported as estimated.
#'
#' @export
calculate_uhc_billion <- function(df,
                                  year = "year",
                                  iso3 = "iso3",
                                  ind = "ind",
                                  transform_value = "transform_value",
                                  value = "value",
                                  scenario = NULL,
                                  type_col = "type",
                                  source_col = "source",
                                  source = sprintf("WHO DDI calculation, %s", format(Sys.Date(), "%B %Y")),
                                  projected_year = 2020,
                                  ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, year, iso3, ind, scenario, transform_value)
  assert_ind_ids(ind_ids, "uhc")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)
  assert_same_length(value, transform_value)

  # add billions group
  bill_df <- df %>%
    dplyr::mutate("_billion_group_temp" := dplyr::case_when(
      .data[[ind]] %in% ind_ids[c("fp", "dtp3", "anc4", "pneumo")] ~ "_rmnch_temp",
      .data[[ind]] %in% ind_ids[c("tb", "art", "itn", "uhc_sanitation")] ~ "_cd_temp",
      .data[[ind]] %in% ind_ids[c("uhc_tobacco", "bp", "fpg")] ~ "_ncd_temp",
      .data[[ind]] %in% ind_ids[c("beds", "hwf", "espar")] ~ "_sca_temp",
      .data[[ind]] == ind_ids["fh"] ~ "_fh_temp"))

  # calculate billion for each set of transform_value / value and join to original df
  for (i in 1:length(value)) {
    bill_df <- calculate_uhc_billion_single(bill_df,
                                            year,
                                            iso3,
                                            ind,
                                            transform_value[i],
                                            value[i],
                                            scenario,
                                            source_col,
                                            source,
                                            type_col,
                                            projected_year,
                                            ind_ids)
  }

    dplyr::bind_rows(df, bill_df)
}

#' Calculate UHC Billion for one set of columns
#'
#' @inheritParams calculate_uhc_billion
calculate_uhc_billion_single <- function(df,
                                         year,
                                         iso3,
                                         ind,
                                         transform_value,
                                         value,
                                         scenario,
                                         source_col,
                                         source,
                                         type_col,
                                         projected_year,
                                         ind_ids) {
  df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[!(ind_ids %in% c(ind_ids["nurses"], ind_ids["doctors"]))]) %>% # nurses doctors already aggregated to hwf
    dplyr::group_by(dplyr::across(dplyr::any_of(c(year, iso3, scenario, "_billion_group_temp")))) %>%
    dplyr::summarize(!!sym(transform_value) := mean(.data[[transform_value]], na.rm = TRUE),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "_billion_group_temp",
                       values_from = transform_value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate("asc" := mean(dplyr::c_across(c("_cd_temp", "_ncd_temp", "_rmnch_temp", "_sca_temp")),
                                na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("uhc_sm" := .data[["asc"]] * (100 - .data[["_fh_temp"]]) / 100) %>%
    dplyr::select(-dplyr::any_of(c("_cd_temp", "_ncd_temp", "_rmnch_temp", "_sca_temp", "_fh_temp"))) %>%
    tidyr::pivot_longer(c("asc", "uhc_sm"),
                        names_to = "ind",
                        values_to = transform_value) %>%
    dplyr::mutate(!!sym(type_col) := ifelse(.data[[year]] >= !!projected_year,
                                            "projected",
                                            "estimated"),
                  !!sym(source_col) := !!source,
                  !!sym(value) := .data[[transform_value]])
}