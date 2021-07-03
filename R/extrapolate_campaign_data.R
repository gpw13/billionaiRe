#' Extrapolate campaign data for the transform value, type, and source
#'
#' Works across multiple `transform_value` columns.
#'
#' @param pathogen Indicator IDs of the pathogens
#' @param pathogen_year Latest year of data to use for flat extrapolation (extrapolating after that year)
#' @inheritParams calculate_hep_components
extrapolate_campaign_data <- function(pathogen,
                                      pathogen_year,
                                      df,
                                      ind,
                                      year,
                                      transform_value,
                                      source) {
  dplyr::mutate(df,
                dplyr::across(transform_value,
                              ~ dplyr::case_when(
                                .data[[ind]] %in% pathogen & .data[[year]] <= pathogen_year ~ .x,
                                .data[[ind]] %in% pathogen & .data[[year]] > pathogen_year ~ .x[.data[[year]] == pathogen_year],
                                TRUE ~ .x
                              )),
                "_billionaiRe_type_temp" := dplyr::case_when(
                  .data[[ind]] %in% pathogen & .data[[year]] <= pathogen_year ~ "reported",
                  .data[[ind]] %in% pathogen & .data[[year]] > pathogen_year ~ "projected",
                  TRUE ~ .data[["_billionaiRe_type_temp"]]
                ),
                "_billionaiRe_source_temp" := dplyr::case_when(
                  .data[[ind]] %in% pathogen ~ !!source,
                  TRUE ~ .data[["_billionaiRe_type_temp"]]
                )) %>%
    dplyr::arrange(.data[[year]], .by_group = TRUE) %>%
    dplyr::filter(dplyr::across(transform_value,
                                ~ dplyr::row_number() >= min(which(.x > 0), Inf))) # dropping rows before data exists
}
