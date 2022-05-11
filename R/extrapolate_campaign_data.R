#' Extrapolate campaign data for the transform value, type, and source
#'
#' Works across multiple `transform_value_col` columns.
#'
#' @param pathogen Indicator IDs of the pathogens
#' @param pathogen_year Latest year of data to use for flat extrapolation (extrapolating after that year)
#' @inheritParams calculate_hep_components
extrapolate_campaign_data <- function(pathogen,
                                      pathogen_year,
                                      df,
                                      transform_value_col,
                                      source) {
  assert_columns(df, transform_value_col, "year", "ind")

  if (!is.null(pathogen_year)) {
    dplyr::mutate(
      df,
      dplyr::across(
        !!transform_value_col,
        ~ dplyr::case_when(
          .data[["ind"]] %in% pathogen & .data[["year"]] <= pathogen_year ~ .x,
          .data[["ind"]] %in% pathogen & .data[["year"]] > pathogen_year ~ .x[.data[["year"]] == pathogen_year],
          TRUE ~ .x
        )
      ),
      "_billionaiRe_type_temp" := dplyr::case_when(
        .data[["ind"]] %in% pathogen & .data[["year"]] <= pathogen_year ~ "reported",
        .data[["ind"]] %in% pathogen & .data[["year"]] > pathogen_year ~ "projected",
        TRUE ~ .data[["_billionaiRe_type_temp"]]
      ),
      "_billionaiRe_source_temp" := dplyr::case_when(
        .data[["ind"]] %in% pathogen ~ !!source,
        TRUE ~ .data[["_billionaiRe_type_temp"]]
      )
    ) %>%
      dplyr::arrange(.data[["year"]], .by_group = TRUE) %>%
      dplyr::filter(dplyr::across(
        transform_value_col,
        ~ dplyr::row_number() >= min(which(.x > 0), Inf)
      )) # dropping rows before data exists
  } else {
    df
  }
}

#' Rolling sum and extrapolate campaigns data
#'
#' Takes rolling sum over observed data and then flat extrapolates from last reported
#'
#' @param x Vector of campaigns data, either numerator or denominator
#' @param n Years to take rolling average for
extrapolate_campaign_vector <- function(x, n) {
  not_na <- which(!is.na(x))
  if (length(not_na) > 0) {
    x_sum <- zoo::rollapply(x,
      n,
      sum,
      na.rm = T,
      partial = TRUE,
      align = "right"
    )



    flat_spot <- max(not_na)

    ifelse(1:length(x) <= flat_spot,
      x_sum,
      x_sum[flat_spot]
    )
  } else {
    x
  }
}
