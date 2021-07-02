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

  df
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
  df <- billionaiRe_add_columns(df, value, NA_real_)

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
