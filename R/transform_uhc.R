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
                               value_col = "value",
                               transform_glue = "transform_{value_col}",
                               ind_ids = billion_ind_codes("uhc"),
                               recycle = FALSE,
                               ...) {
  assert_columns(df, "ind", value_col)
  assert_ind_ids(ind_ids, billion = "uhc")

  # get transform column names
  transform_value_col <- glue::glue(transform_glue)

  params <- list(...)
  params_assert_data_calculations <- get_right_parameters(params, assert_data_calculation_uhc)

  if (!is.null(params_assert_data_calculations)) {
    do.call(
      assert_data_calculation_uhc,
      c(
        list(
          df = df,
          value_col = value_col
        ),
        params_assert_data_calculations
      )
    )
  } else {
    assert_data_calculation_uhc(df, value_col = value_col)
  }

  if (recycle) {
    params_recycle <- get_right_parameters(params, recycle_data)

    df <- do.call(
      recycle_data,
      c(
        list(
          df = df,
          billion = "uhc",
          value_col = value_col,
          ind_ids = ind_ids
        ),
        params_recycle
      )
    )
  }


  # transform each
  for (i in 1:length(value_col)) {
    df <- transform_uhc_single(df, value_col[i], transform_value_col[i], ind_ids)
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
#' @param transform_value_col Column name of column(s) with transformed
#'    indicator values
#'
#' @return A single column data frame of transformed values.
transform_uhc_single <- function(df,
                                 value_col,
                                 transform_value_col,
                                 ind_ids) {

  # check if transform column in data and create if not
  if (!(transform_value_col %in% names(df))) {
    df[[transform_value_col]] <- NA_real_
  }

  dplyr::mutate(
    df,
    !!sym(transform_value_col) := dplyr::case_when(
      is.na(.data[[value_col]]) ~ .data[[transform_value_col]],
      .data[["ind"]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "espar", "itn")] ~ trim_transforms(.data[[value_col]]),
      .data[["ind"]] == ind_ids["bp"] ~ transform_bp(.data[[value_col]]),
      .data[["ind"]] == ind_ids["fpg"] ~ transform_glucose(.data[[value_col]]),
      .data[["ind"]] == ind_ids["beds"] ~ transform_hosp_beds(.data[[value_col]]),
      .data[["ind"]] %in% ind_ids[c("uhc_tobacco", "fh")] ~ transform_inversion(.data[[value_col]]),
      .data[["ind"]] == ind_ids["hwf"] ~ transform_hwf(.data[[value_col]]),
      TRUE ~ .data[[transform_value_col]]
    )
  )
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
                                 transform_value_col = "transform_value",
                                 value_col = stringr::str_remove(transform_value_col, "transform_"),
                                 ind_ids = billion_ind_codes("uhc")) {
  assert_columns(df, "ind", transform_value_col)
  assert_string(value_col, length(transform_value_col))
  assert_ind_ids(ind_ids, "uhc")

  for (i in 1:length(value_col)) {
    df <- untransform_uhc_single(df, transform_value_col[i], value_col[i], ind_ids)
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
                                   transform_value_col,
                                   value_col,
                                   ind_ids) {
  df <- billionaiRe_add_columns(df, value_col, NA_real_)

  df %>%
    dplyr::mutate(!!sym(value_col) := dplyr::case_when(
      is.na(.data[[transform_value_col]]) ~ .data[[value_col]],
      .data[[ind_col]] %in% ind_ids[c("fp", "anc4", "dtp3", "pneumo", "tb", "art", "uhc_sanitation", "espar", "fh", "itn")] ~ .data[[transform_value_col]],
      .data[[ind_col]] %in% ind_ids["uhc_tobacco"] ~ transform_inversion(.data[[value_col]]),
      .data[[ind_col]] == ind_ids["bp"] ~ untransform_bp(.data[[transform_value_col]]),
      .data[[ind_col]] == ind_ids["fpg"] ~ untransform_glucose(.data[[transform_value_col]]),
      .data[[ind_col]] == ind_ids["beds"] ~ untransform_hosp_beds(.data[[transform_value_col]]),
      .data[[ind_col]] == ind_ids["hwf"] ~ untransform_hwf(.data[[transform_value_col]]),
      TRUE ~ .data[[value_col]]
    ))
}
