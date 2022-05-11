#' Transform Raw Indicator Values for HPOP Billion
#'
#' `transform_hpop_data()` applies transformations on HPOP Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report. Values in the transform column, if it already exists,
#' are replaced for HPOP indicators that have data in the `value_col` column, otherwise
#' the column keeps its original data.
#'
#' For more details on the HPOP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hpop.html}{\code{vignette("hpop", package = "billionaiRe")}}
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param value_col Column name of column with indicator values.
#' @param transform_glue Glue expression to be passed to [glue::glue()]. Defaults to
#'     `'transform_{value_col}'` which will create new column names by prefixing `transform_`
#'     to the original name.
#' @param ind_ids Named vector of indicator codes for input indicators to the Billion.
#'     Although separate indicator codes can be used than the standard, they must
#'     be supplied as a named vector where the names correspond to the output of
#'     `billion_ind_codes()`.
#' @param recycle Boolean to indicate if data should be recycled
#' @param ... additional parameters to to pass to `recycle_data`
#'
#' @return Data frame in long format.
#'
#' @export
transform_hpop_data <- function(df,
                                value_col = "value",
                                transform_glue = "transform_{value_col}",
                                ind_ids = billion_ind_codes("hpop"),
                                recycle = FALSE,
                                ...) {
  assert_columns(df, "iso3", "ind", value_col)
  assert_ind_ids(ind_ids, billion = "hpop")

  params <- list(...)
  params_assert_data_calculations <- get_right_params(params, assert_data_calculation_hpop)

  if (!is.null(params_assert_data_calculations)) {
    assert_data_calculation_hpop(df, value_col = value_col, params_assert_data_calculations)
  } else {
    assert_data_calculation_hpop(df, value_col = value_col)
  }


  if (recycle) {
    params_recycle <- get_right_params(params, recycle_data)

    df <- do.call(
      recycle_data,
      c(
        list(
          df = df,
          billion = "hpop",
          value_col = value_col,
          ind_ids = ind_ids
        ),
        params_recycle
      )
    )
  }

  # get transform column names and add to df
  transform_value_col <- glue::glue(transform_glue)
  df <- billionaiRe_add_columns(df, transform_value_col, NA_real_)

  # transform each
  for (i in 1:length(value_col)) {
    df <- transform_hpop_single(df, value_col[i], transform_value_col[i], ind_ids)
  }

  df
}

#' Perform a transformation on a single column
#'
#' This function is used within [transform_hpop_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams transform_hpop_data
#' @param transform_col Column to put transformed values into
#'
#' @return A single column data frame of transformed values.
transform_hpop_single <- function(df,
                                  value_col,
                                  transform_col,
                                  ind_ids) {
  df %>%
    dplyr::mutate(!!sym(transform_col) := dplyr::case_when(
      is.na(.data[[value_col]]) ~ .data[[transform_col]],
      .data[["ind"]] %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "fuel")] ~ trim_transforms(.data[[value_col]]),
      .data[["ind"]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese", "pm25")] ~ transform_inversion(.data[[value_col]]),
      .data[["ind"]] == ind_ids["suicide"] ~ transform_suicide_rate(.data[[value_col]]),
      .data[["ind"]] == ind_ids["alcohol"] ~ transform_alcohol(.data[[value_col]]),
      .data[["ind"]] == ind_ids["road"] ~ transform_road_safety(.data[[value_col]], .data[["iso3"]]),
      .data[["ind"]] == ind_ids["transfats"] ~ transform_transfats(.data[[value_col]]),
      TRUE ~ .data[[transform_col]]
    ))
}

#' Untransform Indicator Values for HPOP Billion
#'
#' `untransform_hpop_data()` reverses transformations on HPOP Billion indicators to
#' return raw indicator values. Details on the specific transformations applied
#' can be found within the Billions methods report.
#'
#' For more details on the HPOP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hpop.html}{\code{vignette("hpop", package = "billionaiRe")}}
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param transform_value_col Column name(s) of column with transformed values to retrieve.
#' @param value_col Column name(s) of column to place untransformed values. Must be same
#'     length as `transform_value_col`. If a column already exists, values are overwritten
#'     wherever `ind` and `transform_value_col` are available to be untransformed for
#'     this Billion, but otherwise, the column retains its
#'     other values.
#' @param ind_ids Named vector of indicator codes for input indicators to the Billion.
#'     Although separate indicator codes can be used than the standard, they must
#'     be supplied as a named vector where the names correspond to the output of
#'     `billion_ind_codes()`.
#'
#' @return Data frame in long format.
#'
#' @export
untransform_hpop_data <- function(df,
                                  transform_value_col = "transform_value_col",
                                  value_col = stringr::str_remove(transform_value_col, "transform_"),
                                  ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, "iso3", "ind", transform_value_col)
  assert_string(value_col, length(transform_value_col))
  assert_ind_ids(ind_ids, "hpop")

  df <- billionaiRe_add_columns(df, value_col, NA_real_)

  for (i in 1:length(value_col)) {
    df <- untransform_hpop_single(df, transform_value_col[i], value_col[i], ind_ids)
  }

  df
}

#' Perform a transformation on a single column
#'
#' This function is used within [untransform_hpop_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams untransform_hpop_data
#'
#' @return A single column data frame of transformed values.
untransform_hpop_single <- function(df,
                                    transform_value_col,
                                    value_col,
                                    ind_ids) {
  df %>%
    dplyr::mutate(!!sym(value_col) := dplyr::case_when(
      is.na(.data[[transform_value_col]]) ~ .data[[value_col]],
      .data[["ind"]] %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "fuel")] ~ .data[[transform_value_col]],
      .data[["ind"]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese", "pm25")] ~ transform_inversion(.data[[transform_value_col]]),
      .data[["ind"]] == ind_ids["suicide"] ~ untransform_suicide_rate(.data[[transform_value_col]]),
      .data[["ind"]] == ind_ids["alcohol"] ~ untransform_alcohol(.data[[transform_value_col]]),
      .data[["ind"]] == ind_ids["road"] ~ untransform_road_safety(.data[[transform_value_col]], .data[["iso3"]]),
      .data[["ind"]] == ind_ids["transfats"] ~ untransform_transfats(.data[[transform_value_col]]),
      TRUE ~ .data[[value_col]]
    ))
}
