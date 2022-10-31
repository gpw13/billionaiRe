#' Transform Raw Indicator Values for HEP Billion
#'
#' `transform_hep_data()` applies transformations on HEP Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report.
#'
#' Currently, this function only changes Prevent campaign data by calculating
#' the total sum of campaigns for each year for use in Prevent calculations.
#' For more details on the HEP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hpop.html}{\code{vignette("hep", package = "billionaiRe")}}
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @param source Source to use for prevent data that is flat extrapolated
#'     that has more than one unique value.
#' @param extrapolate_to Year to extrapolate Prevent data to, defaults to 2025
#'
#' @return Data frame in long format.
#'
#' @export
transform_hep_data <- function(df,
                               scenario_col = NULL,
                               value_col = "value",
                               transform_glue = "transform_{value_col}",
                               source = "WUENIC/IVB/WHO Technical Programme",
                               ind_ids = billion_ind_codes("hep", include_calculated = TRUE),
                               extrapolate_to = 2025,
                               recycle = FALSE,
                               ...) {
  assert_columns(df, "iso3", "ind", value_col, scenario_col)
  assert_ind_ids(ind_ids, "hep")
  assert_unique_rows(df, scenario_col, ind_ids)

  params <- list(...)
  params_assert_data_calculations <- get_right_parameters(params, assert_data_calculation_hep)

  if (!is.null(params_assert_data_calculations)) {
    do.call(
      assert_data_calculation_hep,
      c(
        list(
          df = df,
          value_col = value_col,
          scenario_col = scenario_col,
          ind_ids = ind_ids
        ),
        params_assert_data_calculations
      )
    )
  } else {
    assert_data_calculation_hep(
      df = df,
      value_col = value_col,
      scenario_col = scenario_col,
      ind_ids = ind_ids
    )
  }

  if (recycle) {
    params_recycle <- get_right_parameters(params, recycle_data)

    df <- do.call(
      recycle_data,
      c(
        list(
          df = df,
          billion = "hep",
          scenario_col = scenario_col,
          value_col = value_col,
          ind_ids = ind_ids
        ),
        params_recycle
      )
    )
  }

  transform_value_col <- glue::glue(transform_glue)

  df <- billionaiRe_add_columns(df, c("type", "source"), NA_character_)
  df <- billionaiRe_add_columns(df, transform_value_col, NA_real_)

  new_df <- df %>%
    dplyr::filter(dplyr::if_any(tidyselect::all_of(value_col), ~ !is.na(.x))) %>%
    transform_prev_cmpgn_data(
      scenario_col,
      value_col,
      transform_value_col,
      source,
      ind_ids,
      extrapolate_to
    )%>%
    transform_prev_routine_data(
      value_col,
      transform_value_col,
      scenario_col,
      ind_ids
    )

  # get transform values for HEP indicators not transformed above
  for (i in 1:length(transform_value_col)) {
    new_df <- dplyr::mutate(new_df, !!sym(transform_value_col[i]) := dplyr::case_when(
      is.na(.data[[transform_value_col[i]]]) & .data[["ind"]] %in% ind_ids ~ .data[[value_col[i]]],
      TRUE ~ .data[[transform_value_col[i]]]
    ))
  }

  new_df
}

#' Transform Prevent routine data
#'
#' Prevent routine data is now stored raw using the percent coverage of the indicator.
#' We want to transform this back into a numerator value for use within `pathogen_calc`.
#'
#' @inheritParams transform_hep_data
#' @inheritParams calculate_uhc_billion
transform_prev_routine_data <- function(df,
                                        value_col,
                                        transform_value_col,
                                        scenario_col,
                                        ind_ids) {
  routine_inds <- ind_ids[c("measles_routine", "polio_routine", "meningitis_routine", "yellow_fever_routine")]
  inf_ind <- ind_ids[c("surviving_infants")]

  routine_match <- ind_ids[c("measles_routine_num", "polio_routine_num", "meningitis_routine_num", "yellow_fever_routine_num")]
  names(routine_match) <- routine_inds

  # get data frame of surviving infants, the denominator for routine data

  inf_val_names <- paste0("_inf_temp_", value_col)

  inf_ind_values <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(!!scenario_col))) %>%
    dplyr::filter(.data[["ind"]] %in% c(!!routine_match, !!routine_inds)) %>%
    dplyr::select(dplyr::all_of(c("iso3", "year", !!scenario_col))) %>%
    dplyr::distinct()

  if(nrow(inf_ind_values) > 0){
    inf_ind_values <- inf_ind_values %>%
      dplyr::mutate(
        !!sym("ind") := inf_ind,
        !!sym(value_col) := wppdistro::get_population(.data[["iso3"]], .data[["year"]], age_range = "under_1"),
        !!sym("type") := dplyr::if_else(.data[["year"]] <= 2019, "reported", "projected"),
        !!sym("use_dash") := TRUE,
        !!sym("use_calc") := TRUE,
        !!sym("source") := "United Nations, Department of Economic and Social Affairs, Population Division (2019). World Population Prospects 2019, Online Edition. Rev. 1"
      )
  }

  df <- df %>%
    dplyr::filter(!.data[["ind"]] %in% inf_ind) %>%
    dplyr::bind_rows(inf_ind_values)

  inf_df <- df %>%
    dplyr::filter(.data[["ind"]] %in% !!inf_ind) %>%
    dplyr::select(dplyr::all_of(c("iso3", "year", !!value_col))) %>%
    dplyr::distinct() %>%
    # in case multiple surviving inf scenario data
    dplyr::rename_with(~ inf_val_names[which(!!value_col == .x)], .cols = !!value_col)

  # join to main data frame
  num_df <- dplyr::left_join(df, inf_df, by = c("iso3", "year")) %>%
    dplyr::filter(.data[["ind"]] %in% !!routine_inds)

  # for each value column, turn data into numerators
  for (i in 1:length(value_col)) {
    num_df <- dplyr::mutate(num_df, !!sym(value_col[i]) := .data[[value_col[i]]] * .data[[inf_val_names[i]]] / 100)
  }

  # rename ind names to be routine numerators and return with full data
  final_df <- num_df %>%
    dplyr::select(-!!inf_val_names) %>%
    dplyr::mutate(!!sym("ind") := routine_match[.data[["ind"]]]) %>%
    dplyr::bind_rows(df, .)

  # add transform value for routine indicators
  for (i in 1:length(value_col)) {
    final_df <- dplyr::mutate(
      final_df,
      !!sym(transform_value_col[i]) := ifelse(.data[["ind"]] %in% c(routine_inds, routine_match),
        .data[[value_col[i]]],
        .data[[transform_value_col[i]]]
      )
    ) %>%
      dplyr::distinct()
  }

  return(final_df)
}

#' Transform Prevent campaigns data
#'
#' Prevent campaign data uses aggregates across years that a vaccine provides
#' protection against a specific pathogen. Thus, we want to do some specific
#' aggregation so that this analysis can be brought into the overall HEP
#' calculations. This function does just that. For each pathogen, we take the
#' data out to the latest year observed, or a separate year if provided. Then we
#' do the rolling sums for them, and ensure that the rows make sense (filtering
#' out for instance years before there were any pathogens reported for a country).
#'
#' These transform values are then flat extrapolated from their latest year out to
#' a specific year, the default being 2023 If latest year values are provided
#' for a specific pathogen, those years are used for calculating the rolling
#' average out to, otherwise, the latest year with observed values is used.
#'
#' @inheritParams transform_hep_data
#' @inheritParams calculate_uhc_billion
transform_prev_cmpgn_data <- function(df,
                                      scenario_col,
                                      value_col,
                                      transform_value_col,
                                      source,
                                      ind_ids,
                                      extrapolate_to) {
  ind_check <- c(
    "meningitis_campaign_denom",
    "meningitis_campaign_num",
    "cholera_campaign_num",
    "cholera_campaign_denom",
    "yellow_fever_campaign_num",
    "yellow_fever_campaign_denom",
    "ebola_campaign_num",
    "ebola_campaign_denom",
    "covid_campaign_num",
    "covid_campaign_denom",
    "measles_campaign_num",
    "measles_campaign_denom"
  )

  # split data frames to edit cmpgn_df and later join back up to old_df
  cmpgn_df <- dplyr::filter(df, .data[["ind"]] %in% ind_ids[ind_check])
  old_df <- dplyr::filter(df, !(.data[["ind"]] %in% ind_ids[ind_check]))

  if (nrow(cmpgn_df) == 0) {
    return(billionaiRe_add_columns(df, transform_value_col, NA_real_))
  }

  # expand data frame with or without scenarios

  if (!is.null(scenario_col)) {
    exp_df <- tidyr::expand_grid(
      "iso3" := unique(cmpgn_df[["iso3"]]),
      "year" := min(cmpgn_df[["year"]]):extrapolate_to,
      "ind" := unique(cmpgn_df[["ind"]]),
      !!sym(scenario_col) := unique(cmpgn_df[[scenario_col]])
    )
  } else {
    exp_df <- tidyr::expand_grid(
      "iso3" := unique(cmpgn_df[["iso3"]]),
      "year" := min(cmpgn_df[["year"]]):extrapolate_to,
      "ind" := unique(cmpgn_df[["ind"]])
    )
  }

  # expand data frame to prepare for rolling sums
  new_df <- cmpgn_df %>%
    dplyr::right_join(exp_df,
      by = c("iso3", "year", "ind", scenario_col)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", !!scenario_col)))) %>%
    dplyr::filter(dplyr::if_any(!!value_col, ~ any(!is.na(.x)))) %>%
    dplyr::arrange(.data[["year"]], .by_group = TRUE)

  # rolling sums and flat extrapolation from latest campaign value

  for (i in 1:length(value_col)) {
    new_df <- dplyr::mutate(new_df, !!sym(transform_value_col[i]) := dplyr::case_when(
      .data[["ind"]] %in% ind_ids[c("cholera_campaign_num", "cholera_campaign_denom")] ~ extrapolate_campaign_vector(.data[[value_col[i]]], 3),
      .data[["ind"]] %in% ind_ids[c("meningitis_campaign_num", "meningitis_campaign_denom")] ~ extrapolate_campaign_vector(.data[[value_col[i]]], 10),
      .data[["ind"]] %in% ind_ids[c(
        "yellow_fever_campaign_num", "yellow_fever_campaign_denom",
        "ebola_campaign_num", "ebola_campaign_denom",
        "covid_campaign_num", "covid_campaign_denom",
        "measles_campaign_num", "measles_campaign_denom"
      )] ~ extrapolate_campaign_vector(.data[[value_col[i]]], length(.data[[value_col[i]]]))
    ))
  }

  # Extrapolate out type and source for each pathogen
  new_df %>%
    dplyr::filter(dplyr::row_number() >= min(which(!is.na(.data[["type"]])), Inf)) %>%
    dplyr::mutate(
      !!sym("type") := dplyr::case_when(
        !is.na(.data[["type"]]) ~ .data[["type"]],
        dplyr::row_number() <= max(which(!is.na(.data[[value_col[i]]])), -Inf) ~ "reported",
        TRUE ~ "projected"
      ),
      !!sym("source") := ifelse(length(unique(.data[["source"]][!is.na(.data[["source"]])])) == 1,
        unique(.data[["source"]][!is.na(.data[["source"]])]),
        !!source
      )
    ) %>%
    dplyr::bind_rows(old_df, .)
}
