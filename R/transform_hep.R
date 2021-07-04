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
#' @param type_col Column name of column with type data.
#' @param source Source to use for data that is projected within the transformation.
#' @param year Column name of column with year data.
#' @param cholera_latest_year Latest year to calculate rolling sum for cholera campaign
#'     data. From there, data is flat extrapolated. If `NULL`, uses the latest
#'     year with observations in the data.
#' @param meningitis_latest_year Latest year to calculate rolling sum for meningitis campaign
#'     data. From there, data is flat extrapolated. If `NULL`, uses the latest
#'     year with observations in the data.
#' @param yellow_fever_latest_year Latest year to calculate rolling sum for yellow
#'     fever campaign data. From there, data is flat extrapolated. If `NULL`,
#'     uses the latest year with observations in the data.
#' @param ebola_latest_year Latest year to calculate rolling sum for yellow
#'     fever campaign data. From there, data is flat extrapolated. If `NULL`,
#'     uses the latest year with observations in the data.
#' @param covid_latest_year Latest year to calculate rolling sum for yellow
#'     fever campaign data. From there, data is flat extrapolated. If `NULL`,
#'     uses the latest year with observations in the data.
#' @param measles_latest_year Latest year to calculate rolling sum for yellow
#'     fever campaign data. From there, data is flat extrapolated. If `NULL`,
#'     uses the latest year with observations in the data.
#' @param extrapolate_to Year to extrapolate Prevent data to, defaults to 2023
#'
#' @return Data frame in long format.
#'
#' @export
transform_hep_data <- function(df,
                               iso3 = "iso3",
                               year = "year",
                               ind = "ind",
                               scenario = NULL,
                               value = "value",
                               transform_glue = "transform_{value}",
                               type_col = "type",
                               source_col = "source",
                               source = sprintf("WHO DDI flat extrapolation, %s", format(Sys.Date(), "%B %Y")),
                               ind_ids = billion_ind_codes("hep"),
                               cholera_latest_year = NULL,
                               meningitis_latest_year = NULL,
                               yellow_fever_latest_year = NULL,
                               ebola_latest_year = NULL,
                               covid_latest_year = NULL,
                               measles_latest_year = NULL,
                               extrapolate_to = 2023) {
  assert_columns(df, iso3, ind, value)
  assert_ind_ids(ind_ids, "hep")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)

  transform_value <- glue::glue(transform_glue)

  df <- billionaiRe_add_columns(df, c(type_col, source_col), NA_character_)
  df <- billionaiRe_add_columns(df, transform_value, NA_real_)

  new_df <- df %>%
    dplyr::filter(!is.na(.data[[value]])) %>%
    transform_prev_cmpgn_data(iso3,
                              year,
                              ind,
                              scenario,
                              value,
                              transform_value,
                              type_col,
                              source_col,
                              source,
                              ind_ids,
                              cholera_latest_year,
                              meningitis_latest_year,
                              yellow_fever_latest_year,
                              ebola_latest_year,
                              covid_latest_year,
                              measles_latest_year,
                              extrapolate_to) %>%
    transform_prev_routine_data(iso3,
                                year,
                                ind,
                                value,
                                transform_value,
                                ind_ids)

  # get transform values for HEP indicators not transformed above
  for (i in 1:length(transform_value)) {
    new_df <- dplyr::mutate(new_df, !!sym(transform_value[i]) := dplyr::case_when(
      is.na(.data[[transform_value[i]]]) & .data[[ind]] %in% ind_ids ~ .data[[value[i]]],
      TRUE ~ .data[[transform_value[i]]]
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
                                        iso3,
                                        year,
                                        ind,
                                        value,
                                        transform_value,
                                        ind_ids) {
  routine_inds <- ind_ids[c("measles_routine", "polio_routine", "meningitis_routine", "yellow_fever_routine")]
  inf_ind <- ind_ids[c("surviving_infants")]

  routine_match <- ind_ids[c("measles_routine_num", "polio_routine_num", "meningitis_routine_num", "yellow_fever_routine_num")]
  names(routine_match) <- routine_inds

  # get data frame of surviving infants, the denominator for routine data

  inf_val_names <- paste0("_inf_temp_", value)

  inf_df <- df %>%
    dplyr::filter(.data[[ind]] %in% inf_ind) %>%
    dplyr::select(dplyr::all_of(c(iso3, year, value))) %>%
    dplyr::rename_with(~ inf_val_names[which(value == .x)], .cols = value)

  # join to main data frame
  num_df <- dplyr::left_join(df, inf_df, by = c(iso3, year)) %>%
    dplyr::filter(.data[[ind]] %in% routine_inds)

  # for each value column, turn data into numerators
  for (i in 1:length(value)) {
    num_df <- dplyr::mutate(num_df, !!sym(value[i]) := .data[[value[i]]] * .data[[inf_val_names[i]]] / 100)
  }

  # rename ind names to be routine numerators and return with full data
  # add transform value for routine indicators
  num_df %>%
    dplyr::select(-inf_val_names) %>%
    dplyr::mutate(!!sym(ind) := routine_match[.data[[ind]]]) %>%
    dplyr::bind_rows(df, .) %>%
    dplyr::mutate(!!sym(transform_value) := ifelse(.data[[ind]] %in% c(routine_inds, routine_match),
                                                   .data[[value]],
                                                   .data[[transform_value]]))
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
                                      iso3,
                                      year,
                                      ind,
                                      scenario,
                                      value,
                                      transform_value,
                                      type_col,
                                      source_col,
                                      source,
                                      ind_ids,
                                      cholera_latest_year = NULL,
                                      meningitis_latest_year = NULL,
                                      yellow_fever_latest_year = NULL,
                                      ebola_latest_year = NULL,
                                      covid_latest_year = NULL,
                                      measles_latest_year = NULL,
                                      extrapolate_to = 2023) {
  ind_check <- c("meningitis_campaign_denom",
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
                 "measles_campaign_denom")

  cmpgn_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids[ind_check])

  if (nrow(cmpgn_df) == 0) {
    return(billionaiRe_add_columns(df, transform_value, NA_real_))
  }

  yrs <- get_latest_year(cmpgn_df,
                         ind,
                         year,
                         ind_ids,
                         cholera_latest_year,
                         meningitis_latest_year,
                         yellow_fever_latest_year,
                         ebola_latest_year,
                         covid_latest_year,
                         measles_latest_year)

  # expand data frame to prepare for rolling sums
  new_df <- cmpgn_df %>%
    dplyr::right_join(tidyr::expand_grid(!!sym(iso3) := unique(.[[iso3]]),
                                         !!sym(year) := min(.[[year]]):extrapolate_to,
                                         !!sym(ind) := unique(.[[ind]])),
                      by = c(iso3, year, ind)) %>%
    dplyr::group_by(dplyr::across(c(iso3, ind, scenario))) %>%
    dplyr::filter(dplyr::if_any(value, ~ any(!is.na(.x)))) %>%
    dplyr::arrange(.data[[year]], .by_group = TRUE)

  # rolling sums

  for (i in 1:length(value)) {
    new_df <- dplyr::mutate(new_df, !!sym(transform_value[i]) := dplyr::case_when(
      .data[[ind]] %in% ind_ids[c("cholera_campaign_num", "cholera_campaign_denom")] ~ zoo::rollapply(.data[[value[i]]],
                                                                                                      3,
                                                                                                      sum,
                                                                                                      na.rm = T,
                                                                                                      partial = T,
                                                                                                      align = "right"),
      .data[[ind]] %in% ind_ids[c("meningitis_campaign_num", "meningitis_campaign_denom")] ~ zoo::rollapply(.data[[value[i]]],
                                                                                                            10,
                                                                                                            sum,
                                                                                                            na.rm = T,
                                                                                                            partial = TRUE,
                                                                                                            align = "right"),
      .data[[ind]] %in% ind_ids[c("yellow_fever_campaign_num", "yellow_fever_campaign_denom",
                                  "ebola_campaign_num", "ebola_campaign_denom",
                                  "covid_campaign_num", "covid_campaign_denom",
                                  "measles_campaign_num", "measles_campaign_denom")] ~ zoo::rollapply(.data[[value[i]]],
                                                                                                      length(.data[[value[i]]]),
                                                                                                      sum,
                                                                                                      na.rm = T,
                                                                                                      partial = TRUE,
                                                                                                      align = "right")
    ))
  }

  # Extrapolate out latest values for each pathogen

  pathogens <- list(ind_ids[c("cholera_campaign_num", "cholera_campaign_denom")],
                    ind_ids[c("meningitis_campaign_num", "meningitis_campaign_denom")],
                    ind_ids[c("yellow_fever_campaign_num", "yellow_fever_campaign_denom")],
                    ind_ids[c("ebola_campaign_num", "ebola_campaign_denom")],
                    ind_ids[c("covid_campaign_num", "covid_campaign_denom")],
                    ind_ids[c("measles_campaign_num", "measles_campaign_denom")])

  new_df <- billionaiRe_add_columns(new_df, c("_billionaiRe_type_temp", "_billionaiRe_source_temp"), NA_character_)

  for (i in 1:length(pathogens)) {
    new_df <- extrapolate_campaign_data(pathogens[[i]],
                                        yrs[[i]],
                                        new_df,
                                        ind,
                                        year,
                                        transform_value,
                                        source)
  }

  # Join up with original data frame

  new_df %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(dplyr::filter(df, !(.data[[ind]] %in% ind_ids[ind_check]))) %>%
    dplyr::mutate(!!sym(type_col) := ifelse(is.na(.data[[type_col]]) & !is.na(.data[["_billionaiRe_type_temp"]]),
                                        .data[["_billionaiRe_type_temp"]],
                                        .data[[type_col]]),
                  !!sym(source_col) := ifelse(is.na(.data[[source_col]]) & !is.na(.data[["_billionaiRe_source_temp"]]),
                                              .data[["_billionaiRe_source_temp"]],
                                              .data[[source_col]])) %>%
    dplyr::select(-c("_billionaiRe_type_temp", "_billionaiRe_source_temp"))
}
