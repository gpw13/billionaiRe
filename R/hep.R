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
#' @param type Column name of column with type data.
#' @param year Column name of column with year data.
#' @param cholera_latest_year Latest year to calculate rolling average for cholera.
#'     If `NULL`, uses the latest year with observations in the data.
#' @param meningitis_latest_year Latest year to calculate rolling average for meningitis
#'     If `NULL`, uses the latest year with observations in the data.
#' @param yellow_fever_latest_year Latest year to calculate rolling average for meningitis
#'     If `NULL`, uses the latest year with observations in the data.
#' @param extrapolate_to Year to extrapolate Prevent data to, defaults to 2023.
#'
#' @return Data frame in long format.
#'
#' @export
transform_hep_data <- function(df,
                               iso3 = "iso3",
                               year = "year",
                               ind = "ind",
                               value = "value",
                               type = "type",
                               ind_ids = billion_ind_codes("hep"),
                               cholera_latest_year = NULL,
                               meningitis_latest_year = NULL,
                               yellow_fever_latest_year = NULL,
                               extrapolate_to = 2023) {
  assert_columns(df, iso3, ind, value)

  new_df <- df %>%
    dplyr::filter(!is.na(.data[[value]])) %>%
    transform_prev_cmpgn_data(iso3,
                              year,
                              ind,
                              value,
                              type,
                              ind_ids,
                              cholera_latest_year,
                              meningitis_latest_year,
                              yellow_fever_latest_year,
                              extrapolate_to)

  new_df %>%
    dplyr::mutate(!!sym("transform_value") := ifelse(is.na(.data[["transform_value"]]),
                                                     .data[[value]],
                                                     .data[["transform_value"]]))
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
#' a specific year, the default being 2023. If latest year values are provided
#' for a specific pathogen, those years are used for calculating the rolling
#' average out to, otherwise, the latest year with observed values is used.
#'
#' @inheritParams transform_hep_data
transform_prev_cmpgn_data <- function(df,
                                      iso3,
                                      year,
                                      ind,
                                      value,
                                      type,
                                      ind_ids,
                                      cholera_latest_year = NULL,
                                      meningitis_latest_year = NULL,
                                      yellow_fever_latest_year = NULL,
                                      extrapolate_to = 2023) {
  ind_check <- c("meningitis_campaign_denom",
                 "meningitis_campaign",
                 "cholera_campaign",
                 "cholera_campaign_denom",
                 "yellow_fever_campaign",
                 "yellow_fever_campaign_denom")

  cmpgn_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids[names(ind_ids) %in% ind_check])

  if (nrow(cmpgn_df) == 0) {
    return(dplyr::mutate(df, "transform_value" := NA))
  }

  yrs <- get_latest_year(cmpgn_df,
                         ind,
                         year,
                         ind_ids,
                         cholera_latest_year,
                         meningitis_latest_year,
                         yellow_fever_latest_year)

  # Apply sums to various
  new_df <- cmpgn_df %>%
    dplyr::right_join(tidyr::expand_grid(!!sym(iso3) := unique(.[[iso3]]),
                                         !!sym(year) := min(.[[year]]):extrapolate_to,
                                         !!sym(ind) := unique(.[[ind]])),
                      by = c(iso3, year, ind)) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::filter(any(!is.na(.data[[value]]))) %>%
    tidyr::pivot_wider(c(iso3, year),
                       names_from = ind,
                       values_from = value) %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::arrange(.data[[year]],
                   .by_group = TRUE) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("cholera"),
                                ~zoo::rollapply(.x,
                                                4,
                                                sum,
                                                na.rm = T,
                                                partial = TRUE,
                                                align = "right")),
                  dplyr::across(dplyr::contains("meningitis"),
                                ~zoo::rollapply(.x,
                                                11,
                                                sum,
                                                na.rm = T,
                                                partial = TRUE,
                                                align = "right")),
                  dplyr::across(dplyr::contains("yellow_fever"),
                                ~zoo::rollapply(.x,
                                                length(.x),
                                                sum,
                                                na.rm = T,
                                                partial = TRUE,
                                                align = "right"))) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(-c(iso3, year),
                        names_to = "ind",
                        values_to = "transform_value") %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]])

  # Extrapolate out latest values

  if (!is.null(yrs[[1]])) {
    new_df <- dplyr::mutate(new_df,
                            !!sym("transform_value") := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "cholera") & .data[[year]] <= yrs[[1]] ~ .data[["transform_value"]],
                              stringr::str_detect(.data[[ind]], "cholera") & .data[[year]] > yrs[[1]] ~ .data[["transform_value"]][.data[[year]] == yrs[[1]]],
                              TRUE ~ .data[["transform_value"]]
                            ),
                            "billionaiRe_type_temp" := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "cholera_campaign") & .data[[year]] <= yrs[[1]] ~ "reported",
                              stringr::str_detect(.data[[ind]], "cholera_campaign") & .data[[year]] > yrs[[1]] ~ "projected"
                              ))
  }

  if (!is.null(yrs[[2]])) {
    new_df <- dplyr::mutate(new_df,
                            !!sym("transform_value") := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "meningitis") & .data[[year]] <= yrs[[2]] ~ .data[["transform_value"]],
                              stringr::str_detect(.data[[ind]], "meningitis") & .data[[year]] > yrs[[2]] ~ .data[["transform_value"]][.data[[year]] == yrs[[2]]],
                              TRUE ~ .data[["transform_value"]]
                            ),
                            "billionaiRe_type_temp" := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "meningitis") & .data[[year]] <= yrs[[2]] ~ "reported",
                              stringr::str_detect(.data[[ind]], "meningitis") & .data[[year]] > yrs[[2]] ~ "projected"
                            ))
  }

  if (!is.null(yrs[[3]])) {
    new_df <- dplyr::mutate(new_df,
                            !!sym("transform_value") := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "yellow_fever") & .data[[year]] <= yrs[[3]] ~ .data[["transform_value"]],
                              stringr::str_detect(.data[[ind]], "yellow_fever") & .data[[year]] > yrs[[3]] ~ .data[["transform_value"]][.data[[year]] == yrs[[3]]],
                              TRUE ~ .data[["transform_value"]]
                            ),
                            "billionaiRe_type_temp" := dplyr::case_when(
                              stringr::str_detect(.data[[ind]], "yellow_fever") & .data[[year]] <= yrs[[3]] ~ "reported",
                              stringr::str_detect(.data[[ind]], "yellow_fever") & .data[[year]] > yrs[[3]] ~ "projected"
                            ))
  }

  # Arrange data and drop rows with no data
  # Necessary since some series are longer than others

  new_df <- new_df %>%
    dplyr::arrange(.data[[year]],
                   .by_group = TRUE) %>%
    dplyr::filter(dplyr::row_number() >= min(which(.data[["transform_value"]] > 0), Inf)) %>%
    dplyr::ungroup()

  # Join up with original data frame
  # and export
  new_df %>%
    dplyr::full_join(df, by = c(iso3, ind, year)) %>%
    dplyr::mutate(!!sym(type) := ifelse(is.na(.data[[type]]) & !is.na(.data[["billionaiRe_type_temp"]]),
                                        .data[["billionaiRe_type_temp"]],
                                        .data[[type]])) %>%
    dplyr::select(-"billionaiRe_type_temp")
}

#' Get latest years for pathogens
#'
#' Since campaign data is not projected, we only want to do rolling sums on the
#' variables until the latest year that data is available. A user can either
#' provide a latest year for each pathogen, or it will be determined by the available
#' data. This function processes that for all three pathogens to get the latest
#' available year.
#'
#' @inheritParams transform_hep_data
get_latest_year <- function(df,
                            ind,
                            year,
                            ind_ids,
                            cholera_latest_year,
                            meningitis_latest_year,
                            yellow_fever_latest_year) {

  valid_year <- function(x) {
    valid <- (length(x) == 1 & is.numeric(x) & dplyr::between(x, 1900, 2100)) | is.infinite(x)
    if (!valid) {
      y <- deparse(substitute(x))
      stop(sprintf("`%s` must be a numeric value between 1900 or 2100, or NULL.",
                   y),
           call. = FALSE)
    }
    if (is.infinite(x)) {
      NULL
    } else {
      x
    }
  }

  if (is.null(cholera_latest_year)) {
    cholera_latest_year <- df %>%
      dplyr::filter(stringr::str_detect(.data[[ind]], ind_ids[names(ind_ids) == "cholera_campaign"])) %>%
      dplyr::pull(.data[[year]]) %>%
      max(-Inf, na.rm = TRUE)
  }
  cholera_latest_year <- valid_year(cholera_latest_year)

  if (is.null(meningitis_latest_year)) {
    meningitis_latest_year <- df %>%
      dplyr::filter(stringr::str_detect(.data[[ind]], ind_ids[names(ind_ids) == "meningitis_campaign"])) %>%
      dplyr::pull(.data[[year]]) %>%
      max(-Inf, na.rm = TRUE)
  }

  meningitis_latest_year <- valid_year(meningitis_latest_year)

  if (is.null(yellow_fever_latest_year)) {
    yellow_fever_latest_year <- df %>%
      dplyr::filter(stringr::str_detect(.data[[ind]], "yellow_fever_campaign")) %>%
      dplyr::pull(.data[[year]]) %>%
      max(-Inf, na.rm = TRUE)
  }

  yellow_fever_latest_year <- valid_year(yellow_fever_latest_year)

  list(cholera_latest_year,
       meningitis_latest_year,
       yellow_fever_latest_year)
}

#' Calculate HEP component indicators
#'
#' Currently, the Prepare and DNR indicators are already calculated in the input
#' data, so only Prevent is calculated in this function. It takes the numerator
#' and denominator data from the inputs and calculates the vaccination coverage
#' per year and country for each relevant pathogen, as well as the overall Prevent
#' score. For DNR and its components, the level is calculated in this function.
#'
#' @inherit transform_hep_data return params
#' @inheritParams transform_hep_data
#'
#' @param transform_value Column name of column with transformed values.
#' @param level Column name to create to hold levels data.
#' @param hepi_start_year First year to calculate HEP index for.
#'
#' @export
calculate_hep_components <- function(df,
                                     iso3 = "iso3",
                                     year = "year",
                                     ind = "ind",
                                     transform_value = "transform_value",
                                     type = "type",
                                     level = "level",
                                     hepi_start_year = 2018,
                                     ind_ids = billion_ind_codes("hep")) {
  assert_columns(df, iso3, ind, transform_value)

  df %>%
    dplyr::bind_rows(prevent_calculations(.,
                                          iso3,
                                          year,
                                          ind,
                                          type,
                                          transform_value,
                                          ind_ids)) %>%
    dplyr::bind_rows(calculate_hepi(.,
                                    iso3,
                                    year,
                                    ind,
                                    type,
                                    transform_value,
                                    hepi_start_year,
                                    ind_ids)) %>%
    dplyr::mutate(!!sym(level) := dplyr::case_when(
      !(.data[[ind]] %in% ind_ids[names(ind_ids) %in% c("detect_respond",
                                                        "detect",
                                                        "notify",
                                                        "respond",
                                                        "prevent",
                                                        "espar",
                                                        "hep_idx")]) ~ NA_real_,
      .data[[transform_value]] < 30 ~ 1,
      .data[[transform_value]] < 50 ~ 2,
      .data[[transform_value]] < 70 ~ 3,
      .data[[transform_value]] < 90 ~ 4,
      .data[[transform_value]] >= 90 ~ 5
    ))

}

#' Calculate prevent indicators
#'
#' This function calculates the overall vaccine coverage scores for each pathogen,
#' including the overall prevent indicator. It is used within `calculate_hep_components()`
#' and primarily relies on using `purrr::pmap_dfr()` to apply the `pathogen_calc()`
#' function for each prevent component and overall score.
#'
#' @inheritParams calculate_hep_components
prevent_calculations <- function(df,
                                 iso3,
                                 year,
                                 ind,
                                 type,
                                 transform_value,
                                 ind_ids) {

  df <- dplyr::group_by(df, .data[[iso3]], .data[[year]])

  args <- list(name = c("meningitis", "yellow_fever", "cholera", "polio", "measles", "prevent"),
               numerator = list(c("meningitis_campaign", "meningitis_routine"),
                                c("yellow_fever_campaign", "yellow_fever_routine"),
                                c("cholera_campaign"),
                                c("polio_routine"),
                                c("measles_routine"),
                                c("meningitis_campaign", "meningitis_routine", "yellow_fever_campaign", "yellow_fever_routine", "cholera_campaign", "polio_routine", "measles_routine")),
               denominator = list(c("meningitis_campaign_denom", "surviving_infants"),
                                  c("yellow_fever_campaign_denom", "surviving_infants"),
                                  c("cholera_campaign_denom"),
                                  c("surviving_infants"),
                                  c("surviving_infants"),
                                  c("meningitis_campaign_denom", "yellow_fever_campaign_denom", "cholera_campaign_denom", "surviving_infants")),
               multiply_surviving_infs = c(rep(FALSE, 5), TRUE))

  purrr::pmap_dfr(args,
                  pathogen_calc,
                  df = df,
                  ind = ind,
                  iso3 = iso3,
                  year = year,
                  transform_value = transform_value,
                  type = type,
                  ind_ids = ind_ids)
}

#' Calculate the vaccine coverage for a specific pathogen
#'
#' Using numerators and denominators pre-supplied for each ISO3 and year,
#' the overall vaccine coverage for a pathogen is calculated. This function
#' currently counts the number of routine vaccinations included in the numerator,
#' and multiplies the surviving infants denominator by that number.
#'
#' This function is currently called from the `prevent_calculations()` function
#' that sits within `calculate_hep_components()`.
#'
#' @inheritParams calculate_hep_components
#' @param name Name of pathogen to provide in the data frame.
#' @param numerators Indicator names for numerators.
#' @param denominators Indicator names for denominators.
#' @param multiply_surviving_infs Logical, multiple surviving infant population by
#'     number of routine vaccines in numerator.
#'
pathogen_calc <- function(df,
                          name,
                          ind,
                          iso3,
                          year,
                          numerators,
                          denominators,
                          transform_value,
                          type,
                          ind_ids,
                          multiply_surviving_infs = TRUE) {
  df <- dplyr::filter(df,
                      .data[[ind]] %in% ind_ids[names(ind_ids) %in% c(numerators, denominators)],
                      any(ind_ids[names(ind_ids) %in% numerators] %in% .data[[ind]]))

  if (multiply_surviving_infs) {
    df <- dplyr::mutate(df, !!sym(transform_value) := dplyr::case_when(
        .data[[ind]] == ind_ids[names(ind_ids) == "surviving_infants"] ~ .data[[transform_value]] * sum(unique(.data[[ind]]) %in% ind_ids[stringr::str_detect(names(ind_ids), "routine")]),
        TRUE ~ .data[[transform_value]]
      ))
  }

  df %>%
    dplyr::summarize(!!sym(transform_value) := 100 * sum(.data[[transform_value]][.data[[ind]] %in% ind_ids[names(ind_ids) %in% numerators]]) /
                       sum(.data[[transform_value]][.data[[ind]] %in% ind_ids[names(ind_ids) %in% denominators]]),
                     !!sym(type) := ifelse(length(unique(.data[[type]])) == 1, unique(.data[[type]]), "Projection"),
                     !!sym(ind) := name,
                     .groups = "drop")
}

#' Calculate HEPI
#'
#' Function to calculate HEPI as the average of DNR, Prepare, and Prevent.
#' Used within `calculate_hep_components()`.
#'
#' @inheritParams calculate_hep_components
#' @param earliest_year Earliest year for HEPI calculation.
calculate_hepi <- function(df,
                           iso3,
                           year,
                           ind,
                           type,
                           transform_value,
                           earliest_year,
                           ind_ids) {
  df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[names(ind_ids) %in% c("detect_respond",
                                                                  "prevent",
                                                                  "espar")],
                  .data[[year]] >= earliest_year) %>%
    dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
    dplyr::summarize(!!sym(transform_value) := mean(.data[[transform_value]],
                                                    na.rm = TRUE),
                     !!sym(type) := ifelse(length(unique(.data[[type]])) == 1,
                                           unique(.data[[type]]),
                                           "Projection"),
                     !!sym(ind) := "hep_idx",
                     .groups = "drop")
}

#' Calculate HEP Billion
#'
#' `calculate_hep_billion()` calculates country-level HEP Billion based on
#' the component indicators. It calculates the change for Prevent and Prepare between
#' a start year and end year, and estimated the change for Detect and Respond based on
#' its level in the end year. If data is not available for the end year for Detect and
#' Respond, the latest year of observed data is used. Details are available in
#' the methods report for the exact method applied.
#'
#' For more details on the HEPP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hep.html}{\code{vignette("hep", package = "billionaiRe")}}
#'
#' @inherit transform_hpop_data return params
#' @inheritParams calculate_hpop_contributions
#' @param level Column name of column with indicator levels.
#'
#' @export
calculate_hep_billion <- function(df,
                                  iso3 = "iso3",
                                  ind = "ind",
                                  year = "year",
                                  transform_value = "transform_value",
                                  level = "level",
                                  start_year = 2018,
                                  end_year = 2023,
                                  ind_ids = billion_ind_codes("hep")) {
  assert_columns(df, iso3, ind, year, transform_value, level)

  df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[names(ind_ids) %in% c("prevent",
                                                                  "detect_respond",
                                                                  "espar",
                                                                  "hep_idx")],
                  .data[[year]] <= end_year) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::filter(!((.data[[ind]] %in% ind_ids[names(ind_ids) == "detect_respond"]) & (.data[[year]] < max(.data[[year]], -Inf))),
                  !((.data[[ind]] %in% c("prevent", "espar", "hep_idx")) & !(.data[[year]] %in% c(start_year, end_year)))) %>%
    dplyr::arrange(.data[[year]],
                   .by_group = TRUE) %>%
    dplyr::mutate("change" := dplyr::case_when(
      .data[[ind]] %in% ind_ids[names(ind_ids) == "detect_respond"] ~ ifelse(.data[[level]] == 1,
                                                                             0,
                                                                             .data[[level]]),
      .data[[ind]] %in% ind_ids[names(ind_ids) %in% c("prevent", "espar", "hep_idx")] ~ .data[[transform_value]] - dplyr::lag(.data[[transform_value]])
    )) %>%
    dplyr::filter(.data[[year]] == max(.data[[year]])) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, year, ind, "change") %>%
    dplyr::mutate("contribution" := ifelse(.data[[ind]] == ind_ids[names(ind_ids) == "hep_idx"],
                                           NA,
                                           .data[["change"]] * wppdistro::get_population(.data[[iso3]], 2023) / 100)) %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::mutate("contribution" := ifelse(.data[[ind]] == ind_ids[names(ind_ids) == "hep_idx"],
                                           sum(.data[["contribution"]], na.rm = TRUE),
                                           .data[["contribution"]])) %>%
    dplyr::ungroup()
}

