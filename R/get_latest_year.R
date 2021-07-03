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
                            yellow_fever_latest_year,
                            ebola_latest_year,
                            covid_latest_year,
                            measles_latest_year) {

  purrr::map2(ind_ids[c("cholera_campaign_num",
                        "meningitis_campaign_num",
                        "yellow_fever_campaign_num",
                        "ebola_campaign_num",
                        "covid_campaign_num",
                        "measles_campaign_num")],
              c(cholera_latest_year,
                meningitis_latest_year,
                yellow_fever_latest_year,
                ebola_latest_year,
                covid_latest_year,
                measles_latest_year),
              get_latest_year_single,
              df,
              ind,
              year)
}

#' @noRd
get_latest_year_single <- function(pathogen,
                                   pathogen_year,
                                   df,
                                   ind,
                                   year) {
  if (is.null(pathogen_year)) {
    df %>%
      dplyr::filter(.data[[ind]] %in% pathogen) %>%
      dplyr::pull(.data[[year]]) %>%
      max(-Inf, na.rm = TRUE) %>%
      valid_year()
  } else {
    pathogen_year
  }
}

#' Check if year value is valid and controls for infinites
#'
#' @param x Year
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
