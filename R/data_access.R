#' Indicator codes for the Billions
#'
#' Provides analysis codes required for input into the Triple Billion calculations.
#' The named vector returned by the function is the default names assumed to be
#' in any data frame passed to calculate Billions.
#'
#' @param billion Billion indicator names to return, either "hep", "hpop", or "uhc".
#'
#' @return Character vector of indicator names.
#'
#' @export
billion_ind_codes <- function(billion = c("hep", "hpop", "uhc")) {
  billion <- rlang::arg_match(billion)
  df <- billionaiRe::indicator_df
  codes <- df[["analysis_code"]][df[[billion]]]
  names(codes) <- codes
  codes
}

#' Get SDI ratio data
#'
#' Matches country ISO3 codes to data in the `sdi_ratio` data frame.
#'
#' @param iso3 Country ISO3 codes.
#'
#' @return Numeric vector of SDI ratios.
get_sdi_ratio <- function(iso3) {
  sdi_ratio[['sdiratio']][match(iso3, sdi_ratio[['iso3']])]
}
