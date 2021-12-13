#' Indicator codes for the Billions
#'
#' Provides analysis codes required for input into the Triple Billion calculations.
#' The named vector returned by the function is the default names assumed to be
#' in any data frame passed to calculate Billions.
#'
#' @param billion Billion indicator names to return, either "hep", "hpop", "uhc"
#' , or "all".
#' @param include_covariates Logical, whether or not to include covariates when getting
#'     the Billions indicator codes.
#' @param include_calculated Logical, whether or not to include variables calculated
#'     from within the package when getting the Billion indicator codes. Useful
#'     for UHC average service coverage.
#'
#' @return Character vector of indicator names.
#'
#' @export
billion_ind_codes <- function(billion = c("hep", "hpop", "uhc", "all"),
                              include_covariates = FALSE,
                              include_calculated = FALSE) {
  billion <- rlang::arg_match(billion)
  df <- billionaiRe::indicator_df
  if (!include_covariates) {
    df <- dplyr::filter(df, !.data[["covariate"]])
  }

  if (!include_calculated) {
    df <- dplyr::filter(df, !.data[["calculated"]])
  }
  if (billion == "all") {
    codes <- df[["ind"]]
  } else {
    codes <- df[["ind"]][df[[billion]]]
  }
  names(codes) <- codes
  return(codes)
}

#' Convert indicator codes between types
#'
#' `convert_ind_codes()` allows easy conversion of indicator codes from dashboard
#' IDs, xMart IDs, and GHO codes.
#'
#' @param ind_codes Character vector of indicator codes
#' @param from Type of code being passed to `convert_ind_codes()`.
#' @param to Type of code to return.
#'
#' @return A character vector.
#'
#' @export
#'
convert_ind_codes <- function(ind_codes,
                              from = c("dashboard_id", "ind", "gho_code"),
                              to = c("dashboard_id", "ind", "gho_code")) {
  from <- rlang::arg_match(from)
  to <- rlang::arg_match(to)
  df <- billionaiRe::indicator_df
  df[[to]][match(ind_codes, df[[from]])]
}

#' Get metadata for a Triple Billions indicator(s)
#'
#' This is a convenience function that looks up the relevant metadata for one or
#' more indicators in the indicator_df table.
#'
#' @param ind_codes A character vector with indicator (analysis) codes
#' @param metadata_col The name of the indicator_df column with the desired metadata.
#' Must be One of "dashboard_id", "ind", "gho_code", "ind_type", "uhc",
#' "hpop", "hep","covariate", and "calculated"
#'
#' @return A character vector with the metadata. The positions correspond to the
#' order of the ind_codes input.
#'
#' @export
#' @examples
#' # Find the gho_code for UHC Tobacco
#' get_ind_metadata("uhc_tobacco", "gho_code")
#'
#' # Find the indicator type for multiple indicators
#' get_ind_metadata(c("alcohol", "hwf"), "ind_type")
get_ind_metadata <- function(ind_codes,
                             metadata_col = names(billionaiRe::indicator_df)) {
  # Assertions and checks
  metadata_col <- rlang::arg_match(metadata_col)
  assert_type(ind_codes, "character")
  testit::assert("The indicator codes are valid", {
    valid_inds <- purrr::map(c("hep", "hpop", "uhc"), ~ {
      billion_ind_codes(.x)
    }) %>%
      unlist()
    all(ind_codes %in% valid_inds)
  })

  # Get the indicator metadata
  output <- billionaiRe::indicator_df[[metadata_col]][match(ind_codes, billionaiRe::indicator_df[["ind"]])]

  # Ensure the function returns a non-null object
  testit::assert("The output is not NULL", !is.null(output))

  return(output)
}

#' Get SDI ratio data
#'
#' Matches country ISO3 codes to data in the `sdi_ratio` data frame.
#'
#' @param iso3 Country ISO3 codes.
#'
#' @return Numeric vector of SDI ratios.
get_sdi_ratio <- function(iso3) {
  billionaiRe::sdi_ratio[["sdiratio"]][match(iso3, billionaiRe::sdi_ratio[["iso3"]])]
}

#' Get country shares data
#'
#' Matches country ISO3 codes to data in the `country_shares` data frame.
#'
#' @param iso3 Country ISO3 codes.
#' @param billion Relevant Billion, one of "hep", "hpop", or "uhc".
#' @param share_type Type of share to return, either in total numbers of people ("n"),
#'     or as a percent of total projected population in 2023 ("perc").
#'
#' @export
get_country_shares <- function(iso3,
                               billion = c("hep", "hpop", "uhc"),
                               share_type = c("n", "perc")) {
  bill <- rlang::arg_match(billion)
  share_type <- rlang::arg_match(share_type)

  df <- dplyr::filter(
    billionaiRe::country_shares,
    .data[["billion"]] == bill
  )
  df[[paste0("share_", share_type)]][match(iso3, df[["iso3"]])]
}
