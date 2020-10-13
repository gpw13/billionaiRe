#' Dataset of indicators used within the Billions calculations.
#'
#' A dataset containing dashboard and analysis indicator codes and their uses within the GPW13
#' Billions.
#'
#' @format A data frame with `r nrow(indicator_df)` rows and `r ncol(indicator_df)` variables:
#' \describe{
#'   \item{dashboard_id}{Dashboard ID used within the GPW13 xMart4 instance}
#'   \item{analysis_code}{Code used in the analysis scripts within the billionaiRe package}
#'   \item{uhc}{Logical, is a UHC Billions indicator}
#'   \item{hpop}{Logical, is an HPOP Billions indicator}
#'   \item{hep}{Logical, is a HEP Billions indicator}
#' }
"indicator_df"

#' Socio-Demographic Index data used in road safety transform.
#'
#' Used internally to transform road safety data. Exact methods available in methods report
#'
#' @format A data frame with `r nrow(indicator_df)` rows and `r ncol(indicator_df)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{sdiratio}{SDI ratio.}
#' }
"sdi_ratio"
