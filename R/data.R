#' Dataset of indicators used within the Billions calculations.
#'
#' A dataset containing dashboard and analysis indicator codes and their uses within the GPW13
#' Billions.
#'
#' @format A data frame with `r nrow(indicator_df)` rows and `r ncol(indicator_df)` variables:
#' \describe{
#'   \item{dashboard_id}{Dashboard ID used within the GPW13 xMart4 instance}
#'   \item{ind}{Code used in the analysis scripts within the billionaiRe package}
#'   \item{gho_code}{GHO storage code for indicator}
#'   \item{ind_type}{"proj_data" if the indicator is projected by a technical programme,
#'       and "unproj_data" if projected by DDI}
#'   \item{uhc}{Logical, is a UHC Billions indicator}
#'   \item{hpop}{Logical, is an HPOP Billions indicator}
#'   \item{hep}{Logical, is a HEP Billions indicator}
#'   \item{covariate}{Logical, is the indicator just a covariate for the Billions}
#'   \item{calculated}{Logical, is the indicator calculated within the package and thus not an input}
#'   \item{sdg}{Indicator code from SDG and attributed if not in SDG}
#'   \item{short_name}{Short name of the indicator}
#'   \item{unit_raw}{Unit of raw values for indicator}
#'   \item{unit_transformed}{Unit of raw values for indicator}
#'   \item{medium_name}{Medium length name of indicator (typically used for tables)}
#'   \item{transformed_name}{Name of indicator after transformation}
#'   \item{order}{Order of the indicators}
#'   \item{pillar}{Pillar in which the indicators sits for UHC and HEP (e.g. Prevent, Non communicable diseases (NCDs), etc.)}
#'   \item{small_is_best}{Logical, is a lower value is a better public health outcome.}
#'   \item{gho_query}{Query to be passed to ghost to retrieve the data}
#'   \item{data_source}{Name of the source of the data for a specific indicator.}
#'   \item{acceleration_scenario}{Definition of the acceleration scenario.}
#'   \item{sdg_scenario}{Definition of the acceleration scenario.}
#' }
#'
#' @family metadata
#'
"indicator_df"

#' HPOP Billion population links
#'
#' A dataset linking each HPOP Billion indicator to relevant population groups
#' to be used for double counting correction. Used within `generate_hpop_populations()`.
#'
#' @format A data frame with `r nrow(pop_links)` rows and `r ncol(pop_links)` variables:
#' \describe{
#'   \item{ind}{HPOP indicator code.}
#'   \item{pop_group}{Population group.}
#' }
#'
#' @family populations
"pop_links"

#' Socio-Demographic Index data
#'
#' Used internally to transform road safety data for the HPOP Billion. Exact methods available in methods report.
#'
#' @format A data frame with `r nrow(sdi_ratio)` rows and `r ncol(sdi_ratio)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{sdiratio}{SDI ratio.}
#' }
#'
#' @keywords internal
"sdi_ratio"

#' HPOP generated example data
#'
#' Generated (fake) HPOP data used to test the Billions calculations code within the billionaiRe
#' package.
#'
#' See the HPOP vignette for its example use:
#'
#' \href{../doc/hpop.html}{\code{vignette("hpop", package = "billionaiRe")}}
#'
#' @format A data frame with `r nrow(hpop_df)` rows and `r ncol(hpop_df)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{year}{Year.}
#'   \item{ind}{HPOP indicator code.}
#'   \item{value}{Raw indicator value.}
#'   \item{type}{Data type.}
#' }
#'
#' @family hpop
"hpop_df"

#' UHC example data
#'
#' Fake UHC data used to test the Billions calculations code within the billionaiRe
#' package.
#'
#' See the UHC vignette for its example use:
#'
#' \href{../doc/uhc.html}{\code{vignette("uhc", package = "billionaiRe")}}
#'
#' @format A data frame with `r nrow(uhc_df)` rows and `r ncol(uhc_df)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{year}{Year.}
#'   \item{ind}{UHC indicator code.}
#'   \item{value}{Raw indicator value.}
#'   \item{type}{Data type.}
#' }
#'
#' @family uhc
"uhc_df"

#' HEP generated example data
#'
#' Generated (fake) HEP data used to test the Billions calculations code within the billionaiRe
#' package.
#'
#' See the HEP vignette for its example use:
#'
#' \href{../doc/hep.html}{\code{vignette("hep", package = "billionaiRe")}}
#'
#' @format A data frame with `r nrow(hep_df)` rows and `r ncol(hep_df)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{year}{Year.}
#'   \item{ind}{HPOP indicator code.}
#'   \item{value}{Raw indicator value.}
#'   \item{type}{Data type.}
#' }
#'
#' @family hep
"hep_df"

#' Country shares data
#'
#' Country shares data for UHC and HPOP Billions for all 194 WHO member
#' states.
#'
#' @format A data frame with `r nrow(country_shares)` rows and `r ncol(country_shares)` variables:
#' \describe{
#'   \item{iso3}{Country ISO3 codes.}
#'   \item{billion}{Relevant billion}
#'   \item{share_n}{Share, in number of people.}
#'   \item{share_perc}{Share, as percent of total projected population in 2023.}
#' }
#'
#' @keywords internal

"country_shares"
