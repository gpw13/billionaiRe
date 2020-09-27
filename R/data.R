#' Dataset of indicators used within the Billions calculations.
#'
#' A dataset containing various indicator codes and their uses within the GPW13
#' Billions. Its internal use in the package ensures that switching storage locations
#' for data (between GHO and xMart) does not impact the data pipeline, all that is
#' needed is to update this data frame.
#'
#' This file will eventually be stored within the GPW13 xMart4 instance, but until
#' a more stable version is realized, it is being managed within the billionaiRe
#' R package.
#'
#' @format A data frame with `r nrow(indicator_df)` rows and `r ncol(indicator_df)` variables:
#' \describe{
#'   \item{unique_id}{Unique ID for each indicator}
#'   \item{dashboard_id}{Dashboard ID used within the GPW13 xMart4 instance}
#'   \item{gho_code}{Indicator ID in the GHO}
#'   \item{gho_code_2}{Indicator ID in the GHO, if data stored in 2 separate locations}
#'   \item{gho_queries}{OData queries to use when accessing the GHO}
#'   \item{xmart_code}{Indicator ID in the xMart4 GPW13 instance}
#'   \item{analysis_code}{Code used in the analysis scripts within the billionaiRe package}
#'   \item{input}{Logical, data is an input into Billions calculation (sourced externally)}
#'   \item{calculated}{Logical, data is calculated as part of the Billions calculation (internally)}
#'   \item{covariate}{Logical, data is used as a covariate in infilling and projections}
#'   \item{storage_location}{Where the data is stored, GHO, xMart, or both}
#'   \item{data_source}{Source for the data, to be used if no source available for a data point}
#'   \item{uhc}{Logical, is a UHC Billions indicator}
#'   \item{hpop}{Logical, is an HPOP Billions indicator}
#'   \item{hep}{Logical, is a HEP Billions indicator}
#'   \item{hep_prev}{Logical, is a HEP Billions Prevent indicator}
#'   \item{hep_prep}{Logical, is a HEP Billions Prepare indicator}
#'   \item{hep_dr}{Logical, is a HEP Billions Detect & Respond indicator}
#'   \item{outcome}{Logical, is an outcome indicator}
#' }
"indicator_df"
