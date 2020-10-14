#' Reverse indicator value
#'
#' Typically reversed from a maximum of 100
reverse_ind <- function(x, lim = 100) {
  lim - x
}

#' Transform blood pressure data
#'
#' Reverses the indicator and rescales from [50,100] to [0,100]
transform_bp <- function(x) {
  x <- reverse_ind(x)
  x <- scales::rescale(x, to = c(0, 100), from = c(50, 100))
  pmax(0, x)
}

#' Transform UHC tobacco data
#'
#' Reverses the indicator and rescales from [29,100] to [0,100]
transform_uhc_tobacco <- function(x) {
  x <- reverse_ind(x)
  x <- scales::rescale(x, to = c(0, 100), from = c(29, 100))
  x <- pmin(x, 100)
  pmax(x, 0)
}

#' Transform hospital beds data
#'
#' Rescales hospital beds according to Billions methods report
transform_hosp_beds <- function(x) {
  pmin((x / 18) * 100, 100)
}

#' Transform health workforce data
#'
#' Rescales healh workforce data  according to Billions methods report
transform_hwf <- function(x) {
  pmin((x / 154.74) * 100, 100)
}

#' Transform FPG data
#'
#' Rescales FPG data from [7.1,5.1] to [0,100]
transform_glucose <- function(x) {
  x <- scales::rescale(x, from = c(7.1, 5.1), to = c(0,100))
  x <- pmin(x, 100)
  pmax(x, 0)
}

#' Transform alcohol data
#'
#' Rescales hospital data according to Billions methods report
transform_alcohol <- function(x) {
  x <- 100 - (x * 4)
  pmax(0, x)
}

#' Transform road safety data
#'
#' Rescales road safety data using the SDI ratio according to the Billions methods report
transform_road_safety <- function(x, iso3) {
  sdi_rti <- get_sdi_ratio(iso3)
  x <- x * 5 * sdi_rti / 1000
  reverse_ind(x)
}

#' Transform suicide rate data
#'
#' Rescales suicide rate data according to the Billions methods report
transform_suicide_rate <- function(x) {
  x <- x * 5 * 20 * 100 / 100000
  reverse_ind(x)
}

#' Transform transfats policy data
#'
#' Rescales transfats policy data according to the Billions methods report
transform_transfats <- function(x) {
  100 - 14.3 + 2.1 * x / 100
}

trim_clean_fuels <- function(x) {
  x <- pmin(x, 95)
  pmax(x, 5)
}
