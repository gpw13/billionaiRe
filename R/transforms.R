#' Reverse indicator value
#'
#' Typically reversed from a maximum of 100
#'
#' @param x Indicator value.
#' @param lim Value to reverse from, defaults to 100.
reverse_ind <- function(x, lim = 100) {
  lim - x
}

#' Trim all transformed data
#'
#' Trims transform data to ensure values are between 0 to 100
#'
#' @inheritParams reverse_ind
#' @param min_val Minimum possible value for transform to take
#' @param max_val Maximum possible value for transform to take
trim_transforms <- function(x, min_val = 0, max_val = 100) {
  x <- pmin(x, max_val)
  pmax(x, min_val)
}

#' Transform blood pressure data
#'
#' @inheritParams reverse_ind
transform_bp <- function(x) {
  trim_transforms(x)
}

#' Untransform blood pressure data
#'
#' @inheritParams reverse_ind
untransform_bp <- function(x) {
  x
}


#' Transform UHC tobacco data
#'
#' Reverses the indicator and rescales from `[29,100]` to `[0,100]`
#'
#' @inheritParams reverse_ind
transform_uhc_tobacco <- function(x) {
  x <- reverse_ind(x)
  x <- scales::rescale(x, to = c(0, 100), from = c(29, 100))
  trim_transforms(x)
}

#' Transform hospital beds data
#'
#' Rescales hospital beds according to Billions methods report
#'
#' @inheritParams reverse_ind
transform_hosp_beds <- function(x) {
  x <- (x / 18) * 100
  trim_transforms(x)
}

#' Untransform hospital beds data
#'
#' Reverses transformation from [transform_hosp_beds()]
#'
#' @inheritParams reverse_ind
untransform_hosp_beds <- function(x) {
  x <- (x * 18) / 100
  x
}

#' Transform health workforce data
#'
#' Rescales healh workforce data  according to Billions methods report
#'
#' @inheritParams reverse_ind
transform_hwf <- function(x) {
  x <- (x / 154.74) * 100
  trim_transforms(x)
}

#' Untransform health workforce data
#'
#' Reverses transformation from [transform_hwf()]
#'
#' @inheritParams reverse_ind
untransform_hwf <- function(x) {
  x <- (x * 154.74) / 100
  x
}

#' Transform FPG data
#'
#' Rescales FPG data from `[70,0]` to `[0,100]`
#'
#' @inheritParams reverse_ind
transform_glucose <- function(x) {
  x <- scales::rescale(x, from = c(70, 0), to = c(0, 100))
  trim_transforms(x)
}

#' Unransform FPG data
#'
#' Reverses transformation from [transform_glucose()]
#'
#' @inheritParams reverse_ind
untransform_glucose <- function(x) {
  x <- scales::rescale(x, to = c(70, 0), from = c(0, 100))
  x
}

#' Transform alcohol data
#'
#' Rescales alcohol data according to Billions methods report
#'
#' @inheritParams reverse_ind
transform_alcohol <- function(x) {
  x <- 100 - (x * 4)
  trim_transforms(x)
}

#' Untransform alcohol data
#'
#' Untransforms alcohol data according to Billions methods report
#'
#' @inheritParams reverse_ind
untransform_alcohol <- function(x) {
  x <- (100 - x) / 4
  x
}


#' Transform road safety data
#'
#' Rescales road safety data using the SDI ratio according to the Billions methods report
#'
#' @inheritParams reverse_ind
#' @param iso3 Country ISO3 code.
transform_road_safety <- function(x, iso3) {
  sdi_rti <- get_sdi_ratio(iso3)
  x <- x * 5 * sdi_rti / 1000
  x <- reverse_ind(x)
  trim_transforms(x)
}

#' Untransform road safety data
#'
#' Unscales road safety data using the SDI ratio according to the Billions methods report
#'
#' @inheritParams reverse_ind
#' @param iso3 Country ISO3 code.
untransform_road_safety <- function(x, iso3) {
  sdi_rti <- get_sdi_ratio(iso3)
  x <- reverse_ind(x)
  x <- x * 1000 / (5 * sdi_rti)
  x
}

#' Transform suicide rate data
#'
#' Rescales suicide rate data according to the Billions methods report
#'
#' @inheritParams reverse_ind
transform_suicide_rate <- function(x) {
  x <- x * 5 * 20 * 100 / 100000
  x <- reverse_ind(x)
  trim_transforms(x)
}

#' Untransform suicide rate date
#'
#' Rescales transformed suicide rate date according to the Billions methods report
#'
#' @inheritParams reverse_ind
untransform_suicide_rate <- function(x) {
  x <- reverse_ind(x)
  x <- x * 100000 / (5 * 20 * 100)
  x
}

#' Transform transfats policy data
#'
#' Rescales transfats policy data according to the Billions methods report
#'
#' @inheritParams reverse_ind
transform_transfats <- function(x) {
  x <- 100 - 14.3 + 2.1 * x / 100
  trim_transforms(x)
}

#' Untransform transfats policy data
#'
#' Unscales transfats policy data according to the Billions methods report
#'
#' @inheritParams reverse_ind
untransform_transfats <- function(x) {
  x <- (x - 100 + 14.3) * 100 / 2.1
  round(x)
}

#' Reverse indicator and cap
#'
#' Reverse indicator and capbetween 0 and 100
#'
#' @inheritParams reverse_ind
transform_inversion <- function(x) {
  x <- reverse_ind(x)
  trim_transforms(x)
}
