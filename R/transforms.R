#' @export
reverse_ind <- function(x, lim = 100) {
  lim - x
}

#' @export
transform_bp <- function(x) {
  x <- reverse_ind(x)
  x <- scales::rescale(x, to = c(0, 100), from = c(50, 100))
  pmax(0, x)
}

#' @export
transform_tobacco <- function(x) {
  x <- reverse_ind(x)
  x <- scales::rescale(x, to = c(0, 100), from = c(30, 100))
  x <- pmin(x, 100)
  pmax(x, 0)
}

#' @export
transform_hosp_beds <- function(x) {
  pmin((x / 18) * 100, 100)
}

#' @export
transform_hwf <- function(x) {
  pmin((x / 154.74) * 100, 100)
}

#' @export
transform_glucose <- function(x) {
  x <- scales::rescale(x, from = c(7.1, 5.1), to = c(0,100))
  x <- pmin(x, 100)
  pmax(x, 0)
}

#' @export
transform_alcohol <- function(x) {
  x <- 100 - (x * 4)
  pmax(0, x)
}

#' @export
transform_road_safety <- function(x, rti) {
  x <- x * 5 * rti / 1000
  reverse_ind(x)
}

#' @export
transform_suicide_rate <- function(x) {
  x <- x * 5 * 20 * 100 / 10000
  reverse_ind(x)
}

#' @export
transform_totl_pop <- function(x_mle, x_fmle, pop_male, pop_fmle) {
  pop_totl <- pop_male + pop_fmle
  x <- (x_mle * pop_male) + (x_fmle * pop_fmle)
  x / pop_totl
}
