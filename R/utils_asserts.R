#' @noRd
assert_columns <- function(df, ...) {
  columns <- c(...)
  bad_cols <- columns[!(columns %in% names(df))]
  if (length(bad_cols) > 0) {
    stop(sprintf("Column(s) %s are not in the data frame",
                 paste(bad_cols, collapse = ", ")),
         call. = FALSE)
  }
}

#' @noRd
assert_strings <- function(...) {
  arg_names <- sys.call()[-1]
  args <- list(...)
  classes <- sapply(args, class)
  if (!all(classes == "character")) {
    stop(sprintf("%s must be a character vector of length one, not %s",
                 paste(arg_names[!classes == "character"], collapse = ", "),
                 paste(classes[!classes == "character"], collapse = ", ")),
         call. = FALSE)
  }
  lens <- sapply(args, length)
  if (!all(lens == 1)) {
    stop(sprintf("%s must be of length one, not length %s",
                 paste(arg_names[lens != 1], collapse = ", "),
                 paste(lens[lens != 1], collapse = ", ")),
         call. = FALSE)
  }
}

#' @noRd
assert_numeric <- function(df, ...) {
  args <- c(...)
  nums <- sapply(args, function(x) is.numeric(df[[x]]))
  if(!all(nums)) {
    stop(sprintf("%s must be numeric not %s",
                 paste(args[!nums], collapse = ", "),
                 paste(sapply(args[!nums], function(x) class(df[[x]])), collapse = ", ")),
         call. = FALSE)
  }
}

#' Assert that `x` is a character vector of length n
#'
#' @param x Supposed string to test
#' @param n Length to test
assert_string <- function(x, n) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!((is.character(x) & (lx == n)))) {
      stop(sprintf("`%s` must be a character vector of length %d, not %s of length %d.",
                   deparse(substitute(x)),
                   n,
                   class(x),
                   lx))
    }
  }
}

#' Assert that `df` is a data frame
#'
#' @param df Supposed data frame
assert_df <- function(df) {
  if (!is.data.frame(df)) {
    stop(sprintf("`df` must be a data frame, not a %s.",
                 class(df)[1]),
         call. = FALSE)
  }
}

#' Assert that ind_ids is the correct named vector
#'
#' @param ind_ids Indicator ids to check
#' @param billion Billion which we're checking for
assert_ind_ids <- function(ind_ids, billion) {
  ind_check <- billion_ind_codes(billion)
  ind_check_nms <- all(ind_check %in% names(ind_ids))
  if (!ind_check_nms) {
    stop(sprintf("`ind_ids` must be a named vector with all `billion_ind_codes('%s')` present as names.",
                 billion),
         call. = FALSE)
  }
}

#' Assert unique rows of df
#'
#' Makes sure there are distinct rows for each ind, iso3, and year
assert_unique_rows <- function(df,
                               ind,
                               iso3,
                               year,
                               ind_ids) {
  ind_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids)
  dist_df <- dplyr::distinct(ind_df, .data[[ind]], .data[[iso3]], .data[[year]])
  if (nrow(ind_df) != nrow(dist_df)) {
    stop("`df` does not have distinct rows for each combination of `ind`, `iso3`, and `year`, please make distinct.",
         call. = FALSE)
  }
}
