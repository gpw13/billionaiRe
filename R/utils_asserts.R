#' Assert that columns exist in a data frame
#'
#' @param df Data frame
#' @param ... Column names
assert_columns <- function(df, ...) {
  columns <- c(...)
  bad_cols <- columns[!(columns %in% names(df))]
  if (length(bad_cols) > 0) {
    stop(sprintf("Column(s) %s are not in the data frame",
                 paste(bad_cols, collapse = ", ")),
         call. = FALSE)
  }
}

#' Assert that arguments passed in are length 1 character vectors
#'
#' @param ... Character vectors to check
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

#' Assert that columns are numeric in a data frame
#'
#' @param df Data frame
#' @param ... Column names
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
#' Makes sure there are distinct rows for each ind, iso3, year, and scenario if
#' being used.
#'
#' @inheritParams transform_hpop_data
#' @param year Column name of column with year.
assert_unique_rows <- function(df,
                               ind,
                               iso3,
                               year,
                               scenario = NULL,
                               ind_ids) {
  ind_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids)
  dist_df <- dplyr::distinct(ind_df, dplyr::across(dplyr::any_of(c(!!ind, !!iso3, !!year, !!scenario))))
  if (nrow(ind_df) != nrow(dist_df)) {
    stop("`df` does not have distinct rows for each combination of `ind`, `iso3`, and `year` (by `scenario` if present), please make distinct.",
         call. = FALSE)
  }
}

#' Assert that two vectors are the same length
#'
#' @param ... Arguments to pass two vectors that should be the same length.
assert_same_length <- function(...) {
  arg_names <- sys.call()[-1]
  args <- list(...)
  lns <- sapply(args, length)

  if (lns[1] != lns[2]) {
    stop(sprintf("%s must have the same length.",
                 paste(arg_names, collapse = " and ")),
         call. = FALSE)
  }
}

#' Assert that end years are always later than start year
#'
#' @param start_year Start year
#' @param end_year End year(s)
assert_years <- function(start_year, end_year) {
  if (!all(start_year < end_year)) {
    stop("`end_year` must always be after `start_year`.",
         call. = FALSE)
  }
}
