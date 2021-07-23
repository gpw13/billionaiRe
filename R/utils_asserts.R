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
                   lx),
           call. = FALSE)
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


#' Assert that the data frame only has one value (is homogeneous) for a given column
#'
#' @param df input data frame
#' @param col_name string specifying the column to check
assert_homogeneous_col<- function(df, col_name) {

  if (length(unique(df[[col_name]])) > 1) {
    stop(
      sprintf("This function should have only one unique value in the %s column.", col_name),
      call. = FALSE
    )
  }
}

#' Assert unique rows of df
#'
#' Makes sure there are distinct rows for each ind, iso3, year, and scenario if
#' being used.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#'
assert_unique_rows <- function(df,
                               ind,
                               iso3,
                               year,
                               scenario = NULL,
                               ind_ids) {
  ind_df <- dplyr::filter(df, .data[[ind]] %in% ind_ids)
  dist_df <- dplyr::distinct(ind_df, dplyr::across(dplyr::any_of(c(ind, iso3, year, scenario))))
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


#' Warn user when any/all of the row are missing values for the the specified column
#'
#' @param df Input data frame
#' @param col_name string specifying the name of column
#' @param how string specifying whether to check for any/all missing values
warning_col_missing_values <- function(df,
                                       col_name,
                                       how) {

  if (how == "any") {
    if (any(is.na(df[[col_name]]))) {
      warning(sprintf("Some of the rows are missing a %s value.",
                      col_name),
              call. = FALSE)
    }
  } else {
    if (all(is.na(df[[col_name]]))) {
      warning(sprintf("All of the rows are missing a %s value.",
                      col_name),
              call. = FALSE)
    }
  }
}

#' Asserts that provided ISO is valid
#'
#' Checks that provided ISO code is a valid ISO3 code for a WHO member state,
#' using [whoville::is_who_member()].
#'
#' @param iso Single ISO3 code
assert_who_iso <- function(iso) {
  assert_string(iso, 1)
  if (!whoville::is_who_member(iso)) {
    stop(strwrap("`iso` must be a valid WHO member state ISO3 code.
                 All valid codes are available through `whoville::who_member_states()`."),
         call. = FALSE)

  }
}


#' Assert that `df` is a list
#'
#' @param df Supposed list
assert_list <- function(df) {
  if (!is.list(df)) {
    stop(sprintf("`df` must be a list, not a %s.",
                 class(df)[1]),
         call. = FALSE)
  }
}


#' Assert that summarize_hpop_country_data output data are present
#'
#' @param df Data frame following structure exported by `summarize_hpop_country_data()`
#' @param dfs character vector with names of data frame to be found in output
assert_summarize <- function(df,
                             dfs = c(
                               "ind_df",
                               "latest_reported",
                               "baseline_proj",
                               "hpop_contrib",
                               "hpop_billion",
                               "df_iso",
                               "transformed_time_series")){
  assert_list(df)

  bad_list <- dfs[!(dfs %in% names(df))]
  if (length(bad_list) > 0) {
    stop(sprintf("Data frame(s) %s are not in the list",
                 paste(bad_list, collapse = ", ")),
         call. = FALSE)
  }
}
