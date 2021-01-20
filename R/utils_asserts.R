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

#' Assert that `x` is a character vector of length 1
#'
#' @param x Supposed string to test
assert_string_l1 <- function(x) {
  if (!is.null(x)) {
    lx <- length(x)
    if (!(is.character(x) & (lx == 1))) {
      stop(sprintf("`%s` must be a character vector of length 1, not %s of length %d.",
                   deparse(substitute(x)),
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

