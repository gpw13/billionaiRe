# Unclassified ------------------------------------------------------------

#' Assert that ind_ids is the correct named vector
#'
#' @param ind_ids Indicator ids to check
#' @param billion Billion which we're checking for
assert_ind_ids <- function(ind_ids, billion) {
  ind_check <- billion_ind_codes(billion)
  ind_check_nms <- all(ind_check %in% names(ind_ids))
  if (!ind_check_nms) {
    stop(sprintf(
      "`ind_ids` must be a named vector with all `billion_ind_codes('%s')` present as names.",
      billion
    ),
    call. = FALSE
    )
  }
}

#' Assert that end years are always later than start year
#'
#' @param start_year Start year
#' @param end_year End year(s)
assert_years <- function(start_year, end_year) {
  if (!all(start_year < end_year)) {
    stop("`end_year` must always be after `start_year`.",
      call. = FALSE
    )
  }
}

#' Assert that a file's extension is one of a few options
#'
#' @param file_names (character vector) The file names.
#' @param valid_exts (character vector) A list of the valid extensions.
assert_fileext <- function(file_names, valid_exts) {
  # Check that file_names and valid_exts are character vectors
  assert_type(file_names, "character")
  assert_type(valid_exts, "character")

  # Extract the file extensions
  ext <- stringr::str_match(file_names, "(.+)\\.(.+)")[, 3]

  if (any(is.na(ext))) {
    stop("One or more files do not have an extension.", call. = FALSE)
  }

  cond <- all(ext %in% valid_exts)
  if (!cond) {
    stop(
      sprintf("File extensions must be one of: {%s}.", paste(valid_exts, collapse = ", ")),
      call. = FALSE
    )
  }
}

#' Assert that the elements of the vector are unique
#'
#' @param x (vector)
assert_unique_vector <- function(x) {
  if (length(x) != length(unique(x))) {
    stop(sprintf("%s has duplicate elements", deparse(substitute(x))))
  }
}

# Data frame checks ------------------------------------------------------

#' Assert that columns exist in a data frame
#'
#' @param df Data frame
#' @param ... Column names
assert_columns <- function(df, ...) {
  columns <- c(...)
  bad_cols <- columns[!(columns %in% names(df))]
  if (length(bad_cols) > 0) {
    stop(sprintf(
      "Column(s) %s are not in the data frame",
      paste(bad_cols, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that the given data frame columns are numeric
#'
#' @param df Data frame
#' @param ... Column names
assert_numeric <- function(df, ...) {
  args <- c(...)
  nums <- sapply(args, function(x) is.numeric(df[[x]]))
  if (!all(nums)) {
    stop(sprintf(
      "%s must be numeric not %s",
      paste(args[!nums], collapse = ", "),
      paste(sapply(args[!nums], function(x) class(df[[x]])), collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Warn user when any/all of the row are missing values for the the specified column
#'
#' @param df Input data frame
#' @param col_name string specifying the name of column
#' @param how string specifying whether to check for any/all missing values
warning_col_missing_values <- function(df, col_name, how) {
  if (how == "any") {
    if (any(is.na(df[[col_name]]))) {
      warning(sprintf(
        "Some of the rows are missing a %s value.",
        col_name
      ),
      call. = FALSE
      )
    }
  } else {
    if (all(is.na(df[[col_name]]))) {
      warning(sprintf(
        "All of the rows are missing a %s value.",
        col_name
      ),
      call. = FALSE
      )
    }
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
      call. = FALSE
    )
  }
}

#' Assert that the data frame only has one value (is homogeneous) for a given column
#'
#' @param df input data frame
#' @param col_name string specifying the column to check
assert_homogeneous_col <- function(df, col_name) {
  if (length(unique(df[[col_name]])) > 1) {
    stop(
      sprintf("This function should have only one unique value in the %s column.", col_name),
      call. = FALSE
    )
  }
}

# Argument checks ---------------------------------------------------------
#' Assert that argument exists
#'
#' Check that a given argument exists and is not NA or NULL. Useful for functions
#' where an argument is required for the rest of the code to work.
#'
#' @param x argument to check
#' @param error_template A template for generating the error message. Used as the
#'   input to an `sprintf()` call. Must include %s, which corresponds to the input x.
assert_arg_exists <- function(x, error_template = "The %s argument is required and cannot be NA or NULL") {
  if (is.null(x) || is.na(x)) {
    stop(
      sprintf(error_template, deparse(substitute(x))),
      call. = FALSE
    )
  }
}

# Object type checks -----------------------------------------------------------

#' Assert that an object is (or is not) of a given type
#'
#' @param x The input object
#' @param expected_type (string) The expected type of x
#' @param reverse Invert the test (i.e., the type of x is not)
assert_type <- function(x, expected_type, reverse = FALSE) {
  assert_string(expected_type, 1)

  cond <- if (reverse) typeof(x) == expected_type else typeof(x) != expected_type
  msg <- if (reverse) "must not be" else "must be"
  msg <- paste("%s", msg, "of type %s")

  if (!is.null(x) & cond) {
    stop(sprintf(msg, deparse(substitute(x)), expected_type), call. = FALSE)
  }
}

#' Assert that `df` is a data frame
#'
#' @param df Supposed data frame
assert_df <- function(df) {
  if (!is.data.frame(df)) {
    stop(sprintf(
      "`df` must be a data frame, not a %s.",
      class(df)[1]
    ),
    call. = FALSE
    )
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
      stop(sprintf(
        "`%s` must be a character vector of length %d, not %s of length %d.",
        deparse(substitute(x)),
        n,
        class(x),
        lx
      ))
    }
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
    stop(sprintf(
      "%s must be a character vector of length one, not %s",
      paste(arg_names[!classes == "character"], collapse = ", "),
      paste(classes[!classes == "character"], collapse = ", ")
    ),
    call. = FALSE
    )
  }
  lens <- sapply(args, length)
  if (!all(lens == 1)) {
    stop(sprintf(
      "%s must be of length one, not length %s",
      paste(arg_names[lens != 1], collapse = ", "),
      paste(lens[lens != 1], collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

# Vector lengths ----------------------------------------------------------

#' Assert that a vector is of length n
#'
#' @param x (vector)
#' @param n (integer) the expected length of x
assert_length <- function(x, n) {
  l <- length(x)
  if (l != n) {
    stop(sprintf("%s must be a vector of length %s, not length %s", deparse(substitute(x)), n, l),
      call. = FALSE
    )
  }
}

#' Assert that a vector has a minimum length n
#'
#' @param x (vector)
#' @param n (integer) the minimum allowed size of the vector
assert_min_length <- function(x, n) {
  l <- length(x)
  if (l < n) {
    stop(sprintf("%s must have a minimum length of %s elements", deparse(substitute(x)), n),
      call. = FALSE
    )
  }
}

# Object matching ---------------------------------------------------------

#' Assert that x and y are (or are not) equal/identical
#'
#' @param x (vector)
#' @param y (vector)
#' @param identical (logical) whether to use the `identical()` function for the test
#' @param reverse (logical) whether to reverse the condition (i.e., the two vectors are
#'   not equal/identical)
#' @param msg_suffix (string) A string to be appended to the end of the error message
assert_equals <- function(x, y, identical = FALSE, reverse = FALSE, msg_suffix = NULL) {
  cond <- if (identical) identical(x, y) else x == y
  cond <- if (reverse) !cond else cond
  msg <- "%s must "
  msg <- if (reverse) paste0(msg, "not be ") else paste0(msg, "be ")
  msg <- if (identical) paste0(msg, "identical ") else paste0(msg, "equal ")
  msg <- paste0(msg, "to %s")

  if (!is.null(msg_suffix)) {
    assert_type(msg_suffix, "character")
    assert_length(msg_suffix, 1)
    msg <- paste(msg, msg_suffix)
  }

  if (!cond) {
    stop(
      sprintf(msg, deparse(substitute(x)), deparse(substitute(y))),
      call. = FALSE
    )
  }
}

#' Assert that all elements in x are members of y
#'
#' In other words, assert that x is a subset of y. Useful for ensuring that an
#' argument is one of a given set of options.
#'
#' @param x (vector)
#' @param y (vector)
assert_x_in_y <- function(x, y) {
  cond <- x %in% y
  if (!all(cond)) {
    stop(sprintf(
      "`%s` must be one of `%s`.",
      paste0(x[!cond], collapse = ", `"),
      deparse(substitute(y))
    ),
    call. = FALSE
    )
  }
}

#' Assert that two or more vectors are the same length
#'
#' @param ... Two or more vectors that should be the same length.
#' @param recycle (logical) Whether vectors of length one can be recycled to match the length
#'   of the other vectors.
#' @param remove_null (logical) whether NULL values should be removed from the inputs before
#'   comparison
assert_same_length <- function(..., recycle = FALSE, remove_null = FALSE) {
  # Extract just the names of the ... arguments
  arg_names <- sys.call()
  end_idx <- length(arg_names) - 2
  arg_names <- arg_names[2:end_idx]

  args <- list(...)

  # Ensure that the input has at least two vectors for comparison
  assert_min_length(args, 2)

  if (remove_null) {
    args <- args[!sapply(args, is.null)]
  }

  # If recycle = TRUE
  if (recycle) {
    length_one_vecs <- args[sapply(args, length) == 1]

    # If all the vectors are of length 1, then return immediately
    if (length(length_one_vecs) == length(args)) {
      return(invisible())
    }
    # Otherwise, remove the length one vectors from the list of vector to check
    # because they can always be replicated
    else {
      args <- args[sapply(args, length) != 1]
    }
  }

  cond <- purrr::map(args, length) %>%
    purrr::reduce(`==`)

  if (!cond) {
    stop(sprintf(
      "%s must have the same length.",
      paste(arg_names, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}
