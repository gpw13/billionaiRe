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

#' Assert that x is a valid timestamp string
#'
#' @param x (string)
assert_timestamp <- function(x) {
  if (!stringr::str_detect(x, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}$")) {
    stop(sprintf("%s is not a valid `yyyy-mm-ddTHH-MM-SS` formatted string", x))
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

#' Assert that the column types of a data frame are as expected
#'
#' @param df a data frame
#' @param expected a **named vector**, whose names are the names of the columns (i.e.,
#' `names(df) == names(expected)`) and whose elements are the expected class/type
#' of the column.
assert_col_types <- function(df, expected) {
  assert_type(expected, "character")
  assert_has_names(expected)
  assert_df(df)

  # Create an empty data frame with the given column names and types
  expected_df <- readr::read_csv(I("\n"), col_names = names(expected), col_types = expected)

  # Compare using waldo
  compare_obj <- waldo::compare(
    utils::head(df, 0), # Remove all rows so the comparison is only on the df columns
    expected_df,
    x_arg = "df",
    y_arg = "expected",
    list_as_map = TRUE # Ensures column order is not considered
  )

  if (length(compare_obj) != 0) {
    lab <- deparse(substitute(df))

    rlang::abort(
      sprintf("The columns of `%s` do not have the expected types.\n", lab),
      body = compare_obj
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
                               scenario_col = NULL,
                               ind_ids) {
  ind_df <- dplyr::filter(df, .data[["ind"]] %in% ind_ids)
  dist_df <- dplyr::distinct(ind_df, dplyr::across(dplyr::any_of(c("ind", "iso3", "year", scenario_col))))
  if (nrow(ind_df) != nrow(dist_df)) {
    stop("`df` does not have distinct rows for each combination of `ind`, `iso3`, and `year` (by `scenario_col` if present), please make distinct.",
      call. = FALSE
    )
  }
}

#' Assert unique rows by key columns
#'
#' Ensures that all the rows of a given data frame are unique for each combination
#' of a set of key columns.
#'
#' @param df a data frame
#' @param key_cols (character vector) the names of the key columns
assert_distinct_rows <- function(df, key_cols) {
  assert_type(key_cols, "character")

  distinct_df <- dplyr::distinct(df, dplyr::across(dplyr::all_of(key_cols)))
  if (nrow(df) != nrow(distinct_df)) {
    stop(sprintf(
      "`%s` does not have distinct rows for each combination of (%s)",
      deparse(substitute(df)),
      paste("`", key_cols, "`", sep = "", collapse = ", ")
    ))
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

#' Assert that paired columns have values
#'
#' Ensures that if rows of a given column has a value (i.e., is not NA), then
#' other columns in those rows also have a value (i.e., are not NA). Useful for
#' enforcing dependencies between columns.
#'
#' @param df a data frame
#' @param col_name (string) the name of a column in the data frame
#' @param pair_cols (character vectors) the names of the columns which are paired
#'   with the `col_name` column
assert_col_paired_with <- function(df, col_name, pair_cols) {
  assert_string(col_name, 1)
  assert_type(pair_cols, "character")
  assert_columns(df, c(col_name, pair_cols))

  df <- dplyr::filter(df, !is.na(.data[[col_name]]))

  invalid_rows <- df %>%
    dplyr::filter(dplyr::if_any(tidyselect::all_of(pair_cols), is.na))

  if (nrow(invalid_rows) != 0) {
    lab <- deparse(substitute(df))
    cli::cli_abort(
      "All rows with non-NA values for {.field {col_name}} must have a
      corresponding non-NA value for {cli::qty(pair_cols)} column{?s} {.field {pair_cols}}")
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

#' Assert that an object is (or is not) of a given (range of) type(s)
#'
#' @param x The input object
#' @param expected (character) The expected type(s) of x
#' @param reverse Invert the test (i.e., the type of x is not)
assert_type <- function(x, expected, reverse = FALSE) {
  stopifnot(typeof(expected) == "character", typeof(reverse) == "logical")

  cond <- any(typeof(x) == expected)
  cond <- if (reverse) !cond else cond

  if (!cond) {
    msg <- if (reverse) "must **not** be one of" else "must be one of"
    msg <- sprintf(
      "The type of %s %s {%s}.",
      deparse(substitute(x)),
      msg,
      paste0(expected, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
}

#' Assert that an object is (or is not) of a given (range of) class(es)
#'
#' @param x The input object
#' @param expected (character) The expected class(es) of x
#' @param reverse Invert the test (i.e., the class of x is not)
#' @param how One of "any" and "all". When `expected` is a vector, and
#'   * `how = "any"`, the test is passed if `x` inherits from any of the elements
#'     of `expected`
#'   * `how = "all"`, the test is passed only is `x` inherits from all the elements
#'     of `expected`.
assert_class <- function(x, expected, reverse = FALSE, how = c("any", "all")) {
  how <- rlang::arg_match(how)
  assert_type(expected, "character")
  assert_type(reverse, "logical")

  if (how == "any") {
    cond <- any(expected %in% class(x))
  } else {
    cond <- identical(sort(expected), sort(class(x)))
  }

  cond <- if (reverse) !cond else cond

  if (!cond) {
    reverse_toggle <- if (reverse) "must **not**" else "must"
    how_toggle <- paste(how, "of")
    msg <- sprintf(
      "`%s` %s inherit from %s {%s}.",
      deparse(substitute(x)),
      reverse_toggle,
      how_toggle,
      paste0(expected, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
}

#' Assert that an object has names
#'
#' @param x an object
assert_has_names <- function(x) {
  if (is.null(names(x))) {
    lab <- deparse(substitute(x))
    cli::cli_abort("{.var {lab}} must be a named object.")
  }
}

#' Assert that `df` is a data frame
#'
#' @param df Supposed data frame
assert_df <- function(df) {
  assert_class(df, "data.frame")
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

# rapporteur --------------------------------------------------------------

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
      call. = FALSE
    )
  }
}


#' Assert that `df` is a list
#'
#' @param df Supposed list
assert_list <- function(df) {
  if (!is.list(df)) {
    stop(sprintf(
      "`df` must be a list, not a %s.",
      class(df)[1]
    ),
    call. = FALSE
    )
  }
}

#' Assert that `params` are valid formal argument to [openxlsx::createStyle()]
#'
#' @param ... character vector of parameters to [openxlsx::createStyle()]
assert_style_param <- function(...) {
  params <- list(...)
  createStylesParams <- names(formals(openxlsx::createStyle))
  bad_params <- params[!names(params) %in% createStylesParams]

  if (length(bad_params) > 0) {
    stop(sprintf(
      "Params(s) %s are not valid formal argument to openxlsx::createStyle",
      paste(bad_params, collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that x is in list or is NULL
#'
#' @param x value to be checked
#' @param list list of values to be checked against
assert_in_list_or_null <- function(x, list) {
  if (!is.null(x)) {
    if (!x %in% list) {
      stop(sprintf(
        "%s must be present in %s or NULL",
        x, paste(list, collapse = ", ")
      ),
      call. = FALSE
      )
    }
  }
}

#' Asserts that iso3 (and scenario if provided) is not only NAs
#'
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
assert_iso3_not_empty <- function(df, scenario_col = NULL, value_col = "value") {
  empty_iso3 <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", scenario_col)))) %>%
    dplyr::summarise(all_NA = dplyr::case_when(
      sum(is.na(.data[[value_col]])) / dplyr::n() == 1 ~ TRUE,
      TRUE ~ FALSE
    ), .groups = "drop") %>%
    dplyr::filter(.data[["all_NA"]])

  if (nrow(empty_iso3) > 0) {
    warning(sprintf(
      "%s have only missing values (in at least one scenario, if provided). \nMissing values in:\n",
      paste(unique(empty_iso3[["iso3"]]), collapse = ", ")
    ),
    paste(utils::capture.output(print(empty_iso3)), collapse = "\n"),
    call. = FALSE
    )
  }
}

#' Asserts start and end year are present
#'
#' Asserts that there are values at the start and end year by iso3 (and
#' scenarios if provided).
#'
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams calculate_hpop_contributions

assert_start_end_year <- function(df,
                                  value_col = "value",
                                  start_year = 2018,
                                  end_year = 2025,
                                  scenario_col = "scenario") {
  missing_years <- df %>%
    dplyr::filter(.data[["year"]] %in% c(start_year, end_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", scenario_col)))) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", scenario_col))) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::n() < length(c(start_year, end_year))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!sym("year") := dplyr::case_when(
      .data[["year"]] == start_year ~ end_year,
      .data[["year"]] == end_year ~ start_year,
      TRUE ~ .data[["year"]]
    ))

  if (nrow(missing_years) > 0) {
    warning(sprintf(
      "%s have missing start_year or end_year (in at least one scenario, if provided).
  Each iso3 and year (and scenario_col if provided) should have values for start_year and end_year for the billion's calculation to be done properly.
  Missing values in:\n",
      paste(unique(missing_years[["iso3"]]), collapse = ", ")
    ),
    paste(utils::capture.output(print(missing_years)), collapse = "\n"),
    call. = FALSE
    )
  }

  return(df)
}

#' Asserts indicators have values at start and end year
#'
#' Asserts that there are values at the start and end year for all indicators
#' provided in ind_ids, by iso3 (and scenarios if provided).
#'
#' @param ind_ids named list of indicators to be checked for values. Follows
#' similar structure as `billion_ind_codes` indicator lists.
#' @inheritParams transform_hpop_data
#' @inheritParams transform_hep_data
#' @inheritParams calculate_hpop_contributions
assert_ind_start_end_year <- function(df,
                                      value_col = "value",
                                      start_year = 2018,
                                      end_year = 2020,
                                      ind_ids,
                                      scenario_col = "scenario") {
  if (!is.null(scenario_col)) {
    full_df <- tidyr::expand_grid(
      iso3 = unique(df[["iso3"]]),
      ind = ind_ids,
      year = c(start_year, end_year),
      !!sym(scenario_col) := unique(df[[scenario_col]])
    )
  } else {
    full_df <- tidyr::expand_grid(
      iso3 = unique(df[["iso3"]]),
      ind = ind_ids,
      !!sym(year) := c(start_year, end_year)
    )
  }

  missing_values <- df %>%
    dplyr::full_join(full_df, by = c("iso3", "ind", "year", scenario_col)) %>%
    dplyr::filter(
      .data[["year"]] %in% c(start_year, end_year),
      .data[["ind"]] %in% ind_ids
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", scenario_col, "ind")))) %>%
    dplyr::filter(is.na(.data[[value]])) %>%
    dplyr::select(-.data[[value]])

  if (nrow(missing_values) > 0) {
    stop(sprintf(
      "%s have missing values in start_year or end_year (in at least one scenario, if provided).
Each iso3 and year (and scenario_col if provided) should have values for start_year and end_year for the billion's calculation to be done properly.
Missing values in:\n",
      paste(unique(missing_values[[iso3]]), collapse = ", ")
    ),
    paste(utils::capture.output(print(missing_values)), collapse = "\n"),
    call. = FALSE
    )
  }
}

assert_vector_in_column <- function(df, vector, column) {
  unique_col_value <- unique(df[[column]])

  if (any(!vector %in% unique_col_value)) {
    stop(sprintf(
      "%s not in %s column",
      paste(vector[!vector %in% unique_col_value], collapse = ", "),
      column
    ),
    call. = FALSE
    )
  }
}

assert_scenario_in_df <- function(df, scenario, scenario_col = "scenario") {
  assert_vector_in_column(df, scenario, scenario_col)
}

assert_ind_ids_in_df <- function(df, ind_ids, ind_col = "ind", by_iso3 = TRUE, iso3_col = "iso3") {
  if (by_iso3) {
    df %>%
      dplyr::group_by(.data[[iso3_col]]) %>%
      dplyr::group_walk(~ assert_vector_in_column(df = .x, vector = ind_ids, column = ind_col))
  } else {
    assert_vector_in_column(df, ind_ids, ind_col)
  }
}
