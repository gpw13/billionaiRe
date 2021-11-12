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

#' Assert that argument exists
#'
#' Check that a given argument exists and is not NA or NULL. Useful for functions
#' where an argument is required for the rest of the code to work.
#'
#' @param x argument to check
assert_arg_exists <- function(x) {
  if (is.na(x) | is.null(x)) {
    stop(
      sprintf("The %s argument is required and cannot be NA or NULL", deparse(substitute(x))),
      call. = FALSE
    )
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

#' Assert that columns are numeric in a data frame
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
      ),
      call. = FALSE
      )
    }
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

#' Assert that two vectors are the same length
#'
#' @param ... Arguments to pass two vectors that should be the same length.
assert_same_length <- function(...) {
  arg_names <- sys.call()[-1]
  args <- list(...)
  lns <- sapply(args, length)

  if (lns[1] != lns[2]) {
    stop(sprintf(
      "%s must have the same length.",
      paste(arg_names, collapse = " and ")
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

#' Assert that an object is of a given type
#'
#' @param x The input object
#' @param expected_type The expected type of x
assert_type <- function(x, expected_type) {
  assert_string(expected_type, 1)
  if (typeof(x) != expected_type) {
    stop(sprintf("%s must be of type %s", deparse(substitute(x)), expected_type), call. = FALSE)
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
assert_iso3_not_empty <- function(df, iso3 = "iso3", scenario = NULL, value = "value") {
  empty_iso3 <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario)))) %>%
    dplyr::summarise(all_NA = dplyr::case_when(
      sum(is.na(.data[[value]])) / dplyr::n() == 1 ~ TRUE,
      TRUE ~ FALSE
    ), .groups = "drop") %>%
    dplyr::filter(.data[["all_NA"]])

  if (nrow(empty_iso3) > 0) {
    warning(sprintf(
      "%s have only missing values (in at least one scenario, if provided). \nMissing values in:\n",
      paste(unique(empty_iso3[[iso3]]), collapse = ", ")
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
                                  iso3 = "iso3",
                                  year = "year",
                                  value = "value",
                                  start_year = 2018,
                                  end_year = 2025,
                                  scenario = "scenario") {
  missing_years <- df %>%
    dplyr::filter(.data[[year]] %in% c(start_year, end_year)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario)))) %>%
    dplyr::select(dplyr::any_of(c(iso3, year, scenario))) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::n() < length(c(start_year, end_year))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!sym(year) := dplyr::case_when(
      .data[[year]] == start_year ~ end_year,
      .data[[year]] == end_year ~ start_year,
      TRUE ~ .data[[year]]
    ))

  if (nrow(missing_years) > 0) {
    warning(sprintf(
      "%s have missing start_year or end_year (in at least one scenario, if provided).
  Each iso3 and year (and scenario if provided) should have values for start_year and end_year for the billion's calculation to be done properly.
  Missing values in:\n",
      paste(unique(missing_years[[iso3]]), collapse = ", ")
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
                                      iso3 = "iso3",
                                      year = "year",
                                      value = "value",
                                      start_year = 2018,
                                      end_year = 2020,
                                      ind = "ind",
                                      ind_ids,
                                      scenario = "scenario") {
  if (!is.null(scenario)) {
    full_df <- tidyr::expand_grid(
      !!sym(iso3) := unique(df[[iso3]]),
      !!sym(ind) := ind_ids,
      !!sym(year) := c(start_year, end_year),
      !!sym(scenario) := unique(df[[scenario]])
    )
  } else {
    full_df <- tidyr::expand_grid(
      !!sym(iso3) := unique(df[[iso3]]),
      !!sym(ind) := ind_ids,
      !!sym(year) := c(start_year, end_year)
    )
  }

  missing_values <- df %>%
    dplyr::full_join(full_df, by = c(iso3, ind, year, scenario)) %>%
    dplyr::filter(
      .data[[year]] %in% c(start_year, end_year),
      .data[[ind]] %in% ind_ids
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario, ind)))) %>%
    dplyr::filter(is.na(.data[[value]])) %>%
    dplyr::select(-.data[[value]])

  if (nrow(missing_values) > 0) {
    stop(sprintf(
      "%s have missing values in start_year or end_year (in at least one scenario, if provided).
Each iso3 and year (and scenario if provided) should have values for start_year and end_year for the billion's calculation to be done properly.
Missing values in:\n",
      paste(unique(missing_values[[iso3]]), collapse = ", ")
    ),
    paste(utils::capture.output(print(missing_values)), collapse = "\n"),
    call. = FALSE
    )
  }
}


assert_scenario_in_df <- function(df, scenario, scenario_col = "scenario"){

  scenarios_in_col <- unique(df[[scenario_col]])

  if(any(!scenario %in% scenarios_in_col)){
    stop(sprintf(
      "%s not in `df` %s column",
      paste(scenario[!scenario %in% scenarios_in_col], collapse = ", "),
      scenario_col
    ),
    call. = FALSE
    )
  }

}
