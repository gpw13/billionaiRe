#' Calculate HEP Billion
#'
#' `calculate_hep_billion()` calculates country-level HEP Billion based on
#' the component indicators. It calculates the change for Prevent and Prepare between
#' a start year and end year, and estimated the change for Detect and Respond based on
#' its level in the end year. If data is not available for the end year for Detect and
#' Respond, the latest year of observed data is used. Details are available in
#' the methods report for the exact method applied.
#'
#' For more details on the HEP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hep.html}{\code{vignette("hep", package = "billionaiRe")}}
#'
#' @inherit transform_hpop_data return params
#' @inheritParams calculate_hpop_contributions
#' @inheritParams add_hpop_populations
#' @param level Column name of column(s) with indicator levels. Should be same length
#'     as `transform_value`.
#'
#' @export
calculate_hep_billion <- function(df,
                                  iso3 = "iso3",
                                  ind = "ind",
                                  year = "year",
                                  scenario = NULL,
                                  transform_value = "transform_value",
                                  level = stringr::str_replace(transform_value, "transform_value", "level"),
                                  contribution = stringr::str_replace(transform_value, "transform_value", "contribution"),
                                  start_year = 2018,
                                  end_year = 2019:2023,
                                  pop_year = 2023,
                                  ind_ids = billion_ind_codes("hep")) {
  assert_columns(df, iso3, ind, year, transform_value, level)
  assert_ind_ids(ind_ids, "hep")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)
  assert_years(start_year, end_year)
  assert_same_length(transform_value, level)
  assert_same_length(transform_value, contribution)

  bill_df <- df %>%
    dplyr::filter(.data[[ind]] %in% ind_ids[c("prevent",
                                              "detect_respond",
                                              "espar",
                                              "hep_idx")],
                  .data[[year]] %in% c(!!start_year, !!end_year)) %>%
    dplyr::mutate("_pop_temp" := wppdistro::get_population(.data[[iso3]], pop_year))

  bill_df <- billionaiRe_add_columns(bill_df, contribution, NA_real_)

  for (i in 1:length(contribution)) {
    bill_df <- bill_df %>%
      dplyr::group_by(dplyr::across(c(iso3, ind, scenario))) %>%
      dplyr::mutate(!!sym(contribution[i]) := calculate_hep_contribution(.data[[ind]],
                                                                         .data[[year]],
                                                                         !!start_year,
                                                                         .data[[transform_value[i]]],
                                                                         .data[[level[i]]],
                                                                         .data[["_pop_temp"]],
                                                                         !!ind_ids)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(c(iso3, year, scenario))) %>%
      dplyr::mutate(
        !!sym(contribution[i]) := ifelse(.data[[ind]] == ind_ids["hep_idx"],
                                         sum(.data[[contribution[i]]], na.rm = T),
                                         .data[[contribution[i]]])) %>%
      dplyr::ungroup()
  }

  bill_df <- bill_df %>%
    dplyr::filter(.data[[year]] %in% !!end_year) %>%
    dplyr::select(-"_pop_temp")

  # remove matching rows from original data frame
  keys <- c(iso3, year, ind, scenario)
  keys <- keys[!is.null(keys)]

  df <- dplyr::anti_join(df, bill_df, by = keys)

  dplyr::bind_rows(df, bill_df)
}


#' Calculates HEP contribution for use in calculate_hep_billion
#'
#' @noRd
calculate_hep_contribution <- function(ind, year, start_year, value, level, pop, ind_ids) {
  if (all(ind %in% ind_ids["detect_respond"])) {
    ifelse(level == 1,
           0,
           level * pop / 100)
  } else if (all(ind %in% ind_ids[c("espar", "prevent")])) {
    (value - value[year == start_year]) * pop / 100
  } else {
    NA_real_
  }
}
