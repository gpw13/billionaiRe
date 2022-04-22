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
#'     as `transform_value_col`.
#'
#' @export
calculate_hep_billion <- function(df,
                                  scenario_col = NULL,
                                  transform_value_col = "transform_value",
                                  level_col = stringr::str_replace(transform_value_col, "transform_value", "level"),
                                  contribution_col = stringr::str_replace(transform_value_col, "transform_value", "contribution"),
                                  contribution_pct_col = paste0(contribution_col, "_percent"),
                                  start_year = 2018,
                                  end_year = 2019:2025,
                                  pop_year = 2025,
                                  ind_ids = billion_ind_codes("hep", include_calculated = TRUE)) {
  assert_columns(df, "iso3", "ind", "year", transform_value_col, level_col, scenario_col)
  assert_ind_ids(ind_ids, "hep")
  assert_unique_rows(df, scenario_col, ind_ids)
  assert_years(start_year, end_year)
  assert_same_length(transform_value_col, level_col)
  assert_same_length(transform_value_col, contribution_col)

  bill_df <- df %>%
    dplyr::filter(
      .data[["ind"]] %in% ind_ids[c(
        "prevent",
        "detect_respond",
        "espar",
        "hep_idx"
      )],
      .data[["year"]] %in% c(!!start_year, !!end_year)
    ) %>%
    dplyr::mutate("_pop_temp" := wppdistro::get_population(.data[["iso3"]], pop_year))

  bill_df <- billionaiRe_add_columns(bill_df, c(contribution_col, contribution_pct_col), NA_real_)

  for (i in 1:length(contribution_col)) {
    bill_df <- bill_df %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "ind", scenario_col)))) %>%
      dplyr::mutate(
        !!sym(contribution_pct_col[i]) := calculate_hep_contribution_pct(
          .data[["ind"]],
          .data[["year"]],
          !!start_year,
          .data[[transform_value_col[i]]],
          .data[[level_col[i]]],
          !!ind_ids
        ),
        !!sym(contribution_col[i]) := ifelse(.data[["ind"]] == ind_ids["hep_idx"],
          NA_real_,
          .data[[contribution_pct_col[i]]] * .data[["_pop_temp"]] / 100
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(c("iso3", "year", scenario_col)))) %>%
      dplyr::mutate(
        !!sym(contribution_col[i]) := ifelse(.data[["ind"]] == ind_ids["hep_idx"],
          sum(.data[[contribution_col[i]]], na.rm = T),
          .data[[contribution_col[i]]]
        ),
        !!sym(contribution_pct_col[i]) := ifelse(.data[["ind"]] == ind_ids["hep_idx"],
          100 * .data[[contribution_col[i]]] / .data[["_pop_temp"]],
          .data[[contribution_pct_col[i]]]
        )
      ) %>%
      dplyr::ungroup()
  }

  bill_df <- bill_df %>%
    dplyr::filter(.data[["year"]] %in% !!end_year) %>%
    dplyr::select(-"_pop_temp")

  # remove matching rows from original data frame
  df <- dplyr::anti_join(df, bill_df, by = c("iso3", "year", "ind", scenario_col))

  dplyr::bind_rows(df, bill_df)
}


#' Calculates HEP contribution for use in calculate_hep_billion
#'
#' @noRd
calculate_hep_contribution_pct <- function(ind, year, start_year, value, level, ind_ids) {
  if (all(ind %in% ind_ids["detect_respond"])) {
    ifelse(level == 1,
      0,
      level
    )
  } else if (all(ind %in% ind_ids[c("espar", "prevent")])) {
    if (length(value[year == start_year]) == 0) {
      NA_real_
    } else {
      value - value[year == start_year]
    }
  } else {
    NA_real_
  }
}
