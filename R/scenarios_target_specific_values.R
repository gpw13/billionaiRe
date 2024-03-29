#' Scenario to add a linear percentage point aimed at quantiles
#'
#' `scenario_quantile` aims to reach the mean quantile average annual change
#' (ARC) in which a country is at `quantile_year`. The target is based on the
#' ARC between `quantile_year` and `baseline_quantile_year`. If ARC is under the mean
#' of the quantile, it will aim at the mean, and at the higher limit of the
#' quantile if above the mean.
#'
#' Calculates the quantile target, then wraps around
#' `scenario_linear_change_col` to aim at the target.
#'
#' @param n number of quantile to create (5 for quintile, 4 for quartiles, etc.)
#' @param quantile_year year at which the the quantiles ARC should be calculated.
#' @param baseline_quantile_year baseline year at which the quantiles ARC should
#' be calculated.
#' @inheritParams scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#'
#' @family comparing_scenario
#'
#' @export
#'
scenario_quantile <- function(df,
                              n = 5,
                              value_col = "value",
                              start_year = 2018,
                              end_year = 2025,
                              quantile_year = start_year,
                              baseline_quantile_year = start_year-5,
                              baseline_year = start_year,
                              scenario_name = glue::glue("quantile_{n}"),
                              scenario_col = "scenario",
                              trim = TRUE,
                              small_is_best = FALSE,
                              keep_better_values = TRUE,
                              upper_limit = 100,
                              lower_limit = 0,
                              trim_years = TRUE,
                              start_year_trim = start_year,
                              end_year_trim = end_year,
                              ind_ids = billion_ind_codes("all"),
                              default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col, scenario_col)
  assert_unique_rows(df, scenario_col, ind_ids = ind_ids)
  assert_ind_start_end_year(df, value_col, quantile_year, baseline_quantile_year, ind_ids = unique(df[["ind"]]))

  quantile_df <- df %>%
    dplyr::group_by(.data[["ind"]]) %>%
    dplyr::filter(.data[["year"]] %in% c(quantile_year, baseline_quantile_year)) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", value_col))) %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = !!value_col
    ) %>%
    dplyr::mutate(
      quantile = get_quantile(.data[[glue::glue("{quantile_year}")]], n = n),
      arc = (.data[[glue::glue("{quantile_year}")]] - .data[[glue::glue("{baseline_quantile_year}")]]) / (quantile_year - baseline_quantile_year)
    ) %>%
    dplyr::group_by(.data[["quantile"]]) %>%
    dplyr::mutate(
      maxq = max(.data[["arc"]]),
      meanq = mean(.data[["arc"]]),
      minq = min(.data[["arc"]]),
      qtarget = dplyr::case_when(
        !small_is_best & .data[["arc"]] < meanq ~ meanq,
        !small_is_best & .data[["arc"]] >= meanq ~ maxq,
        small_is_best & .data[["arc"]] > meanq ~ meanq,
        small_is_best & .data[["arc"]] <= meanq ~ minq
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("iso3", "ind", "qtarget") %>%
    dplyr::distinct()

  df %>%
    dplyr::left_join(quantile_df, by = c("iso3", "ind")) %>%
    scenario_linear_change_col(
      linear_value_col = "qtarget",
      value_col = value_col,
      start_year = start_year,
      end_year = end_year,
      baseline_year = baseline_year,
      target_year = end_year,
      scenario_name = scenario_name,
      scenario_col = scenario_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim,
      default_scenario = default_scenario
    ) %>%
    dplyr::select(-"qtarget")
}

#' Scenario to add a linear percentage point aimed at regional values
#'
#' `scenario_best_in_region` aims to reach the mean regional average annual change in
#' which a country is at `quantile_year`. The target is based on the ARC between
#' `quantile_year` and `quantile_year` - 5. If ARC is under the mean of the
#' region, it will aim at the mean, and at the best value of the quantile if
#' above the mean. `small_is_best` can be used to indicate is lower value is
#' best or not.
#'
#' Calculates the regional target, then wraps around
#' `scenario_linear_change_col` to aim at the target.
#'
#' @inheritParams scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#'
#' @family comparing_scenario
#'
#' @export

scenario_best_in_region <- function(df,
                                    value_col = "value",
                                    start_year = 2018,
                                    end_year = 2025,
                                    baseline_year = 2018,
                                    target_year = 2013,
                                    scenario_col = "scenario",
                                    scenario_name = "best_in_region",
                                    ind_ids = billion_ind_codes("all"),
                                    trim = TRUE,
                                    small_is_best = FALSE,
                                    keep_better_values = TRUE,
                                    upper_limit = 100,
                                    lower_limit = 0,
                                    trim_years = TRUE,
                                    start_year_trim = start_year,
                                    end_year_trim = end_year,
                                    default_scenario = "default") {
  assert_columns(df, "year", "iso3", "ind", value_col)
  assert_unique_rows(df, ind_ids = ind_ids)
  assert_ind_start_end_year(df, value_col, baseline_year, target_year, ind_ids = ind_ids[unique(df[["ind"]])])
  assert_strings(scenario_col)

  region_df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind")))) %>%
    dplyr::filter(.data[["year"]] %in% c(baseline_year, target_year)) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", value_col))) %>%
    tidyr::pivot_wider(
      names_from = "year",
      values_from = !!value_col
    ) %>%
    dplyr::mutate(
      region = whoville::iso3_to_regions(.data[["iso3"]]),
      arc = sign(-(target_year - baseline_year)) * (.data[[glue::glue("{baseline_year}")]] - .data[[glue::glue("{target_year}")]]) / abs(baseline_year - target_year)
    ) %>%
    dplyr::group_by(.data[["region"]]) %>%
    dplyr::mutate(
      maxq = max(.data[["arc"]]),
      meanq = mean(.data[["arc"]]),
      minq = min(.data[["arc"]]),
      rtarget = dplyr::case_when(
        !small_is_best & .data[["arc"]] < meanq ~ meanq,
        !small_is_best & .data[["arc"]] >= meanq ~ maxq,
        small_is_best & .data[["arc"]] > meanq ~ meanq,
        small_is_best & .data[["arc"]] <= meanq ~ minq
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("iso3", "ind", "rtarget") %>%
    dplyr::distinct()

  df %>%
    dplyr::left_join(region_df, by = c("iso3", "ind")) %>%
    scenario_linear_change_col(
      linear_value_col = "rtarget",
      value_col = value_col,
      start_year = start_year,
      end_year = end_year,
      baseline_year = start_year,
      target_year = end_year,
      scenario_name = scenario_name,
      scenario_col = scenario_col,
      trim = trim,
      small_is_best = small_is_best,
      keep_better_values = keep_better_values,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      trim_years = trim_years,
      start_year_trim = start_year_trim,
      end_year_trim = end_year_trim,
      default_scenario = default_scenario
    ) %>%
    dplyr::select(-c("rtarget"))
}
