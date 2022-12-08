#' Scenario to reach the top performing rate of change countries
#'
#' `scenario_top_n_iso3` aims to reach the top `n` (or `prop`) performing countries
#' Annual Rate of Change (AAROC) in `df` `default_scenario` (by `group_col`
#' when provided). This is done by calculated the annual rate of change of all
#' countries with at least two reported or estimated values between `baseline_year`
#' and `aroc_end_year`. Then the values of the top `n` or top `prop` percent
#' performing countries (in `group_col` if provided) are averaged out to have a
#' single AROC at which all countries (by `group_col` if provided) will aim.
#'
#' If `prop` is used and there is an insufficient number of countries to select
#' at least one, then the best perfoming is kept. For instance, if there are two
#' countries with data and `prop` is 0.1, then it is not possible to select 10%
#' of 2 countries. So only the best performing will be kept.
#'
#' In this case, best performing is defined by the direction identified in
#' `indicator_df` `small_is_best` column.
#'
#' @param n (integer) number of countries to picked in the top performing group.
#' @param group_col (character) string identifying by which column the top should
#' be grouped by usinge `dplyr::group_by`. Default to NULL.
#' @param use_prop (Boolean) identifying if `prop` should be used instead of `n`
#' in `dplyr::slice_max` or `dplyr::slice_min`
#' @param prop proportion of countries to be selected. See `dplyr::slice_max()`
#' for details.
#' @param aroc_end_year (integer) year identifying the end of the AROC interval.
#' @param no_data_no_scenario (Boolean) if TRUE, then no scenario at all is generated
#' when there is less than 2 reported/estimated values between `baseline_year`
#' and `aroc_end_year`
#' @inherit scenario_fixed_target
#' @inheritParams trim_values
#' @inheritParams transform_hpop_data
#' @inheritParams accelerate_alcohol

scenario_top_n_iso3 <- function(df,
                                n = 10,
                                group_col = NULL,
                                use_prop = FALSE,
                                prop = NULL,
                                value_col = "value",
                                scenario_col = "scenario",
                                start_year = 2018,
                                end_year = 2025,
                                baseline_year = 2013,
                                aroc_end_year = 2018,
                                target_year = end_year,
                                scenario_name = glue::glue("top_{n}_{group_col}_aroc"),
                                small_is_best = FALSE,
                                trim = TRUE,
                                keep_better_values = TRUE,
                                upper_limit = 100,
                                lower_limit = 0,
                                trim_years = TRUE,
                                ind_ids = billion_ind_codes("all"),
                                bau_scenario = "historical",
                                default_scenario = "default",
                                no_data_no_scenario = FALSE,
                                ...){

  if(use_prop){
    n <- glue::glue("{prop*100}_percent")
  }

  if(is.null(group_col)){
    scenario_name <- glue::glue("top_{n}_aroc")
  }else{
    scenario_name <- glue::glue("top_{n}_{group_col}_aroc")
  }

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario)

  df_full_year <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "ind", "iso3", scenario_col))

  if(!is.null(group_col)){
    df_full_year <- df_full_year %>%
      dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
      tidyr::fill(dplyr::all_of(group_col), .direction = "down") %>%
      dplyr::ungroup()
  }

  df_with_data <- df_full_year %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) > 1) %>%
    dplyr::filter(sum(.data[["year"]] %in% c(baseline_year:aroc_end_year) & .data[["type"]] %in% c("estimated", "reported")) > 1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_without_data <- df_full_year %>%
    dplyr::anti_join(df_with_data, by = c("iso3", "year", "ind")) %>%
    dplyr::filter(.data[[scenario_col]] %in% c(default_scenario, bau_scenario))

  if(nrow(df_with_data)>0){
    df_aroc <- df_with_data %>%
      dplyr::filter(.data[["type"]] %in% c("estimated", "reported")) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, "iso3", "ind")))) %>%
      dplyr::mutate(
        baseline_year = get_last_interval_year(.data[["year"]], .data[["type"]], start_year = baseline_year, end_year = aroc_end_year),
        baseline_value = get_last_interval_value(.data[[value_col]],
                                                 .data[["year"]],
                                                 .data[["type"]],
                                                 start_year = baseline_year, end_year = aroc_end_year,
                                                 type_filter = c("reported", "estimated")),
        aroc_end_value = get_baseline_value(.data[[value_col]], .data[["year"]], .data[["type"]], baseline_year = aroc_end_year),
        aroc_end_year = get_baseline_year(.data[["year"]], .data[["type"]], baseline_year = aroc_end_year, type_filter = c("reported", "estimated")),
        aroc = calculate_aroc(.data[["baseline_year"]],.data[["baseline_value"]], end_year = .data[["aroc_end_year"]], .data[["aroc_end_value"]])
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c("iso3", "ind", scenario_col, group_col, "aroc"))) %>%
      dplyr::distinct()

    max_aroc <- df_aroc %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, group_col, "ind"))))

    if(use_prop){
      max_aroc_n <- max_aroc %>%
        dplyr::tally() %>%
        dplyr::mutate(prop = dplyr::if_else(round(n*prop) == 0, 1, round(n*prop)))

      max_aroc <- max_aroc %>%
        dplyr::left_join(max_aroc_n, by = c("ind", group_col, scenario_col))
    }

    if (small_is_best){
      if(use_prop){
        max_aroc <- max_aroc %>%
          dplyr::group_map(~dplyr::slice_min(.x, .x[["aroc"]], n = unique(.x[["prop"]])), .keep = TRUE) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, group_col, "ind"))))
      }else{
        max_aroc <- max_aroc %>%
          dplyr::slice_min(.data[["aroc"]], n = n)
      }
    }else{
      if(use_prop){
        max_aroc <- max_aroc %>%
          dplyr::group_map(~dplyr::slice_max(.x, .x[["aroc"]], n = unique(.x[["prop"]])), .keep = TRUE) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, group_col, "ind"))))
      }else{
        max_aroc <- max_aroc %>%
          dplyr::slice_max(.data[["aroc"]], n = n)
      }
    }

    max_aroc <- dplyr::summarise(max_aroc, aroc = mean(.data[["aroc"]]))

    df_with_data_aroc <- df_with_data %>%
      dplyr::left_join(max_aroc, by = c(scenario_col, "ind", group_col)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", group_col)))) %>%
      dplyr::mutate(baseline_value = get_baseline_value(
        .data[[value_col]],
        .data[["year"]],
        .data[["type"]],
        baseline_year = start_year,
        type_filter = c("all", "reported", "estimated", "projected", "imputed"))) %>%
      dplyr::mutate(
        scenario_value = dplyr::case_when(
          .data[["year"]] == start_year ~ as.numeric(.data[[value_col]]),
          .data[["year"]] > start_year ~ .data[["baseline_value"]] + (.data[["aroc"]]*(.data[["year"]] - start_year)),
          TRUE ~ NA_real_
        ),
        !!sym(scenario_col) := scenario_name
      ) %>%
      dplyr::select(-c("baseline_value", "aroc")) %>%
      trim_values(
        col = "scenario_value", value_col = value_col, trim = trim, small_is_best = small_is_best,
        keep_better_values = keep_better_values, upper_limit = upper_limit,
        lower_limit = lower_limit, trim_years = trim_years, start_year = start_year, end_year = end_year
      ) %>%
      dplyr::filter(.data[[scenario_col]] == !!scenario_name)
  }else{
    df_with_data_aroc <- tibble::tibble()
  }

  if (nrow(df_without_data) > 0 & !no_data_no_scenario) {

    params_bau <- get_dots_and_call_parameters(...) %>%
      get_right_parameters(scenario_bau)

    df_without_data <- exec_scenario(df_without_data,
                                     scenario_bau,
                                     params_bau) %>%
      dplyr::filter(.data[[scenario_col]] == !!scenario_name,
                    .data[["year"]] >= aroc_end_year)
  } else {
    df_without_data <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_aroc, df_without_data)
}
