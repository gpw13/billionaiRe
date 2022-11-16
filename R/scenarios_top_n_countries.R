scenario_top_n_iso3 <- function(df,
                                n = 10,
                                group_col = NULL,
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
                                default_scenario = "default"){

  if(is.null(group_col)){
    scenario_name <- glue::glue("top_{n}_aroc")
  }

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "{scenario_col}" := default_scenario
  )

  df_full_year <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "ind", "iso3", scenario_col))

  df_with_data <- df_full_year %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) > 1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_without_data <- df_full_year %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) < 2) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  if(nrow(df_with_data)>0){
    df_aroc <- df_with_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, "iso3", "ind")))) %>%
      dplyr::mutate(
        baseline_value = get_baseline_value(.data[[value_col]], .data[["year"]], baseline_year),
        aroc_end_value = get_baseline_value(.data[[value_col]], .data[["year"]], aroc_end_year),
        aroc = calculate_aroc(baseline_year,.data[["baseline_value"]], end_year = aroc_end_year, .data[["aroc_end_value"]])
        ) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c("iso3", "ind", scenario_col, group_col, "aroc"))) %>%
      dplyr::distinct()

    max_aroc <- df_aroc %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(scenario_col, group_col, "ind"))))

    if (small_is_best){
      max_aroc <- max_aroc %>%
        dplyr::slice_min(.data[["aroc"]], n = n)

    }else{
      max_aroc <- max_aroc %>%
        dplyr::slice_max(.data[["aroc"]], n = n) %>%
        dplyr::summarise(aroc = mean(.data[["aroc"]]))
    }

    df_with_data_aroc <- df_with_data %>%
      dplyr::left_join(max_aroc, by = c(scenario_col, "ind", group_col)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", group_col)))) %>%
      dplyr::mutate(baseline_value = get_baseline_value(.data[[value_col]], .data[["year"]], start_year)) %>%
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
      )
  }else{
    df_with_data_aroc <- tibble::tibble()
  }

  if(nrow(df_without_data) > 0){
    df_without_data <- df_without_data %>%
      scenario_bau(value_col = value_col,
                   start_year = start_year,
                   end_year = end_year,
                   scenario_name = scenario_name,
                   scenario_col = scenario_col,
                   trim = trim,
                   small_is_best = small_is_best,
                   keep_better_values = keep_better_values,
                   upper_limit = upper_limit,
                   lower_limit = lower_limit,
                   trim_years = trim_years,
                   ind_ids = ind_ids,
                   bau_scenario = bau_scenario,
                   default_scenario = default_scenario)
  }else{
    df_without_data <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_aroc, df_without_data)
}
