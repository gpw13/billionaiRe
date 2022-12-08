test_data <- load_misc_data("test_data/test_data/test_data_2022-11-24T12-07-52.parquet")

test_data_measles_routine <- test_data %>%
  dplyr::filter(ind == "measles_routine") %>%
  make_default_scenario() %>%
  dplyr::mutate(region = whoville::iso3_to_regions(iso3))

get_avg_aroc_top_n <- function(df, n = 10, prop = NULL, start_year = 2018, end_year = 2025,
                               grp_by = NULL, bau_scenario = "pre_covid_bau"){

  full_years_df <- tidyr::expand_grid(
    "year" := start_year:end_year,
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]]),
    "scenario" := "default"
  )

  df_full_year <- df %>%
    dplyr::full_join(full_years_df, by = c("iso3", "year", "ind", "scenario"))

  df_with_data <- df_full_year %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::filter(.data[["scenario"]] == "default") %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) > 1) %>%
    dplyr::filter(sum(.data[["year"]] %in% 2013:start_year & .data[["type"]] %in% c("estimated", "reported")) > 1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(scenario == "default")

  df_without_data <- df_full_year %>%
    dplyr::anti_join(df_with_data, by = c("iso3", "year", "ind")) %>%
    dplyr::filter(scenario %in% c("default", bau_scenario))

  avg_top_aroc <- df_with_data %>%
    dplyr::filter(scenario == "default") %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(
      baseline_year = get_last_interval_year(.data[["year"]], .data[["type"]], start_year = 2013, end_year = 2018, type_filter = c("reported", "estimated")),
      baseline_value = get_last_interval_value(.data[["value"]], .data[["year"]], .data[["type"]], start_year = 2013, end_year = 2018, type_filter = c("reported", "estimated")),
      aroc_end_value = get_baseline_value(.data[["value"]], .data[["year"]], .data[["type"]], baseline_year = 2018, type_filter = c("reported", "estimated")),
      aroc_end_year = get_baseline_year(.data[["year"]], .data[["type"]], baseline_year = 2018, type_filter = c("reported", "estimated")),
      aroc =   (aroc_end_value - baseline_value)/(aroc_end_year - baseline_year),
      region = whoville::iso3_to_regions(iso3)) %>%
    dplyr::select(iso3, ind, aroc, region) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ind", grp_by))))

  if(is.null(prop)){
    avg_top_aroc <- avg_top_aroc %>%
      dplyr::slice_max(.data[["aroc"]], n = n) %>%
      dplyr::summarise(aroc = mean(aroc), .groups = "drop")
  }else{

    max_aroc_n <- avg_top_aroc %>%
      dplyr::tally() %>%
      dplyr::mutate(prop = dplyr::if_else(round(n*prop) == 0, 1, round(n*prop)))

    avg_top_aroc <- avg_top_aroc %>%
      dplyr::left_join(max_aroc_n, by = c("ind", grp_by)) %>%
      dplyr::group_map(~dplyr::slice_max(.x, .x[["aroc"]], n = unique(.x[["prop"]])), .keep = TRUE) %>%
      purrr::reduce(dplyr::bind_rows) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(grp_by, "ind")))) %>%
      dplyr::summarise(aroc = mean(aroc), .groups = "drop")
  }

  test_avg_values_2025 <- df_with_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", grp_by)))) %>%
    dplyr::mutate(region = whoville::iso3_to_regions(iso3)) %>%
    dplyr::left_join(avg_top_aroc, by = c("ind", grp_by)) %>%
    dplyr::mutate(
      scenario = dplyr::case_when(
        year >= 2018 ~ glue::glue("top_{n}_aroc"),
        TRUE ~ as.character(scenario)),
      value = dplyr::case_when(
        year > 2018 ~ .data[["value"]][.data[["year"]] == 2018] + (.data[["aroc"]]*(.data[["year"]] - 2018)),
        TRUE ~ as.numeric(value)),
      value = dplyr::if_else(value > 100, 100, value)
    ) %>%
    dplyr::filter(scenario == glue::glue("top_{n}_aroc"),
                  year == 2025)

  test_avg_values_2025_without_data <- df_without_data %>%
    dplyr::filter(scenario == !!bau_scenario, year >= start_year) %>%
    dplyr::mutate(scenario = glue::glue("top_{n}_aroc")) %>%
    dplyr::filter(scenario == glue::glue("top_{n}_aroc"),
                  year == 2025)

  dplyr::bind_rows(test_avg_values_2025, test_avg_values_2025_without_data) %>%
    dplyr::arrange(iso3) %>%
    dplyr::pull(value)
}

get_test_df <- function(ind, type = "reported"){
  tibble::tibble(
    iso3 = unlist(purrr::map(c("AFG", "BHR", "EGY", "KWT", "SDN",
                               "FIN", "CHE", "FRA", "GBR", "ARM"), rep, 6)),
    year = rep(2013:2018, 10),
    value = c(20:25, seq(20,40, length.out = 6), seq(20,33, length.out = 6), seq(20,22, length.out = 6), seq(20,20, length.out = 6),
              30:35, seq(30,40, length.out = 6), seq(39,40, length.out = 6), seq(1,40, length.out = 6), seq(20,40, length.out = 6)),
    ind = "measles_routine",
    scenario = "default",
    type = type,
    region = whoville::iso3_to_regions(iso3))
}

test_top_n_scenario <- function(ind = "measles_routine", n = 3, prop = NULL, grp_by = NULL){

  arocs <- list(emr = c(1, 4, 2.6, 0.4, 0),
                eur = c(1, 2, 0.2, 7.8, 4))

  if(is.null(grp_by)){
    scenario_name <- glue::glue("top_{n}_aroc")
    test_name <- glue::glue("top {n} average AROC returns correct results:")
    arocs <- unlist(arocs)
    avg_aroc <- mean(head(sort(arocs, decreasing = !get_ind_metadata(ind, "small_is_best")), n))
  }else{
    scenario_name <- glue::glue("top_{n}_{grp_by}_aroc")
    test_name <- glue::glue("top {n} by {grp_by} average AROC returns correct results:")
    avg_aroc <- as.numeric(purrr::map(arocs, ~mean(head(sort(., decreasing = !get_ind_metadata(ind, "small_is_best")), n))))
  }

  testthat::test_that(test_name,{
    test_df <- get_test_df(ind)

    if(is.null(prop)){
      test_avg_values_2025 <- get_avg_aroc_top_n(test_df,
                                                 n = n,
                                                 grp_by = grp_by)

      data_scenario_top_n <- test_df %>%
        scenario_top_n_iso3(n = n,
                            group_col = grp_by)

    }else{
      test_avg_values_2025 <- get_avg_aroc_top_n(test_df,
                                                 prop = prop,
                                                 grp_by = grp_by)
      data_scenario_top_n <- test_df %>%
        scenario_top_n_iso3(use_prop = TRUE,
                            prop = prop,
                            group_col = grp_by)
    }

    data_scenario_top_n <- data_scenario_top_n %>%
      dplyr::filter(scenario == scenario_name)

    data_scenario_top_n_aroc <- data_scenario_top_n %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", grp_by)))) %>%
      dplyr::filter(year %in% c(2024, 2025)) %>%
      tidyr::pivot_wider(names_from = year, values_from = value) %>%
      dplyr::mutate(aroc = `2025` - `2024`) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c("aroc", grp_by))) %>%
      dplyr::pull(aroc) %>%
      round(digits =7) %>%
      unique()

    testthat::expect_equal(data_scenario_top_n_aroc, avg_aroc)

    data_scenario_top_n_2025 <- data_scenario_top_n %>%
      dplyr::filter(year == 2025) %>%
      dplyr::arrange(iso3) %>%
      dplyr::pull(value)

    testthat::expect_equal(data_scenario_top_n_2025, test_avg_values_2025)
  })
}

test_top_n_scenario()
test_top_n_scenario(n = 1)
test_top_n_scenario(grp_by = "region")
test_top_n_scenario(n = 1, grp_by = "region")

testthat::test_that("top 3 average AROC returns correct results with real life data:",{
  test_avg_values_2025 <- get_avg_aroc_top_n(test_data_measles_routine,
                                             n = 3)

  data_scenario_top_3 <- test_data_measles_routine %>%
    scenario_top_n_iso3(n = 3) %>%
    dplyr::filter(scenario == "top_3_aroc",
                  year == 2025) %>%
    dplyr::arrange(iso3) %>%
    dplyr::pull(value)

  testthat::expect_equal(data_scenario_top_3, test_avg_values_2025)
})

testthat::test_that("top 10 average AROC returns correct results with real life data:",{

  test_avg_values_2025 <- get_avg_aroc_top_n(test_data_measles_routine,
                                             n = 10)

  data_scenario_top_3 <- test_data_measles_routine %>%
    scenario_top_n_iso3(n = 10) %>%
    dplyr::filter(scenario == "top_10_aroc",
                  year == 2025) %>%
    dplyr::arrange(iso3) %>%
    dplyr::pull(value)

  testthat::expect_equal(data_scenario_top_3, test_avg_values_2025)
})

testthat::test_that("top 10% by region average AROC returns correct results with real life data:",{

  test_avg_values_2025 <- get_avg_aroc_top_n(test_data_measles_routine,
                                             prop = .1,
                                             grp_by = "region")

  data_scenario_top_3 <- test_data_measles_routine %>%
    scenario_top_n_iso3(use_prop = TRUE,
                        prop = .1,
                        group_col = "region") %>%
    dplyr::filter(scenario == "top_10_percent_region_aroc",
                  year == 2025) %>%
    dplyr::arrange(iso3) %>%
    dplyr::pull(value)

  testthat::expect_equal(data_scenario_top_3, test_avg_values_2025)
})

testthat::test_that("top 10% by region average AROC returns correct results when there is no data for a country:",{

  test_data_anc4 <- test_data %>%
    dplyr::filter(ind == "anc4") %>%
    make_default_scenario(default_scenario = "bau_2019_then_flat", end_year = 2021, billion = "uhc") %>%
    dplyr::mutate(region = whoville::iso3_to_regions(iso3))

  test_avg_values_2025 <- get_avg_aroc_top_n(test_data_anc4,
                                             prop = .1,
                                             grp_by = "region")

  data_scenario_top_3 <- test_data_anc4 %>%
    scenario_top_n_iso3(use_prop = TRUE,
                        prop = .1,
                        group_col = "region",
                        bau_scenario = "pre_covid_bau") %>%
    dplyr::filter(scenario == "top_10_percent_region_aroc",
                  year == 2025) %>%
    dplyr::arrange(iso3) %>%
    dplyr::pull(value)

  testthat::expect_equal(data_scenario_top_3, test_avg_values_2025)
})
