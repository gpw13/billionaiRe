#' Accelerate adult_obese
#'
#' Accelerate adult_obese by halting upwards trend in the data to the 2010 value.
#' Runs:
#'   - `scenario_halt_rise(df, baseline_year = 2010, small_is_best = TRUE,...)`.
#'
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_adult_obese <- function(df,
                                   ind_ids = billion_ind_codes("hpop"),
                                   end_year = 2025,
                                   scenario_col = "scenario",
                                   default_scenario = "default",
                                   ...) {
  assert_columns(df, scenario_col, "ind")
  this_ind <- ind_ids["adult_obese"]

  params <- get_right_parameters(list(...), scenario_halt_rise)
  params["baseline_year"] <- 2010
  params["scenario_name"] <- "acceleration"
  params["end_year"] <- end_year
  params["default_scenario"] <- default_scenario

  params_halt_rise <- get_right_parameters(params, scenario_halt_rise)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  assert_ind_start_end_year(df_this_ind,
    start_year = 2010, end_year = 2018,
    ind_ids = ind_ids["adult_obese"], scenario_col = scenario_col
  )

  df_accelerated <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind, target_year = end_year), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate alcohol
#'
#' Accelerate alcohol by picking the best results between business as usual,
#' halt downwards trend from 2018, and -10 percent from 2010.
#'
#' Runs:
#'
#'  - `scenario_halt_rise(df, baseline_year = 2018, small_is_best = TRUE,...)`,
#'  - `scenario_bau(df, small_is_best = TRUE,...)`, and
#'  - `scenario_percent_baseline(df, percent_change = -10, baseline_year = 2010, small_is_best = TRUE, ...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @param default_scenario name of the default scenario.
#' @param bau_scenario name of scenario to be used for business as usual.
#' Default is `historical`.
#' @param ... additional parameters to be passed to scenario function
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams recycle_data
#'
accelerate_alcohol <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               end_year = 2025,
                               scenario_col = "scenario",
                               default_scenario = "default",
                               bau_scenario = "historical",
                               ...) {
  assert_columns(df, scenario_col, "ind")

  this_ind <- ind_ids[stringr::str_detect(ind_ids, "alcohol")]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2010, end_year = 2018,
    ind_ids = ind_ids[this_ind], scenario_col = scenario_col
  )

  params <- list(...)
  params["end_year"] <- end_year
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_bau <- get_right_parameters(params, scenario_bau)

  params_neg10_2010 <- get_right_parameters(params, scenario_fixed_target_col)
  params_neg10_2010["scenario_name"] <- "-10_2010"

  params_halt_rise <- get_right_parameters(params, scenario_halt_rise)
  params_halt_rise["baseline_year"] <- 2018

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  neg_10_targets <- df_this_ind_default %>%
    dplyr::filter(.data[["year"]] == 2010) %>%
    dplyr::mutate(target = .data[["value"]] * (100 - 10) / 100) %>%
    dplyr::select("iso3", "ind", "target")

  df_perc_baseline <- df_this_ind_default %>%
    dplyr::left_join(neg_10_targets, by = c("iso3", "ind"))

  df_perc_baseline <- do.call(
    scenario_fixed_target_col, c(list(df = df_perc_baseline, target_col = "target"), params_neg10_2010)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "-10_2010") %>%
    dplyr::select(-"target")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default, target_year = end_year), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  df_binded <- df_perc_baseline %>%
    dplyr::bind_rows(df_halt_rise) %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "-10_2020",
      "halt_rise"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}
#' Accelerate child_obese
#'
#' Accelerate child_obese by halting upwards trend in the data to the 2010 value.
#'
#' @inherit accelerate_alcohol
#'
accelerate_child_obese <- function(df,
                                   ...) {
  ind_ids <- "child_obese"
  names(ind_ids) <- "adult_obese"

  accelerate_adult_obese(
    df = df,
    ind_ids = ind_ids,
    ...
  )
}

#' Accelerate child_viol
#'
#' Accelerate child_viol by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_alcohol
#' @param start_year Year from which the acceleration scenario begins, inclusive.
#' @param value_col Name of the column containing indicator value in `df`.
#'
accelerate_child_viol <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario_col = "scenario",
                                  start_year = 2018,
                                  value_col = "value",
                                  default_scenario = "default",
                                  ...) {
  assert_columns(df, scenario_col, "ind")

  this_ind <- ind_ids["child_viol"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind[["iso3"]]),
    "year" := start_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind[[scenario_col]])
  )

  latest_values <- df_this_ind %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", scenario_col)))) %>%
    dplyr::filter(.data[["year"]] <= start_year) %>%
    dplyr::filter(max(.data[["year"]]) == .data[["year"]]) %>%
    dplyr::mutate(latest_value = .data[[value_col]]) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("iso3", "ind", scenario_col, "latest_value"))

  df_this_ind <- df_this_ind %>%
    dplyr::full_join(full_df, by = c("iso3", "year", "ind", scenario_col)) %>%
    dplyr::left_join(latest_values, by = c("iso3", "ind", scenario_col)) %>%
    dplyr::mutate(
      !!sym(value_col) := dplyr::case_when(
        is.na(.data[[value_col]]) ~ .data[["latest_value"]],
        TRUE ~ .data[[value_col]]
      )
    ) %>%
    dplyr::ungroup()

  params <- list(...)
  params["target_value"] <- 0
  params["target_year"] <- 2030
  params["scenario_name"] <- "acceleration"
  params["end_year"] <- end_year
  params["start_year"] <- start_year
  params["value_col"] <- value_col
  params["default_scenario"] <- default_scenario

  params_fixed_target <- get_right_parameters(params, scenario_fixed_target)

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params_fixed_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate devontrack
#'
#' Accelerate devontrack by targeting 80 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 80, target_year = 2030, small_is_best = FALSE,...)`,
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_devontrack <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario_col = "scenario",
                                  start_year = 2018,
                                  value_col = "value",
                                  default_scenario = "default",
                                  ...) {
  assert_columns(df, scenario_col, "ind")

  this_ind <- ind_ids["devontrack"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind[["iso3"]]),
    "year" := start_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind[[scenario_col]])
  )

  latest_values <- df_this_ind %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", scenario_col)))) %>%
    dplyr::filter(.data[["year"]] <= start_year) %>%
    dplyr::filter(max(.data[["year"]]) == .data[["year"]]) %>%
    dplyr::mutate(latest_value = .data[[value_col]]) %>%
    dplyr::ungroup() %>%
    dplyr::select(c("iso3", "ind", scenario_col, "latest_value"))

  df_this_ind <- df_this_ind %>%
    dplyr::full_join(full_df, by = c("iso3", "year", "ind", scenario_col)) %>%
    dplyr::left_join(latest_values, by = c("iso3", "ind", scenario_col)) %>%
    dplyr::mutate(
      !!sym(value_col) := dplyr::case_when(
        is.na(.data[[value_col]]) ~ .data[["latest_value"]],
        TRUE ~ .data[[value_col]]
      )
    ) %>%
    dplyr::ungroup()

  params <- list(...)
  params["target_value"] <- 80
  params["target_year"] <- 2030
  params["scenario_name"] <- "acceleration"
  params["end_year"] <- end_year
  params["scenario_col"] <- scenario_col
  params["start_year"] <- 2018
  params["value_col"] <- "value"

  params_fixed_target <- get_right_parameters(params,scenario_fixed_target)

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params_fixed_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate fuel
#'
#' Accelerate fuel by keeping business as usual for high income countries
#' (HIC) (according to the World Bank), and by aiming at the best rate of change
#' between 2013 and 2015 in the region for Lower-middle income countries (LMC),
#' Low-income countries (LIC), High-income countries(HIC), and for unclassified
#' countries.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = FALSE,...)` for HIC,
#'  - `scenario_best_in_region(df, target_year = 2018, baseline_year = 2013, small_is_best = FALSE,...)` for other income groups.
#'
#' @inherit accelerate_alcohol
#'
accelerate_fuel <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario_col = "scenario",
                            default_scenario = "default",
                            bau_scenario = "historical",
                            ...) {
  assert_columns(df, scenario_col, "ind", "iso3")

  this_ind <- ind_ids["fuel"]

  this_ind_df <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)%>%
    dplyr::mutate(wb_ig = whoville::iso3_to_regions(.data[["iso3"]], region = "wb_ig"))

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario


  if ("HIC" %in% unique(this_ind_df$wb_ig)) {
    params_bau <- get_right_parameters(params, "scenario_bau")
    params_bau["scenario_name"] <- "acceleration"

    high_income_df <- this_ind_df %>%
      dplyr::filter(.data[["wb_ig"]] == "HIC",
                    .data[[scenario_col]] == bau_scenario)

    high_income <- do.call(
      scenario_bau, c(list(df = high_income_df), params_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    high_income <- this_ind_df[0, ]
  }

  # for non hic a regional approach is used using years 2018 to 2023.
  if (sum(c("LMC", "LIC", "UMC") %in% unique(this_ind_df$wb_ig)) > 0 | sum(is.na(this_ind_df$wb_ig)) > 0) {
    other_df <- this_ind_df %>%
      dplyr::filter(.data[["wb_ig"]] != "HIC" | is.na(.data[["wb_ig"]]),
                    .data[[scenario_col]] == default_scenario)

    params_others <- get_right_parameters(params, scenario_best_in_region)
    params_others["baseline_year"] <- 2013
    params_others["target_year"] <- 2018
    params_others["scenario_name"] <- "acceleration"

    assert_ind_start_end_year(other_df,
      start_year = 2013, end_year = 2018,
      ind_ids = ind_ids[this_ind], scenario_col = scenario_col
    )

    other <- do.call(
      scenario_best_in_region, c(list(df = other_df), params_others)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    other <- this_ind_df[0, ]
  }

  df %>%
    dplyr::bind_rows(other) %>%
    dplyr::bind_rows(high_income) %>%
    dplyr::select(-"wb_ig")
}

#' Accelerate hpop_sanitation
#'
#' Accelerate hpop_sanitation by aiming at best value in quintile.
#'
#' Runs:
#'
#'  - `scenario_quantile(df, n = 5, quantile_year = 2019, baseline_quantile_year = 2018, upper_limit = 99, small_is_best = FALSE,...)`
#'
#' @inherit accelerate_alcohol
accelerate_hpop_sanitation <- function(df,
                                       ind_ids = billion_ind_codes("hpop"),
                                       end_year = 2025,
                                       scenario_col = "scenario",
                                       default_scenario = "default",
                                       ...) {
  assert_columns(df, scenario_col, "ind")

  this_ind <- ind_ids["hpop_sanitation"]

  params <- get_right_parameters(list(...), scenario_quantile)
  params["n"] <- 5
  params["quantile_year"] <- 2019
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["scenario_name"] <- "acceleration"
  params["end_year"] <- end_year
  params["default_scenario"] <- default_scenario
  params["scenario_col"] <- scenario_col


  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[["ind"]], this_ind),
                  .data[[scenario_col]] == default_scenario) %>%
    dplyr::mutate("_temp_ind" := .data[["ind"]],
                  "ind" := this_ind)

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration") %>%
    dplyr::mutate("ind":= .data[["_temp_ind"]]) %>%
    dplyr::select(-"_temp_ind")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate hpop_sanitation_rural
#'
#' Accelerate hpop_sanitation_rural by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_hpop_sanitation_rural <- function(df,
                                             ...) {
  df %>%
    accelerate_hpop_sanitation(
      ind_ids = c("hpop_sanitation" = "hpop_sanitation_rural"),
      ...
    )
}

#' Accelerate hpop_sanitation_urban
#'
#' Accelerate hpop_sanitation_urban by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_hpop_sanitation_urban <- function(df,
                                             ...) {
  df %>%
    accelerate_hpop_sanitation(
      ind_ids = c("hpop_sanitation" = "hpop_sanitation_urban"),
      ...
    )
}

#' Accelerate hpop_tobacco
#'
#' Accelerate hpop_tobacco by picking the best value between business as usual,
#' halt the rise in 2018, or a custom version of scenario_percent_baseline. The
#' custom `scenario_percent_baseline` is taking similar parameters to
#' `scenario_percent_baseline`'s `percent_change` = -30, `baseline_year` = 2010,
#' but values are added to the `start_year` value, rather than the `baseline_year`
#' values.
#'
#' Runs:
#'
#'  - custom scenario_percent_baseline (see above).
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_halt_rise(df, baseline_year= 2018, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_hpop_tobacco <- function(df,
                                    ind_ids = billion_ind_codes("hpop"),
                                    scenario_col = "scenario",
                                    value_col = "value",
                                    start_year = 2018,
                                    end_year = 2025,
                                    default_scenario = "default",
                                    bau_scenario = "historical",
                                    ...) {
  this_ind <- ind_ids["hpop_tobacco"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind_default[["iso3"]]),
    "year" := start_year:end_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind_default[[scenario_col]])
  )

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2010, end_year = 2018,
    ind_ids = ind_ids[this_ind], scenario_col = scenario_col
  )

  params <- list(...)
  params["end_year"] <- end_year
  params["scenario_col"] <- scenario_col
  params["value_col"] <- value_col
  params["start_year"] <- start_year
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  df_scenario_percent_baseline <- df_this_ind_default %>%
    dplyr::full_join(full_df, by = (c("iso3", "year", "ind", scenario_col))) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(
      has_estimates = any(.data[["type"]] == "estimated"),
      baseline_value = .data[[value_col]][.data[["year"]] == start_year],
      old_baseline_value = .data[[value_col]][.data[["year"]] == 2010]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      goalend = .data[["old_baseline_value"]] + ((.data[["old_baseline_value"]] * (100 - 30) / 100) - .data[["old_baseline_value"]]) * (end_year - 2010) / (2025 - 2010),
      "{scenario_col}" := "-30_2020",
      scenario_value = dplyr::if_else(
        .data[["year"]] >= start_year & .data[["year"]] <= 2025 & .data[["has_estimates"]],
        .data[["baseline_value"]] + (.data[["goalend"]] - .data[["baseline_value"]]) * (.data[["year"]] - 2018) / (end_year - start_year),
        NA_real_
      ),
      "type_" := dplyr::if_else(
        is.na(.data[["type"]]) & .data[["year"]] >= start_year,
        "projected",
        .data[["type"]])
    ) %>%
    dplyr::select(-c("baseline_value", "goalend", "old_baseline_value", "has_estimates")) %>%
    trim_values(
      col = "scenario_value",
      trim = TRUE,
      small_is_best = params[["small_is_best"]],
      keep_better_values = FALSE,
      upper_limit = 100,
      lower_limit = 0,
      trim_years = TRUE
    )

  params_bau <- get_right_parameters(params, scenario_bau)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  params_halt_rise <- get_right_parameters(params, scenario_halt_rise)
  params_halt_rise["baseline_year"] <- 2018

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default, target_year = end_year), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  df_binded <- df_scenario_percent_baseline %>%
    dplyr::bind_rows(df_halt_rise) %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "-30_2020",
      "halt_rise",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate ipv
#'
#' Accelerate ipv by targeting 0 by 2030.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 0, target_year = 2030, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_alcohol
#'
accelerate_ipv <- function(df,
                           ...) {
  ind_ids <- "ipv"
  names(ind_ids) <- "child_viol"

  accelerate_child_viol(
    df = df,
    ind_ids = ind_ids,
    ...
  )
}

#' Accelerate overweight
#'
#' Accelerate overweight by picking the best value between business as usual and
#' AROC of 3 by 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_aroc(df, aroc_type = "target", target_value = 3, target_year = 2030, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams accelerate_alcohol
#'
accelerate_overweight <- function(df,
                                  ind_ids = billion_ind_codes("hpop"),
                                  end_year = 2025,
                                  scenario_col = "scenario",
                                  default_scenario = "default",
                                  bau_scenario = "historical",
                                  ...) {
  this_ind <- ind_ids["overweight"]

  params <- list(...)
  params["end_year"] <- end_year
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_aroc <- get_right_parameters(params, scenario_aroc)
  params_aroc["aroc_type"] <- "target"
  params_aroc["target_year"] <- 2030
  params_aroc["target_value"] <- 3

  params_bau <- get_right_parameters(params, scenario_bau)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind_default), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "aroc_target")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "aroc_target",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate pm25
#'
#' Accelerate pm25 by picking the best value between business as usual, and
#' linear change of -2% * 2018 `value_col` per year.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`
#'  - `scenario_linear_change(df, linear_value = df$value_col[df$"year" == 2018] * -0.02, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_pm25 <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario_col = "scenario",
                            value_col = "value",
                            default_scenario = "default",
                            bau_scenario = "historical",
                            ...) {
  this_ind <- ind_ids["pm25"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["value_col"] <- value_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  linear_value_df <- df_this_ind_default %>%
    dplyr::filter(.data[["year"]] == 2018) %>%
    dplyr::mutate(linear_value = .data[[value_col]] * -0.02) %>%
    dplyr::select("iso3", "linear_value")

  df_this_ind_default <- df_this_ind_default %>%
    dplyr::left_join(linear_value_df, by = "iso3")

  params_linear <- get_right_parameters(params, scenario_linear_change_col)

  params_linear[["linear_value_col"]] <- "linear_value"

  params_bau <- get_right_parameters(params, scenario_bau)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_linear <- do.call(
    scenario_linear_change_col, c(list(df = df_this_ind_default), params_linear)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "linear_change")

  df_binded <- df_bau %>%
    dplyr::bind_rows(df_linear)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "linear_change"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate road
#'
#' Accelerate road by picking the best results between business as usual, and
#' -50 percent from 2020 to 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`, and
#'  - `scenario_percent_baseline(df, percent_change = -50, baseline_year = 2020, target_year = 2030, small_is_best = TRUE, ...)`
#'
#' Then picks the best result between the two scenarios.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams recycle_data
#' @inheritParams accelerate_alcohol
#'
accelerate_road <- function(df,
                            ind_ids = billion_ind_codes("hpop"),
                            scenario_col = "scenario",
                            default_scenario = "default",
                            bau_scenario = "historical",
                            ...) {
  this_ind <- ind_ids["road"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_percent_baseline <- get_right_parameters(params, scenario_percent_baseline)
  params_percent_baseline["percent_change"] <- -50
  params_percent_baseline["target_year"] <- 2030
  params_percent_baseline["baseline_year"] <- 2020

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2018, end_year = 2020,
    ind_ids = ind_ids[this_ind], scenario_col = scenario_col
  )

  df_percent_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind_default), params_percent_baseline)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "-50_2020")

  params_bau <- get_right_parameters(params, scenario_bau)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_binded <- df_percent_baseline %>%
    dplyr::bind_rows(df_bau)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "-50_2020",
      "business_as_usual"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate stunting
#'
#' Accelerate stunting by picking the best results between business as usual,
#' halt downwards trend, and AROC of -50% change between 2012 and 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`,
#'  - `scenario_aroc(df, aroc_type = "percent_change", percent_change = -50, baseline_year = 2012, target_year = 2030, small_is_best = TRUE, ...)`
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_alcohol
accelerate_stunting <- function(df,
                                ind_ids = billion_ind_codes("hpop"),
                                scenario_col = "scenario",
                                default_scenario = "default",
                                bau_scenario = "historical",
                                ...) {
  this_ind <- ind_ids["stunting"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_aroc <- get_right_parameters(params, scenario_aroc)
  params_aroc["aroc_type"] <- "percent_change"
  params_aroc["percent_change"] <- -50
  params_aroc["baseline_year"] <- 2012
  params_aroc["target_year"] <- 2030

  params_halt <- get_right_parameters(params, scenario_halt_rise)

  params_bau <- get_right_parameters(params, scenario_bau)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2012, end_year = 2018,
    ind_ids = this_ind, scenario_col = scenario_col
  )

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind_default), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "aroc_percent_change")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default), params_halt)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_bau) %>%
    dplyr::bind_rows(df_halt_rise)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "halt_rise",
      "aroc_percent_change"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate suicide
#'
#' Accelerate suicide by picking the best results between business as usual,
#' halt downwards trend, and -33.333% points between 2015 and 2030.
#'
#' Runs:
#'
#'  - `scenario_bau(df, small_is_best = TRUE,...)`,
#'  - `scenario_percent_baseline(df, percent_change = -33.333, baseline_year = 2015, target_year = 2030, small_is_best = TRUE, ...)`
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_alcohol
accelerate_suicide <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               scenario_col = "scenario",
                               default_scenario = "default",
                               bau_scenario = "historical",
                               ...) {
  this_ind <- ind_ids["suicide"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_percent_baseline <- get_right_parameters(params, scenario_percent_baseline)
  params_percent_baseline["percent_change"] <- -33.333
  params_percent_baseline["baseline_year"] <- 2015
  params_percent_baseline["target_year"] <- 2030

  params_halt <- get_right_parameters(params, scenario_halt_rise)

  params_bau <- get_right_parameters(params, scenario_bau)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2015, end_year = 2018,
    ind_ids = this_ind, scenario_col = scenario_col
  )

  df_percent_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind_default), params_percent_baseline)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "-33.333_2015")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default), params_halt)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_binded <- df_percent_baseline %>%
    dplyr::bind_rows(df_bau) %>%
    dplyr::bind_rows(df_halt_rise)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "business_as_usual",
      "halt_rise",
      "-33.333_2015"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate transfats
#'
#' Accelerate transfats by targeting 100 by 2025.
#'
#' Runs:
#'
#'  - `scenario_fixed_target(df, target_value = 100, target_year = 2025, small_is_best = TRUE,...)`,
#'
#' @inherit accelerate_alcohol
#'
accelerate_transfats <- function(df,
                                 ind_ids = billion_ind_codes("hpop"),
                                 scenario_col = "scenario",
                                 default_scenario = "default",
                                 ...) {
  this_ind <- ind_ids["transfats"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario

  params_target <- get_right_parameters(params, scenario_fixed_target)
  params_target["target_value"] <- 100
  params_target["target_year"] <- 2025
  params_target["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind_default), params_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate wasting
#'
#' Accelerate wasting by picking the best results between halt downwards trend
#' from 2018, and AROC by 3% by 2030.
#'
#' Runs:
#'
#'  - `scenario_halt_rise(df, small_is_best = TRUE,...)`
#'  - `scenario_aroc(df, aroc_type = "target", target_value = 3, target_year = 2030, small_is_best = TRUE,...)`
#'
#' Then picks the best result between the three scenarios.
#'
#' @inherit accelerate_adult_obese
#' @inheritParams scenario_fixed_target
#' @inheritParams accelerate_alcohol
#'
accelerate_wasting <- function(df,
                               ind_ids = billion_ind_codes("hpop"),
                               end_year = 2025,
                               scenario_col = "scenario",
                               default_scenario = "default",
                               bau_scenario = "historical",
                               ...) {
  this_ind <- ind_ids["wasting"]

  params <- list(...)
  params["end_year"] <- end_year
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_aroc <- get_right_parameters(params, scenario_aroc)
  params_aroc["aroc_type"] <- "target"
  params_aroc["target_year"] <- 2030
  params_aroc["target_value"] <- 3

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind, .data[["year"]] >= 2008)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  has_2018_value <- df_this_ind_default %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind", scenario_col)))) %>%
    dplyr::filter(.data[["year"]] == 2018, .data[["ind"]] == this_ind)

  no_2018_value <- df_this_ind_default %>%
    dplyr::filter(!.data[["iso3"]] %in% unique(has_2018_value$iso3))

  if(nrow(no_2018_value) > 0){
    last_reported <- no_2018_value %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("iso3", "ind",scenario_col)))) %>%
      dplyr::filter(.data[["year"]] <= 2018) %>%
      get_last_value() %>%
      dplyr::mutate(type = "imputed",
                    year = 2018,
                    "{scenario_col}" := default_scenario)
  }else{
    last_reported <- no_2018_value
  }

  df_this_ind_default <- dplyr::bind_rows(df_this_ind_default, last_reported) %>%
    dplyr::distinct()

  df_aroc <- do.call(
    scenario_aroc, c(list(df = df_this_ind_default), params_aroc)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "aroc_target") %>%
    flat_extrapolation("value", group_col = c("iso3", "ind"))

  params_halt_rise <- get_right_parameters(params, scenario_halt_rise)

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  df_binded <- df_aroc %>%
    dplyr::bind_rows(df_halt_rise)

  params_best_of <- get_right_parameters(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = df_binded, scenario_names = c(
      "aroc_target",
      "halt_rise"
    )), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate water
#'
#' Accelerate water by aiming at best value in quintile.
#'
#' Runs:
#'
#'  - `scenario_quantile(df, n = 5, quantile_year = 2017, baseline_quantile_year = 2018, upper_limit = 99, lower_limit = 0 small_is_best = FALSE,...)`
#'
#' @inherit accelerate_alcohol
accelerate_water <- function(df,
                             ind_ids = billion_ind_codes("hpop"),
                             scenario_col = "scenario",
                             default_scenario = "default",
                             ...) {
  this_ind <- "water"

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["n"] <- 5
  params["quantile_year"] <- 2017
  params["baseline_quantile_year"] <- 2018
  params["upper_limit"] <- 99
  params["lower_limit"] <- 0
  params["scenario_name"] <- "acceleration"

  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[["ind"]], this_ind))

  unique_inds <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::distinct(temp_ind = .data[["ind"]]) %>%
    dplyr::ungroup()

  df_this_ind <- df_this_ind %>%
    dplyr::mutate("ind" := this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  assert_ind_start_end_year(df_this_ind_default,
    start_year = 2017, end_year = 2018,
    ind_ids = this_ind, scenario_col = scenario_col
  )

  params_quantile <- get_right_parameters(params, scenario_quantile)

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind_default), params_quantile)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration") %>%
    dplyr::left_join(unique_inds, by = c("iso3")) %>%
    dplyr::mutate(ind = .data[["temp_ind"]]) %>%
    dplyr::select(-.data[["temp_ind"]])

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate water_rural
#'
#' Accelerate water_rural by aiming at best value in quintile.
#'
#' @inherit accelerate_water
accelerate_water_rural <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ind_ids = c("water" = "water_rural"),
      ...
    )
}

#' Accelerate water_urban
#'
#' Accelerate water_urban by aiming at best value in quintile.
#'
#' @inherit accelerate_alcohol
accelerate_water_urban <- function(df,
                                   ...) {
  df %>%
    accelerate_water(
      ind_ids = c("water" = "water_urban"),
      ...
    )
}
