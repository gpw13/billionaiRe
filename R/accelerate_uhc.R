#' Accelerate anc4
#'
#' Accelerate anc4 by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, the acceleration scenario_col is the same
#' as business as usual.
#' - For countries with reported data, scenarios with both a **fixed target of 95%
#' by 2030** and a **linear change of 2.6 per year till 2025** are tried, with the easiest
#' to achieve of the two selected. The selected scenario is then compared against
#' the business as usual scenario for reported data, and the best of the two chosen
#' as the acceleration scenario.
#'
#' @inherit accelerate_alcohol
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
#'
accelerate_anc4 <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario_col = "scenario",
                            default_scenario = "default",
                            bau_scenario = "historical",
                            ...) {
  this_ind <- ind_ids["anc4"]

  params <- list(...)
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario


  params_without_data_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  params_with_data_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "with_data_bau")
  )

  params_with_data_fixed_target <- c(
    get_right_params(params, scenario_fixed_target),
    list(scenario_name = "with_data_fixed_target", target_value = 95, target_year = 2030)
  )

  params_with_data_linear <- c(
    get_right_params(params, scenario_linear_change),
    list(scenario_name = "with_data_linear", linear_value = 2.6)
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) <= 1)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(df_with_data) > 0) {
    df_with_data_bau <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data_bau), params_with_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_data_bau")

    df_with_data_default <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)


    df_with_data_fixed_target <- do.call(
      scenario_fixed_target, c(list(df = df_with_data_default), params_with_data_fixed_target)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_data_fixed_target")

    df_with_data_linear <- do.call(
      scenario_linear_change, c(list(df = df_with_data_default), params_with_data_linear)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_data_linear")

    params_scenario_best_of_linear_fixed <- c(get_right_params(params, scenario_best_of))
    params_scenario_best_of_linear_fixed["small_is_best"] <- TRUE

    df_with_data_best <- dplyr::bind_rows(df_with_data_fixed_target, df_with_data_linear)
    df_with_data_best <- do.call(
      scenario_best_of, c(list(
        df = df_with_data_best,
        scenario_names = c("with_data_fixed_target", "with_data_linear"),
        scenario_name = "best_of_linear_and_fixed_target"),
        params_scenario_best_of_linear_fixed)) %>%
      dplyr::filter(.data[[scenario_col]] == "best_of_linear_and_fixed_target")

    # Elliott: when binding the df_with_data_best and df_with_data_bau duplicated rows are created. adding a distinct to remove them for now.
    # With new version of scenario_best_of, this shouldn't happen though.

    params_scenario_best <- c(get_right_params(params, scenario_best_of))

    df_with_data_accelerated <- dplyr::bind_rows(df_with_data_best, df_with_data_bau) %>%
      dplyr::distinct()

    df_with_data_accelerated <- do.call(
      scenario_best_of, c(list(
        df = df_with_data_accelerated,
        scenario_names = c("with_data_bau", "best_of_linear_and_fixed_target"),
        scenario_name = "acceleration"),
        params_scenario_best)) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")

  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_without_data) > 0) {
    df_with_data_bau <- df_without_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_with_data_bau), params_without_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_without_data_accelerated, df_with_data_accelerated)
}

#' Accelerate art
#'
#' Accelerate art by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, business as usual is returned.
#' - For countries with reported data, the best of business as usual and **fixed
#'  target of 90.25% by 2025** is chosen.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_art <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario_col = "scenario",
                           start_year = 2018,
                           default_scenario = "default",
                           bau_scenario = "historical",
                           ...) {
  this_ind <- ind_ids["art"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_with_data_bau <- get_right_params(params, scenario_bau)
  params_without_data_bau <- get_right_params(params, scenario_bau)
  params_without_data_bau[["scenario_name"]] <- "acceleration"

  params_with_data_fixed_target <- c(
    get_right_params(params, scenario_fixed_target),
    list(
      target_value = 90.25, scenario_name = "fixed_target",
      target_year = 2025, upper_limit = 95
    )
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) > 1) %>%
    dplyr::ungroup()


  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= start_year) <= 1) %>%
    dplyr::ungroup()

  if (nrow(df_without_data) > 0) {
    df_without_data_bau <- df_without_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_without_data_bau), params_without_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_data) > 0) {
    df_with_data_bau <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data_bau), params_with_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "business_as_usual")

    df_with_data_default <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_with_data_fixed_target <- do.call(
      scenario_fixed_target, c(list(df = df_with_data_default), params_with_data_fixed_target)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "fixed_target")

    params_scenario_best_of <- c(get_right_params(params, scenario_best_of),
                                 scenario_name = "acceleration"
    )

    df_with_data_accelerated <- dplyr::bind_rows(df_with_data_bau, df_with_data_fixed_target)
    df_with_data_accelerated <- do.call(
      scenario_best_of, c(list(
        df = df_with_data_accelerated,
        scenario_names = c("business_as_usual", "fixed_target")
      ), params_scenario_best_of)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_accelerated, df_without_data_accelerated)
}

#' Accelerate beds
#'
#' Accelerate beds by first dividing countries into two groups:
#' - For countries with 18 or more beds for all years after 2018, business
#' as usual is returned.
#' - For countries which have less than 18 beds for any of the years after 2018 (inclusive),
#' the best of business as usual and a **linear change of 0.36 per year up to 2025**,
#' with an upper limit of 18, is returned.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_beds <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario_col = "scenario",
                            value_col = "value",
                            start_year = 2018,
                            default_scenario = "default",
                            bau_scenario = "historical",
                            ...) {
  this_ind <- ind_ids["beds"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["value_col"] <- value_col
  params["start_year"] <- start_year
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_no_scenario_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  params_with_scenario_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "with_scenario_bau")
  )

  params_with_scenario_linear <- c(
    get_right_params(params, scenario_linear_change),
    list(scenario_name = "with_scenario_linear", linear_value = 0.36, upper_limit = 18)
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_with_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(any((.data[[value_col]] < 18 & .data[["year"]] >= start_year))) %>%
    dplyr::ungroup()

  df_no_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!any((.data[[value_col]] < 18 & .data[["year"]] >= start_year))) %>%
    dplyr::ungroup()

  if (nrow(df_no_scenario) > 0) {

    df_no_scenario_bau <- df_no_scenario %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_no_scenario_accelerated <- do.call(
      scenario_bau, c(list(df = df_no_scenario_bau), params_no_scenario_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_no_scenario_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_scenario) > 0) {
    df_with_scenario_bau <- df_with_scenario %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_with_scenario_bau <- do.call(
      scenario_bau, c(list(df = df_with_scenario), params_with_scenario_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_scenario_bau")

    df_with_scenario_default <- df_with_scenario %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_with_scenario_linear <- do.call(
      scenario_linear_change, c(list(df = df_with_scenario), params_with_scenario_linear)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_scenario_linear")

    params_scenario_best_of <- c(get_right_params(params, scenario_best_of),
                                 scenario_name = "acceleration"
    )

    df_with_scenario_accelerated <- dplyr::bind_rows(df_with_scenario_bau, df_with_scenario_linear)

    df_with_scenario_accelerated <- do.call(
      scenario_best_of, c(list(
        df = df_with_scenario_accelerated,
        scenario_names = c("with_scenario_bau", "with_scenario_linear")),
        params_scenario_best_of)) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")

  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_no_scenario_accelerated, df_with_scenario_accelerated)
}

#' Accelerate bp
#'
#' Accelerate bp by aiming at reaching 80% by 2030.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_bp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          value_col = "value",
                          scenario_col = "scenario",
                          start_year = 2018,
                          end_year = 2025,
                          default_scenario = "default",
                          bau_scenario = "historical",
                          ...) {
  this_ind <- ind_ids["bp"]

  params <- list(...)
  params["end_year"] <- end_year
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "business_as_usual")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  params_fixed_target <- get_right_params(params, scenario_fixed_target)
  params_fixed_target <- c(
    params_fixed_target,
    list(target_value = 80, target_year = 2030, scenario_name = "fixed_target")
  )

  df_fixed_target <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind_default), params_fixed_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "fixed_target")

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of[["scenario_name"]] <- "acceleration"
  params_best_of[["scenario_names"]] <- c("business_as_usual", "fixed_target")

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = dplyr::bind_rows(df_bau, df_fixed_target)), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate doctors
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
accelerate_doctors <- function(df,
                               ind_ids = billion_ind_codes("uhc"),
                               scenario_col = "scenario",
                               bau_scenario = "historical",
                               ...) {
  this_ind <- ind_ids["doctors"]

  params <- list(...)
  params["upper_limit"] <- 10000
  params["scenario_col"] <- scenario_col
  params["bau_scenario"] <- bau_scenario

  params <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == bau_scenario)

  df_accelerated <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate nurses
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inherit accelerate_alcohol
#'
accelerate_nurses <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario_col = "scenario",
                              bau_scenario = "historical",
                              ...) {
  this_ind <- ind_ids["nurses"]

  params <- list(...)
  params["upper_limit"] <- 10000
  params["scenario_col"] <- scenario_col
  params["bau_scenario"] <- bau_scenario

  params <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)


  df_accelerated <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate hwf
#'
#' Accelerate hwf by first dividing countries into two groups:
#' - For countries with a 2018 value greater than or equal to the 2018 global median,
#' business as usual is returned.
#' - For countries with a 2018 value less than the 2018 global median, a **linear change
#' of 4.54 per year from 2018 to 2025** is returned.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
accelerate_hwf <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario_col = "scenario",
                           value_col = "value",
                           start_year = 2018,
                           default_scenario = "default",
                           bau_scenario = "historical",
                           ...) {
  this_ind <- ind_ids["hwf"]

  params <- list(...)
  params["upper_limit"] <- 10000
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_with_scenario_linear <- c(
    get_right_params(params, scenario_linear_change),
    list(scenario_name = "acceleration", linear_value = 4.54)
  )

  params_no_scenario_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_with_scenario <- df_this_ind %>%
    dplyr::mutate(glob_med = stats::median(.data[[value_col]][.data[["year"]] == start_year])) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(any(.data[[value_col]] < .data[["glob_med"]] & .data[["year"]] == start_year)) %>%
    dplyr::ungroup()

  df_no_scenario <- df_this_ind %>%
    dplyr::mutate(glob_med = stats::median(.data[[value_col]][.data[["year"]] == start_year])) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!any(.data[[value_col]] < .data[["glob_med"]] & .data[["year"]] == start_year)) %>%
    dplyr::ungroup()

  if (nrow(df_with_scenario) > 0) {

    df_with_scenario_default <- df_with_scenario %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_with_scenario_accelerated <- do.call(
      scenario_linear_change, c(list(df = df_with_scenario_default), params_with_scenario_linear)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  if (nrow(df_no_scenario) > 0) {
    df_no_scenario_bau <- df_no_scenario %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_no_scenario_accelerated <- do.call(
      scenario_bau, c(list(df = df_no_scenario), params_no_scenario_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_scenario_accelerated, df_no_scenario_accelerated)
}

#' Accelerate dtp3
#'
#' Accelerate dtp3 using a customised version of scenario_fixed_target with the
#' following peculiarities:
#' - baseline_year = 2019;
#' - the 2020 value is kept identical to the 2019 (baseline) value;
#' - the target_year is 2030; and
#' - the scenario is then a straight line to the target_value and target_year
#' - the target values for each country are provided by the technical program.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_dtp3 <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario_col = "scenario",
                            value_col = "value",
                            start_year = 2018,
                            end_year = 2025,
                            default_scenario = "default",
                            ...) {
  baseline_year <- 2019
  target_year <- 2030

  this_ind <- ind_ids["dtp3"]

  params <- list(...)

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  full_df <- tidyr::expand_grid(
    "iso3" := unique(df_this_ind[["iso3"]]),
    "year" := start_year:end_year,
    "ind" := this_ind,
    "{scenario_col}" := unique(df_this_ind[[scenario_col]])
  )

  df_target_values <- load_misc_data(
    file_path = "scenarios/dtp3/IA ZD and coverage targets_GPW13.xlsx",
    skip = 1
  ) %>%
    dplyr::select(!!sym("iso3") := .data[["ISO"]], target = "DTP 3 Target") %>%
    dplyr::mutate(!!sym("iso3") := toupper(.data[["iso3"]]), target = .data[["target"]] * 100)

  df_accelerated <- df_this_ind %>%
    dplyr::full_join(full_df, by = c("iso3", "year", "ind", scenario_col)) %>%
    dplyr::group_by(.data[["iso3"]], .data[[scenario_col]]) %>%
    dplyr::mutate(baseline_value = .data[[value_col]][.data[["year"]] == baseline_year]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(df_target_values, by = "iso3") %>%
    dplyr::mutate(
      "acceleration" := dplyr::case_when(
        .data[["year"]] > 2018 & .data[["year"]] <= 2020 ~ as.numeric(.data[["baseline_value"]]),
        .data[["year"]] >= baseline_year + 1 & .data[["year"]] <= target_year & .data[["baseline_value"]] < .data[["target"]] ~
          as.numeric(.data[["baseline_value"]] + (.data[["target"]] - .data[["baseline_value"]]) * (.data[["year"]] - baseline_year - 1) / (target_year - baseline_year - 1)),
        .data[["year"]] >= baseline_year + 1 & .data[["year"]] <= target_year & .data[["baseline_value"]] >= .data[["target"]] ~ as.numeric(.data[["baseline_value"]]),
        .data[["year"]] == 2018 ~ as.numeric(.data[[value_col]]),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(!c("baseline_value", "target")) %>%
    dplyr::filter(!is.na(.data[["acceleration"]])) %>%
    dplyr::select(!dplyr::all_of(c(value_col))) %>%
    dplyr::rename(!!sym(value_col) := "acceleration") %>%
    dplyr::mutate(!!sym(scenario_col) := "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}


#' Accelerate fh
#'
#' Accelerate fh by taking the best of business as usual and halting upward trends
#' in the data to the 2018 value.
#'
#' @inherit accelerate_alcohol
#'
accelerate_fh <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario_col = "scenario",
                          default_scenario = "default",
                          bau_scenario = "historical",
                          ...) {
  this_ind <- ind_ids["fh"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_bau <- get_right_params(params, scenario_bau)
  params_halt_rise <- get_right_params(params, scenario_halt_rise)
  params_halt_rise[["baseline_year"]] <- 2018

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)


  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind_default), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "halt_rise")

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of["scenario_name"] <- "acceleration"

  df_accelerated <- dplyr::bind_rows(df_bau, df_halt_rise)

  df_accelerated <- do.call(
    scenario_best_of, c(
      list(
        df = df_accelerated,
        scenario_names = c("business_as_usual", "halt_rise")
      ),
      params_best_of)) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate fp
#'
#' Accelerate fp by dividing the countries into two groups:
#' - For BRN, CYP, FSM, ISL, LUX, and SYC, return business as usual.
#' - For all other countries, take the best of business as usual and the quantile
#' target for quantile_year = 2018 and 5 quantiles (capped by the maximum regional
#' value in 2018).
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_fp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario_col = "scenario",
                          value_col = "value",
                          default_scenario = "default",
                          bau_scenario = "historical",
                          ...) {
  this_ind <- ind_ids["fp"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  exclude_countries <- c(
    whoville::who_member_states("small"),
    "BRN", "CYP", "FSM", "ISL", "LUX", "SYC"
  )

  params <- list(...)
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario
  params["small_is_best"] <- get_ind_metadata(this_ind, "small_is_best")

  params_exclude_bau <- get_right_params(params, scenario_bau)
  params_exclude_bau["scenario_name"] <- "acceleration"

  params_main_bau <- get_right_params(params, scenario_bau)
  params_main_bau["scenario_name"] <- "business_as_usual"

  params_main_quantile <- get_right_params(params, scenario_quantile)
  params_main_quantile["n"] <- 5
  params_main_quantile["scenario_name"] <- "quantile_5"

  df_exclude <- df_this_ind %>%
    dplyr::filter(.data[["iso3"]] %in% exclude_countries)

  df_main <- df_this_ind %>%
    dplyr::filter(!.data[["iso3"]] %in% exclude_countries)

  if (nrow(df_exclude) > 0) {
    # Run only scenario_bau for exclude_countries defined above
    df_exclude_bau <- df_exclude %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_exclude_accelerated <- do.call(
      scenario_bau, c(list(df = df_exclude_bau), params_exclude_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_exclude_accelerated <- tibble::tibble()
  }

  if (nrow(df_main) > 0) {
    # Run scenario_bau and scenario_quantile(n = 5) on the remaining countries
    # then find the best of the two options

    df_main_bau <- df_main %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_main_bau <- do.call(
      scenario_bau, c(list(df = df_main_bau), params_main_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "business_as_usual")

    # scenario_quantile values have an upper cap defined by the maximum regional value in 2018

    df_main_default <- df_main %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    df_regional <- df_main_default %>%
      dplyr::filter(.data[["year"]] == 2018) %>%
      dplyr::group_by("region" := whoville::iso3_to_regions(.data[["iso3"]])) %>%
      dplyr::summarise(regional_max = max(.data[[value_col]]))

    df_main_quantile <- do.call(
      scenario_quantile, c(list(df = df_main_default), params_main_quantile)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "quantile_5") %>%
      dplyr::mutate("region" := whoville::iso3_to_regions(.data[["iso3"]])) %>%
      dplyr::left_join(df_regional, by = "region") %>%
      dplyr::mutate(!!sym(value_col) := pmin(.data[[value_col]], .data[["regional_max"]])) %>%
      dplyr::select(!c("region", "regional_max"))

    params_best_of <- get_right_params(params, scenario_best_of)
    params_best_of[["scenario_names"]] <- c("business_as_usual", "quantile_5")
    params_best_of[["scenario_name"]] <- "acceleration"
    df_main_accelerated <- do.call(
      scenario_best_of, c(list(df = dplyr::bind_rows(df_main_quantile, df_main_bau)), params_best_of)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_main_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_exclude_accelerated, df_main_accelerated)
}

#' Accelerate fpg
#'
#' Accelerate fpg by halting the rise to 2010 value.
#'
#' @inherit accelerate_alcohol
#'
accelerate_fpg <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario_col = "scenario",
                           ...) {
  ind_ids <- "fpg"
  names(ind_ids) <- "adult_obese"

  accelerate_adult_obese(
    df = df,
    ind_ids = ind_ids,
    ...
  )
}

#' Accelerate itn
#'
#' Accelerate itn by taking the best of business as usual and a **fixed target of
#' 80 by 2030**.
#'
#' @inherit accelerate_anc4
#'
accelerate_itn <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario_col = "scenario",
                           default_scenario = "default",
                           bau_scenario = "historical",
                           ...) {
  this_ind <- ind_ids["itn"]

  params <- list(...)
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_bau <- get_right_params(params, scenario_bau)
  params_bau["scenario_name"] <- "business_as_usual"


  params_fixed_target <- get_right_params(params, scenario_fixed_target)
  params_fixed_target <- c(
    params_fixed_target,
    list(target_value = 80, target_year = 2030, scenario_name = "fixed_target")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_fixed_target <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind_default), params_fixed_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "fixed_target")

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of[["scenario_names"]] <- c("business_as_usual", "fixed_target")
  params_best_of[["scenario_name"]] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = dplyr::bind_rows(df_fixed_target, df_bau)), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate pneumo
#'
#' Accelerate pneumo by taking the best of business as usual and a **fixed target
#' of 90 by 2025** for countries with two or more data points since 2000. Otherwise,
#' the business as usual scenario is used.
#'
#' @inherit accelerate_alcohol
#'
accelerate_pneumo <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario_col = "scenario",
                              default_scenario = "default",
                              bau_scenario = "historical",
                              ...) {
  this_ind <- ind_ids["pneumo"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  params_bau <- get_right_params(params, scenario_bau)
  params_bau["scenario_name"] <- "business_as_usual"

  params_linear_change <- get_right_params(params, scenario_linear_change)
  params_linear_change <- c(
    params_linear_change,
    list(linear_value = 3, target_year = 2025, scenario_name = "3_percent_change",
         upper_limit = 90)
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  iso3_more_2_values_since_2020 <- df_this_ind_default %>%
    dplyr::filter(.data[["type"]] %in% c("reported", "estimated"),
                  .data[["year"]] >= 2000) %>%
    dplyr::group_by(dplyr::across(c("iso3", "ind"))) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::filter(.data[["n"]] >= 2) %>%
    dplyr::pull(.data[["iso3"]])

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  df_linear_change <- do.call(
    scenario_linear_change, c(list(df = df_this_ind_default), params_linear_change)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "3_percent_change",
                  .data[["iso3"]] %in% iso3_more_2_values_since_2020)

  df_bau_more_2_values_since_2020 <- df_bau %>%
    dplyr::filter(.data[["iso3"]] %in% iso3_more_2_values_since_2020)

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of[["scenario_names"]] <- c("business_as_usual", "3_percent_change")
  params_best_of[["scenario_name"]] <- "acceleration"
  params_best_of[["maximize_end_year"]] <- TRUE

  if(nrow(dplyr::bind_rows(df_linear_change, df_bau_more_2_values_since_2020)) > 0){
    df_best_of_3_percent_change_bau <- do.call(
      scenario_best_of, c(list(df = dplyr::bind_rows(df_linear_change, df_bau_more_2_values_since_2020)), params_best_of)
    )
  }else{
    df_best_of_3_percent_change_bau <- dplyr::bind_rows(df_linear_change, df_bau_more_2_values_since_2020)
  }

  df_bau_no_more_2_values_since_2020 <- df_bau %>%
    dplyr::filter(!.data[["iso3"]] %in% iso3_more_2_values_since_2020) %>%
    dplyr::mutate("{scenario_col}" := "acceleration")

  df_accelerated <- dplyr::bind_rows(df_bau_no_more_2_values_since_2020, df_best_of_3_percent_change_bau)%>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate tb
#'
#' Accelerate tb by using a **fixed target of 90 by 2025**.
#'
#' @inherit accelerate_alcohol
#'
accelerate_tb <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario_col = "scenario",
                          default_scenario = "default",
                          bau_scenario = "historical",
                          ...) {
  this_ind <- ind_ids["tb"]

  params <- list(...)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_this_ind_bau <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == bau_scenario)

  df_this_ind_default <- df_this_ind %>%
    dplyr::filter(.data[[scenario_col]] == default_scenario)

  params_fixed_target <- get_right_params(params, scenario_fixed_target)

  params_fixed_target["scenario_name"] <- "fixed_target"
  params_fixed_target["target_value"] <- 90

  df_fixed_target <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind_default), params_fixed_target)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "fixed_target")

  params_bau <- get_right_params(params, scenario_bau)
  params_bau["scenario_name"] <- "business_as_usual"

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind_bau), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "business_as_usual")

  params_best_of <- get_right_params(params, scenario_best_of)
  params_best_of[["scenario_names"]] <- c("business_as_usual", "fixed_target")
  params_best_of[["scenario_name"]] <- "acceleration"

  df_accelerated <- do.call(
    scenario_best_of, c(list(df = dplyr::bind_rows(df_bau, df_fixed_target)), params_best_of)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate uhc_sanitation
#'
#' Accelerate uhc_sanitation by encouraging the country to reach the mean (or upper
#' threshold) of the quantile it belongs to in 2017, with n = 5 quantiles. Lower
#' and upper limits of 0 and 99, respectively, are also imposed on the results.
#'
#' @inherit accelerate_anc4
#' @inheritParams recycle_data
#'
accelerate_uhc_sanitation <- function(df,
                                      ind_ids = billion_ind_codes("uhc"),
                                      scenario_col = "scenario",
                                      default_scenario = "default",
                                      ...) {
  this_ind <- ind_ids["uhc_sanitation"]

  params <- list(...)
  params <- get_right_params(params, scenario_quantile)
  params["scenario_col"] <- scenario_col
  params["default_scenario"] <- default_scenario

  params <- c(
    params,
    list(
      n = 5, quantile_year = 2017, trim = TRUE, lower_limit = 0,
      upper_limit = 99
    )
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind, scenario_name = "acceleration"), params)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}
# @Alice, there are no countries without data
# @Alice, why is scenario_bau called twice for withdata_df?
# @Alice, need explanation on following comments:
# NB cannot take hpop outputs because the imputed data (45 coutnries) is removed for hpop tobacco
# Is the input for this function hpop_tobacco, instead of uhc_tobacco due to the missing data for UHC?

#' Accelerate uhc_tobacco
#'
#' Accelerate uhc_tobacco by first dividing countries into two groups:
#' - For countries without any routine (i.e., estimated) data, business as usual
#' is returned
#' - For countries with routine (i.e., estimated) data, the best of business as
#' usual and a **percent decrease of 30% between 2010 and 2025** is returned. Both
#' scenarios are run on the **crude tobacco usage** values, which are then converted
#' to their age-standardised equivalents using an approximation.
#'
#' @inherit accelerate_alcohol
#' @inheritParams accelerate_child_viol
#'
accelerate_uhc_tobacco <- function(df,
                                   ind_ids = billion_ind_codes("uhc"),
                                   scenario_col = "scenario",
                                   value_col = "value",
                                   end_year = 2025,
                                   start_year = 2018,
                                   default_scenario = "default",
                                   bau_scenario = "historical",
                                   ...) {
  this_ind <- ind_ids["uhc_tobacco"]

  params <- list(...)
  params["end_year"] <- end_year
  params["start_year"] <- start_year
  params["default_scenario"] <- default_scenario
  params["bau_scenario"] <- bau_scenario


  params_without_data_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  params_with_data_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "with_data_bau", value = "crude")
  )

  params_with_data_perc_baseline <- c(
    get_right_params(params, scenario_percent_baseline),
    list(
      scenario_name = "with_data_perc_baseline",
      percent_change = -30,
      baseline_year = 2010,
      target_year = end_year,
      start_year = start_year,
      end_year = end_year
    )
  )

  params_with_data_perc_baseline[[value_col]] <- "crude"

  par_wd_pb <- params_with_data_perc_baseline

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!any(.data[["type"]] == "estimated")) %>%
    dplyr::ungroup()

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(any(.data[["type"]] == "estimated")) %>%
    dplyr::ungroup()

  if (nrow(df_without_data) > 0) {
    df_without_data_bau <- df_without_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_without_data_bau), params_without_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_data) > 0) {
    trajectory_df <- load_misc_data(
      file_path = "scenarios/uhc_tobacco/Tobacco_UHC Billion_Trajectory conversion.xlsx",
      sheet = "Tobacco Data",
      range = cellranger::cell_cols(2:7)
    ) %>%
      dplyr::filter(.data[["sex"]] == "Total") %>%
      dplyr::select(c("iso3", "measure", "year", "value"))

    tobacco_ratio_df <- trajectory_df %>%
      dplyr::mutate(measure = ifelse(.data[["measure"]] == "Crude", "crude", "agestd")) %>%
      tidyr::pivot_wider(names_from = .data[["measure"]], values_from = .data[[value_col]]) %>%
      dplyr::mutate(ratio_agestd_over_crude = .data[["agestd"]] / .data[["crude"]])

    # Extending the input trajectories to end_year, using flat_extrap from 2023 values
    tobacco_ratio_df <- tidyr::expand_grid(
      iso3 = unique(tobacco_ratio_df[["iso3"]]),
      year = 2000:end_year
    ) %>%
      dplyr::full_join(tobacco_ratio_df, by = c("iso3", "year")) %>%
      flat_extrapolation(col = "agestd") %>%
      flat_extrapolation(col = "crude") %>%
      flat_extrapolation(col = "ratio_agestd_over_crude") %>%
      dplyr::select(!c("pred")) %>%
      dplyr::rename("iso3" := "iso3", "year" := "year")

    tobm <- tobacco_ratio_df %>%
      dplyr::group_by(.data[["year"]]) %>%
      dplyr::summarise(m = mean(.data[["ratio_agestd_over_crude"]]))

    df_with_data <-tidyr::expand_grid(
      iso3 = unique(df_with_data[["iso3"]]),
      ind = unique(df_with_data[["ind"]]),
      year = 2000:end_year) %>%
      dplyr::full_join(df_with_data, by = c("iso3", "year", "ind")) %>%
      dplyr::left_join(tobacco_ratio_df, by = c("iso3", "year")) %>%
      dplyr::left_join(tobm, by = "year") %>%
      dplyr::mutate(
        ratio_agestd_over_crude = ifelse(is.na(.data[["ratio_agestd_over_crude"]]), .data[["m"]], .data[["ratio_agestd_over_crude"]]),
        crude = .data[[value_col]] / .data[["ratio_agestd_over_crude"]]
      ) %>%
      dplyr::select(-c("m"))

    df_with_data_bau <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == bau_scenario)

    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data_bau), params_with_data_bau)
    ) %>%
      dplyr::filter(.data[[scenario_col]] == "with_data_bau") %>%
      flat_extrapolation(col = "crude",
                         group_col = c("iso3", "ind")) %>%
      dplyr::mutate(!!sym(value_col) := .data[["crude"]])

    df_with_data_default <- df_with_data %>%
      dplyr::filter(.data[[scenario_col]] == default_scenario)

    full_df <-  tidyr::expand_grid(
      "iso3" := unique(df_with_data_default[["iso3"]]),
      "year" := start_year:end_year,
      "ind" := this_ind,
      "{scenario_col}" := default_scenario)

    df_with_data_perc_baseline <- df_with_data_default %>%
      dplyr::full_join(full_df, by = c("iso3", "year", "ind", scenario_col)) %>%
      dplyr::group_by(.data[["iso3"]]) %>%
      dplyr::mutate(valtemp = .data[[par_wd_pb[["value"]]]]) %>%
      dplyr::mutate(baseline_value = .data[["valtemp"]][.data[["year"]] == par_wd_pb[["start_year"]]]) %>%
      dplyr::mutate(old_baseline_value = .data[["valtemp"]][.data[["year"]] == par_wd_pb[["baseline_year"]]]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(goal2025 = .data[["old_baseline_value"]] * (100 + par_wd_pb[["percent_change"]]) / 100) %>%
      dplyr::mutate(
        goalend = .data[["old_baseline_value"]] + (.data[["goal2025"]] - .data[["old_baseline_value"]]) *
          (par_wd_pb[["end_year"]] - par_wd_pb[["baseline_year"]]) / (par_wd_pb[["target_year"]] - par_wd_pb[["baseline_year"]])
      ) %>%
      dplyr::mutate(
        "{par_wd_pb[['scenario_name']]}" := ifelse(
          .data[["year"]] >= par_wd_pb[["start_year"]] & .data[["year"]] <= par_wd_pb[["target_year"]],
          .data$baseline_value + (.data[["goalend"]] - .data[["baseline_value"]]) * (.data[["year"]] - par_wd_pb[["start_year"]]) / (par_wd_pb[["end_year"]] - par_wd_pb[["start_year"]]),
          NA_real_
        )
      ) %>%
      flat_extrapolation(col = "crude", group_col = c("iso3", "ind")) %>%
      dplyr::select(!c("valtemp", "baseline_value", "goalend", "goal2025", "old_baseline_value")) %>%
      dplyr::filter(!is.na(.data[[par_wd_pb[["scenario_name"]]]]))

    # Replace crude column with {scenario_name} column and set scenario = {scenario_name}
    # Now both df_with_data_bau and df_with_data_perc_baseline have the scenario-projected values in the crude column
    # with the scenario column disambiguating between the two scenarios
    df_with_data_perc_baseline <- df_with_data_perc_baseline %>%
      dplyr::select(-c("crude")) %>%
      dplyr::rename("crude" := .data[[par_wd_pb[["scenario_name"]]]]) %>%
      dplyr::mutate("{scenario_col}" := par_wd_pb[["scenario_name"]]) %>%
      dplyr::select(-c("agestd", "ratio_agestd_over_crude")) %>%
      dplyr::left_join(dplyr::select(tobacco_ratio_df, -"crude"), by = c("iso3", "year")) %>%
      dplyr::left_join(tobm, by = "year") %>%
      dplyr::mutate(ultimate_ratio = dplyr::if_else(is.na(.data[["ratio_agestd_over_crude"]]), .data[["m"]], .data[["ratio_agestd_over_crude"]])) %>%
      dplyr::mutate(!!sym(value_col) := .data[["crude"]] * .data[["ultimate_ratio"]]) %>%
      dplyr::select(-c("agestd", "crude", "ratio_agestd_over_crude", "m"))

    params_best_of <- get_right_params(params, scenario_best_of)
    params_best_of[["scenario_names"]] <- c("with_data_bau", "with_data_perc_baseline")
    params_best_of[["scenario_name"]] <- "acceleration"
    params_best_of[["maximize_end_year"]] <- TRUE

    df_with_data_accelerated <- do.call(
      scenario_best_of,
      c(list(df = dplyr::bind_rows(df_with_data_bau, df_with_data_perc_baseline)),
        params_best_of)) %>%
      dplyr::filter(.data[[scenario_col]] == "acceleration")

  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_accelerated, df_without_data_accelerated)
}
