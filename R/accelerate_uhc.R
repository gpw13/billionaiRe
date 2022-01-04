#' Accelerate anc4
#'
#' Accelerate anc4 by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, the acceleration scenario is the same
#' as business as usual.
#' - For countries with reported data, scenarios with both a **fixed target of 95%
#' by 2030** and a **linear change of 2.6 per year till 2025** are tried, with the easiest
#' to achieve of the two selected. The selected scenario is then compared against
#' the business as usual scenario for reported data, and the best of the two chosen
#' as the acceleration scenario.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario` is
#' set to `acceleration`
#'
accelerate_anc4 <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario = "scenario",
                            ind = "ind",
                            ...) {
  # CHECK: error, potentially because of without_data
  # @Elliott, what should the @inhereitParams be for the UHC functions?
  # Elliott: The error is due to duplicated rows in df_with_data_accelerated
  this_ind <- ind_ids["anc4"]

  params <- list(...)

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
    dplyr::filter(.data[[ind]] == this_ind)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) <= 1)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] == "reported", na.rm = TRUE) > 1) %>%
    dplyr::ungroup()

  if (nrow(df_with_data) > 0) {
    # With data: take easiest out of 95% by 2030 and 2.6/year which is top 10 performers
    # if bau is better take that
    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data), params_with_data_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_data_bau")

    df_with_data_fixed_target <- do.call(
      scenario_fixed_target, c(list(df = df_with_data), params_with_data_fixed_target)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_data_fixed_target")

    df_with_data_linear <- do.call(
      scenario_linear_change, c(list(df = df_with_data), params_with_data_linear)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_data_linear")

    # Elliott: see art for call to scenario_best_of with do.call

    df_with_data_best <- dplyr::bind_rows(df_with_data_fixed_target, df_with_data_linear) %>%
      scenario_best_of(
        scenario_names = c("with_data_fixed_target", "with_data_linear"),
        scenario_name = "best_of_linear_and_fixed_target",
        small_is_best = TRUE # hard-coding true because we want to take whichever of the two options is easiest for the country to accomplish
        # elliott: not necessary to hard code, it should be included in params
      ) %>%
      dplyr::filter(.data[["scenario"]] == "best_of_linear_and_fixed_target")

    # Elliott: when binding the df_with_data_best and df_with_data_bau duplicated rows are created. adding a distinct to remove them for now.
    # With new version of scenario_best_of, this shouldn't happen though.

    df_with_data_accelerated <- dplyr::bind_rows(df_with_data_best, df_with_data_bau) %>%
      dplyr::distinct() %>%
      scenario_best_of(
        scenario_names = c("with_data_bau", "best_of_linear_and_fixed_target"),
        scenario_name = "acceleration",
        small_is_best = params[["small_is_best"]]
      ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_without_data) > 0) {
    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_without_data), params_without_data_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
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
#' @inherit accelerate_anc4
#'
accelerate_art <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["art"]

  params <- list(...)

  params_with_data_bau <- get_right_params(params, scenario_bau)
  params_without_data_bau <- get_right_params(params, scenario_bau)
  params_without_data_bau[["scenario_name"]] <- "acceleration"

  params_with_data_fixed_target <- c(
    get_right_params(params, scenario_fixed_target),
    list(target_value = 90.25, scenario_name = "fixed_target")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= 2018) > 1) %>%
    dplyr::ungroup()

  # Elliott: get warnings here because df_without_data is empty (with test_data at least).
  # You could simply have an if-else statement to avoid this:

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::filter(sum(.data[["type"]] %in% c("estimated", "reported") & .data[["year"]] >= 2000 & .data[["year"]] <= 2018) <= 1) %>%
    dplyr::ungroup()

  if (nrow(df_without_data) > 0) {
    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_without_data), params_without_data_bau)
    ) %>%
      dplyr::filter(scenario == "acceleration")
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_data) > 0) {
    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data), params_with_data_bau)
    ) %>%
      dplyr::filter(scenario == "business_as_usual")

    df_with_data_fixed_target <- do.call(
      scenario_fixed_target, c(list(df = df_with_data), params_with_data_fixed_target)
    ) %>%
      dplyr::filter(scenario == "fixed_target")

    # Elliott: better to pass scenario_best_of to do.call to be able to pass params.
    # Can be done for all scenario_best_of calls.

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
      dplyr::filter(.data[[scenario]] == "acceleration")
  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_accelerated, df_without_data_accelerated)
}

# CHECK
#' Accelerate beds
#'
#' Accelerate beds by first dividing countries into two groups:
#' - For countries with 18 or more beds for all years after 2018, business
#' as usual is returned.
#' - For countries which have less than 18 beds for any of the years after 2018 (inclusive),
#' the best of business as usual and a **linear change of 0.36 per year up to 2025**,
#' with an upper limit of 18, is returned.
#'
#' @inherit accelerate_anc4
#'
accelerate_beds <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario = "scenario",
                            ind = "ind",
                            ...) {
  this_ind <- ind_ids["beds"]

  params <- list(...)

  params_no_sceanrio_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  params_with_sceanrio_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "with_scenario_bau")
  )

  params_with_sceanrio_linear <- c(
    get_right_params(params, scenario_linear_change),
    list(scenario_name = "with_scenario_linear", linear_value = 0.36, upper_limit = 18)
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_with_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(any((.data[["value"]] < 18 & .data[["year"]] >= 2018))) %>%
    dplyr::ungroup()

  df_no_scenario <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!any((.data[["value"]] < 18 & .data[["year"]] >= 2018))) %>%
    dplyr::ungroup()

  # Empty df (with test_data), so returns warnings. if-else statement as in art?

  if (nrow(df_no_scenario) > 0) {
    df_no_scenario_accelerated <- do.call(
      scenario_bau, c(list(df = df_no_scenario), params_no_sceanrio_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
  } else {
    df_no_scenario_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_scenario) > 0) {
    df_with_scenario_bau <- do.call(
      scenario_bau, c(list(df = df_with_scenario), params_with_sceanrio_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_scenario_bau")

    df_with_scenario_linear <- do.call(
      scenario_linear_change, c(list(df = df_with_scenario), params_with_sceanrio_linear)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_scenario_linear")

    # Elliott: see art for call to scenario_best_of with do.call

    df_with_scenario_accelerated <- dplyr::bind_rows(df_with_scenario_bau, df_with_scenario_linear) %>%
      scenario_best_of(
        scenario_names = c("with_scenario_bau", "with_scenario_linear"),
        scenario_name = "acceleration",
        small_is_best = params[["small_is_best"]]
      ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_no_scenario_accelerated, df_with_scenario_accelerated)
}

#' Accelerate bp
#'
#' Accelerate bp by taking the best of business as usual and a **decrease of 25% from
#' 2010 to 2025**. These scenarios are run on the crude bp values, which
#' are then converted back to their age-standardised equivalents using an approximation.
#'
#' @inherit accelerate_anc4
#'
accelerate_bp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["bp"]

  params <- list(...)

  params_perc_baseline <- c(
    get_right_params(params, scenario_percent_baseline),
    list(
      value = "crude",
      percent_change = -25,
      baseline_year = 2010,
      start_year = 2018,
      end_year = 2025,
      scenario_name = "percent_baseline"
    )
  )

  params_bau <- c(
    get_right_params(params, scenario_bau),
    list(value = "crude", scenario_name = "business_as_usual")
  )

  # Preliminary wrangling from the bp.R script to get bp_as_cr_ratio
  bp_agestd <- load_misc_data("scenarios/bp/NCD-RisC_Model78_hypertension_treatment_country_estimates_Age-standardised.csv") %>%
    dplyr::filter(.data[["Metric"]] == "Hypertension") %>%
    dplyr::select(-c("Type", "Metric")) %>%
    dplyr::mutate(
      Country = ifelse(.data[["Country"]] == "Macedonia (TFYR)", "Macedonia", .data[["Country"]]),
      iso3 = whoville::names_to_iso3(.data[["Country"]])
    ) %>%
    suppressMessages() %>%
    dplyr::rename(
      "country" = "Country",
      "year" = "Year",
      "sex" = "Sex",
      "value" = "Prevalence",
      "lower" = "95% lower limit",
      "upper" = "95% upper limit",
    ) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
    dplyr::group_by(.data[["iso3"]], .data[["year"]]) %>%
    dplyr::summarize("value" := mean(.data[["value"]], na.rm = TRUE), .groups = "drop") %>% # total value is average of male and female
    dplyr::mutate(ind = "bp_agestd", type = ifelse(.data[["year"]] <= 2019, "estimated", "projected")) # check

  bp_crude <- load_misc_data("scenarios/bp/NCD-RisC_Model78_hypertension_treatment_country_estimates_Crude.csv") %>%
    dplyr::filter(.data[["Metric"]] == "Hypertension") %>%
    dplyr::select(-c("Type", "Metric")) %>%
    dplyr::mutate(ind = "bp_crude") %>%
    dplyr::mutate(
      Country = ifelse(.data[["Country"]] == "Macedonia (TFYR)", "Macedonia", .data[["Country"]]),
      iso3 = whoville::names_to_iso3(.data[["Country"]])
    ) %>%
    suppressMessages() %>%
    dplyr::rename(
      "country" = "Country",
      "year" = "Year",
      "sex" = "Sex",
      "value" = "Prevalence",
      "lower" = "95% lower limit",
      "upper" = "95% upper limit",
    ) %>%
    dplyr::left_join(wppdistro::wpp_population, by = c("iso3", "year", "sex")) %>%
    dplyr::mutate(
      pop_30_79 = .data[["30_34"]] + .data[["35_39"]] + .data[["40_44"]] + .data[["45_49"]] + .data[["50_54"]] + .data[["55_59"]] + .data[["60_64"]] + .data[["65_69"]] + .data[["70_74"]] + .data[["75_79"]]
    ) %>%
    dplyr::select(c("iso3", "year", "ind", "sex", "value", "lower", "upper", "pop_30_79")) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]])) %>%
    dplyr::group_by(.data[["iso3"]], .data[["year"]]) %>%
    dplyr::summarize("value" := stats::weighted.mean(.data[["value"]], w = .data[["pop_30_79"]]), .groups = "drop") %>%
    dplyr::mutate(ind = "bp_crude", type = ifelse(.data[["year"]] <= 2019, "estimated", "projected"))

  bp_ratio <- bp_agestd %>%
    dplyr::bind_rows(bp_crude) %>%
    tidyr::pivot_wider(names_from = .data[[ind]], values_from = .data[["value"]]) %>%
    # filter(year==2019) %>%
    dplyr::mutate(ratio_agestd_over_crude = .data[["bp_agestd"]] / .data[["bp_crude"]]) %>%
    dplyr::select(c("year", "iso3", "ratio_agestd_over_crude"))

  bp_agestd_crude_ratio <- bp_ratio %>%
    # @Alice, should there final year be 2023 or 2025?
    dplyr::full_join(tidyr::expand_grid(iso3 = unique(bp_agestd$iso3), year = 2020:2023)) %>%
    dplyr::left_join(
      bp_ratio %>%
        dplyr::filter(.data[["year"]] == 2019) %>%
        dplyr::select(-.data[["year"]], ratio = .data[["ratio_agestd_over_crude"]])
    ) %>%
    # @Alice, please explain the logic for the ifelse(is.na(...)) statement
    # given that ratio and ratio_agestd_over_crude are the same column
    dplyr::mutate(ratio_agestd_over_crude = ifelse(is.na(.data[["ratio_agestd_over_crude"]]), .data[["ratio"]], .data[["ratio_agestd_over_crude"]])) %>%
    dplyr::select(!c("ratio"))

  bp_agestd_crude_ratio <- augury::expand_df(
    bp_agestd_crude_ratio,
    iso3 = unique(bp_agestd_crude_ratio$iso3),
    year = 1990:2025,
    response = "ratio_agestd_over_crude",
    keep_before_obs = TRUE,
    keep_no_obs = TRUE
  ) %>%
    augury::predict_simple("flat_extrap", col = "ratio_agestd_over_crude")

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind) %>%
    # @Alice
    # convert to crude, run hpop scenario on crude, convert back to agrestd
    # NB need to recalculate ratios????
    dplyr::left_join(bp_agestd_crude_ratio) %>%
    dplyr::mutate(ratio_agestd_over_crude = ifelse(is.na(.data[["ratio_agestd_over_crude"]]), 1, .data[["ratio_agestd_over_crude"]])) %>%
    dplyr::mutate(crude = .data[["value"]] / .data[["ratio_agestd_over_crude"]])

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_perc_baseline <- do.call(
    scenario_percent_baseline, c(list(df = df_this_ind), params_perc_baseline)
  ) %>%
    dplyr::filter(.data[[scenario]] == "percent_baseline")

  # Elliott: see art for call to scenario_best_of with do.call

  df_accelerated <- dplyr::bind_rows(df_bau, df_perc_baseline) %>%
    scenario_best_of(
      value = "crude",
      scenario_names = c("business_as_usual", "percent_baseline"),
      scenario_name = "acceleration",
      small_is_best = params[["small_is_best"]]
    ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration") %>%
    # convert crude values back to age-standardised
    dplyr::mutate(value = .data$value * .data$ratio_agestd_over_crude)

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: warnings with column last_value
#' Accelerate doctors
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
accelerate_doctors <- function(df,
                               ind_ids = billion_ind_codes("uhc"),
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  this_ind <- ind_ids["doctors"]

  params <- list(...)
  params <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: warnings with column last_value
#' Accelerate nurses
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inherit accelerate_anc4
#'
accelerate_nurses <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario = "scenario",
                              ind = "ind",
                              ...) {
  this_ind <- ind_ids["nurses"]

  params <- list(...)
  params <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK
# @Alice, For noscenario_df, why is scenario_bestof called when scen_linear = scen_bau
# and so both options being compared are exactly the same?
#
# @Alice, In both cases, after scenario_bestof is called, scen_acceleration is
# manually set, even though scenarrio_best_of is called. Is the scenario_best_of call
# unnecessary for acceleration scenarios?

#' Accelerate hwf
#'
#' Accelerate hwf by first dividing countries into two groups:
#' - For countries with a 2018 value greater than or equal to the 2018 global median,
#' business as usual is returned.
#' - For countries with a 2018 value less than the 2018 global median, a **linear change
#' of 4.54 per year from 2018 to 2025** is returned.
#'
#' @inherit accelerate_anc4
#'
accelerate_hwf <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["hwf"]

  params <- list(...)

  params_with_scenario_linear <- c(
    get_right_params(params, scenario_linear_change),
    list(scenario_name = "acceleration", linear_value = 4.54)
  )

  params_no_scenario_bau <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_with_scenario <- df_this_ind %>%
    dplyr::mutate(glob_med = stats::median(.data$value[.data$year == 2018])) %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::filter(any(.data$value < .data$glob_med & .data$year == 2018)) %>%
    dplyr::ungroup()

  df_no_scenario <- df_this_ind %>%
    dplyr::mutate(glob_med = stats::median(.data$value[.data$year == 2018])) %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::filter(!any(.data$value < .data$glob_med & .data$year == 2018)) %>%
    dplyr::ungroup()

  if (nrow(df_with_scenario) > 0) {
    df_with_scenario_accelerated <- do.call(
      scenario_linear_change, c(list(df = df_with_scenario), params_with_scenario_linear)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
  } else {
    df_with_scenario_accelerated <- tibble::tibble()
  }

  if (nrow(df_no_scenario) > 0) {
    df_no_scenario_accelerated <- do.call(
      scenario_bau, c(list(df = df_no_scenario), params_no_scenario_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
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
#' @inherit accelerate_anc4
#'
accelerate_dtp3 <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario = "scenario",
                            ind = "ind",
                            ...) {

  start_year = 2018
  end_year = 2025
  baseline_year = 2019
  target_year = 2030

  this_ind <- ind_ids["dtp3"]

  params <- list(...)

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_target_values <- load_misc_data(
    file_name = "scenarios/dtp3/IA ZD and coverage targets_GPW13.xlsx",
    skip = 1
  ) %>%
    dplyr::select(iso3 = .data$ISO, target = "DTP 3 Target") %>%
    dplyr::mutate(iso3 = toupper(.data$iso3), target = .data$target * 100)

  df_accelerated <- df_this_ind %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::mutate(baseline_value = .data$value[.data$year == baseline_year]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(df_target_values, by = "iso3") %>%
    dplyr::mutate(
      "acceleration" := dplyr::case_when(
        .data$year > 2018 & .data$year <= 2020 ~ .data$baseline_value,
        .data$year >= baseline_year + 1 & .data$year <= target_year & .data$baseline_value < .data$target ~
            .data$baseline_value + (.data$target - .data$baseline_value) * (.data$year - baseline_year - 1) / (target_year - baseline_year - 1),
        .data$year >= baseline_year + 1 & .data$year <= target_year & .data$baseline_value >= .data$target ~ .data$baseline_value,
        .data$year == 2018 ~ .data$value,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(!c("baseline_value", "target")) %>%
    dplyr::filter(!is.na(.data[["acceleration"]])) %>%
    # Replace value column with {scenario_name} column and set scenario = {scenario_name}
    dplyr::select(!c("value")) %>%
    dplyr::rename("value" = "acceleration") %>%
    dplyr::mutate(scenario = "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: error due to missing values

#' Accelerate fh
#'
#' Accelerate fh by taking the best of business as usual and halting upward trends
#' in the data to the 2018 value.
#'
#' @inherit accelerate_anc4
#'
accelerate_fh <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["fh"]

  params <- list(...)

  params_bau <- get_right_params(params, scenario_bau)
  params_halt_rise <- get_right_params(params, scenario_halt_rise)

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau) # need to pass df_this_ind
  ) %>%
    dplyr::filter(.data[[scenario]] == "business_as_usual")

  df_halt_rise <- do.call(
    scenario_halt_rise, c(list(df = df_this_ind), params_halt_rise)
  ) %>%
    dplyr::filter(.data[[scenario]] == "halt_rise")

  df_accelerated <- dplyr::bind_rows(df_bau, df_halt_rise) %>%
    scenario_best_of(
      scenario_names = c("business_as_usual", "halt_rise"),
      scenario_name = "acceleration",
      small_is_best = params[["small_is_best"]]
    ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK

#' Accelerate fp
#'
#' Accelerate fp by dividing the countries into two groups:
#' - For BRN, CYP, FSM, ISL, LUX, and SYC, return business as usual.
#' - For all other countries, take the best of business as usual and the quantile
#' target for quantile_year = 2018 and 5 quantiles (capped by the maximum regional
#' value in 2018).
#'
#' @inherit accelerate_anc4
#'
accelerate_fp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["fp"]

  exclude_countries <- c(
    whoville::who_member_states("small"),
    "BRN", "CYP", "FSM", "ISL", "LUX", "SYC"
  )

  params <- list(...)
  params["small_is_best"] <- get_ind_metadata(this_ind, "small_is_best")

  params_exclude_bau <- get_right_params(params, scenario_bau)
  params_exclude_bau["scenario_name"] <- "acceleration"

  params_main_bau <- get_right_params(params, scenario_bau)
  params_main_bau["scenario_name"] <- "business_as_usual"

  params_main_quantile <- get_right_params(params, scenario_quantile)
  params_main_quantile["n"] <- 5
  params_main_quantile["scenario_name"] <- "quantile_5"

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_exclude <- df_this_ind %>%
    dplyr::filter(.data[["iso3"]] %in% exclude_countries)

  df_main <- df_this_ind %>%
    dplyr::filter(!.data[["iso3"]] %in% exclude_countries)

  if (nrow(df_exclude) > 0) {
    # Run only scenario_bau for exclude_countries defined above
    df_exclude_accelerated <- do.call(
      scenario_bau, c(list(df = df_exclude), params_exclude_bau)
    ) %>%
      dplyr::filter(.data[[scenario]] == "acceleration")
  } else {
    df_exclude_accelerated <- tibble::tibble()
  }

  if (nrow(df_main) > 0) {
    # Run scenario_bau and scenario_quantile(n = 5) on the remaining countries
    # then find the best of the two options
    df_main_bau <- do.call(
      scenario_bau, c(list(df = df_main), params_main_bau)
    ) %>%
      dplyr::filter(.data[[scenario]] == "business_as_usual")

    # scenario_quantile values have an upper cap defined by the maximum regional value in 2018
    df_regional <- df_main %>%
      dplyr::filter(.data$year == 2018) %>%
      dplyr::group_by("region" := whoville::iso3_to_regions(.data[["iso3"]])) %>%
      dplyr::summarise(regional_max = max(.data[["value"]]))

    df_main_quantile <- do.call(
      scenario_quantile, c(list(df = df_main), params_main_quantile)
    ) %>%
      dplyr::filter(.data[[scenario]] == "quantile_5") %>%
      dplyr::mutate("region" := whoville::iso3_to_regions(.data[["iso3"]])) %>%
      dplyr::left_join(df_regional, by = "region") %>%
      dplyr::mutate("value" := pmin(.data[["value"]], .data[["regional_max"]])) %>%
      dplyr::select(!c("region", "regional_max"))

    # Elliott: see art for call to scenario_best_of with do.call

    df_main_accelerated <- dplyr::bind_rows(df_main_bau, df_main_quantile) %>%
      scenario_best_of(
        scenario_names = c("business_as_usual", "quantile_5"),
        scenario_name = "acceleration",
        small_is_best = params[["small_is_best"]]
      ) %>%
      dplyr::filter(.data[[scenario]] == "acceleration")
  } else {
    df_main_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_exclude_accelerated, df_main_accelerated)
}

# CHECK: warnings, also returns 24,800 scenario rows when it should only around 1,500 or so.

#' Accelerate fpg
#'
#' Accelerate fpg using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
accelerate_fpg <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["fpg"]

  params <- list(...)
  params <- c(
    get_right_params(params, scenario_bau),
    list(scenario_name = "acceleration")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_bau, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: warnings

#' Accelerate itn
#'
#' Accelerate itn by taking the best of business as usual and a **fixed target of
#' 80 by 2030**.
#'
#' @inherit accelerate_anc4
#'
accelerate_itn <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["itn"]

  params <- list(...)

  params_bau <- get_right_params(params, scenario_bau)
  params_bau["scenario_name"] <- "business_as_usual"

  params_fixed_target <- get_right_params(params, scenario_fixed_target)
  params_fixed_target <- c(
    params_fixed_target,
    list(target_value = 80, target_year = 2030, scenario_name = "fixed_target")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(scenario == "business_as_usual")

  df_fixed_target <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params_fixed_target)
  ) %>%
    dplyr::filter(scenario == "fixed_target")

  df_accelerated <- dplyr::bind_rows(df_fixed_target, df_bau) %>%
    scenario_best_of(
      scenario_names = c("business_as_usual", "fixed_target"),
      scenario_name = "acceleration",
      small_is_best = params[["small_is_best"]]
    ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK

#' Accelerate pneumo
#'
#' Accelerate pneumo by taking the best of business as usual and a **fixed target
#' of 90 by 2025**.
#'
#' @inherit accelerate_anc4
#'
accelerate_pneumo <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario = "scenario",
                              ind = "ind",
                              ...) {
  this_ind <- ind_ids["pneumo"]

  params <- list(...)

  params_bau <- get_right_params(params, scenario_bau)
  params_bau["scenario_name"] <- "business_as_usual"

  params_fixed_target <- get_right_params(params, scenario_fixed_target)
  params_fixed_target <- c(
    params_fixed_target,
    list(target_value = 90, target_year = 2025, scenario_name = "fixed_target")
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(scenario == "business_as_usual")

  df_fixed_target <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params_fixed_target)
  ) %>%
    dplyr::filter(scenario == "fixed_target")

  df_accelerated <- dplyr::bind_rows(df_fixed_target, df_bau) %>%
    scenario_best_of(
      scenario_names = c("business_as_usual", "fixed_target"),
      scenario_name = "acceleration",
      small_is_best = params[["small_is_best"]]
    ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK
# @Elliott, Should target value be hard-coded or an argument of this function?
# Yes, it's a scenario requirement.

#' Accelerate tb
#'
#' Accelerate tb by using a **fixed target of 90 by 2025**.
#'
#' @inherit accelerate_anc4
#'
accelerate_tb <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["tb"]

  params <- list(...)
  params["scenario_name"] <- "acceleration"
  params["target_value"] <- 90

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_fixed_target, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: error becuase of missing values

#' Accelerate uhc_sanitation
#'
#' Accelerate uhc_sanitation by encouraging the country to reach the mean (or upper
#' threshold) of the quantile it belongs to in 2017, with n = 5 quantiles. Lower
#' and upper limits of 0 and 99, respectively, are also imposed on the results.
#'
#' @inherit accelerate_anc4
#'
accelerate_uhc_sanitation <- function(df,
                                      ind_ids = billion_ind_codes("uhc"),
                                      scenario = "scenario",
                                      ind = "ind",
                                      ...){
  this_ind <- ind_ids["uhc_sanitation"]

  params <- list(...)
  params <- get_right_params(params, scenario_quantile)
  params <- c(
    params,
    list(
      n = 5, quantile_year = 2017, trim = TRUE, lower_limit = 0,
      upper_limit = 99
    )
  )

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_accelerated <- do.call(
    scenario_quantile, c(list(df = df_this_ind), params)
  ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

# CHECK: warnings due to empty without_data_df and crude/agestd values
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
#' @inherit accelerate_anc4
#'
accelerate_uhc_tobacco <- function(df,
                                   ind_ids = billion_ind_codes("uhc"),
                                   scenario = "scenario",
                                   ind = "ind",
                                   ...) {
  this_ind <- ind_ids["uhc_tobacco"]

  params <- list(...)

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
      value = "crude",
      baseline_year = 2010,
      target_year = 2025,
      start_year = 2018,
      end_year = 2025
    )
  )

  par_wd_pb <- params_with_data_perc_baseline

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  df_without_data <- df_this_ind %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::filter(!any(.data$type == "estimated")) %>%
    dplyr::ungroup()

  df_with_data <- df_this_ind %>%
    dplyr::group_by(.data$iso3) %>%
    dplyr::filter(any(.data$type == "estimated")) %>%
    dplyr::ungroup()

  if (nrow(df_without_data) > 0) {
    df_without_data_accelerated <- do.call(
      scenario_bau, c(list(df = df_without_data), params_without_data_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration")
  } else {
    df_without_data_accelerated <- tibble::tibble()
  }

  if (nrow(df_with_data) > 0) {
    trajectory_df <- load_misc_data(
      file_name = "scenarios/uhc_tobacco/Tobacco_UHC Billion_Trajectory conversion.xlsx",
      sheet = "Tobacco Data",
      range = cellranger::cell_cols(2:7)
    ) %>%
      dplyr::filter(.data$sex == "Total") %>%
      dplyr::select(c("iso3", "measure", "year", "value"))

    tobacco_ratio_df <- trajectory_df %>%
      dplyr::mutate(measure = ifelse(.data$measure == "Crude", "crude", "agestd")) %>%
      tidyr::pivot_wider(names_from = .data$measure, values_from = .data$value) %>%
      dplyr::mutate(ratio_agestd_over_crude = .data$agestd / .data$crude)

    # Extending the input trajectories to 2025, using flat_extrap from 2023 values
    tobacco_ratio_df <- augury::expand_df(
      tobacco_ratio_df,
      iso3 = unique(tobacco_ratio_df$iso3),
      year = 2000:2025,
      response = c("agestd", "crude"),
      keep_before_obs = TRUE,
      keep_no_obs = TRUE
    ) %>%
      augury::predict_simple("flat_extrap", col = "agestd") %>%
      augury::predict_simple("flat_extrap", col = "crude") %>%
      augury::predict_simple("flat_extrap", col = "ratio_agestd_over_crude") %>%
      dplyr::select(!c("pred"))

    tobm <- tobacco_ratio_df %>%
      dplyr::group_by(.data[["year"]]) %>%
      dplyr::summarise(m = mean(.data[["ratio_agestd_over_crude"]]))

    df_with_data <- df_with_data %>%
      dplyr::left_join(tobacco_ratio_df) %>%
      dplyr::left_join(tobm) %>%
      dplyr::mutate(
        ratio_agestd_over_crude = ifelse(is.na(.data$ratio_agestd_over_crude), .data$m, .data$ratio_agestd_over_crude),
        crude = .data$value / .data$ratio_agestd_over_crude
      ) %>%
      dplyr::select(-c("m"))

    df_with_data_bau <- do.call(
      scenario_bau, c(list(df = df_with_data), params_with_data_bau)
    ) %>%
      dplyr::filter(.data[["scenario"]] == "with_data_bau")

    df_with_data_perc_baseline <- df_with_data %>%
      dplyr::group_by(.data$iso3) %>%
      dplyr::mutate(valtemp = .data[[par_wd_pb[["value"]]]]) %>%
      dplyr::mutate(baseline_value = .data$valtemp[.data$year == par_wd_pb[["start_year"]]]) %>%
      dplyr::mutate(old_baseline_value = .data$valtemp[.data$year == par_wd_pb[["baseline_year"]]]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(goal2025 = .data$old_baseline_value * (100 + par_wd_pb[["percent_change"]]) / 100) %>%
      dplyr::mutate(
        goalend = .data$old_baseline_value + (.data$goal2025 - .data$old_baseline_value) *
          (par_wd_pb[["end_year"]] - par_wd_pb[["baseline_year"]]) / (par_wd_pb[["target_year"]] - par_wd_pb[["baseline_year"]])
      ) %>%
      dplyr::mutate(
        "{par_wd_pb[['scenario_name']]}" := ifelse(
          .data$year >= par_wd_pb[["start_year"]] & .data$year <= par_wd_pb[["target_year"]],
          .data$baseline_value + (.data$goalend - .data$baseline_value) * (.data$year - par_wd_pb[["start_year"]]) / (par_wd_pb[["end_year"]] - par_wd_pb[["start_year"]]),
          NA_real_
        )
      ) %>%
      dplyr::select(!c("valtemp", "baseline_value", "goalend", "goal2025", "old_baseline_value")) %>%
      dplyr::filter(!is.na(.data[[par_wd_pb[["scenario_name"]]]]))

    # Replace crude column with {scenario_name} column and set scenario = {scenario_name}
    # Now both df_with_data_bau and df_with_data_perc_baseline have the scenario-projected values in the crude column
    # with the scenario column disambiguating between the two scenarios
    df_with_data_perc_baseline <- df_with_data_perc_baseline %>%
      dplyr::select(-c("crude")) %>%
      dplyr::rename("crude" = .data[[par_wd_pb[["scenario_name"]]]]) %>%
      dplyr::mutate(scenario = par_wd_pb[["scenario_name"]])

    # Elliott: see art for call to scenario_best_of with do.call

    df_with_data_accelerated <- dplyr::bind_rows(df_with_data_bau, df_with_data_perc_baseline) %>%
      scenario_best_of(
        value = "crude",
        scenario_names = c("with_data_bau", "with_data_perc_baseline"),
        scenario_name = "acceleration",
        small_is_best = params[["small_is_best"]]
      ) %>%
      dplyr::filter(.data[["scenario"]] == "acceleration") %>%
      # Converting crude values back to age-standardised
      dplyr::mutate(value = .data$crude * .data$ratio_agestd_over_crude) %>%
      dplyr::select(-c("agestd", "crude", "ratio_agestd_over_crude"))
  } else {
    df_with_data_accelerated <- tibble::tibble()
  }

  df %>%
    dplyr::bind_rows(df_with_data_accelerated, df_without_data_accelerated)
}
