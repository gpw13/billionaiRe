#' Accelerate espar
#'
#' Accelerate espar by aiming at the best value between the regional average
#' (WHO regions) and the value last year of the last year with complete espar
#' data (with categories and sub-categories).
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams recycle_data
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_espar <- function(df,
                             value_col = "value",
                             ind_ids = billion_ind_codes("hep"),
                             scenario_col = "scenario",
                             start_year = 2018,
                             end_year = 2025,
                             default_scenario = "default",
                             ...) {
  assert_columns(df, "iso3", "year", value_col, "ind", "type", scenario_col)

  espar_inds <- ind_ids[stringr::str_detect(ind_ids, "^espar")]

  espar_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}$")]

  espar_cat_sub_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}.{0,3}")]

  espar_sub_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}_[0-9]{2}$")]

  espar_cat_not_in_sub_cat <- espar_cat[!stringr::str_detect(espar_cat, paste0(unique(stringr::str_remove(espar_sub_cat, "_[0-9]{2}")), collapse = "|"))]

  espar_sub_cat <- c(espar_sub_cat, espar_cat_not_in_sub_cat)

  assert_ind_ids_in_df(df, ind_ids = espar_sub_cat, by_iso3 = FALSE)

  espar_data <- df %>%
    dplyr::filter(.data[["ind"]] %in% espar_inds,
                  .data[[scenario_col]] == default_scenario)

  last_year_reported <- espar_data %>%
    dplyr::filter(.data[["type"]] == "reported") %>%
    dplyr::filter(.data[["year"]] == max(.data[["year"]])) %>%
    dplyr::select("year") %>%
    dplyr::distinct() %>%
    dplyr::pull(.data[["year"]])

  baseline_year_espar <- espar_data %>%
    dplyr::select(dplyr::all_of(c("ind", "iso3", "year", value_col))) %>%
    dplyr::filter(
      .data[["ind"]] == ind_ids["espar"],
      .data[["year"]] %in% start_year:last_year_reported,
      !is.na(.data[[value_col]])
    ) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    tidyr::pivot_wider(names_from = .data[["year"]], names_prefix = "value_", values_from = .data[[value_col]]) %>%
    dplyr::mutate(baseline = dplyr::case_when(
      !is.na(.data[[glue::glue("{value_col}_{last_year_reported - 1}")]]) & (is.na(.data[[glue::glue("{value_col}_{start_year}")]]) | .data[[glue::glue("{value_col}_{last_year_reported - 1}")]] > .data[[glue::glue("{value_col}_{start_year}")]]) ~ as.integer(last_year_reported - 1),
      is.na(.data[[glue::glue("{value_col}_{last_year_reported - 1}")]]) & is.na(.data[[glue::glue("{value_col}_{start_year}")]]) & !is.na(.data[[glue::glue("{value_col}_{last_year_reported}")]]) ~ as.integer(last_year_reported),
      TRUE ~ as.integer(start_year)
    )) %>%
    dplyr::select("iso3", "baseline")

  baseline_year_complete <- espar_data %>%
    dplyr::filter(.data[["ind"]] %in% espar_sub_cat) %>%
    dplyr::group_by(.data[["iso3"]], .data[["year"]]) %>%
    dplyr::tally() %>%
    dplyr::filter(.data[["n"]] == length(espar_sub_cat)) %>%
    dplyr::summarise(baseline_complete = min(.data[["year"]]))

  espar_full <- espar_data %>%
    dplyr::left_join(baseline_year_espar, by = "iso3") %>%
    dplyr::left_join(baseline_year_complete, by = "iso3") %>%
    dplyr::mutate(region = whoville::iso3_to_regions(.data[["iso3"]])) %>%
    dplyr::filter(
      !is.na(.data[["region"]]),
      .data[["year"]] >= start_year
    ) %>%
    dplyr::mutate(
      is_cat = dplyr::if_else(.data[["ind"]] %in% espar_cat, TRUE, FALSE),
      is_sub_cat = dplyr::if_else(.data[["ind"]] %in% espar_sub_cat, TRUE, FALSE)
    )

  espar_regional <- espar_full %>%
    dplyr::filter(
      .data[["is_sub_cat"]],
      .data[["year"]] == .data[["baseline_complete"]] & .data[["baseline_complete"]] <= last_year_reported
    ) %>%
    dplyr::group_by(.data[["region"]], .data[["ind"]]) %>%
    dplyr::summarise(reg_av_sub = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::select("ind", "region", "reg_av_sub")

  espar_year_complete <- espar_full %>%
    dplyr::filter(.data[["year"]] == .data[["baseline_complete"]])

  espar_year_complete_sub_cat <- espar_year_complete %>%
    dplyr::filter(.data[["is_sub_cat"]])

  espar_sub_target <- tidyr::expand_grid("iso3" := whoville::who_member_states(),
    "ind" := unique(espar_regional[["ind"]])
  ) %>%
    dplyr::mutate(region = whoville::iso3_to_regions(.data[["iso3"]])) %>%
    dplyr::left_join(espar_year_complete_sub_cat, by = c("iso3", "ind", "region")) %>%
    dplyr::left_join(espar_regional, by = c("ind", "region")) %>%
    dplyr::mutate(target = pmax(.data[["reg_av_sub"]], .data[[value_col]], na.rm = TRUE)) %>%
    dplyr::select("iso3", "ind", "target")

  espar_cat_target <- espar_sub_target %>%
    dplyr::mutate("ind" := stringr::str_replace(.data[["ind"]], "_[0-9]{2}$", "")) %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]]) %>%
    dplyr::summarise(target = mean(.data[["target"]]), .groups = "drop") %>%
    dplyr::select("iso3", "ind", "target")

  espar_target <- espar_cat_target %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::summarise(
      target = mean(.data[["target"]]),
      "ind" := "espar",
      .groups = "drop"
    ) %>%
    dplyr::select("iso3", "ind", "target") %>%
    dplyr::full_join(baseline_year_espar, by = "iso3") %>%
    dplyr::mutate(baseline = dplyr::case_when(
      is.na(.data[["baseline"]]) ~ as.numeric(start_year),
      TRUE ~ as.numeric(.data[["baseline"]])
    ))

  espar_df <- df %>%
    dplyr::filter(
      .data[["ind"]] == "espar",
      .data[["year"]] >= start_year
    ) %>%
    dplyr::left_join(espar_target, by = c("iso3", "ind"))

  params_target_col <- get_right_params(list(...), scenario_fixed_target_col)
  params_target_col["scenario_name"] <- "acceleration"
  params_target_col["target_col"] <- "target"
  params_target_col["baseline_year"] <- NULL

  df_accelerated <- espar_df %>%
    dplyr::group_by(.data[["baseline"]]) %>%
    dplyr::group_modify(
      ~ do.call(
        scenario_fixed_target_col, c(list(
          df = .x,
          baseline_year = .y[[1]][1],
          value_col = value_col,
          ind_ids = ind_ids
        ), params_target_col)
      )
    ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration") %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("baseline", "target"))

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate detect
#'
#' Accelerate detect by taking the business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`

accelerate_detect <- function(df,
                              ind_ids = billion_ind_codes("hep"),
                              scenario_col = "scenario",
                              bau_scenario = "historical",
                              ...) {
  this_ind <- ind_ids["detect"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == bau_scenario)

  params_bau <- get_right_params(list(...), scenario_bau)
  params_bau["scenario_name"] <- "acceleration"
  params_bau["scenario_col"] <- scenario_col
  params_bau["bau_scenario"] <- bau_scenario


  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_bau)
}

#' Accelerate respond
#'
#' Accelerate respond by taking the business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_respond <- function(df,
                               ...) {
  accelerate_detect(df, ind_ids = c("detect" = "respond"), ...)
}

#' Accelerate notify
#'
#' Accelerate notify by taking the business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_notify <- function(df,
                              ...) {
  accelerate_detect(df, ind_ids = c("detect" = "notify"), ...)
}

#' Accelerate detect_respond
#'
#' Accelerate detect_respond by taking the business as usual.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_detect_respond <- function(df,
                                      ...) {
  accelerate_detect(df, ind_ids = c("detect" = "detect_respond"), ...)
}

#' Accelerate cholera_campaign
#'
#' Accelerate cholera_campaign by adding planned cholera campaigns to the provided
#' values in `df`. When a value is reported for a year and country, then this
#' value is kept, even after 2018. Planned values are provided only for the
#' denominator. For some planned campaigns only the denominator is provided. When
#' this is the case, the numerator is calculated by taking the best historical
#' vaccination coverage achieved, or if not available that the best regional
#' historical coverage.
#'
#' Planned campaigns are a mix between planned campaigns and the targets outlined
#' in the \href{https://www.gtfcc.org/about-gtfcc/roadmap-2030/}{roadmap 2030}
#' of the Global Task Force on Cholera Control.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_contributions
#' @inheritParams scenario_percent_baseline
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`

accelerate_cholera_campaign <- function(df,
                                        value_col = "value",
                                        end_year = 2025,
                                        target_year = end_year,
                                        start_year = 2018,
                                        ind_ids = billion_ind_codes("hep"),
                                        scenario_col = "scenario",
                                        default_scenario = "default",
                                        ...) {
  this_ind <- ind_ids["cholera_campaign"]

  purrr::walk(unique(df[["iso3"]]), assert_who_iso3)

  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[["ind"]], this_ind),
                  .data[[scenario_col]] == default_scenario)

  cholera_campaign_num <- ind_ids["cholera_campaign_num"]
  cholera_campaign_denom <- ind_ids["cholera_campaign_denom"]

  raw_global_cholera_roadmap <- load_misc_data("scenarios/cholera_campaign/cholera_campaign_roadmap_2030.csv") %>%
    dplyr::rename(
      "iso3" := "iso3",
      "year" := "year",
      "ind" := "ind",
      "{value_col}" := "value"
    ) %>%
    dplyr::mutate(
      "ind" := dplyr::case_when(
        .data[["ind"]] == "cholera_campaign" ~ this_ind,
        .data[["ind"]] == "cholera_campaign_num" ~ cholera_campaign_num,
        .data[["ind"]] == "cholera_campaign_denom" ~ cholera_campaign_denom,
        TRUE ~ NA_character_
      )
    )

  global_cholera_roadmap_target <- raw_global_cholera_roadmap %>%
    dplyr::filter(.data[["year"]] == 2030, .data[["ind"]] == cholera_campaign_denom) %>%
    dplyr::mutate(yearly_target_cholera_2030 = dplyr::case_when(
      is.na(.data[[value_col]]) ~ 0,
      TRUE ~ .data[[value_col]] / 12
    )) %>%
    dplyr::select(- dplyr::any_of(c(value_col, "year")))

  roadmap_full_years <- tidyr::expand_grid(
    "iso3" := unique(raw_global_cholera_roadmap[["iso3"]]),
    "ind" := unique(raw_global_cholera_roadmap[["ind"]]),
    "year" := min(raw_global_cholera_roadmap[["year"]], na.rm = TRUE):end_year
  )

  global_cholera_roadmap <- raw_global_cholera_roadmap %>%
    dplyr::full_join(roadmap_full_years, by = c("iso3", "year", "ind")) %>%
    dplyr::filter(.data[["year"]] <= end_year) %>%
    dplyr::left_join(global_cholera_roadmap_target, by = c("iso3", "ind")) %>%
    dplyr::mutate("{value_col}" := dplyr::case_when(
      is.na(.data[[value_col]]) & !is.na(.data[["yearly_target_cholera_2030"]]) & .data[["year"]] >= start_year ~ .data[["yearly_target_cholera_2030"]],
      TRUE ~ .data[[value_col]]
    )) %>%
    dplyr::select(-"yearly_target_cholera_2030")

  best_historical_perf_campaign <- df_this_ind %>%
    dplyr::filter(.data[["year"]] <= start_year) %>%
    tidyr::pivot_wider(names_from = "ind", values_from = {{ value_col }}) %>%
    dplyr::mutate(cov = .data[[cholera_campaign_num]] / .data[[cholera_campaign_denom]]) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::filter(!is.na(.data[["cov"]])) %>%
    dplyr::summarise(best_perf = max(.data[["cov"]]))

  best_in_region <- best_historical_perf_campaign %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[["iso3"]]), region = "who_region") %>%
    dplyr::group_by(.data[["who_region"]]) %>%
    dplyr::filter(!is.na(.data[["best_perf"]])) %>%
    dplyr::summarise(best_perf_region = max(.data[["best_perf"]]))

  best_perf_binded <- global_cholera_roadmap %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[["iso3"]])) %>%
    dplyr::filter(.data[["ind"]] == cholera_campaign_num, .data[[value_col]] > 0) %>%
    dplyr::left_join(best_historical_perf_campaign, by = "iso3") %>%
    dplyr::left_join(best_in_region, by = c("who_region")) %>%
    dplyr::mutate(best_perf = dplyr::case_when(
      is.na(.data[["best_perf"]]) ~ .data[["best_perf_region"]],
      TRUE ~ .data[["best_perf"]]
    )) %>%
    dplyr::select("iso3", "best_perf") %>%
    dplyr::distinct()

  iso3_no_historical <- dplyr::setdiff(unique(global_cholera_roadmap[["iso3"]]), unique(best_perf_binded[["iso3"]]))

  cholera_roadmap_num <- global_cholera_roadmap %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[["iso3"]])) %>%
    dplyr::filter(.data[["ind"]] == cholera_campaign_denom) %>%
    dplyr::left_join(best_historical_perf_campaign, by = c("iso3")) %>%
    dplyr::left_join(best_in_region, by = "who_region") %>%
    dplyr::mutate(
      num = dplyr::case_when(
        .data[["iso3"]] %in% iso3_no_historical ~ NA_real_,
        is.na(best_perf) ~ .data[[value_col]] * (best_perf_region),
        TRUE ~ .data[[value_col]] * (best_perf)
      ),
      !!sym("ind") := cholera_campaign_num,
      roadmap_value = .data[["num"]]
    ) %>%
    dplyr::select("iso3", "year", "ind", "roadmap_value")

  last_observed_year <- df_this_ind %>%
    dplyr::filter(.data[["type"]] != "projected") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::summarise(max_year = max(.data[["year"]]))

  full_table <- tidyr::expand_grid(
    "iso3" := unique(global_cholera_roadmap[["iso3"]]),
    "ind" := unique(global_cholera_roadmap[["ind"]]),
    "year" := start_year:end_year
  )

  planned_historical_num <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(cholera_roadmap_num, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = c("iso3")) %>%
    dplyr::filter(.data[["ind"]] == cholera_campaign_num) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate("{value_col}" := dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ as.numeric(.data[[value_col]]),
      TRUE ~ .data[["roadmap_value"]]
    ))

  cholera_roadmap_denom <- global_cholera_roadmap %>%
    dplyr::filter(.data[["ind"]] == cholera_campaign_denom) %>%
    dplyr::mutate(roadmap_value = dplyr::case_when(
      .data[["iso3"]] %in% iso3_no_historical ~ NA_real_,
      TRUE ~ .data[[value_col]]
    )) %>%
    dplyr::select(- dplyr::any_of(value_col))

  planned_historical_denom <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(cholera_roadmap_denom, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = c("iso3")) %>%
    dplyr::filter(.data[["ind"]] == cholera_campaign_denom) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate("{value_col}" := dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ as.numeric(.data[[value_col]]),
      TRUE ~ .data[["roadmap_value"]]
    ))

  final_binded <- dplyr::bind_rows(planned_historical_num, planned_historical_denom) %>%
    dplyr::filter(!.data[["iso3"]] %in% iso3_no_historical) %>%
    dplyr::select(dplyr::any_of(c("iso3", "year", "ind", scenario_value = value_col)))

  full_table <- final_binded %>%
    dplyr::select(-"scenario_value")

  df_accelerated <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(final_binded, by = c("iso3", "year", "ind")) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(
      scenario_value = dplyr::case_when(
        is.na(.data[["scenario_value"]]) ~ .data[[value_col]],
        TRUE ~ .data[["scenario_value"]]
      ),
      "{scenario_col}" := "acceleration"
    ) %>%
    dplyr::distinct()

  params <- get_right_params(list(...), trim_values)
  params[["upper_limit"]] <- Inf
  params[["lower_limit"]] <- 0
  params[["keep_better_values"]] <- TRUE

  df_accelerated <- do.call(
    trim_values, c(list(df = df_accelerated, col = "scenario_value"), params)
  )

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate meningitis_campaign
#'
#' Accelerate meningitis_campaign by adding planned meningitis campaigns to the
#' provided values in `df`. When a value is reported for a year and country, then this
#' value is kept, even after 2018. Some planned values are provided only for the
#' denominator. For some planned campaigns only the denominator is provided. When
#' this is the case, the numerator is calculated by taking the best historical
#' vaccination coverage achieved, or if not available by taking the best historical
#' coverage across all countries.
#'
#' Planned campaigns are the planned campaingns targets provided by WHO technical
#' programs based on member states planifications.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_alcohol
#' @param years_best_performance vector of years with the years in which the
#' best performance should be found.
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`

accelerate_meningitis_campaign <- function(df,
                                           ind_ids = billion_ind_codes("hep"),
                                           scenario_col = "scenario",
                                           value_col = "value",
                                           start_year = 2018,
                                           end_year = 2025,
                                           years_best_performance = 2015:2018,
                                           default_scenario = "default",
                                           ...) {
  this_ind <- as.character(ind_ids["meningitis_campaign"])

  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[["ind"]], this_ind),
                  .data[[scenario_col]] == default_scenario)

  meningitis_campaign_num <- ind_ids["meningitis_campaign_num"]
  meningitis_campaign_denom <- ind_ids["meningitis_campaign_denom"]

  planned_campaign_data <- load_misc_data("scenarios/meningitis_campaign/meningitis_campaign_planned.csv") %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, c(
      "campaign_vaccinated_population" = as.character(meningitis_campaign_num),
      "campaign_coverage" = as.character(this_ind),
      "campaign_targeted_population" = as.character(meningitis_campaign_denom),
      "iso3" = as.character("iso3")
    ))) %>%
    tidyr::pivot_longer(-.data[["iso3"]],
      names_to = c("year", "ind"), values_to = "planned_campaign_values", names_pattern = "([0-9]{4})_(.*)"
    ) %>%
    dplyr::mutate(
      !!sym("planned_campaign_values") := dplyr::case_when(
        .data[["ind"]] == this_ind ~ .data[["planned_campaign_values"]] * 100,
        TRUE ~ .data[["planned_campaign_values"]]
      ),
      "year" := as.integer(.data[["year"]])
    ) %>%
    dplyr::filter(!is.na(.data[["planned_campaign_values"]])) %>%
    dplyr::filter(.data[["planned_campaign_values"]] > 0)

  best_historical_perf <- df_this_ind %>%
    dplyr::filter(
      .data[["year"]] <= start_year,
      !is.na(.data[[value_col]])
    ) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    tidyr::pivot_wider(names_from = "ind", values_from = {{ value_col }}) %>%
    dplyr::mutate(
      cov = .data[[meningitis_campaign_num]] / .data[[meningitis_campaign_num]] * 100,
      best_perf = dplyr::case_when(
        is.na(cov) ~ 0,
        cov > 100 ~ 100,
        TRUE ~ cov
      )
    ) %>%
    dplyr::summarise(best_perf_hist = max(.data[["best_perf"]], na.rm = TRUE)) %>%
    dplyr::ungroup()

  replacement_hist_no_avail <- planned_campaign_data %>%
    dplyr::filter(
      .data[["year"]] <= start_year,
      .data[["planned_campaign_values"]] > 1,
      .data[["ind"]] == this_ind
    ) %>%
    dplyr::summarise(avg_perfs = mean(.data[["planned_campaign_values"]])) %>%
    dplyr::mutate(avg_perfs = dplyr::case_when(
      avg_perfs > 100 ~ 100,
      TRUE ~ avg_perfs
    )) %>%
    dplyr::pull()

  last_observed_year <- df_this_ind %>%
    dplyr::filter(.data[["type"]] != "projected") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::summarise(max_year = max(.data[["year"]]))

  planned_denom <- planned_campaign_data %>%
    dplyr::filter(
      stringr::str_detect(.data[["ind"]], "_denom$"),
      .data[["year"]] > max(years_best_performance)
    ) %>%
    dplyr::distinct()

  planned_num <- planned_denom %>%
    dplyr::left_join(best_historical_perf, by = c("iso3")) %>%
    dplyr::mutate(
      planned_campaign_values = dplyr::case_when(
        is.na(.data[["best_perf_hist"]]) ~ .data[["planned_campaign_values"]] * replacement_hist_no_avail / 100,
        TRUE ~ .data[["planned_campaign_values"]] * .data[["best_perf_hist"]] / 100
      ),
      "ind" := meningitis_campaign_num
    ) %>%
    dplyr::select(-"best_perf_hist")

  those_iso3 <- unique(df_this_ind[["iso3"]])

  full_table <- dplyr::bind_rows(planned_num, df_this_ind) %>%
    dplyr::bind_rows(planned_denom) %>%
    dplyr::filter(.data[["iso3"]] %in% those_iso3) %>%
    dplyr::select(c("iso3", "year", "ind")) %>%
    dplyr::distinct()

  accelerated_num <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::filter(.data[["ind"]] == meningitis_campaign_num) %>%
    dplyr::left_join(planned_num, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = "iso3") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ .data[[value_col]],
      TRUE ~ .data[["planned_campaign_values"]]
    )) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["year"]] < .data[["max_year"]] & is.na(.data[[value_col]]) ~ NA_real_,
      TRUE ~ .data[["planned_campaign_values"]]
    ))

  accelerated_denom <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::filter(.data[["ind"]] == meningitis_campaign_denom) %>%
    dplyr::left_join(planned_denom, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = "iso3") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ .data[[value_col]],
      TRUE ~ .data[["planned_campaign_values"]]
    )) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["year"]] < .data[["max_year"]] & is.na(.data[[value_col]]) ~ NA_real_,
      TRUE ~ .data[["planned_campaign_values"]]
    ))

  final_binded <- dplyr::bind_rows(accelerated_num, accelerated_denom) %>%
    dplyr::select("iso3", "year", "ind", "scenario_value")

  full_table <- final_binded %>% dplyr::select(-"scenario_value")

  df_accelerated <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(final_binded, by = c("iso3", "year", "ind")) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(
      scenario_value = dplyr::case_when(
        is.na(.data[["scenario_value"]]) ~ .data[["value"]],
        TRUE ~ .data[["scenario_value"]]
      ),
      "{scenario_col}" := "acceleration"
    ) %>%
    dplyr::distinct()

  params <- get_right_params(list(...), trim_values)
  params[["upper_limit"]] <- Inf
  params[["lower_limit"]] <- 0
  params[["keep_better_values"]] <- TRUE

  df_accelerated <- do.call(
    trim_values, c(list(df = df_accelerated, col = "scenario_value"), params)
  )

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate measles_routine
#'
#' Accelerate measles_routine by aiming at a +20% percent change between 2013 and 2025 using
#' AROC.
#'
#' Runs:
#'
#'  - `scenario_aroc(df, aroc_type = "percent_change", percent_change = 20, baseline_year = 2013, target_year = 2025, small_is_best = FALSE)`
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`

accelerate_measles_routine <- function(df,
                                       ind_ids = billion_ind_codes("hep"),
                                       scenario_col = "scenario",
                                       default_scenario = "default",
                                       ...) {
  this_ind <- ind_ids["measles_routine"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  assert_ind_start_end_year(df_this_ind, start_year = 2013, end_year = 2018, ind_ids = this_ind)

  params_aroc_percent_change <- get_right_params(list(...), scenario_aroc)
  params_aroc_percent_change["scenario_name"] <- "acceleration"
  params_aroc_percent_change["aroc_type"] <- "percent_change"
  params_aroc_percent_change["percent_change"] <- 20
  params_aroc_percent_change["scenario_name"] <- "acceleration"
  params_aroc_percent_change["baseline_year"] <- 2013

  df_accelerated <- do.call(
    scenario_aroc, c(list(df = df_this_ind), params_aroc_percent_change)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate meningitis_routine
#'
#' Accelerate meningitis_routine by aiming at a 90% 2030, only when value is >= 0
#' (removes cases where value is absent).
#'
#' Runs:
#'
#'  - `scenario_fixed_target_col(df, target_col = "target, target_year = 2025, small_is_best = FALSE, upper_limit = 99)`
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_alcohol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_meningitis_routine <- function(df,
                                          ind_ids = billion_ind_codes("hep"),
                                          value_col = "value",
                                          scenario_col = "scenario",
                                          start_year = 2018,
                                          default_scenario = "default",
                                          ...) {
  this_ind <- ind_ids["meningitis_routine"]

  this_ind_df <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  target_df <- this_ind_df %>%
    dplyr::filter(
      .data[["year"]] == start_year
    ) %>%
    dplyr::mutate(
      target_col = dplyr::case_when(
        .data[[value_col]] >= 0 ~ 90,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(c("iso3", "ind", "target_col"))

  this_ind_df <- this_ind_df %>%
    dplyr::left_join(target_df, by = c("iso3", "ind"))

  params_fixed_target_col <- get_right_params(list(...), scenario_fixed_target_col)
  params_fixed_target_col["scenario_name"] <- "acceleration"
  params_fixed_target_col["target_col"] <- "target_col"
  params_fixed_target_col["target_year"] <- 2030
  params_fixed_target_col["upper_limit"] <- 99

  df_accelerated <- do.call(
    scenario_fixed_target_col, c(list(df = this_ind_df), params_fixed_target_col)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration") %>%
    dplyr::select(-"target_col")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate polio_routine
#'
#' Accelerate polio_routine by aiming at a +20% percent change between 2015 and 2025
#' AROC.
#'
#' Runs:
#'
#'  - `scenario_aroc(df, aroc_type = "percent_change", percent_change = 20, baseline_year = 2015, target_year = 2025, small_is_best = FALSE)`
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_child_viol
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_polio_routine <- function(df,
                                     ind_ids = billion_ind_codes("hep"),
                                     scenario_col = "scenario",
                                     default_scenario = "default",
                                     ...) {
  this_ind <- ind_ids["polio_routine"]

  df_this_ind <- df %>%
    dplyr::filter(.data[["ind"]] == this_ind,
                  .data[[scenario_col]] == default_scenario)

  assert_ind_start_end_year(df_this_ind, start_year = 2015, end_year = 2018, ind_ids = this_ind)

  params_aroc_percent_change <- get_right_params(list(...), scenario_aroc)
  params_aroc_percent_change["scenario_name"] <- "acceleration"
  params_aroc_percent_change["aroc_type"] <- "percent_change"
  params_aroc_percent_change["percent_change"] <- 20
  params_aroc_percent_change["scenario_name"] <- "acceleration"
  params_aroc_percent_change["baseline_year"] <- 2015

  df_accelerated <- do.call(
    scenario_aroc, c(list(df = df_this_ind), params_aroc_percent_change)
  ) %>%
    dplyr::filter(.data[[scenario_col]] == "acceleration")

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate yellow_fever_campaign
#'
#' Accelerate yellow_fever_campaign by adding planned yellow fever campaigns to the
#' provided values in `df`. When a value is reported for a year and country, then this
#' value is kept, even after 2018. Some planned values are provided only for the
#' denominator. For some planned campaigns only the denominator is provided. When
#' this is the case, the numerator is calculated by taking the best historical
#' vaccination coverage achieved, or if not available by taking the best historical
#' coverage across all countries in 2018.
#'
#' Planned campaigns are the planned campaingns targets provided by WHO technical
#' programs based on member states planifications.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_contributions
#' @inheritParams accelerate_meningitis_campaign
#' @inheritParams accelerate_child_viol
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`

accelerate_yellow_fever_campaign <- function(df,
                                             ind_ids = billion_ind_codes("hep"),
                                             scenario_col = "scenario",
                                             value_col = "value",
                                             start_year = 2018,
                                             end_year = 2025,
                                             years_best_performance = 2015:2018,
                                             default_scenario = "default",
                                             ...) {
  this_ind <- as.character(ind_ids["yellow_fever_campaign"])

  df_this_ind <- df %>%
    dplyr::filter(stringr::str_detect(.data[["ind"]], this_ind),
                  .data[[scenario_col]] == default_scenario)

  yellow_fever_campaign_num <- as.character(ind_ids["yellow_fever_campaign_num"])
  yellow_fever_campaign_denom <- as.character(ind_ids["yellow_fever_campaign_denom"])

  planned_campaign_data <- load_misc_data("scenarios/yellow_fever_campaign/yellow_fever_campaign_planned.csv") %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, c(
      "campaign_vaccinated_population" = yellow_fever_campaign_num,
      "campaign_coverage" = this_ind,
      "campaign_targeted_population" = yellow_fever_campaign_denom
    )),
    "iso3" = "iso3"
    ) %>%
    tidyr::pivot_longer(-.data[["iso3"]], names_to = c("year" , "ind"), values_to = "planned_campaign_values", names_pattern = "([0-9]{4})_(.*)") %>%
    dplyr::mutate("year" := as.integer(.data[["year"]])) %>%
    dplyr::filter(!is.na(.data[["planned_campaign_values"]])) %>%
    dplyr::filter(.data[["planned_campaign_values"]] > 0)

  best_perf <- df_this_ind %>%
    dplyr::group_by(.data[["iso3"]], .data[["year"]]) %>%
    tidyr::pivot_wider(names_from = "ind", values_from = {{ value_col }}) %>%
    dplyr::mutate("{this_ind}" := .data[[yellow_fever_campaign_num]] / .data[[yellow_fever_campaign_denom]] * 100) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data[["type"]] != "projected", .data[["year"]] <= start_year) %>%
    dplyr::select("iso3", "year", {{ this_ind }}) %>%
    tidyr::pivot_longer(.data[[this_ind]], names_to = "ind", values_to = {{ value_col }}) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(best_perf = dplyr::case_when(
      is.na(.data[[value_col]]) ~ 0,
      .data[[value_col]] > 100 ~ 100,
      TRUE ~ .data[[value_col]]
    )) %>%
    dplyr::summarise(best_perf_hist = max(.data[["best_perf"]], na.rm = TRUE)) %>%
    dplyr::ungroup()

  replacement_hist_no_avail <- planned_campaign_data %>%
    dplyr::filter(
      .data[["year"]] == start_year,
      .data[["ind"]] == this_ind
    ) %>%
    dplyr::summarise(avg_2018_perfs = mean(.data[["planned_campaign_values"]])) %>%
    dplyr::mutate(avg_2018_perfs = dplyr::case_when(
      avg_2018_perfs > 100 ~ 100,
      TRUE ~ avg_2018_perfs
    )) %>%
    dplyr::pull()

  planned_historical_num_denom <- planned_campaign_data %>%
    dplyr::filter(
      .data[["ind"]] == yellow_fever_campaign_denom,
      .data[["year"]] >= start_year,
      !is.na(.data[["planned_campaign_values"]])
    ) %>%
    dplyr::left_join(best_perf, by = "iso3") %>%
    dplyr::rename("{yellow_fever_campaign_denom}" := .data[["planned_campaign_values"]]) %>%
    dplyr::mutate(
      yellow_fever_campaign_num = dplyr::case_when(
        is.na(.data[["best_perf_hist"]]) ~ .data[[yellow_fever_campaign_denom]] * (replacement_hist_no_avail),
        TRUE ~ .data[[yellow_fever_campaign_denom]] * (.data[["best_perf_hist"]] / 100)
      ),
      "{this_ind}" := .data[[yellow_fever_campaign_num]] / .data[[yellow_fever_campaign_denom]]
    ) %>%
    dplyr::select(-.data[["ind"]], -.data[["best_perf_hist"]]) %>%
    dplyr::group_by(.data[["iso3"]], .data[["year"]]) %>%
    tidyr::pivot_longer(dplyr::starts_with("yellow_fever"), names_to = "ind", values_to = "scenario_value") %>%
    dplyr::distinct()

  last_observed_year <- df_this_ind %>%
    dplyr::filter(.data[["type"]] != "projected") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::summarise(max_year = max(.data[["year"]]))

  those_iso3 <- unique(df_this_ind[["iso3"]])

  full_table <- dplyr::bind_rows(planned_historical_num_denom, df_this_ind) %>%
    dplyr::filter(.data[["iso3"]] %in% those_iso3) %>%
    dplyr::select(c("iso3", "year", "ind")) %>%
    dplyr::distinct()

  accelerated_num <- df %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(planned_historical_num_denom, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = c("iso3")) %>%
    dplyr::filter(.data[["ind"]] == "yellow_fever_campaign_num") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ .data[[value_col]],
      TRUE ~ .data[["scenario_value"]]
    )) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["year"]] < max_year & is.na(.data[[value_col]]) ~ NA_real_,
      TRUE ~ .data[["scenario_value"]]
    ))

  accelerated_denom <- df %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(planned_historical_num_denom, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(last_observed_year, by = c("iso3")) %>%
    dplyr::filter(.data[["ind"]] == "yellow_fever_campaign_denom") %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["type"]] != "projected" & !is.na(.data[[value_col]]) ~ .data[[value_col]],
      TRUE ~ .data[["scenario_value"]]
    )) %>%
    dplyr::mutate(scenario_value = dplyr::case_when(
      .data[["year"]] < max_year & is.na(.data[[value_col]]) ~ NA_real_,
      TRUE ~ .data[["scenario_value"]]
    ))

  final_binded <- dplyr::bind_rows(accelerated_num, accelerated_denom) %>%
    dplyr::select(.data[["iso3"]], .data[["year"]], .data[["ind"]], .data[["scenario_value"]])

  full_table <- final_binded %>% dplyr::select(-.data[["scenario_value"]])

  df_accelerated <- df_this_ind %>%
    dplyr::full_join(full_table, by = c("iso3", "year", "ind")) %>%
    dplyr::left_join(final_binded, by = c("iso3", "year", "ind")) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(
      scenario_value = dplyr::case_when(
        is.na(.data[["scenario_value"]]) ~ .data[["value"]],
        TRUE ~ .data[["scenario_value"]]
      ),
      "{scenario_col}" := "acceleration"
    ) %>%
    dplyr::distinct()

  params <- get_right_params(list(...), trim_values)
  params[["upper_limit"]] <- Inf
  params[["lower_limit"]] <- 0
  params[["keep_better_values"]] <- TRUE

  df_accelerated <- do.call(
    trim_values, c(list(df = df_accelerated, col = "scenario_value"), params)
  )

  df %>%
    dplyr::bind_rows(df_accelerated)
}

#' Accelerate yellow_fever_routine
#'
#' Accelerate yellow_fever_routine by aiming at a +20% percent change between 2015 and 2025
#' AROC.
#'
#' Runs:
#'
#'  - `scenario_aroc(df, aroc_type = "percent_change", percent_change = 20, baseline_year = 2015, target_year = 2025, small_is_best = FALSE)`
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario_col` is
#' set to `acceleration`
accelerate_yellow_fever_routine <- function(df,
                                            ...) {
  df %>%
    accelerate_polio_routine(
      ind_ids = c("polio_routine" = "yellow_fever_routine"),
      ...
    )
}
