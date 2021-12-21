accelerate_espar <- function(df,
                             iso3 = "iso3",
                             year = "year",
                             value = "value",
                             ind_ids = billion_ind_codes("hep"),
                             ind = "ind",
                             type = "type",
                             scenario = "scenario",
                             start_year = 2018,
                             end_year = 2025,
                             ...) {
  assert_columns(df, iso3, year, value, ind, type, scenario)

  espar_inds <- ind_ids[stringr::str_detect(ind_ids, "^espar")]

  espar_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}$")]

  espar_cat_sub_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}.{0,3}")]

  espar_sub_cat <- ind_ids[stringr::str_detect(ind_ids, "espar[0-9]{2}_[0-9]{2}$")]

  espar_cat_not_in_sub_cat <- espar_cat[!stringr::str_detect(espar_cat, paste0(unique(stringr::str_remove(espar_sub_cat, "_[0-9]{2}")), collapse = "|"))]

  espar_sub_cat <- c(espar_sub_cat, espar_cat_not_in_sub_cat)

  assert_ind_ids_in_df(df, ind_col = ind, ind_ids = espar_sub_cat, by_iso3 = FALSE)

  espar_data <- df %>%
    dplyr::filter(.data[[ind]] %in% espar_inds)

  last_year_reported <- espar_data %>%
    dplyr::filter(.data[[type]] == "reported") %>%
    dplyr::filter(.data[[year]] == max(.data[[year]])) %>%
    dplyr::select(.data[[year]]) %>%
    dplyr::distinct() %>%
    dplyr::pull(.data[[year]])

  baseline_year_espar <- espar_data %>%
    dplyr::filter(
      .data[[ind]] == ind_ids["espar"],
      .data[[year]] %in% start_year:last_year_reported,
      !is.na(.data[[value]])
    ) %>%
    dplyr::group_by(.data[[iso3]]) %>%
    tidyr::pivot_wider(names_from = .data[[year]], names_prefix = "value_", values_from = .data[[value]]) %>%
    dplyr::mutate(baseline = dplyr::case_when(
      !is.na(.data[[glue::glue("{value}_{last_year_reported - 1}")]]) & (is.na(.data[[glue::glue("{value}_{start_year}")]]) | .data[[glue::glue("{value}_{last_year_reported - 1}")]] > .data[[glue::glue("{value}_{start_year}")]]) ~ as.integer(last_year_reported - 1),
      is.na(.data[[glue::glue("{value}_{last_year_reported - 1}")]]) & is.na(.data[[glue::glue("{value}_{start_year}")]]) & !is.na(.data[[glue::glue("{value}_{last_year_reported}")]]) ~ as.integer(last_year_reported),
      TRUE ~ as.integer(start_year)
    )) %>%
    dplyr::select(!!iso3, "baseline")

  baseline_year_complete <- espar_data %>%
    dplyr::filter(.data[[ind]] %in% espar_sub_cat) %>%
    dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
    dplyr::tally() %>%
    dplyr::filter(.data[["n"]] == length(espar_sub_cat)) %>%
    dplyr::summarise(baseline_complete = min(.data[[year]]))

  espar_full <- espar_data %>%
    dplyr::left_join(baseline_year_espar, by = iso3) %>%
    dplyr::left_join(baseline_year_complete, by = iso3) %>%
    dplyr::mutate(region = whoville::iso3_to_regions(.data[[iso3]])) %>%
    dplyr::filter(
      !is.na(.data[["region"]]),
      year >= start_year
    ) %>%
    dplyr::mutate(
      is_cat = dplyr::if_else(.data[[ind]] %in% espar_cat, TRUE, FALSE),
      is_sub_cat = dplyr::if_else(.data[[ind]] %in% espar_sub_cat, TRUE, FALSE)
    )

  espar_regional <- espar_full %>%
    dplyr::filter(
      .data[["is_sub_cat"]],
      .data[[year]] == .data[["baseline_complete"]] & .data[["baseline_complete"]] <= last_year_reported
    ) %>%
    dplyr::group_by(.data[["region"]], .data[[ind]]) %>%
    dplyr::summarise(reg_av_sub = mean(.data[[value]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::select(.data[[ind]], "region", "reg_av_sub")

  espar_year_complete <- espar_full %>%
    dplyr::filter(.data[[year]] == .data[["baseline_complete"]])

  espar_year_complete_sub_cat <- espar_year_complete %>%
    dplyr::filter(.data[["is_sub_cat"]])

  espar_sub_target <- tidyr::expand_grid("{iso3}" := whoville::who_member_states(),
    ind = unique(espar_regional[[ind]])
  ) %>%
    dplyr::mutate(region = whoville::iso3_to_regions(.data[[iso3]])) %>%
    dplyr::left_join(espar_year_complete_sub_cat, by = c(iso3, ind, "region")) %>%
    dplyr::left_join(espar_regional, by = c(ind, "region")) %>%
    dplyr::mutate(target = pmax(.data[["reg_av_sub"]], .data[[value]], na.rm = TRUE)) %>%
    dplyr::select(.data[[iso3]], .data[[ind]], "target")

  espar_cat_target <- espar_sub_target %>%
    dplyr::mutate("{ind}" := stringr::str_replace(.data[[ind]], "_[0-9]{2}$", "")) %>%
    dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
    dplyr::summarise(target = mean(.data[["target"]]), .groups = "drop") %>%
    dplyr::select(.data[[iso3]], .data[[ind]], .data[["target"]])

  espar_target <- espar_cat_target %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::summarise(
      target = mean(.data[["target"]]),
      "{ind}" := "espar",
      .groups = "drop"
    ) %>%
    dplyr::select(.data[[iso3]], .data[[ind]], "target") %>%
    dplyr::full_join(baseline_year_espar, by = iso3) %>%
    dplyr::mutate(baseline = dplyr::case_when(
      is.na(.data[["baseline"]]) ~ as.numeric(start_year),
      TRUE ~ as.numeric(.data[["baseline"]])
    ))

  espar_df <- df %>%
    dplyr::filter(
      .data[[ind]] == "espar",
      .data[[year]] >= start_year
    ) %>%
    dplyr::left_join(espar_target, by = c(iso3, ind))

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
          iso3 = iso3,
          year = year,
          value = value,
          ind_ids = ind_ids,
          ind = ind
        ), params_target_col)
      )
    ) %>%
    dplyr::filter(.data[[scenario]] == "acceleration") %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("baseline", "target"))

  df %>%
    dplyr::bind_rows(df_accelerated)
}

accelerate_detect <- function(df,
                              ind_ids = billion_ind_codes("hep"),
                              ind = "ind",
                              ...) {
  this_ind <- ind_ids["detect"]

  df_this_ind <- df %>%
    dplyr::filter(.data[[ind]] == this_ind)

  params_bau <- get_right_params(list(...), scenario_bau)
  params_bau["scenario_name"] <- "acceleration"

  df_bau <- do.call(
    scenario_bau, c(list(df = df_this_ind), params_bau)
  ) %>%
    dplyr::filter(scenario == "acceleration")

  df %>%
    dplyr::bind_rows(df_bau)
}

accelerate_respond <- function(df,
                               ...) {
  accelerate_detect(df, ind_ids = c("detect" = "respond"), ...)
}


accelerate_notify <- function(df,
                              ...) {
  accelerate_detect(df, ind_ids = c("detect" = "notify"), ...)
}

accelerate_detect_respond <- function(df,
                                      ...) {
  accelerate_detect(df, ind_ids = c("detect" = "detect_respond"), ...)
}

accelerate_cholera_campaign <- function(df,
                                        year = "year",
                                        ind = "ind",
                                        iso3 = "iso3",
                                        value = "value",
                                        end_year = 2025,
                                        target_year = end_year,
                                        start_year = 2018,
                                        ...) {
  raw_global_cholera_roadmap <- load_billion_misc("sceanrios/cholera_campaign/cholera_campaign_roadmap_2030.csv")

  global_cholera_roadmap_target <- raw_global_cholera_roadmap %>%
    dplyr::filter(.data[["year"]] == 2030, .data[["ind"]] == "cholera_campaign_denom") %>%
    dplyr::mutate(yearly_target_cholera_2030 = dplyr::case_when(
      is.na(.data[["value"]]) ~ 0,
      TRUE ~ .data[["value"]] / 12
    )) %>%
    dplyr::select(-.data[["year"]], -.data[["value"]])

  global_cholera_roadmap <- raw_global_cholera_roadmap %>%
    dplyr::filter(.data[["year"]] <= target_year) %>%
    dplyr::left_join(global_cholera_roadmap_target, by = c(iso3, ind)) %>%
    dplyr::mutate("{value}" := dplyr::case_when(
      !is.na(.data[["value"]]) & !is.na(.data[["yearly_target_cholera_2030"]]) & .data[["year"]] >= baseline ~ yearly_target_cholera_2030,
      TRUE ~ .data[["value"]]
    )) %>%
    dplyr::select(-.data[["yearly_target_cholera_2030"]])

  best_historical_perf_campaign <- df %>%
    dplyr::filter(.data[[year]] <= baseline) %>%
    tidyr::pivot_wider(names_from = ind, values_from = value) %>%
    dplyr::mutate(cov = cholera_campaign_num / cholera_campaign_denom) %>%
    dplyr::group_by(iso3) %>%
    dplyr::filter(!is.na(cov)) %>%
    dplyr::summarise(best_perf = max(cov))

  best_in_region <- best_historical_perf_campaign %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]]), region = "who_region") %>%
    dplyr::group_by(who_region) %>%
    dplyr::filter(!is.na(best_perf)) %>%
    dplyr::summarise(best_perf_region = max(best_perf))

  best_perf_binded <- global_cholera_roadmap %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]])) %>%
    dplyr::filter(.data[[ind]] == "cholera_campaign_num", .data[[sname]] > 0) %>%
    dplyr::left_join(best_historical_perf_campaign, by = c(iso3)) %>%
    dplyr::left_join(best_in_region, by = c("who_region")) %>%
    dplyr::mutate(best_perf = dplyr::case_when(
      is.na(best_perf) ~ best_perf_region,
      TRUE ~ best_perf
    )) %>%
    dplyr::select(.data[[iso3]], best_perf) %>%
    dplyr::distinct()

  iso3_no_historical <- dplyr::setdiff(unique(global_cholera_roadmap[[iso3]]), unique(best_perf_binded[[iso3]]))

  cholera_roadmap_num <- global_cholera_roadmap %>%
    dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]])) %>%
    dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
    dplyr::left_join(best_historical_perf_campaign, by = iso3) %>%
    dplyr::left_join(best_in_region, by = "who_region") %>%
    dplyr::mutate(
      num = dplyr::case_when(
        .data[[iso3]] %in% iso3_no_historical ~ NA_real_,
        is.na(best_perf) ~ .data[[sname]] * (best_perf_region),
        TRUE ~ .data[[sname]] * (best_perf)
      ),
      !!sym(ind) := "cholera_campaign_num",
      !!sym(sname) := .data[["num"]]
    ) %>%
    dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])

  last_observed_year <- df %>%
    dplyr::filter(.data[[type_col]] != "projected") %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::summarise(max_year = max(year))

  full_table <- tidyr::expand_grid(
    iso3 = unique(global_cholera_roadmap$iso3),
    ind = unique(global_cholera_roadmap$ind),
    year = baseline:target_year
  )

  planned_historical_num <- df %>%
    dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
    dplyr::left_join(cholera_roadmap_num, by = c(iso3, year, ind)) %>%
    dplyr::left_join(last_observed_year, by = c(iso3)) %>%
    dplyr::filter(.data[[ind]] == "cholera_campaign_num") %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(!!sym(sname) := dplyr::case_when(
      .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
      TRUE ~ .data[[sname]]
    ))

  cholera_roadmap_denom <- global_cholera_roadmap %>%
    dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
    dplyr::mutate(!!sym(sname) := dplyr::case_when(
      .data[[iso3]] %in% iso3_no_historical ~ NA_real_,
      TRUE ~ .data[[sname]]
    ))

  planned_historical_denom <- df %>%
    dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
    dplyr::left_join(cholera_roadmap_denom, by = c(iso3, year, ind)) %>%
    dplyr::left_join(last_observed_year, by = c(iso3)) %>%
    dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
    dplyr::group_by(iso3) %>%
    dplyr::mutate(!!sym(sname) := dplyr::case_when(
      .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
      TRUE ~ .data[[sname]]
    ))

  final_binded <- bind_rows(planned_historical_num, planned_historical_denom) %>%
    dplyr::filter(!.data[[iso3]] %in% iso3_no_historical) %>%
    dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])

  full_table <- final_binded %>% dplyr::select(-.data[[sname]])

  a <- df %>%
    dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
    dplyr::left_join(final_binded, by = c(iso3, year, ind)) %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::mutate(!!sym(sname) := dplyr::case_when(
      is.na(.data[[sname]]) ~ .data[["value"]],
      TRUE ~ .data[[sname]]
    )) %>%
    distinct()
}
