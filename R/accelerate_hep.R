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
