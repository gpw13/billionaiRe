#' @export
load_hep_gho_data <- function() {
  gho_ids <- get_gho_codes("hep")
  queries <- get_gho_queries(gho_ids)
  ghost::gho_data(gho_ids, queries) %>%
    dplyr::select("iso3" = "SpatialDim",
                  "year" = "TimeDim",
                  "value" = "NumericValue",
                  "low" = "Low",
                  "high" = "High",
                  "ind" = "IndicatorCode",
                  "source" = "DataSourceDim") %>%
    dplyr::mutate(!!sym("ind") := gho_codes_to_db_id(.data[["ind"]]),
                  use_dash = TRUE,
                  use_cal = TRUE,
                  use_source = FALSE) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]]))
}

#' @export
load_hep_xmart_data <- function() {
  xmart_ids <- get_xmart_codes("hep")
  xmart4::xmart4_table("GPW13", "RAW_INDICATOR_DATA") %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(.data[["ind"]] %in% xmart_ids) %>%
    dplyr::select(-c("comments")) %>%
    readr::type_convert()
}

#' @export
load_hep_data <- function() {
  gho_df <- load_hep_gho_data()
  xmart_df <- load_hep_xmart_data()
  dplyr::bind_rows(gho_df, xmart_df) %>%
    dplyr::arrange("ind", "iso3", "year") %>%
    dplyr::select(-c("low", "high"))
}

#' @export
wrangle_hep_data <- function(df) {
  orig_df <- dplyr::select(df, -dplyr::any_of("value"))
  trans_df <- df %>%
    tidyr::pivot_wider(id_cols = c("iso3", "year"),
                       names_from = "ind",
                       values_from = "value") %>%
    dplyr::mutate(dplyr::across(c("detect", "notify", "respond"),
                                ~dplyr::case_when(
                                  .x <= 1 ~ 100,
                                  .x <= 7 ~ 75,
                                  .x <= 14 ~ 50,
                                  !is.na(.x) ~ 25
                                )),
                  dplyr::across(c("prevent", "measles", "cholera", "meningitis", "yellow_fever", "polio"),
                                ~100 * .x)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ihr = mean(dplyr::c_across(c("ihr1":"ihr13"))),
                  detect_respond = mean(dplyr::c_across(c("detect", "notify", "respond")),
                                        na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::arrange(.data[["year"]], .by_group = TRUE) %>%
    dplyr::mutate(dr_lag1 = lag(detect_respond, 1),
                  dr_lag2 = lag(detect_respond, 2),
                  dr_lag3 = lag(detect_respond, 3),
                  dr_lag4 = lag(detect_respond, 4)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(detect_respond = mean(dplyr::c_across(c(detect_respond, dplyr::starts_with("dr_lag"))),
                                        na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(-c("iso3", "year"),
                        names_to = "ind")
  dplyr::left_join(orig_df, trans_df, by = c("iso3", "year", "ind")) %>%
    dplyr::bind_rows(dplyr::filter(trans_df, .data[["ind"]] %in% c("ihr", "detect_respond"))) %>%
    dplyr::filter(!is.na(.data[["value"]])) %>%
    dplyr::mutate(dplyr::across(c("use_dash", "use_cal"),
                                ~tidyr::replace_na(.x, TRUE)),
                  type = "Actual")
}

#' @export
project_hep_data <- function(df) {
  orig_df <- dplyr::select(df, -"value")
  df %>%
    dplyr::filter(.data[["ind"]] %in% c("detect_respond", "ihr", "prevent")) %>%
    tidyr::pivot_wider(id_cols = c("iso3", "year"),
                       names_from = "ind",
                       values_from = "value") %>%
    dplyr::full_join(tidyr::expand_grid(iso3 = whoville::who_member_states(),
                                         year = 2015:2023),
                      by = c("iso3", "year")) %>%
    dplyr::arrange(.data[["iso3"]], .data[["year"]]) %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("ihr"),
                                ~ifelse(all(is.na(.x)), NA, zoo::na.approx(.x, rule = 2, na.rm = T))),
                  !!sym("detect_respond") := zoo::na.approx(.data[["detect_respond"]], rule = 2, na.rm = FALSE),
                  !!sym("prevent") := project_prevent(.data[["prevent"]], .data[["year"]])) #%>%
    # dplyr::ungroup() %>%
    # tidyr::pivot_longer(-c("iso3", "year"),
    #                     names_to = "ind") %>%
    # dplyr::full_join(orig_df, by = c("iso3", "year", "ind")) %>%
    # dplyr::mutate("type" = tidyr::replace_na(.data[["type"]], "Projection")) %>%
    # dplyr::group_by(.data[["ind"]]) %>%
    # dplyr::filter(dplyr::row_number() >= min(which(.data[["type"]] == "Actual"))) %>%
    # dplyr::mutate(!!sym("use_dash") := tidyr::replace_na(.data[["use_dash"]], TRUE),
    #               !!sym("use_cal") := tidyr::replace_na(.data[["use_cal"]], TRUE),
    #               !!sym("use_source") := tidyr::replace_na(.data[["use_source"]], FALSE)) %>%
    # dplyr::ungroup()
}

#' @export
create_hep_data <- function() {
  df <- load_hep_data() %>%
    wrangle_hep_data() %>%
    project_hep_data()

  df %>%
    transmute(Billions = "HE",
              GeoArea_FK = whoville::iso3_to_names(.data[["iso3"]]),
              Indicator_FK = analysis_id_to_db_id(.data[["ind"]]),
              Year_FK = year,
              Billion_Group = dplyr::case_when(
                stringr::str_starts(.data[["ind"]], "ihr") ~ "Prepare",
                .data[["ind"]] %in% c("prevent", "measles", "cholera", "meningitis", "yellow_fever", "polio") ~ "Prevent",
                .data[["ind"]] %in% c("detect", "notify", "respond", "detect_respond") ~ "Detect and Respond"
              ),
              Normalized_Value = .data[["value"]],
              Raw_Value = .data[["value"]],
              Type = .data[["type"]],
              !!sym("use_dash"),
              !!sym("use_cal"),
              Source = ifelse(!is.na(.data[["source"]]), .data[["source"]], inds_to_source(.data[["ind"]])))
}

#' @export
calc_hep_billions <- function(df) {
  orig_df <- df %>%
    dplyr::mutate(ind = db_id_to_analysis_id(.data[["Indicator_FK"]])) %>%
    dplyr::filter(.data[["ind"]] %in% c("ihr", "prevent", "detect_respond"),
                  .data[["Year_FK"]] %in% c(2018, 2019, 2023)) %>%
    dplyr::mutate(iso3 = whoville::names_to_iso3(.data[["GeoArea_FK"]]))

  chg_df <- orig_df %>%
    dplyr::group_by(.data[["iso3"]]) %>%
    dplyr::arrange(.data[["Year_FK"]], .by_group = TRUE) %>%
    tidyr::pivot_wider(c("iso3", "ind"),
                       names_from = "Year_FK",
                       values_from = "Normalized_Value") %>%
    dplyr::mutate(change_2019 = .data[["2019"]] - .data[["2018"]],
                     change_2023 = .data[["2023"]] - .data[["2018"]],
                     dr_level_2019 = case_when(
                       .data[["2019"]] >= 90 ~ 5,
                       .data[["2019"]] >= 70 ~ 4,
                       .data[["2019"]] >= 50 ~ 3,
                       .data[["2019"]] >= 30 ~ 2,
                       !is.na(.data[["2019"]]) ~ 0
                     ),
                     dr_level_2023 = case_when(
                       .data[["2023"]] >= 90 ~ 5,
                       .data[["2023"]] >= 70 ~ 4,
                       .data[["2023"]] >= 50 ~ 3,
                       .data[["2023"]] >= 30 ~ 2,
                       !is.na(.data[["2023"]]) ~ 0
                     )) %>%
    dplyr::summarize(change_2019 = sum(ifelse(.data[["ind"]] %in% c("prevent", "ihr"),
                                              .data[["change_2019"]],
                                              .data[["dr_level_2019"]]),
                                       na.rm = TRUE),
                     change_2023 = sum(ifelse(.data[["ind"]] %in% c("prevent", "ihr"),
                                              .data[["change_2023"]],
                                              .data[["dr_level_2023"]]),
                                       na.rm = TRUE))

  orig_df <- orig_df %>%
    dplyr::group_by(.data[["iso3"]], .data[["Year_FK"]]) %>%
    dplyr::summarize(hep_idx = mean(.data[["Normalized_Value"]], na.rm = TRUE),
                     !!sym("Type") := ifelse(all(.data[["Type"]] == "Actual"), "Actual", "Projection"))

  orig_df %>%
    tidyr::pivot_wider("iso3",
                       names_from = "Year_FK",
                       values_from = "hep_idx",
                       names_prefix = "hep_") %>%
    dplyr::left_join(chg_df, by = "iso3") %>%
    dplyr::transmute(Population = wppdistro::get_population(.data[["iso3"]], year = 2023),
                     Target_Perc = 12.5,
                     Guideline = Population * Target_Perc / 100,
                     Baseline = .data[["hep_2018"]],
                     Baseline_Year = 2018,
                     Contribution_2019 = .data[["change_2019"]] * Population / 100,
                     Contribution_2023 = .data[["change_2023"]] * Population / 100,
                     !!sym("hep_2019"),
                     !!sym("hep_2023")) %>%
    tidyr::pivot_longer(c("Contribution_2019":"hep_2023"),
                        names_sep = "_",
                        names_to = c("var", "Year_FK")) %>%
    dplyr::mutate(!!sym("Year_FK") := as.numeric(.data[["Year_FK"]])) %>%
    tidyr::pivot_wider(names_from = "var",
                       values_from = "value") %>%
    dplyr::left_join(orig_df, by = c("iso3", "Year_FK")) %>%
    dplyr::transmute(GeoArea_FK = whoville::iso3_to_names(.data[["iso3"]]),
                     Start_Year = NA,
                     End_Year = NA,
                     !!sym("Year_FK"),
                     Billion = "HE",
                     Billion_Indicators = "HEPI",
                     !!sym("Target_Perc"),
                     !!sym("Guideline"),
                     !!sym("Contribution"),
                     Perc_guideline = 100 * .data[["Contribution"]] / .data[["Guideline"]],
                     Change_Proj_Baseline = .data[["hep"]] - .data[["Baseline"]],
                     !!sym("Baseline_Year"),
                     !!sym("Baseline"),
                     Value_Type = .data[["Type"]],
                     Value = .data[["hep"]],
                     HE_Level = dplyr::case_when(
                       .data[["Value"]] >= 90 ~ 5,
                       .data[["Value"]] >= 70 ~ 4,
                       .data[["Value"]] >= 50 ~ 3,
                       .data[["Value"]] >= 30 ~ 2,
                       !is.na(.data[["Value"]]) ~ 1
                     )) %>%
    dplyr::ungroup()
}

#' @noRd
timeliness_levels <- function(x) {
  dplyr::case_when(
    x <= 1 ~ 5,
    x <= 7 ~ 4,
    x <= 14 ~ 3,
    x > 14 ~ 2,
    is.na(x) ~ 1
  )
}

#' @noRd
project_prevent <- function(x, year) {
  change <- (x[year == 2018] - x[year == 2015]) / 3
  proj_x <- c(rep(NA, 4), x[year == 2018] + change * (1:5))
  proj_x <- pmin(proj_x, 100, na.rm = TRUE)
  ifelse(!is.na(x), x, proj_x)
}
