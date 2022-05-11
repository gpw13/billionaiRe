

#' Impute a COVID-19 shock
#'
#' Impute a COVID-19 shock for each `iso3` and `ind` combination by the imputation flow chart:
#'  - if there is a 2020 value, then 2021 = 2020
#'  - If no 2020 values, but more than 30% of countries in `region` have one:
#'    * Use mean regional chock (difference between 2020 and 2019) to infer a 2020 value
#'
#' @param region (character) Type of region to use for regional average. See \link[whoville]{iso3_to_regions} for more details.
#' @inheritParams recycle_data
#' @inheritParams recycle_data_scenario_single
#' @inheritParams scenario_covid_rapid_return
#' @inheritParams calculate_uhc_billion
#' @inheritParams calculate_hpop_billion
impute_covid_shock <- function(df,
                              region = c("who_region", "un_region", "un_subregion", "un_intermediate_region",
                                         "sdg_region", "sdg_subregion", "gbd_region", "gbd_subregion", "un_desa_region",
                                         "un_desa_subregion", "wb_region", "wb_ig"),
                              start_year = 2018,
                              covid_year = 2020,
                              recovery_year = 2022,
                              end_year =  2025,
                              value_col =  "value",
                              scenario_col = "scenario",
                              default_scenario = "default",
                              scenario_reported_estimated = "routine",
                              scenario_covid_shock = "covid_shock",
                              scenario_reference_infilling = "reference_infilling",
                              source = sprintf("WHO DDI interim infilling and projections, %s", format(Sys.Date(), "%B %Y"))){

  assert_columns(df,value_col, scenario_col, "iso3", "year", "ind", "type")

  region <- rlang::arg_match(region)

  full_years_df <- tidyr::expand_grid(
    "year" := (covid_year-1):(recovery_year-1),
    "iso3" := unique(df[["iso3"]]),
    "ind" := unique(df[["ind"]])
  ) %>%
    dplyr::mutate("{region}" := whoville::iso3_to_regions(.data[["iso3"]], region = !!region))

  df <- billionaiRe_add_columns(df, c("iso3", "ind", "year", "source", scenario_col, value_col, "type"), fill = NA)

  df <- df %>%
    dplyr::mutate("{region}" := whoville::iso3_to_regions(.data[["iso3"]], region = !!region)) %>%
    dplyr::filter(.data[[scenario_col]] %in% c(!!scenario_reported_estimated, !!scenario_covid_shock,!!scenario_reference_infilling ))

  df_check_one_value <- df %>%
    dplyr::filter(.data[["year"]] %in% (covid_year-1):(recovery_year-1)) %>%
    dplyr::group_by(.data[["iso3"]], .data[["ind"]], .data[["year"]]) %>%
    dplyr::tally() %>%
    dplyr::filter(.data[["n"]] > 1)

  if(nrow(df_check_one_value) > 1){
    stop(sprintf("More than one value per `iso3`, `year` and `indicator` between %s and %s.\nPlease check input data to avoid having multiple values, for instance with multiple scenarios.",
                 covid_year-1, recovery_year-1))
  }

  all_iso3_region <- whoville::countries %>%
    dplyr::select(dplyr::all_of(c("iso3", region))) %>%
    dplyr::filter(!is.na(.data[[region]]))

  df_regional_estimation <- df %>%
    dplyr::filter(.data[["year"]] == covid_year) %>%
    dplyr::full_join(all_iso3_region, by = c("iso3", region)) %>%
    dplyr::group_by(.data[[region]]) %>%
    dplyr::summarise(perc_has_data = sum(!is.na(.data[[value_col]]))/dplyr::n()) %>%
    dplyr::filter(.data[["perc_has_data"]] > 0.3)

  if(nrow(df_regional_estimation)>0){
    df_regional_estimation <- df %>%
      dplyr::left_join(df_regional_estimation, by = c(region)) %>%
      dplyr::filter(!is.na(.data[["perc_has_data"]]),
                    .data[["year"]] %in% c((covid_year-1):covid_year)) %>%
      dplyr::group_by(.data[["iso3"]], .data[["ind"]], .data[[region]]) %>%
      dplyr::summarise(covid_shock = diff(.data[[value_col]])) %>%
      dplyr::group_by(.data[[region]], .data[["ind"]]) %>%
      dplyr::summarise(mean_shock = as.numeric(mean(.data[["covid_shock"]])))
  }else{
    df_regional_estimation <- tibble::tibble(
      "ind" := NA_character_,
      "{region}" := NA_character_,
      "mean_shock" := NA_real_
    )
  }

  df_imputed <- df %>%
    dplyr::full_join(full_years_df, by = c("year", "iso3", "ind", region)) %>%
    dplyr::left_join(df_regional_estimation, by = c(region, "ind")) %>%
    dplyr::group_by(dplyr::across(c("iso3", "ind"))) %>%
    dplyr::mutate(
      value_pre_covid = dplyr::case_when(
        is.na(.data[[value_col]][.data[["year"]] == covid_year - 1]) ~ NA_real_,
        TRUE ~ as.numeric(.data[[value_col]][.data[["year"]] == covid_year - 1])
      ),
      value_covid_year = .data[[value_col]][.data[["year"]] == covid_year],
      value_covid_year_plus1 = .data[[value_col]][.data[["year"]] == covid_year + 1]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "{value_col}" := dplyr::case_when(
        .data[["year"]] >= covid_year & !is.na(.data[["value_covid_year"]]) & is.na(.data[[value_col]]) ~ as.numeric(.data[["value_covid_year"]]),
        .data[["year"]] >= covid_year & is.na(.data[["value_covid_year"]]) & is.na(.data[["value_covid_year_plus1"]]) ~ as.numeric(.data[["value_pre_covid"]] + .data[["mean_shock"]]),
        TRUE ~ as.numeric(.data[[value_col]])
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(c("iso3", "ind"))) %>%
    dplyr::mutate(
      "type" := dplyr::case_when(
        is.na(.data[["type"]]) & .data[["year"]] >= covid_year ~ "projected",
        TRUE ~ .data[["type"]]
      ),
      "{scenario_col}" := dplyr::case_when(
        is.na(.data[[scenario_col]]) & .data[["year"]] >= covid_year ~ scenario_covid_shock,
        TRUE ~ .data[[scenario_col]]
      ),
      "source" := dplyr::case_when(
        is.na(.data[["source"]]) ~ source,
        TRUE ~ .data[["source"]]
      )
    ) %>%
    dplyr::select(-c(!!region, "mean_shock", "value_pre_covid", "value_covid_year", "value_covid_year_plus1"))

}
