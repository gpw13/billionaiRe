load_asc_gho_data <- function(gho_ids, queries) {
  ghost::gho_data(gho_ids, queries) %>%
    dplyr::mutate(!!sym("TimeDim") := dplyr::case_when(
      .data[["IndicatorCode"]] != gho_ids[names(gho_ids) == "anc4"] ~ .data[["TimeDim"]], # remove once program supplied dates in GHO
      TRUE ~ as.integer(ceiling((.data[["TimeDimensionBegin"]] + .data[["TimeDimensionEnd"]]) / 2))
    )) %>%
    dplyr::select("iso3" = "SpatialDim",
                  "year" = "TimeDim",
                  "value" = "NumericValue",
                  "ind" = "IndicatorCode",
                  "Dim1",
                  "Dim2",
                  "Dim3",
                  "source" = "Comments") %>% # remove once data sources in correct column
    dplyr::group_by(dplyr::across(c("ind", "Dim1", "Dim2", "Dim3", "year", "iso3"))) %>%
    dplyr::summarize(!!sym("value") := mean(.data[["value"]]), # take mean for duplicate values
                     !!sym("source") := paste(unique(.data[["source"]]), collapse = ", "), # keep unique sources for both values
                     .groups = "drop") %>%
    dplyr::mutate(!!sym("source") := replace(.data[["source"]],
                                             !(.data[["ind"]] %in% gho_ids[names(gho_ids) %in% c("anc4", "beds", "pneumo")]), # remove once source available in GHO
                                             NA_character_)) %>%
    dplyr::mutate(!!sym("ind") := names(gho_ids)[match(.data[["ind"]], gho_ids)]) %>%
    dplyr::filter(whoville::is_who_member(.data[["iso3"]]))
}

wrangle_asc_data <- function(df) {
  df %>%
    dplyr::mutate(!!sym("Dim1") := gsub('_', '-', .data[["Dim1"]])) %>% # prevent issues when dropping last _
    tidyr::pivot_wider(id_cols = c("iso3", "year"),
                       names_from = c("ind", "Dim1"),
                       values_from = "value") %>%
    dplyr::mutate(male = wppdistro::get_population(.data[["iso3"]], .data[["year"]], sex = "male"),
                  female = wppdistro::get_population(.data[["iso3"]], .data[["year"]], sex = "female"),
                  !!sym("bp_BTSX") := transform_totl_pop(.data[["bp_MLE"]],
                                                         .data[["bp_FMLE"]],
                                                         male,
                                                         female),
                  !!sym("fpg_BTSX") := transform_totl_pop(.data[["fpg_MLE"]],
                                                          .data[["fpg_FMLE"]],
                                                          male,
                                                          female),
                  !!sym("ihr_NA") := ifelse(is.na(.data[['ihr_NA']]),
                                            .data[['ihr2018_NA']],
                                            .data[['ihr_NA']]),
                  !!sym("tb_NA") := replace(.data[["tb_NA"]],
                                            .data[["tb_NA"]] > 100 | .data[["tb_NA"]] == 0,
                                            NA)) %>%
    dplyr::select(-dplyr::any_of(c('male', 'female', 'ihr2018_NA')),
                  -dplyr::matches("*_MLE|*_FMLE")) %>%
    dplyr::rename_with(~sub("_[^_]+$", "", .x)) %>% # remove text after last underscore
    dplyr::mutate(dplyr::across(-c("iso3", "year"),
                                ~ifelse(!is.na(.x), "WHO GHO", NA),
                                .names = "{col}_type")) %>%
    dplyr::rename_with(~paste0(.x, "_value"),
                       .cols = !(dplyr::ends_with("_type") | dplyr::one_of(c("iso3", "year"))))
}

#' @export
asc_normal_impute <- function(df,
                              var,
                              type,
                              iso3 = "iso3",
                              year = "year") {
  df <- dplyr::mutate(df, temp_reg = whoville::iso3_to_regions(.data[[iso3]]))
  df <- linear_interp_df(df, var, type, iso3, year)
  df <- flat_extrap_df(df, var, type, iso3, year)
  df <- min_impute_df(df, var, type, iso3, year, txt = "flat trend line")
  df <- med_impute_df(df, var, type, c("temp_reg", year), year, txt = "regional median", filter_col = iso3, filter_fn = whoville::is_large_member_state)
  df %>%
    dplyr::select(dplyr::all_of(c(var, type, iso3, year)))
}

#' @export
pneumo_impute_df <- function(df,
                             pneumo,
                             ari,
                             type,
                             iso3 = "iso3",
                             year = "year",
                             year_range = 2000:2017,
                             txt = "regression") {
  df <- df %>%
    dplyr::mutate(temp_fit = pneumo_model(.data[[pneumo]], .data[[ari]], .data[[iso3]], .data[[year]], year_range))
  df <- linear_interp_df(df, pneumo, type, iso3, year)
  df <- flat_extrap_df(df, pneumo, type, iso3, year)
  df <- min_impute_df(df, pneumo, type, iso3, year, txt = "flat trend line")
  df <- df %>%
    dplyr::mutate(!!sym(pneumo) := ifelse(is.na(.data[[pneumo]]),
                                          101 * exp(temp_fit) / (1 + exp(temp_fit)),
                                          .data[[pneumo]]),
                  !!sym(pneumo) := replace(.data[[pneumo]],
                                           .data[[pneumo]] > 100 & !is.na(.data[[pneumo]]),
                                           100),
                  !!sym(type) := replace(.data[[type]],
                                         is.na(.data[[type]]) & !is.na(.data[[pneumo]]),
                                         txt),
                  !!sym(type) := replace(.data[[type]],
                                           .data[[year]] > 2017 & .data[[type]] == txt,
                                           NA))
  df <- flat_extrap_df(df, pneumo, type, iso3, year)
  df %>%
    dplyr::select(dplyr::all_of(c(iso3, year, pneumo, type)))
}

#' @export
asc_impute <- function(df, asc_vars, pneumo, ari, iso3 = "iso3", year = "year") {
  norm_vals <- paste0(asc_vars, "_value")
  norm_types <- paste0(asc_vars, "_type")
  pneumo_val <- paste0(pneumo, "_value")
  pneumo_type <- paste0(pneumo, "_type")
  ari <- paste0(ari, "_value")
  df <- df %>%
    dplyr::left_join(tidyr::expand(., !!sym(iso3), !!sym(year) := 1975:2019), ., by = c(iso3, year))
  n_df <- purrr::map2(norm_vals,
                      norm_types,
                      ~asc_normal_impute(df, .x, .y),
                      iso3,
                      year) %>%
    purrr::reduce(~dplyr::left_join(.x, .y, by = c(iso3, year)))
  p_df <- pneumo_impute_df(df, pneumo_val, ari, pneumo_type)
  dplyr::left_join(n_df, p_df, by = c(iso3, year))
}

#' @export
asc_transform <- function(df) {
  df %>%
    dplyr::mutate(hwf_value = .data[["phys_value"]] + .data[["nurse_value"]],
                  hwf_type = ifelse(.data[["phys_type"]] == "WHO GHO" & .data[["nurse_type"]] == "WHO GHO",
                                    "WHO GHO",
                                    "imputed")) %>%
    dplyr::select(-dplyr::starts_with(c("phys_", "nurse_"))) %>%
    dplyr::mutate(!!sym("bp_value") := reverse_ind(.data[["bp_value"]]),
                  bp_resc_value = transform_bp(.data[["bp_value"]]),
                  bp_resc_type = .data[["bp_type"]],
                  fpg_resc_value = transform_glucose(.data[["fpg_value"]]),
                  fpg_resc_type = .data[["fpg_type"]],
                  !!sym("tobacco_value") := reverse_ind(.data[["tobacco_value"]]),
                  tobacco_resc_value = transform_tobacco(.data[["tobacco_value"]]),
                  tobacco_resc_type = .data[["tobacco_type"]],
                  beds_resc_value = transform_hosp_beds(.data[["beds_value"]]),
                  beds_resc_type = .data[["beds_type"]],
                  hwf_resc_value = transform_hwf(.data[["hwf_value"]]),
                  hwf_resc_type = .data[["hwf_type"]])
}

#' @export
asc_prepare <- function(df) {
  df <- df %>%
    dplyr::filter(.data[["year"]] >= 2000) %>%
    tidyr::pivot_longer(!dplyr::any_of(c("iso3", "year")),
                        names_to = c("ind", ".value"),
                        names_pattern = "(.*)_([^_]+$)")
  dplyr::mutate(df,
                ind = ifelse(stringr::str_detect(.data[["ind"]], ".*_resc$"),
                             .data[["ind"]],
                             paste0(.data[["ind"]], "_raw")),
  ) %>%
    tidyr::separate(col = .data[["ind"]],
                    into = c("ind", "val_type"),
                    sep = "(\\_)(?=.[^_]+$)") %>%
    tidyr::pivot_wider(names_from = val_type,
                       values_from = value) %>%
    dplyr::mutate(Billions = "UHC",
                  GeoArea_FK = whoville::iso3_to_names(.data[["iso3"]]),
                  Year_FK = .data[["year"]],
                  Billion_Group = dplyr::case_when(
                    .data[["ind"]] %in% c("fp", "anc4", "dtp3", "pneumo") ~ "RMNCH",
                    .data[["ind"]] %in% c("tb", "art", "itn", "sanitation") ~ "Infectious",
                    .data[["ind"]] %in% c("bp", "fpg", "tobacco") ~ "NCD",
                    .data[["ind"]] %in% c("beds", "hwf", "ihr") ~ "Capacity"
                  ),
                  type = .data[["type"]],
                  use_dash = ifelse(.data[["ind"]] %in% "mort_pneumo", "Yes", "No"),
                  use_cal = ifelse(.data[["ind"]] %in% "mort_pneumo", "Yes", "No"),
                  raw = .data[["raw"]],
                  resc = ifelse(is.na(resc), raw, resc),
                  source = "WHO GHO")
}

asc_ids <- c(fp = "FAMILYPLANNINGUNPDUHC",
             pneumo = "WHS4_106",
             itn = "MALARIA_ITNPOP",
             sanitation = "WSH_SANITATION_BASIC",
             tb = "TB_1",
             art = "HIV_ARTCOVERAGE",
             bp = "BP_04",
             tobacco = "SDGTOBACCO",
             fpg = "NCD_GLUC_01",
             beds = "WHS6_102",
             phys = "HWF_0001",
             nurse = "HWF_0006",
             ihr = "SDGIHR",
             ihr2018 = "SDGIHR2018",
             anc4 = "WHS4_154",
             dtp3 = "WHS4_100",
             mort_pneumo = "MORT_200")

asc_filter <- c(fp = NA,
                pneumo = "$filter=Dim1 eq 'BTSX'",
                itn = NA,
                sanitation = "$filter=Dim1 eq 'TOTL'",
                tb = NA,
                art = NA,
                bp = NA,
                tobacco = "$filter=Dim1 eq 'BTSX'",
                fpg = NA,
                beds = NA,
                phys = NA,
                nurse = NA,
                ihr = NA,
                ihr2018 = NA,
                anc4 = NA,
                dtp3 = NA,
                mort_pneumo = "$filter=Dim1 eq 'YEARS0-4' and Dim2 eq 'CH9'")
