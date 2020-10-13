#' @export
transform_hpop_data <- function(df,
                                iso3 = "iso3",
                                ind = "ind",
                                value = "value",
                                dimension = "dimension",
                                ind_ids = billion_ind_codes("hpop")) {
  df %>%
    dplyr::mutate(!!sym(value) := ifelse(.data[[ind]] == ind_ids["fuel"], trim_clean_fuels(.data[[value]]), .data[[value]]),
                  transform_value = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("devontrack", "water", "hpop_sanitation", "pm25", "fuel")] ~ .data[[value]],
                    .data[[ind]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese")] ~ reverse_ind(.data[[value]]),
                    .data[[ind]] == ind_ids["suicide"] ~ transform_suicide_rate(.data[[value]]),
                    .data[[ind]] == ind_ids["alcohol"] ~ transform_alcohol(.data[[value]]),
                    .data[[ind]] == ind_ids["road"] ~ transform_road_safety(.data[[value]], .data[[iso3]]),
                    .data[[ind]] == ind_ids["transfats"] ~ transform_transfats(.data[[value]])
                  ))
}

#' @export
add_hpop_populations <- function(df,
                                 iso3 = "iso3",
                                 ind = "ind",
                                 transform_value = "transform_value",
                                 dimension = "dimension",
                                 ind_ids = billion_ind_codes("hpop")) {
  df %>%
    dplyr::mutate(
      population = dplyr::case_when(
        .data[[ind]] %in% ind_ids[c("hpop_sanitation", "water")] & .data[[dimension]] == "RUR" ~ wppdistro::get_population(.data[[iso3]], 2023, rural_urb = "rural"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation", "water")] & .data[[dimension]] == "URB" ~ wppdistro::get_population(.data[[iso3]], 2023, rural_urb = "urban"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation", "water", "road", "fuel", "pm25", "transfats", "suicide")] ~ wppdistro::get_population(.data[[iso3]], 2023),
        .data[[ind]] %in% ind_ids[c("hpop_tobacco", "alcohol")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "over_14"),
        .data[[ind]] %in% ind_ids[c("adult_obese")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "over_19") + (wppdistro::get_population(.data[[iso3]], 2023, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("child_obese")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "btwn_5_19"),
        .data[[ind]] %in% ind_ids[c("wasting", "stunting", "overweight", "devontrack")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "under_5"),
        .data[[ind]] %in% ind_ids[c("child_viol")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "under_20") - (wppdistro::get_population(.data[[iso3]], 2023, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("ipv")] ~ wppdistro::get_population(.data[[iso3]], 2023, sex = "female", age_range = "over_14")
      ),
      !!sym(ind) := dplyr::case_when(
        .data[[ind]] == ind_ids["hpop_sanitation"] & .data[[dimension]] == "RUR" ~ "hpop_sanitation_rural",
        .data[[ind]] == ind_ids["hpop_sanitation"] & .data[[dimension]] == "URB" ~ "hpop_sanitation_urban",
        .data[[ind]] == ind_ids["water"] & .data[[dimension]] == "RUR" ~ "water_rural",
        .data[[ind]] == ind_ids["water"] & .data[[dimension]] == "URB" ~ "water_urban",
        TRUE ~ names(ind_ids)[match(.data[[ind]], ind_ids)]
      ))
}

#' @export
calculate_hpop_ind_billion <- function(df,
                                       year = "year",
                                       start_year = 2018,
                                       end_year = 2023,
                                       iso3 = "iso3",
                                       ind = "ind",
                                       population = "population",
                                       transform_value = "transform_value") {
  df %>%
    dplyr::filter(.data[[year]] %in% c(start_year, end_year)) %>%
    tidyr::pivot_wider(c(iso3, ind, population),
                       names_from = year,
                       values_from = transform_value) %>%
    dplyr::mutate("change" := .data[[as.character(end_year)]] - .data[[as.character(start_year)]],
                  contribution = .data[["change"]] * .data[[population]] / 100,
                  "start_year" := start_year,
                  "end_year" := end_year)
}

#' @export
calculate_hpop_billion <- function(df,
                                   iso3 = "iso3",
                                   ind = "ind",
                                   change = "change",
                                   ind_ids = billion_ind_codes("hpop")) {
  df %>%
    dplyr::mutate(
      !!sym(ind) := ifelse(.data[[ind]] %in% ind_ids[c("wasting", "overweight")],
                             "child_nutrition",
                             .data[[ind]])) %>%
    dplyr::group_by(.data[[iso3]],
                    .data[[ind]]) %>%
    dplyr::summarize("change" := sum(.data[["change"]]),
                     .groups = "drop") %>%
    dplyr::mutate("delta" := .data[["change"]] / 100,
                  "od" := 1 - abs(.data[["delta"]]),
                  "pos" := .data[["delta"]] > 0) %>%
    dplyr::left_join(generate_hpop_populations(),
                     by = c(iso3 = "iso3", ind = "ind")) %>%
    dplyr::group_by(.data[[iso3]], .data[["pop_group"]], .data[["pos"]]) %>%
    dplyr::summarize("product" := 1 - prod(.data[["od"]]),
                     "population" := unique(.data[["population"]]),
                     "sumi" := sum(.data[["delta"]]),
                     .groups = "drop") %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::summarize("healthier" := sum(.data[["population"]] * .data[["product"]] * .data[["pos"]]),
                     "unhealthier" := -sum(.data[["population"]] * .data[["product"]] * !.data[["pos"]])) %>%
    dplyr::mutate("net_healthier" := .data[["healthier"]] + .data[["unhealthier"]],
                  "perc_healthier" := 100 * .data[["net_healthier"]] / wppdistro::get_population(.data[[iso3]], 2023))
}

#' @export
generate_hpop_populations <- function() {
  dplyr::tibble(iso3 = whoville::who_member_states(),
                under_5_urban = wppdistro::get_population(iso3, 2023, age_range = "under_5", rural_urb = "urban"),
                btwn_5_14_urban = wppdistro::get_population(iso3, 2023, age_range = "btwn_5_14", rural_urb = "urban"),
                btwn_15_18_urban = wppdistro::get_population(iso3, 2023, age_range = "15_19", rural_urb = "urban") / 2,
                btwn_18_19_urban = wppdistro::get_population(iso3, 2023, age_range = "15_19", rural_urb = "urban") / 2,
                over_19_urban = wppdistro::get_population(iso3, 2023, age_range = "over_19", rural_urb = "urban"),
                under_5_rural = wppdistro::get_population(iso3, 2023, age_range = "under_5", rural_urb = "rural"),
                btwn_5_14_rural = wppdistro::get_population(iso3, 2023, age_range = "btwn_5_14", rural_urb = "rural"),
                btwn_15_18_rural = wppdistro::get_population(iso3, 2023, age_range = "15_19", rural_urb = "rural") / 2,
                btwn_18_19_rural = wppdistro::get_population(iso3, 2023, age_range = "15_19", rural_urb = "rural") / 2,
                over_19_rural = wppdistro::get_population(iso3, 2023, age_range = "over_19", rural_urb = "rural")) %>%
    tidyr::pivot_longer(-c("iso3"),
                        names_to = "pop_group",
                        values_to = "population") %>%
    dplyr::full_join(billionaiRe::pop_links, by = "pop_group")
}
