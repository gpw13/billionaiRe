#' Transform Raw Indicator Values for HPOP Billion
#'
#' `transform_hpop_data()` applies transformations on HPOP Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report.
#'
#' For more details on the HPOP Billion calculation process and how this function
#' ties in with the rest, see the vignette:
#'
#' \href{../doc/hpop.html}{\code{vignette("hpop", package = "billionaiRe")}}
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param iso3 Column name of column with country ISO3 codes.
#' @param ind Column name of column with indicator names.
#' @param value Column name of column with indicator values.
#' @param ind_ids Named vector of indicator codes for input indicators to the Billion.
#'     Although separate indicator codes can be used than the standard, they must
#'     be supplied as a named vector where the names correspond to the output of
#'     `billion_ind_codes()`.
#'
#' @return Data frame in long format.
#'
#' @export
transform_hpop_data <- function(df,
                                iso3 = "iso3",
                                ind = "ind",
                                value = "value",
                                ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, value)

  df %>%
    dplyr::mutate(!!sym(value) := ifelse(.data[[ind]] == ind_ids["fuel"], trim_clean_fuels(.data[[value]]), .data[[value]]),
                  transform_value = dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "pm25", "fuel")] ~ .data[[value]],
                    .data[[ind]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese")] ~ reverse_ind(.data[[value]]),
                    .data[[ind]] == ind_ids["suicide"] ~ transform_suicide_rate(.data[[value]]),
                    .data[[ind]] == ind_ids["alcohol"] ~ transform_alcohol(.data[[value]]),
                    .data[[ind]] == ind_ids["road"] ~ transform_road_safety(.data[[value]], .data[[iso3]]),
                    .data[[ind]] == ind_ids["transfats"] ~ transform_transfats(.data[[value]])
                  ))
}

#' Add Population Figures for HPOP Billion
#'
#' `add_hpop_populations()` adds relevant populations to each HPOP Billion indicator
#' and country, so these can be used to calculate indicator-level contributions
#' to the HPOP Billion.
#'
#' @param dimension Column of column with dimension for indicator/country values.
#'     Used within `add_hpop_populations()` to add relevant population for indicators
#'     with shifting dimensions across countries, such as survey-based indicators
#'     where values are derived from total, rural, or urban populations in
#'     specific countries.
#'
#' @inherit transform_hpop_data return details params
#'
#' @export
add_hpop_populations <- function(df,
                                 iso3 = "iso3",
                                 ind = "ind",
                                 dimension = "dimension",
                                 ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, dimension)

  df %>%
    dplyr::mutate(
      population = dplyr::case_when(
        .data[[ind]] %in% ind_ids[c("hpop_sanitation_rural", "water_rural")] & .data[[dimension]] == "RUR" ~ wppdistro::get_population(.data[[iso3]], 2023, rural_urb = "rural"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation_urban", "water_urban")] & .data[[dimension]] == "URB" ~ wppdistro::get_population(.data[[iso3]], 2023, rural_urb = "urban"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation", "water", "road", "fuel", "pm25", "transfats", "suicide")] ~ wppdistro::get_population(.data[[iso3]], 2023),
        .data[[ind]] %in% ind_ids[c("hpop_tobacco", "alcohol")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "over_14"),
        .data[[ind]] %in% ind_ids[c("adult_obese")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "over_19") + (wppdistro::get_population(.data[[iso3]], 2023, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("child_obese")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "btwn_5_19"),
        .data[[ind]] %in% ind_ids[c("wasting", "stunting", "overweight", "devontrack")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "under_5"),
        .data[[ind]] %in% ind_ids[c("child_viol")] ~ wppdistro::get_population(.data[[iso3]], 2023, age_range = "under_20") - (wppdistro::get_population(.data[[iso3]], 2023, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("ipv")] ~ wppdistro::get_population(.data[[iso3]], 2023, sex = "female", age_range = "over_14")
      ),
      !!sym(ind) := dplyr::case_when(
        .data[[ind]] %in% ind_ids[c("hpop_sanitation_urban", "hpop_sanitation_rural"] ~ "hpop_sanitation",
        .data[[ind]] %in% ind_ids[c("water_urban", "water_rural")] ~ "water",
        TRUE ~ names(ind_ids)[match(.data[[ind]], ind_ids)]
      ))
}

#' Calculate HPOP Indicator Contributions
#'
#' `calculate_hpop_contributions()` calculates indicator-level contributions and
#' changes for the HPOP Billion.
#'
#' @param year Column name of column with years.
#' @param start_year Base year for contribution calculation, defaults to 2018.
#' @param end_year End year for contribution calculation, defaults to 2023.
#' @param population Column name of column with population figures.
#' @param transform_value Column name of column with transformed indicator values.
#'
#' @inherit transform_hpop_data return details params
#'
#' @export
calculate_hpop_contributions <- function(df,
                                         year = "year",
                                         start_year = 2018,
                                         end_year = 2023,
                                         iso3 = "iso3",
                                         ind = "ind",
                                         population = "population",
                                         transform_value = "transform_value") {
  assert_columns(df, year, iso3, ind, population, transform_value)

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

#' Calculate HPOP Billion
#'
#' `calculate_hpop_billion()` calculates country-level HPOP Billion based on
#' indicator level changes.
#'
#' @param change Column name of column with indicator-level change between base
#'     year and end year.
#'
#' @inherit transform_hpop_data return details params
#'
#' @export
calculate_hpop_billion <- function(df,
                                   iso3 = "iso3",
                                   ind = "ind",
                                   change = "change",
                                   ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, change)

  df %>%
    dplyr::filter(!is.na(.data[[change]])) %>%
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

#' Generate HPOP Population Table
#'
#' `generate_hpop_populations()` creates the country/population data frame necessary
#' for correcting double counting within the HPOP Billion calculation. This is used
#' within `calculate_hpop_billion()`.
#'
#' Because the underlying data from the wppdistro package is not publicly distributed
#' for specific small WHO member states, this is not exported as an already created
#' (and thus publicly available) table within the billionaiRe package, but instead
#' dynamically generated each R session.
#'
#' @return Data frame in long format.
#'
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
