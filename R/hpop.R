#' Transform Raw Indicator Values for HPOP Billion
#'
#' `transform_hpop_data()` applies transformations on HPOP Billion indicators so
#' that transformed indicator values can be used within Billions calculations.
#' Details on the specific transformations applied can be found within the
#' Billions methods report. Values in the transform column, if it already exists,
#' are replaced for HPOP indicators that have data in the value column, otherwise
#' the column keeps its original data.
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
#' @param transform_glue Glue expression to be passed to [glue::glue()]. Defaults to
#'     `'transform_{value}'` which will create new column names by prefixing `transform_`
#'     to the original name.
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
                                transform_glue = "transform_{value}",
                                ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, value)
  assert_ind_ids(ind_ids, billion = "hpop")

  # get transform column names
  transform_value <- glue::glue(transform_glue)

  # transform each
  for (i in 1:length(value)) {
    df <- transform_hpop_single(df, iso3, ind, value[i], transform_value[i], ind_ids)
  }

  df
}

#' Perform a transformation on a single column
#'
#' This function is used within [transform_hpop_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams transform_hpop_data
#' @param transform_col Column to put transformed values into
#'
#' @return A single column data frame of transformed values.
transform_hpop_single <- function(df,
                                  iso3,
                                  ind,
                                  value,
                                  transform_col,
                                  ind_ids) {

  # check if transform column in data and create if not
  if (!(transform_col %in% names(df))) {
    df[[transform_col]] <- NA_real_
  }
  df %>%
    dplyr::mutate(!!sym(transform_col) := dplyr::case_when(
                    .data[[ind]] %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "fuel")] ~ trim_transforms(.data[[value]]),
                    .data[[ind]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese", "pm25")] ~ transform_inversion(.data[[value]]),
                    .data[[ind]] == ind_ids["suicide"] ~ transform_suicide_rate(.data[[value]]),
                    .data[[ind]] == ind_ids["alcohol"] ~ transform_alcohol(.data[[value]]),
                    .data[[ind]] == ind_ids["road"] ~ transform_road_safety(.data[[value]], .data[[iso3]]),
                    .data[[ind]] == ind_ids["transfats"] ~ transform_transfats(.data[[value]])
                  ))
}


#' Untransform Indicator Values for HPOP Billion
#'
#' `untransform_hpop_data()` reverses transformations on HPOP Billion indicators to
#' return raw indicator values. Details on the specific transformations applied
#' can be found within the Billions methods report.
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
#' @param transform_value Column name(s) of column with transformed values to retrieve.
#' @param value Column name(s) of column to place untransformed values. Must be same
#'     length as `transform_value`. If a column already exists, values are overwritten
#'     wherever `ind` and `transform_value` are available to be untransformed for
#'     this Billion, but otherwise, the column retains its
#'     other values.
#' @param ind_ids Named vector of indicator codes for input indicators to the Billion.
#'     Although separate indicator codes can be used than the standard, they must
#'     be supplied as a named vector where the names correspond to the output of
#'     `billion_ind_codes()`.
#'
#' @return Data frame in long format.
#'
#' @export
untransform_hpop_data <- function(df,
                                  iso3 = "iso3",
                                  ind = "ind",
                                  transform_value = "transform_value",
                                  value = stringr::str_remove(transform_value, "transform_"),
                                  ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, transform_value)
  assert_string(value, length(transform_value))
  assert_ind_ids(ind_ids, "hpop")

  for (i in 1:length(value)) {
    df <- untransform_hpop_single(df, iso3, ind, transform_value[i], value[i], ind_ids)
  }

  df
}


#' Perform a transformation on a single column
#'
#' This function is used within [untransform_hpop_data()] to generate transformed data
#' on a single column.
#'
#' @inheritParams untransform_hpop_data
#'
#' @return A single column data frame of transformed values.
untransform_hpop_single <- function(df,
                                    iso3,
                                    ind,
                                    transform_value,
                                    value,
                                    ind_ids) {

  # check if transform column in data and create if not
  if (!(value %in% names(df))) {
    df[[value]] <- NA_real_
  }

  df %>%
    dplyr::mutate(!!sym(value) := dplyr::case_when(
      is.na(.data[[transform_value]]) ~ .data[[value]],
      .data[[ind]] %in% ind_ids[c("devontrack", "water", "water_urban", "water_rural", "hpop_sanitation", "hpop_sanitation_urban", "hpop_sanitation_rural", "fuel")] ~ .data[[transform_value]],
      .data[[ind]] %in% ind_ids[c("stunting", "overweight", "wasting", "hpop_tobacco", "ipv", "child_viol", "child_obese", "adult_obese", "pm25")] ~ transform_inversion(.data[[transform_value]]),
      .data[[ind]] == ind_ids["suicide"] ~ untransform_suicide_rate(.data[[transform_value]]),
      .data[[ind]] == ind_ids["alcohol"] ~ untransform_alcohol(.data[[transform_value]]),
      .data[[ind]] == ind_ids["road"] ~ untransform_road_safety(.data[[transform_value]], .data[[iso3]]),
      .data[[ind]] == ind_ids["transfats"] ~ untransform_transfats(.data[[transform_value]]),
      TRUE ~ .data[[value]]
    ))
}

#' Add Population Figures for HPOP Billion
#'
#' `add_hpop_populations()` adds relevant populations to each HPOP Billion indicator
#' and country, so these can be used to calculate indicator-level contributions
#' to the HPOP Billion.
#'
#' @inherit transform_hpop_data return details params
#' @param pop_year Year used to pull in HPOP populations, defaults to 2023.
#' @export
add_hpop_populations <- function(df,
                                 iso3 = "iso3",
                                 ind = "ind",
                                 pop_year = 2023,
                                 ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind)

  #populations are fixed for each iso3 x ind combination - so do the hard work on a minimal dataset
  pop_df <- df %>%
    dplyr::ungroup() %>%
  	dplyr::select(iso3, ind) %>%
  	dplyr::distinct() %>%
    dplyr::mutate(
      population = dplyr::case_when(
        .data[[ind]] %in% ind_ids[c("hpop_sanitation_rural", "water_rural")] ~ wppdistro::get_population(.data[[iso3]], pop_year, rural_urb = "rural"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation_urban", "water_urban")] ~ wppdistro::get_population(.data[[iso3]], pop_year, rural_urb = "urban"),
        .data[[ind]] %in% ind_ids[c("hpop_sanitation", "water", "road", "fuel", "pm25", "transfats", "suicide")] ~ wppdistro::get_population(.data[[iso3]], pop_year),
        .data[[ind]] %in% ind_ids[c("hpop_tobacco", "alcohol")] ~ wppdistro::get_population(.data[[iso3]], pop_year, age_range = "over_14"),
        .data[[ind]] %in% ind_ids[c("adult_obese")] ~ wppdistro::get_population(.data[[iso3]], pop_year, age_range = "over_19") + (wppdistro::get_population(.data[[iso3]], pop_year, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("child_obese")] ~ wppdistro::get_population(.data[[iso3]], pop_year, age_range = "btwn_5_19"),
        .data[[ind]] %in% ind_ids[c("wasting", "stunting", "overweight", "devontrack")] ~ wppdistro::get_population(.data[[iso3]], pop_year, age_range = "under_5"),
        .data[[ind]] %in% ind_ids[c("child_viol")] ~ wppdistro::get_population(.data[[iso3]], pop_year, age_range = "under_20") - (wppdistro::get_population(.data[[iso3]], pop_year, age_range = "15_19") / 2),
        .data[[ind]] %in% ind_ids[c("ipv")] ~ wppdistro::get_population(.data[[iso3]], pop_year, sex = "female", age_range = "over_14")
      ))

  df %>%
	  dplyr::left_join(pop_df, by = c("iso3", "ind"))
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
#' @param source Column name of column with source information for the data.
#' @param type Column name of type with type information for the data.
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
                                         source = "source",
                                         type = "type",
                                         value = "value",
                                         transform_value = "transform_value",
                                         ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, year, iso3, ind, population, transform_value)
  assert_ind_ids(ind_ids, "hpop")
  assert_unique_rows(df, ind, iso3, year, ind_ids)

  piv_vals <- c(value, transform_value, source, type)
  piv_vals <- piv_vals[piv_vals %in% names(df)]
  df %>%
    dplyr::filter(.data[[year]] %in% c(start_year, end_year),
                  .data[[ind]] %in% ind_ids) %>%
    tidyr::pivot_wider(c(iso3, ind, population),
                       names_from = year,
                       values_from = piv_vals) %>%
    dplyr::mutate("change" := .data[[paste(transform_value, end_year, sep = "_")]] - .data[[paste(transform_value, start_year, sep = "_")]],
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
#' @inheritParams add_hpop_populations
#'
#' @export
calculate_hpop_billion <- function(df,
                                   iso3 = "iso3",
                                   ind = "ind",
                                   change = "change",
                                   pop_year = 2023,
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
    dplyr::left_join(generate_hpop_populations(pop_year),
                     by = c(iso3 = "iso3", ind = "ind")) %>%
    dplyr::group_by(.data[[iso3]], .data[["pop_group"]], .data[["pos"]]) %>%
    dplyr::summarize("product" := 1 - prod(.data[["od"]]),
                     "pop_group_population" := unique(.data[["pop_group_population"]]),
                     "sumi" := sum(.data[["delta"]]),
                     .groups = "drop") %>%
    dplyr::group_by(.data[[iso3]]) %>%
    dplyr::summarize("healthier" := sum(.data[["pop_group_population"]] * .data[["product"]] * .data[["pos"]]),
                     "unhealthier" := -sum(.data[["pop_group_population"]] * .data[["product"]] * !.data[["pos"]]),
                     .groups = "drop") %>%
    dplyr::mutate("net_healthier" := .data[["healthier"]] + .data[["unhealthier"]],
                  "perc_healthier" := 100 * .data[["net_healthier"]] / wppdistro::get_population(.data[[iso3]], pop_year))
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
#' @inheritParams add_hpop_populations
#'
#' @export
generate_hpop_populations <- function(pop_year) {
  dplyr::tibble(iso3 = whoville::who_member_states(),
                under_5_urban_male = wppdistro::get_population(iso3, pop_year, age_range = "under_5", rural_urb = "urban", sex = "male"),
                btwn_5_14_urban_male = wppdistro::get_population(iso3, pop_year, age_range = "btwn_5_14", rural_urb = "urban", sex = "male"),
                btwn_15_18_urban_male = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "urban", sex = "male") / 2,
                btwn_18_19_urban_male = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "urban", sex = "male") / 2,
                over_19_urban_male = wppdistro::get_population(iso3, pop_year, age_range = "over_19", rural_urb = "urban", sex = "male"),
                under_5_rural_male = wppdistro::get_population(iso3, pop_year, age_range = "under_5", rural_urb = "rural", sex = "male"),
                btwn_5_14_rural_male = wppdistro::get_population(iso3, pop_year, age_range = "btwn_5_14", rural_urb = "rural", sex = "male"),
                btwn_15_18_rural_male = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "rural", sex = "male") / 2,
                btwn_18_19_rural_male = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "rural", sex = "male") / 2,
                over_19_rural_male = wppdistro::get_population(iso3, pop_year, age_range = "over_19", rural_urb = "rural", sex = "male"),
                under_5_urban_female = wppdistro::get_population(iso3, pop_year, age_range = "under_5", rural_urb = "urban", sex = "female"),
                btwn_5_14_urban_female = wppdistro::get_population(iso3, pop_year, age_range = "btwn_5_14", rural_urb = "urban", sex = "female"),
                btwn_15_18_urban_female = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "urban", sex = "female") / 2,
                btwn_18_19_urban_female = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "urban", sex = "female") / 2,
                over_19_urban_female = wppdistro::get_population(iso3, pop_year, age_range = "over_19", rural_urb = "urban", sex = "female"),
                under_5_rural_female = wppdistro::get_population(iso3, pop_year, age_range = "under_5", rural_urb = "rural", sex = "female"),
                btwn_5_14_rural_female = wppdistro::get_population(iso3, pop_year, age_range = "btwn_5_14", rural_urb = "rural", sex = "female"),
                btwn_15_18_rural_female = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "rural", sex = "female") / 2,
                btwn_18_19_rural_female = wppdistro::get_population(iso3, pop_year, age_range = "15_19", rural_urb = "rural", sex = "female") / 2,
                over_19_rural_female = wppdistro::get_population(iso3, pop_year, age_range = "over_19", rural_urb = "rural", sex = "female")) %>%
    tidyr::pivot_longer(-c("iso3"),
                        names_to = "pop_group",
                        values_to = "pop_group_population") %>%
    dplyr::full_join(billionaiRe::pop_links, by = "pop_group")
}

