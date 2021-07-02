#' Calculate HPOP Billion
#'
#' `calculate_hpop_billion()` calculates country-level HPOP Billion based on
#' indicator level changes.
#'
#' @param change Column name of column with indicator-level change between base
#'     year and end year.
#'
#' @inherit transform_hpop_data return details params
#' @inheritParams calculate_hpop_contributions
#'
#' @export
calculate_hpop_billion <- function(df,
                                   year = "year",
                                   start_year = 2018,
                                   end_year = 2019:2023,
                                   iso3 = "iso3",
                                   ind = "ind",
                                   pop_year = 2023,
                                   transform_value = "transform_value",
                                   contribution = stringr::str_replace(transform_value, "transform_value", "contribution"),
                                   scenario = NULL,
                                   ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind, year)
  assert_ind_ids(ind_ids, "hpop")
  assert_unique_rows(df, ind, iso3, year, scenario, ind_ids)
  assert_same_length(transform_value, contribution)
  assert_years(start_year, end_year)

  # add columns if not already existing
  df <- billionaiRe_add_columns(df, contribution, NA_real_)

  # calculate the change

  change_df <- df %>%
    dplyr::filter(.data[[year]] %in% c(!!end_year, !!start_year),
                  .data[[ind]] %in% ind_ids) %>%
    dplyr::mutate(!!sym(ind) := ifelse(.data[[ind]] %in% ind_ids[c("wasting", "overweight")],
                                       "child_nutrition",
                                       .data[[ind]])) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario, ind)))) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(transform_value),
                                ~ .x - .x[.data[[year]] == !!start_year])) %>%
    dplyr::filter(.data[[year]] %in% !!end_year) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, scenario, ind, year)))) %>%
    dplyr::summarize(dplyr::across(transform_value,
                                   ~ sum(.x, na.rm = TRUE)), # for child_nutrition
                     .groups = "drop") %>%
    dplyr::rename_with(~contribution[which(transform_value == .x)], .cols = transform_value)

  # add population groups

  change_df <- dplyr::left_join(change_df,
                                generate_hpop_populations(pop_year),
                                by = c(iso3 = "iso3", ind = "ind"))

  # calculate billions for each contribution column

  change_df_list <- purrr::map(contribution,
                               calculate_hpop_billion_single,
                               df = change_df,
                               iso3 = iso3,
                               ind = ind,
                               year = year,
                               scenario = scenario)

  # remove scenario from keys if NULL
  keys <- c(iso3, ind, year, scenario)
  keys <- keys[!is.null(keys)]

  # join back to change_df
  change_df <- purrr::reduce(change_df_list, left_join, by = keys)

  # return Billions with the rest of the original data
  dplyr::bind_rows(df, change_df)
}

#' Calculate the HPOP Billion for one column of change
#'
#'
#' @inheritParams calculate_hpop_billion
#' @param change Column name of column with change value
calculate_hpop_billion_single <- function(change,
                                          df,
                                          iso3,
                                          ind,
                                          year,
                                          scenario) {
  df %>%
    dplyr::mutate("_delta_temp" := .data[[change]] / 100,
                  "_od_temp" := 1 - abs(.data[["_delta_temp"]]),
                  "_pos_temp" := .data[["_delta_temp"]] > 0) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, year, scenario, "_pop_group_temp", "_pos_temp")))) %>%
    dplyr::summarize("_product_temp" := 1 - prod(.data[["_od_temp"]]),
                     "_pop_group_population_temp" := unique(.data[["_pop_group_population_temp"]]),
                     "_sumi_temp" := sum(.data[["_delta_temp"]]),
                     .groups = "drop") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(iso3, year, scenario)))) %>%
    dplyr::summarize("hpop_healthier_plus" := sum(.data[["_pop_group_population_temp"]] * .data[["_product_temp"]] * .data[["_pos_temp"]]),
                     "hpop_healthier_minus" := -sum(.data[["_pop_group_population_temp"]] * .data[["_product_temp"]] * !.data[["_pos_temp"]]),
                     .groups = "drop") %>%
    dplyr::mutate("hpop_healthier" := .data[["hpop_healthier_plus"]] + .data[["hpop_healthier_minus"]]) %>%
    tidyr::pivot_longer(dplyr::all_of(c("hpop_healthier_plus",
                                        "hpop_healthier_minus",
                                        "hpop_healthier")),
                        names_to = ind,
                        values_to = change)
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
                        names_to = "_pop_group_temp",
                        values_to = "_pop_group_population_temp") %>%
    dplyr::full_join(billionaiRe::pop_links, by = c("_pop_group_temp" = "pop_group"))
}

