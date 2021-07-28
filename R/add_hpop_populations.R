#' Add Population Figures for HPOP Billion
#'
#' `add_hpop_populations()` adds relevant populations to each HPOP Billion indicator
#' and country, so these can be used to calculate indicator-level contributions
#' to the HPOP Billion. The column specified by `population` will be generated and
#' filled with relevant populations for that country and indicator. If the column
#' already exists, only missing values will be replaced by the function.
#'
#' @inherit transform_hpop_data return details params
#' @param pop_year Year used to pull in HPOP populations, defaults to 2023.
#' @param population Column name of column to create with population figures.
#'
#' @export
add_hpop_populations <- function(df,
                                 iso3 = "iso3",
                                 ind = "ind",
                                 population = "population",
                                 pop_year = 2023,
                                 ind_ids = billion_ind_codes("hpop")) {
  assert_columns(df, iso3, ind)
  assert_string(population, 1)
  assert_ind_ids(ind_ids, "hpop")
  assert_numeric(pop_year)

  # add population column if it doesn't already exist
  df <- billionaiRe_add_columns(df, population, NA_real_)

  # add populations for each unique iso3 and ind value
  pop_df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(iso3, ind) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "_temp_population" := dplyr::case_when(
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

  # join up populations and replace missing values in column with generated populations
  df %>%
    dplyr::left_join(pop_df, by = c("iso3", "ind")) %>%
    dplyr::mutate(!!sym(population) := ifelse(is.na(.data[[population]]),
                                              .data[["_temp_population"]],
                                              .data[[population]])) %>%
    dplyr::select(-"_temp_population")
}
