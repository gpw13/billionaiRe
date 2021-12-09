add_scenario_adult_obese <- function(df,
                                     scenario_function,
                                     ind = "ind",
                                     small_is_best = TRUE,
                                     ind_ids = billion_ind_codes("hpop"),
                                     ...) {
  this_ind <- ind_ids["adult_obese"]

  df %>%
    dplyr::filter(.data[[ind]] == this_ind) %>%
    add_scenario_dispatch(
      scenario_function = scenario_function,
      ind = ind,
      small_is_best = small_is_best,
      ind_ids = ind_ids,
      ...
    )
}

add_scenario_water <- function(df,
                               value = "value",
                               ind = "ind",
                               iso3 = "iso3",
                               year = "year",
                               start_year = 2018,
                               end_year = 2025,
                               target_year = end_year,
                               scenario = "scenario",
                               upper_limit = 100,
                               lower_limit = 0,
                               trim = TRUE,
                               keep_better_values = FALSE,
                               small_is_best = TRUE,
                               trim_years = TRUE,
                               ind_ids = billion_ind_codes("hpop"),
                               default_scenario = "default") {

}
