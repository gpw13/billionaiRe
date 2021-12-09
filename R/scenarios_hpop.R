#' Add scenario to adult obese indicator
#'
#' Essentially passes `...` values to `add_scenario_dispatch` while forcing
#' `small_is_best` = TRUE.
#'
#' @param ... additional parameters pass
#' @inherit add_scenario
#' @inheritParams transform_hpop_data
#' @inheritParams scenario_fixed_target
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

# add_scenario_water <- function(df,
#                                scenario_function,
#                                ind = "ind",
#
#                                ind_ids = billion_ind_codes("hpop"),
#                                ...
#                                ) {
#   this_ind <- ind_ids["water"]
#
#   df %>%
#     dplyr::filter(.data[[ind]] == this_ind) %>%
#     add_scenario_dispatch(
#       scenario_function = scenario_function,
#       ind = ind,
#
#       ind_ids = ind_ids,
#       ...
#     )
# }
#
#
# add_transfats_scenarios <- function(df) {
#   df %>%scenario_bau() %>%
#     scenario_fixed_target(target_value=100,target_year=2025,sname="scen_sdg") %>%
#     mutate(scen_acceleration=scen_sdg)
#
# }
# add_ipv_scenarios <- function(df) {
#   df %>% scenario_bau() %>%
#     scenario_fixed_target(target_value=0,target_year=2030,sname="scen_sdg")%>%
#     mutate(scen_acceleration=scen_sdg)
# }
# add_child_viol_scenarios <- function(df) {
#   df %>% scenario_bau() %>%
#     scenario_fixed_target(target_value=0,target_year=2030,sname="scen_sdg")
# }
# add_devontrack_scenarios <- function(df) {
#   df %>% scenario_bau() %>%
#     scenario_fixed_target(target_value=80,target_year=2030,sname="scen_sdg")
# }
#
# add_water_scenarios <- function(df) {
#   df %>%
#     scenario_quintile(
#       quintileyear = 2017,
#       baseline = 2018,
#       trim_min = 0,
#       trim_max = 99,
#       sname = "scen_quintile_2017"
#     ) %>%
#     scenario_bau() %>%
#     rename(scen_acceleration = scen_quintile_2017) %>%
#     scenario_fixed_target(target_value=95,target_year=2030,sname="scen_sdg")
# }
# add_hpop_sanitation_scenarios <- function(df) {
#   # sometimes bau is better - discuss with program
#   df %>%
#     scenario_quintile(
#       quintileyear = 2019,
#       baseline = 2018,
#       trim_min = 0,
#       trim_max = 99,
#       sname = "scen_quintile_2017"
#     ) %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_quintile_2017)%>%
#     scenario_fixed_target(target_value=95,target_year=2030,sname="scen_sdg")
# }
#
# add_stunting_scenarios <- function(df) {
#   # all scenarios are on annual rate of change basis
#   #For stunting the target is to reduce 2012 value by 50% by 2030
#   #Note the bau is based on 2012 -> aarr but this is handled in projections
#   df %>%
#     scenario_aroc(
#       percent_decrease = 50,
#       value = "value",
#       baseline = 2018,
#       old_baseline = 2012,
#       target_year = 2030,
#       aroc_type = "percent_decrease",
#       sname = "scen_perc_catchup_50_2012"
#     ) %>%
#     scenario_halt() %>%
#     scenario_bau() %>%
#     scenario_bestof(c("scen_bau", "scen_halt_2018", "scen_perc_catchup_50_2012"),
#                     sname = "scen_bestof") %>%
#     select(-scen_halt_2018,-bestname) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_wasting_scenarios <- function(df) {
#   # all scenarios are on annual rate of change basis
#   #For wasting the target is to 3  by 2030
#   #Note the bau is based on 2008 -> aarr but this is handled in projections
#   df %>%
#     scenario_bau() %>%
#     scenario_halt() %>%
#     scenario_aroc(
#       target = 3,
#       value = "value",
#       baseline = 2018,
#       target_year = 2030,
#       aroc_type = "target",
#       sname = "scen_tar2030"
#     ) %>%
#     scenario_bestof(c("scen_tar2030", "scen_bau", "scen_halt_2018"), sname =
#                       "scen_bestof") %>%
#     select(-scen_halt_2018,-bestname) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_overweight_scenarios <- function(df) {
#   # all scenarios are on annual rate of change basis
#   #For overweigt the target is to 3 by 2030
#   #Note the bau is based on 2012 -> aarr but this is handled in projections
#   df %>%
#     scenario_aroc(
#       target = 3,
#       value = "value",
#       baseline = 2018,
#       target_year = 2030,
#       aroc_type = "target",
#       sname = "scen_tar2030"
#     ) %>%
#     scenario_bau() %>%
#     scenario_bestof(c("scen_bau", "scen_tar2030"), sname = "scen_bestof") %>%
#     select(-bestname) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_child_obese_scenarios <- function(df) {
#   #NCD target is to halt at 2010, ours is to return to being on track for this
#   df %>%
#     scenario_halt(baseline = 2018,
#                   old_baseline = 2010,
#                   target_year = 2025) %>%
#     scenario_halt() %>%
#     scenario_bau() %>%
#     #mutate(scen_acceleration = scen_catchup_halt_2010)
#     rename(scen_acceleration =  scen_catchup_halt_2010) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
# add_adult_obese_scenarios <- function(df) {
#   #NCD target is to halt at 2010, ours is to return to being on track for this
#   df %>%
#     scenario_halt(baseline = 2018,
#                   old_baseline = 2010,
#                   target_year = 2025) %>%
#     scenario_halt() %>%
#     scenario_bau() %>%
#     rename(scen_acceleration =  scen_catchup_halt_2010) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
#
# add_fuel_scenarios <- function(df) {
#   #handle hic separately with just bau
#   hic_df <-
#     df %>% mutate(wb_ig = iso3_to_regions(iso3, region = "wb_ig")) %>%
#     filter(wb_ig == "HIC") %>%
#     scenario_bau() %>%
#     mutate(scen_region = scen_bau)
#   #for non hic a regional approach is used using years 2018 to 2023.
#   other_df <-
#     df %>% mutate(wb_ig = iso3_to_regions(iso3, region = "wb_ig")) %>%
#     filter(!wb_ig == "HIC" | is.na(wb_ig)) %>%
#     scenario_bau() %>%
#     scenario_region(baseline = 2018, change = 5)
#   hic_df %>% bind_rows(other_df) %>%
#     mutate(scen_acceleration = scen_region) %>%
#     mutate(scen_sdg=scen_acceleration)%>%
#     scenario_fixed_target(
#       target_value = 95,
#       target_year = 2030,
#       sname = "scen_sdg",
#       small_is_best = FALSE
#     )
# }
#
#
# add_pm25_scenarios <- function(df) {
#   #this is not finalised
#   #external pm25targets
#   df %>%
#     scenario_bau() %>%
#     scenario_fixed_target(
#       target_value = 10,
#       target_year = 2030,
#       sname = "scen_10_2030",
#       small_is_best = TRUE
#     ) %>%
#     group_by(iso3) %>%
#     mutate(baseline = value[year == 2018], linear = -baseline * .02) %>%
#     ungroup() %>%
#     select(-baseline) %>%
#     scenario_lin(lin_col = "linear") %>%
#     #scenario_air_baseline(baseline=2018, target_year=2030) %>%
#     #left_join(pm25targets %>% select(iso3,year,target)) %>%
#     #ungroup() %>%
#     #scenario_target_col(target_col = "target",target_year=2030) %>%
#     scenario_bestof(c("scen_linear", "scen_bau"), minv = TRUE) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_10_2030)
#   #%>%
#   #scenario_fixed_multitargets(target_values=c(10,15,25,35), target_year=2030,sname="scen_targets_2030")
#
# }
#
# add_road_scenarios <- function(df) {
#   df %>%
#     scenario_percent_baseline(
#       percent_decrease = 50,
#       baseline = 2018,
#       old_baseline = 2020,
#       target_year = 2030
#     ) %>%
#     scenario_bau() %>%
#     scenario_bestof(c("scen_perc_catchup_50_2020", "scen_bau"), minv = TRUE) %>%
#     rename(scen_acceleration = scen_bestof)%>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_suicide_scenarios <- function(df) {
#   df %>%
#     scenario_bau() %>%
#     scenario_percent_baseline(
#       percent_decrease = 33.333,
#       old_baseline = 2015,
#       target_year = 2030,
#       sname = "scen_perc_catchup_third_2015"
#     )  %>%
#     scenario_halt() %>%
#     mutate(scen_perc_catchup_third_2015 = pmin(scen_perc_catchup_third_2015, scen_halt_2018)) %>%
#     scenario_bestof(c("scen_bau", "scen_perc_catchup_third_2015")) %>%
#     select(-bestname,-scen_halt_2018) %>%
#     rename(scen_acceleration = scen_bestof)%>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_alcohol_scenarios <- function(df) {
#   df %>%
#     scenario_bau() %>%
#     scenario_halt() %>%
#     scenario_percent_baseline(
#       percent_decrease = 10,
#       old_baseline = 2010,
#       target_year = 2025
#     ) %>%
#     mutate(scen_perc_catchup_10_2010 = pmin(scen_perc_catchup_10_2010, scen_halt_2018)) %>%
#     scenario_bestof(c("scen_bau", "scen_halt_2018", "scen_perc_catchup_10_2010")) %>%
#     select(-scen_halt_2018) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
#
# add_hpop_tobacco_scenarios <- function(df) {
#   #this needs bau to be confirmed with program
#   df %>%
#     scenario_bau() %>%
#     #nonstandard definition of intermediate 2023 target
#     scenario_percent_baseline_tobacco(
#       percent = 30,
#       baseline = 2018,
#       old_baseline = 2010,
#       target_year = 2025
#     ) %>%
#     #remove scenarios for site with no data
#     group_by(iso3) %>%
#     mutate(hasestimate = any(type == "estimated")) %>%
#     ungroup() %>%
#     scenario_halt() %>%
#     scenario_bestof(c("scen_bau", "scen_halt_2018", "scen_perc_catchup_30_2010")) %>%
#     mutate(scen_perc_catchup_30_2010 = if_else(hasestimate, scen_perc_catchup_30_2010, NA_real_)) %>%
#     mutate(scen_bau = if_else(hasestimate, scen_bau, NA_real_)) %>%
#     rename(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg=scen_acceleration)
# }
