# add_cholera_campaign_scenarios <- function(df, scenario_ind, ind, scenario, iso3,
#                                            value, year, type_col, start_year,
#                                            end_year,
#                                            transform_value, df_surviving_infants){
#   a <- df %>%
#     scenario_cholera_roadmap(value = value,
#                              transform_value = transform_value,
#                              ind = ind,
#                              year = year,
#                              iso3 = iso3,
#                              type_col = type_col,
#                              sname = "scen_cholera_roadmap",
#                              baseline = start_year,
#                              target_year = end_year) %>%
#     dplyr::ungroup()
# }
#
# add_measles_routine_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                           year, type_col, start_year, end_year,
#                                           transform_value,
#                                           df_surviving_infants){
#
#   a <- df %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     scenario_aroc_historic_fixed_percent(value = value,
#                                          ind = ind,
#                                          scenario_ind = scenario_ind,
#                                          old_baseline = 2013,
#                                          baseline = start_year,
#                                          target_year = end_year,
#                                          fix_percent = 20,
#                                          type_col = type_col,
#                                          sname = NULL,
#                                          year = year,
#                                          iso3 = iso3,
#                                          neg_hold = TRUE,
#                                          df_surviving_infants = df_surviving_infants,
#                                          progress_cap = 99) %>%
#     dplyr::ungroup()
# }
#
# add_meningitis_routine_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value, year,
#                                              type_col, start_year, end_year,
#                                              transform_value, df_surviving_infants){
#
#   targets <- df %>%
#     dplyr::filter(.data[[ind]] == scenario_ind,  .data[[year]] == start_year) %>%
#     dplyr::mutate(
#       target_col = dplyr::case_when(
#         .data[[value]] >= 0 ~ 90,
#         TRUE ~ NA_real_
#       ),
#       target_year = dplyr::case_when(
#         .data[[value]] >= 0 ~ 2030,
#         TRUE ~ NA_real_
#       )) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], target_col, target_year)
#
#   a <- df %>%
#     dplyr::left_join(targets, by = c(iso3, year)) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     tidyr::fill(target_col, target_year, .direction = "downup") %>%
#     scenario_target_col_year_col(
#       value = value,
#       baseline = start_year,
#       target_col = "target_col",
#       target_year = "target_year",
#       sname = "scen_37.5_90_2030",
#       scenario_ind = scenario_ind,
#       small_is_best = FALSE,
#       year = year,
#       iso3 = iso3,
#       ind = ind,
#       df_surviving_infants = df_surviving_infants,
#       progress_cap = 99) %>%
#     dplyr::ungroup()
# }
#
# add_polio_routine_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                         year, type_col, start_year = 2018, end_year,
#                                         transform_value, df_surviving_infants){
#
#   df %>%
#     scenario_aroc_historic_fixed_percent(value = value,
#                                          ind = ind,
#                                          scenario_ind = scenario_ind,
#                                          old_baseline = 2015,
#                                          baseline = start_year,
#                                          target_year = end_year,
#                                          fix_percent = 20,
#                                          type_col = type_col,
#                                          sname = NULL,
#                                          year = year,
#                                          iso3 = iso3,
#                                          neg_hold = TRUE,
#                                          df_surviving_infants = df_surviving_infants,
#                                          progress_cap = 100) %>%
#     dplyr::ungroup()-> a
#
# }
#
# add_yellow_fever_campaign_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                                 year, type_col, start_year = 2018,
#                                                 end_year, transform_value,
#                                                 df_surviving_infants){
#
#   scenario_planned_campaign_yellow_fever(df,
#                                          value = value,
#                                          ind = ind,
#                                          scenario_ind = scenario_ind,
#                                          year = year,
#                                          iso3 = iso3,
#                                          type_col = type_col,
#                                          target_year = end_year,
#                                          baseline = start_year,
#                                          sname = "scen_best_performance",
#                                          planned_campaign_file =  "data/yellow_fever/yellow_fever_campaign_planned.csv") %>%
#     dplyr::ungroup()
#
# }
#
#
# add_yellow_fever_routine_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                                year, type_col, start_year = 2018,
#                                                end_year, transform_value,
#                                                df_surviving_infants){
#   a <- df %>%
#     scenario_aroc_historic_fixed_percent(value = value,
#                                          ind = ind,
#                                          scenario_ind = scenario_ind,
#                                          old_baseline = 2015,
#                                          baseline = start_year,
#                                          target_year = end_year,
#                                          fix_percent = 20,
#                                          type_col = type_col,
#                                          sname = NULL,
#                                          year = year,
#                                          iso3 = iso3,
#                                          neg_hold = TRUE,
#                                          df_surviving_infants = df_surviving_infants,
#                                          progress_cap = 100) %>%
#     dplyr::ungroup()
#
# }
#
#
# add_meningitis_campaign_scenarios  <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                                year, type_col, start_year = 2018,
#                                                end_year, transform_value,
#                                                df_surviving_infants){
#
#   a<-df %>%
#     scenario_planned_campaign_meningitis(value = value,
#                                          ind = ind,
#                                          scenario_ind = scenario_ind,
#                                          year = year,
#                                          iso3 = iso3,
#                                          type_col = type_col,
#                                          years_best_performance = 2015:2018,
#                                          baseline = start_year,
#                                          sname = "scen_best_performance",
#                                          planned_campaign_file =  "data/meningitis/meningitis_campaign_planned.csv") %>%
#     dplyr::ungroup()
# }
#
# add_detect_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                  year, type_col, start_year = 2018,
#                                  end_year, transform_value,
#                                  df_surviving_infants){
#
#   df %>%
#     scenario_bau() %>%
#     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
#     tidyr::fill("scen_bau", .direction = "up")
# }
#
#
# add_respond_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                   year, type_col, start_year = 2018,
#                                   end_year, transform_value,
#                                   df_surviving_infants){
#
#   df %>%
#     scenario_bau(baseline = start_year) %>%
#     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
#     tidyr::fill("scen_bau", .direction = "up")
# }
#
#
# add_surviving_infants_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                             year, type_col, start_year = 2018,
#                                             end_year, transform_value,
#                                             df_surviving_infants){
#
#   df %>%
#     dplyr::mutate(!!sym("scen_proj"):= .data[[value]])
# }
#
#
# # add_prevent_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
# #                                  year, type_col, transform_value,
# #                                  baseline = 2018){
# #
# #   df %>%
# #     scenario_bau(value = value) %>%
# #     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
# #     tidyr::fill("scen_bau", .direction = "up")
# # }
# #
#
# add_detect_respond_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                          year, type_col, start_year = 2018,
#                                          end_year, transform_value,
#                                          df_surviving_infants){
#   df %>%
#     scenario_bau() %>%
#     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
#     tidyr::fill("scen_bau", .direction = "up")
# }
#
# add_notify_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                  year, type_col, start_year = 2018, end_year,
#                                  transform_value, df_surviving_infants){
#   df %>%
#     scenario_bau() %>%
#     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
#     tidyr::fill("scen_bau", .direction = "up")
# }
#
#
#
# add_espar_scenarios <- function(df, scenario_ind, ind, scenario, iso3, value,
#                                 year, type_col, start_year = 2018, end_year = 2025,
#                                 transform_value, df_surviving_infants) {
#
#   espar_target <- readr::read_csv("data/saved/spar_target_202108.csv") #%>% select(-`...1`) # show_col_types = FALSE) %>%
#   # dplyr::select(-`...1`)
#
#   if(length(end_year) > 1){
#     end_year <- max(end_year)
#   }
#
#   alle <- df %>% left_join(espar_target) %>% #select(-year)) %>%
#     group_by(iso3) %>%
#     mutate(hasdata = any(!is.na(value) & type == "reported")) #%>%
#   #mutate(last_year=max(year[hasdata],2018))
#
#   missing <- alle %>% filter(is.na(target)) %>% select(-baseline) %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau)
#
#   all2019 <- alle %>% filter(baseline == 2019) %>%
#     group_by(ind, iso3) %>%
#     scenario_target_col(baseline = 2019,
#                         target_col = "target",
#                         target_year = end_year) %>%
#     #scenario_target_col_max(baseline=2019,target_col="target",target_year=2023,sname="smax") %>%
#     group_by(iso3) %>%
#     mutate(fill_value = min(value[type == "reported" &
#                                     year >=2018 & year<= 2019])) %>%
#     ungroup() %>%
#     arrange(ind, iso3, -year) %>%
#     mutate(scen_target = if_else(year == 2018 &
#                                    is.na(scen_target), fill_value, scen_target)) %>%
#     rename(scen_acceleration = scen_target)
#
#   all2020 <- alle %>% filter(baseline == 2020) %>%
#     group_by(ind, iso3) %>%
#     scenario_target_col(baseline = 2020,
#                         target_col = "target",
#                         target_year = end_year) %>%
#     group_by(iso3) %>%
#     mutate(fill_value = min(value[type == "reported" &
#                                     year >=2018 & year<= 2020])) %>%
#     ungroup() %>%
#     arrange(ind, iso3, -year) %>%
#     mutate(scen_target = if_else(year == 2019 &
#                                    is.na(scen_target), fill_value, scen_target)) %>%
#     mutate(scen_target = if_else(year == 2018 &
#                                    is.na(scen_target), fill_value, scen_target)) %>%
#     rename(scen_acceleration = scen_target)
#
#   all2018 <- alle %>% filter(baseline == 2018) %>%
#     group_by(ind, iso3) %>%
#     scenario_target_col(baseline = 2018,
#                         target_col = "target",
#                         target_year = end_year) %>%
#     arrange(ind, iso3, -year) %>%
#     rename(scen_acceleration = scen_target)
#
#   missing %>% bind_rows(all2018) %>% bind_rows(all2019) %>% bind_rows(all2020)   %>%
#     select(-target,-hasdata)
# }
#
#
# # add_hep_idx_scenarios <- function(df, espar, scenario_ind, ind, scenario, iso3, value,
# #                                   year, type_col, transform_value,
# #                                   baseline = 2018){
# #   df %>%
# #     scenario_bau() %>%
# #     dplyr::arrange(.data[[iso3]], .data[[year]], .data[[ind]]) %>%
# #     tidyr::fill("scen_bau", .direction = "up")
# # }
#
# scenario_cholera_roadmap <- function(df,
#                                      value = "value",
#                                      transform_value = "transform_value",
#                                      ind = "ind",
#                                      year = "year",
#                                      iso3 = "iso3",
#                                      type_col = "type",
#                                      baseline = 2018,
#                                      target_year = 2025,
#                                      sname = "scen_cholera_roadmap"){
#
#   raw_global_cholera_roadmap <- readr::read_csv("data/cholera/cholera_roadmap_2030.csv") %>%
#     # show_col_types = FALSE) %>%
#     tidyr::pivot_longer(-iso3, names_to = c(year, ind), values_to = sname, names_pattern = "([0-9]{4})_(.*)") %>%
#     dplyr::mutate(year = as.numeric(.data[[year]])) %>%
#     dplyr::mutate(!!sym(ind) := dplyr::case_when(
#       .data[[ind]] == "campaign_coverage" ~ "cholera_campaign",
#       .data[[ind]] == "vaccinated_population" ~ "cholera_campaign_num",
#       .data[[ind]] == "targeted_population" ~ "cholera_campaign_denom",
#       TRUE ~ .data[[ind]])
#     )
#
#   global_cholera_roadmap_target <- raw_global_cholera_roadmap %>%
#     dplyr::filter(.data[[year]] == 2030, .data[[ind]] == "cholera_campaign_denom") %>%
#     dplyr::mutate(yearly_target_cholera_2030 = dplyr::case_when(
#       is.na(.data[[sname]]) ~ 0,
#       TRUE ~.data[[sname]]/12)) %>%
#     dplyr::select(-.data[[year]], -.data[[sname]])
#
#   global_cholera_roadmap <- raw_global_cholera_roadmap %>%
#     dplyr::filter(.data[[year]] <= target_year) %>%
#     dplyr::left_join(global_cholera_roadmap_target, by = c( iso3, ind)) %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       !is.na(.data[[sname]]) & !is.na(yearly_target_cholera_2030) & .data[[year]] >= baseline  ~ yearly_target_cholera_2030,
#       TRUE ~ .data[[sname]]
#     )) %>%
#     dplyr::select(-yearly_target_cholera_2030)
#
#   best_historical_perf_campaign <- df %>%
#     dplyr::filter(.data[[year]] <= baseline) %>%
#     tidyr::pivot_wider(names_from = ind, values_from = value) %>%
#     dplyr::mutate(cov = cholera_campaign_num / cholera_campaign_denom) %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::filter(!is.na(cov)) %>%
#     dplyr::summarise(best_perf = max(cov))
#
#   best_in_region <- best_historical_perf_campaign %>%
#     dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]]), region = "who_region") %>%
#     dplyr::group_by(who_region) %>%
#     dplyr::filter(!is.na(best_perf)) %>%
#     dplyr::summarise(best_perf_region = max(best_perf))
#
#   best_perf_binded <- global_cholera_roadmap %>%
#     dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]])) %>%
#     dplyr::filter(.data[[ind]] == "cholera_campaign_num",  .data[[sname]]>0) %>%
#     dplyr::left_join(best_historical_perf_campaign, by = c(iso3)) %>%
#     dplyr::left_join(best_in_region, by = c("who_region")) %>%
#     dplyr::mutate(best_perf = dplyr::case_when(
#       is.na(best_perf) ~ best_perf_region,
#       TRUE ~ best_perf
#     )) %>%
#     dplyr::select(.data[[iso3]], best_perf) %>%
#     dplyr::distinct()
#
#   iso3_no_historical <- dplyr::setdiff(unique(global_cholera_roadmap[[iso3]]), unique(best_perf_binded[[iso3]]))
#
#   cholera_roadmap_num <- global_cholera_roadmap %>%
#     dplyr::mutate(who_region = whoville::iso3_to_regions(.data[[iso3]])) %>%
#     dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
#     dplyr::left_join(best_historical_perf_campaign, by = iso3) %>%
#     dplyr::left_join(best_in_region, by = "who_region") %>%
#     dplyr::mutate(num = dplyr::case_when(
#       .data[[iso3]] %in% iso3_no_historical ~ NA_real_,
#       is.na(best_perf) ~ .data[[sname]]*(best_perf_region),
#       TRUE ~ .data[[sname]]*(best_perf)),
#       !!sym(ind) := "cholera_campaign_num",
#       !!sym(sname):= .data[["num"]]) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   last_observed_year <- df %>%
#     dplyr::filter(.data[[type_col]] != "projected") %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::summarise(max_year = max(year))
#
#   full_table <- tidyr::expand_grid(
#     iso3 = unique(global_cholera_roadmap$iso3),
#     ind = unique(global_cholera_roadmap$ind),
#     year = baseline:target_year
#   )
#
#   planned_historical_num <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(cholera_roadmap_num, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = c(iso3)) %>%
#     dplyr::filter(.data[[ind]] == "cholera_campaign_num") %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     ))
#
#   cholera_roadmap_denom <- global_cholera_roadmap %>%
#     dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       .data[[iso3]] %in% iso3_no_historical ~ NA_real_,
#       TRUE ~  .data[[sname]]
#     ))
#
#   planned_historical_denom <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(cholera_roadmap_denom, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = c(iso3)) %>%
#     dplyr::filter(.data[[ind]] == "cholera_campaign_denom") %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     ))
#
#   final_binded <- bind_rows(planned_historical_num, planned_historical_denom) %>%
#     dplyr::filter(!.data[[iso3]] %in% iso3_no_historical) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   full_table <- final_binded %>% dplyr::select(-.data[[sname]])
#
#   a <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(final_binded, by = c(iso3, year, ind)) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       is.na(.data[[sname]]) ~ .data[["value"]],
#       TRUE ~ .data[[sname]]
#     )) %>% distinct()
# }
#
#
# scenario_planned_campaign_yellow_fever <- function(df,
#                                                    value = "value",
#                                                    ind = "ind",
#                                                    scenario_ind = "yellow_fever_campaign",
#                                                    year = "year",
#                                                    iso3 = "iso3",
#                                                    type_col = "type",
#                                                    baseline = 2018,
#                                                    target_year = 2025,
#                                                    sname = "scen_best_performance",
#                                                    planned_campaign_file){
#
#   #TODO: generalize to take all type of planned campaigns (shouldn't be too hard)
#
#   planned_campaign_data <- readr::read_csv(planned_campaign_file)%>%
#     # show_col_types = FALSE) %>%
#     dplyr::rename_with(~stringr::str_replace_all(.x, c("campaign_vaccinated_population" = paste0(scenario_ind, "_num"),
#                                                        "campaign_coverage" = scenario_ind,
#                                                        "campaign_targeted_population" = paste0(scenario_ind, "_denom")))) %>%
#     tidyr::pivot_longer(-iso3, names_to = c(year, ind), values_to = "planned_campaign_values", names_pattern = "([0-9]{4})_(.*)",
#                         names_transform = list(year = as.integer))
#
#   best_perf <- df %>%
#     dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
#     tidyr::pivot_wider(names_from = ind, values_from = value) %>%
#     dplyr::mutate(yellow_fever_campaign = yellow_fever_campaign_num/yellow_fever_campaign_denom*100) %>%
#     dplyr::ungroup()
#
#   best_historical_perf <- best_perf %>%
#     dplyr::filter(.data[[type_col]] != "projected", .data[[year]] <= baseline) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], yellow_fever_campaign) %>%
#     tidyr::pivot_longer(yellow_fever_campaign,names_to = ind, values_to = value) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::mutate(value = dplyr::case_when(is.na(value) ~ 0,
#                                            value > 100 ~ 100,
#                                            TRUE ~ value)) %>%
#     dplyr::summarise(best_perf_hist = max(value, na.rm = TRUE)) %>%
#     dplyr::ungroup()
#
#   replacement_hist_no_avail <- planned_campaign_data %>%
#     dplyr::filter(.data[[year]] == baseline,
#                   planned_campaign_values >0,
#                   .data[[ind]] == scenario_ind) %>%
#     dplyr::summarise(avg_2018_perfs = mean(planned_campaign_values)) %>%
#     dplyr::mutate(avg_2018_perfs = dplyr::case_when(
#       avg_2018_perfs > 100 ~ 100,
#       TRUE ~ avg_2018_perfs
#     )) %>% pull()
#
#   planned_historical_num_denom <- planned_campaign_data %>%
#     dplyr::filter(stringr::str_detect(.data[[ind]], "_denom$"),
#                   .data[[year]] >= baseline,
#                   !is.na(planned_campaign_values)) %>%
#     dplyr::left_join(best_historical_perf, by = iso3) %>%
#     dplyr::rename(yellow_fever_campaign_denom = planned_campaign_values) %>%
#     dplyr::mutate(yellow_fever_campaign_num = dplyr::case_when(
#       is.na(best_perf_hist) ~ yellow_fever_campaign_denom * (replacement_hist_no_avail),
#       TRUE ~ yellow_fever_campaign_denom * (best_perf_hist/100)
#     ),
#     yellow_fever_campaign = yellow_fever_campaign_num/yellow_fever_campaign_denom) %>%
#     dplyr::select(-.data[[ind]],-best_perf_hist) %>%
#     dplyr::group_by(.data[[iso3]], .data[[year]]) %>%
#     tidyr::pivot_longer(dplyr::starts_with("yellow_fever"),names_to = as.character(ind), values_to = sname) %>%
#     distinct()
#
#   last_observed_year <- df %>%
#     dplyr::filter(.data[[type_col]]!="projected") %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::summarise(max_year = max(year))
#
#   full_table <- tidyr::expand_grid(
#     iso3 = unique(planned_historical_num_denom$iso3),
#     ind = unique(planned_historical_num_denom$ind),
#     year = baseline:target_year
#   )
#
#   planned_historical_num <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(planned_historical_num_denom, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = c(iso3)) %>%
#     dplyr::filter(.data[[ind]] == "yellow_fever_campaign_num") %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     )) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[year]] < max_year & is.na(.data[[value]]) ~ NA_real_,
#       TRUE ~ .data[[sname]]
#     ))
#
#   planned_historical_denom <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(planned_historical_num_denom, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = c(iso3)) %>%
#     dplyr::filter(.data[[ind]] == "yellow_fever_campaign_denom") %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     )) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[year]] < max_year & is.na(.data[[value]]) ~ NA_real_,
#       TRUE ~ .data[[sname]]
#     ))
#
#   final_binded <- bind_rows(planned_historical_num, planned_historical_denom) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   full_table <- final_binded %>% dplyr::select(-.data[[sname]])
#
#   df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(final_binded, by = c(iso3, year, ind)) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       is.na(.data[[sname]]) ~ .data[["value"]],
#       TRUE ~ .data[[sname]]
#     )) %>% distinct()
# }
#
# scenario_target_col_year_col <- function(df,
#                                          value = "value",
#                                          baseline = 2018,
#                                          target_col = "target",
#                                          target_year = "target_year",
#                                          sname,
#                                          small_is_best = FALSE,
#                                          scenario_ind = "meningitis_routine",
#                                          year = "year",
#                                          ind = "ind",
#                                          iso3 = "iso3",
#                                          df_surviving_infants = df_surviving_infants,
#                                          progress_cap = 95) {
#   # use target column to say what the target is
#   if (!hasArg(sname)) {
#     sname = paste0("scen_target")
#   }
#
#   baseline_values <- df %>%
#     dplyr::filter(.data[[year]] == baseline) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::mutate(baseline_value = .data[[value]],
#                   annual_change = (.data[[target_col]]-.data[[value]])/(.data[[target_year]]-baseline)) %>%
#     dplyr::select(.data[[iso3]], .data[[ind]], baseline_value, annual_change)
#
#   target_routine <- df %>%
#     dplyr::filter(.data[[ind]] == scenario_ind) %>%
#     dplyr::left_join(baseline_values, by = c(iso3, ind)) %>%
#     dplyr::mutate(!!sym(sname):= dplyr::case_when(
#       .data[[year]] >= baseline & annual_change > 0 & !small_is_best ~ baseline_value + (annual_change * (.data[[year]]-baseline)),
#       .data[[year]] >= baseline & annual_change < 0 & !small_is_best ~ baseline_value - (annual_change * (.data[[year]]-baseline)),
#       TRUE ~ .data[[value]]
#     ),
#     !!sym(sname):= dplyr::case_when(
#       .data[[sname]] > progress_cap ~ baseline_value,
#       TRUE ~ .data[[sname]]
#     )
#     ) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   df_surviving_infants <- df_surviving_infants %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[value]]) %>%
#     dplyr::rename(surviving_infants = !!sym(value))
#
#   target_num <- df %>%
#     dplyr::left_join(df_surviving_infants, by = c(iso3, year)) %>%
#     dplyr::left_join(target_routine, by = c(iso3, year, ind)) %>%
#     dplyr::filter(.data[[ind]] == scenario_ind) %>%
#     dplyr::mutate(!!sym(sname) := surviving_infants * .data[[sname]]/100,
#                   !!sym(ind) := paste0(scenario_ind, "_num"))
#
#   binded <- dplyr::bind_rows(target_routine, target_num) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   df %>%
#     dplyr::left_join(binded, by = c(iso3, ind, year)) %>%
#     dplyr::select(-.data[[target_col]], .data[[target_year]])-> a
# }
#
# scenario_planned_campaign_meningitis <- function(df,
#                                                  value = "value",
#                                                  ind = "ind",
#                                                  scenario_ind = "meningitis_campaign",
#                                                  year = "year",
#                                                  iso3 = "iso3",
#                                                  type_col = "type",
#                                                  years_best_performance = 2015:2018,
#                                                  baseline = 2018,
#                                                  sname = "scen_best_performance",
#                                                  planned_campaign_file){
#
#   planned_campaign_data <- readr::read_csv(planned_campaign_file) %>% #show_col_types = FALSE) %>%
#     dplyr::rename_with(~stringr::str_replace_all(.x, c("campaign_vaccinated_population" = paste0(scenario_ind, "_num"),
#                                                        "campaign_coverage" = scenario_ind,
#                                                        "campaign_targeted_population" = paste0(scenario_ind, "_denom")))) %>%
#     tidyr::pivot_longer(-iso3, names_to = c(year, ind), values_to = "planned_campaign_values", names_pattern = "([0-9]{4})_(.*)",
#                         names_transform = list(year = as.integer)) %>%
#     dplyr::mutate(!!sym("planned_campaign_values") := dplyr::case_when(
#       .data[[ind]] == scenario_ind ~ .data[["planned_campaign_values"]]*100,
#       TRUE ~ .data[["planned_campaign_values"]])) %>%
#     dplyr::filter(!is.na(planned_campaign_values)) %>%
#     dplyr::filter(planned_campaign_values > 0)
#
#   best_historical_perf <- df %>%
#     dplyr::filter(.data[[year]] <= baseline,
#                   !is.na(.data[[value]])) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     tidyr::pivot_wider(names_from = ind, values_from = value) %>%
#     dplyr::mutate(
#       cov = !!sym(glue::glue("{scenario_ind}_num")) / !!sym(glue::glue("{scenario_ind}_denom"))*100,
#       best_perf = dplyr::case_when(is.na(cov) ~ 0,
#                                    cov > 100 ~ 100,
#                                    TRUE ~ cov)) %>%
#     dplyr::summarise(best_perf_hist = max(best_perf, na.rm = TRUE)) %>%
#     dplyr::ungroup()
#
#   replacement_hist_no_avail <- planned_campaign_data %>%
#     dplyr::filter(.data[[year]] <= baseline ,
#                   planned_campaign_values >1,
#                   .data[[ind]] == scenario_ind) %>%
#     dplyr::summarise(avg_perfs = mean(planned_campaign_values)) %>%
#     dplyr::mutate(avg_perfs = dplyr::case_when(
#       avg_perfs > 100 ~ 100,
#       TRUE ~ avg_perfs
#     )) %>% pull()
#
#   last_observed_year <- df %>%
#     dplyr::filter(.data[[type_col]]!="projected") %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::summarise(max_year = max(year))
#
#   planned_denom <- planned_campaign_data %>%
#     dplyr::filter(stringr::str_detect(.data[[ind]], "_denom$"),
#                   .data[[year]] > max(years_best_performance)) %>%
#     dplyr::rename(!!sym(sname) := planned_campaign_values) %>%
#     dplyr::distinct()
#
#   planned_num <- planned_denom %>%
#     dplyr::left_join(best_historical_perf, by = c(iso3)) %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       is.na(best_perf_hist) ~ .data[[sname]]*replacement_hist_no_avail/100,
#       TRUE ~ .data[[sname]]*best_perf_hist/100
#     ),
#     !!sym(ind) := glue::glue("{scenario_ind}_num")) %>%
#     dplyr::select(-best_perf_hist)
#
#   full_table <- tidyr::expand_grid(
#     iso3 = c(unique(df$iso3), unique(planned_campaign_data$iso3)),
#     year = unique(df$year),
#     ind = unique(df$ind)
#   )
#
#   planned_historical_num <- df %>%
#     dplyr::left_join(planned_num, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = iso3) %>%
#     dplyr::filter(.data[[ind]] == glue::glue("{scenario_ind}_num")) %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     )) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[year]] < max_year & is.na(.data[[value]]) ~ NA_real_,
#       TRUE ~ .data[[sname]]
#     ))
#
#   planned_historical_denom <- df %>%
#     dplyr::left_join(planned_denom, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(last_observed_year, by = c(iso3)) %>%
#     dplyr::filter(.data[[ind]] == glue::glue("{scenario_ind}_denom")) %>%
#     dplyr::group_by(iso3) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[type_col]] != "projected" & !is.na(.data[[value]]) ~ .data[[value]],
#       TRUE ~ .data[[sname]]
#     )) %>%
#     dplyr::mutate(!!sym(sname)  := dplyr::case_when(
#       .data[[year]] < max_year & is.na(.data[[value]]) ~ NA_real_,
#       TRUE ~ .data[[sname]]
#     ))
#
#   final_binded <- bind_rows(planned_historical_num, planned_historical_denom) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[ind]], .data[[sname]])
#
#   full_table <- final_binded %>% dplyr::select(-.data[[sname]])
#
#   a <- df %>%
#     dplyr::full_join(full_table, by = c(iso3, year, ind)) %>%
#     dplyr::left_join(final_binded, by = c(iso3, year, ind)) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::mutate(!!sym(sname) := dplyr::case_when(
#       is.na(.data[[sname]]) ~ .data[["value"]],
#       TRUE ~ .data[[sname]]
#     )) %>% distinct()
#
# }
#
# scenario_aroc_historic_fixed_percent <- function(df,
#                                                  value = "value",
#                                                  ind = "ind",
#                                                  scenario_ind,
#                                                  old_baseline = 2015,
#                                                  baseline = 2018,
#                                                  target_year = 2025,
#                                                  fix_percent = 20,
#                                                  type_col = type_col,
#                                                  sname = NULL,
#                                                  iso3 = "iso3",
#                                                  year = "year",
#                                                  neg_hold = FALSE,
#                                                  df_surviving_infants = df_surviving_infants,
#                                                  progress_cap = 99){
#
#   if(is.null(sname)){
#     sname <- glue::glue("scen_aroc_historic_{fix_percent}_{target_year}")
#   }
#
#   aroc <- df %>%
#     dplyr::filter(.data[[ind]] == scenario_ind) %>%
#     dplyr::group_by(.data[[iso3]]) %>%
#     dplyr::filter(.data[[year]] %in% c(old_baseline,baseline)) %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[value]]) %>%
#     tidyr::pivot_wider(values_from = value , names_from = year) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(aroc = (!!sym(as.character(baseline))-!!sym(as.character(old_baseline)))/(baseline-old_baseline)*(1+fix_percent/100)) %>%
#     dplyr::select(.data[[iso3]], aroc)
#
#
#   if(neg_hold){
#     aroc <- aroc %>% mutate(aroc = pmax(0, aroc))
#   }
#
#   df_surviving_infants <- df_surviving_infants %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[value]]) %>%
#     dplyr::rename(surviving_infants = !!sym(value))
#
#   proj_cov <- df %>%
#     dplyr::filter(.data[[ind]] == scenario_ind) %>%
#     dplyr::left_join(aroc, by = iso3) %>%
#     dplyr::group_by(.data[[iso3]], .data[[ind]]) %>%
#     dplyr::mutate(baseline_value = dplyr::if_else(.data[[year]] == baseline, .data[[value]], NA_real_)) %>%
#     tidyr::fill(baseline_value, .direction = "updown") %>%
#     dplyr::mutate(
#       !!sym(paste0(sname,"_cov")) := dplyr::case_when(
#         .data[[year]] <= baseline ~ .data[[value]],
#         .data[[year]] > baseline & .data[["aroc"]] > 0 ~ baseline_value + ((.data[["aroc"]])*(.data[[year]] - baseline)),
#         .data[["aroc"]] == 0 & .data[[year]] > baseline ~ baseline_value,
#         TRUE ~ .data[[value]]
#       )) %>%
#     dplyr::mutate(
#       !!sym(paste0(sname,"_cov")) := dplyr::case_when(
#         .data[[paste0(sname,"_cov")]] > progress_cap ~ progress_cap,
#         TRUE ~  .data[[paste0(sname,"_cov")]]
#       )) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(.data[[iso3]], .data[[year]], .data[[paste0(sname,"_cov")]])
#
#   a <-  df %>%
#     dplyr::left_join(proj_cov, by = c(iso3, year)) %>%
#     dplyr::left_join(df_surviving_infants, by = c(iso3, year)) %>%
#     dplyr::mutate(!!sym(sname):= dplyr::case_when(
#       str_detect(.data[[ind]], "_num$") ~ .data[[paste0(sname, "_cov")]]/100 *.data[["surviving_infants"]],
#       .data[[ind]]  == scenario_ind ~ .data[[paste0(sname, "_cov")]]
#     )) %>%
#     dplyr::select(-.data[[paste0(sname, "_cov")]], -.data[["surviving_infants"]])
# }
