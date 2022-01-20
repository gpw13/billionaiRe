# add_fpg_scenarios <- function(df) {
#   add_default_scenarios(df)
# }
#
# add_fh_scenarios <- function(df) {
#
#   df %>%scenario_halt() %>%
#     scenario_bau() %>%
#     scenario_bestof(c("scen_halt_2018", "scen_bau"), minv = TRUE) %>%
#     rename(scen_acceleration = scen_bestof)
# }
#
# add_doctors_scenarios <- function(df) {
#   add_default_scenarios(df)
# }
# add_nurses_scenarios <- function(df) {
#   add_default_scenarios(df)
# }
#
#
#
# add_anc4_scenarios <- function(df) {
#   withdata_df <- df %>%
#     group_by(iso3) %>%
#     filter(sum(type == "reported",na.rm=TRUE) > 1) %>%
#     ungroup() %>%
#     #easiest out of 95% by 2030 and 2.6/year which is top 10 performers
#     #if bau is better take that
#     scenario_bau() %>%
#     scenario_fixed_target(target_value = 95,
#                           target_year = 2030,
#                           sname = "scen_95_2030") %>%
#     mutate("linear" = 2.6) %>%
#     scenario_lin(sname = "scen_linear") %>%
#     scenario_bestof(c("scen_95_2030", "scen_linear"),
#                     minv = TRUE,
#                     sname = "scen_join") %>%
#     select(-bestname) %>%
#     scenario_bestof(c("scen_join", "scen_bau"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_bestof) %>%
#     mutate(scen_sdg = scen_95_2030) %>%
#     select(-bestname,-scen_linear,-scen_join)
#   withoutdata_df <- df %>%
#     group_by(iso3) %>%
#     filter(sum(type == "reported",na.rm=TRUE) <= 1) %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_bau)
#   withdata_df %>% bind_rows(withoutdata_df)
# }
#
# add_tb_scenarios <- function(df) {
#   df %>%
#     scenario_bau() %>%
#     scenario_fixed_target(target_value = 90,
#                           target_year = 2025,
#                           sname = "scen_90_2025") %>%
#     mutate(scen_acceleration = scen_90_2025, scen_sdg = scen_acceleration)
# }
#
#
# add_fp_scenarios <- function(df) {
#   exclude_fp <-
#     c(
#       "AND" ,
#       "BRN",
#       "COK",
#       "CYP",
#       "DMA",
#       "FSM",
#       "ISL",
#       "KNA",
#       "LUX",
#       "MCO",
#       "MHL",
#       "NIU",
#       "NRU",
#       "PLW",
#       "SMR",
#       "SYC",
#       "TUV"
#     )
#   df_exclude <- df %>% filter(iso3 %in% exclude_fp) %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_acceleration)
#   #use quintiles but capped by regional max in 2018
#   df_main <- df %>% filter(!(iso3 %in% exclude_fp)) %>%
#     scenario_bau() %>%
#     scenario_quintile() %>%
#     mutate(region = iso3_to_regions(iso3)) %>%
#     group_by(region) %>%
#     mutate(max_region = max(value[year == 2018])) %>%
#     mutate(scen_quintile = pmin(max_region, scen_quintile)) %>%
#     ungroup() %>%
#     scenario_bestof(c("scen_bau", "scen_quintile"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_bestof, scen_sdg = scen_acceleration) %>%
#     select(-bestname,-region, -max_region)
#   df_main %>% bind_rows(df_exclude)
# }
#
# add_dtp3_scenarios <- function(df) {
#   df %>%
#     scenario_bau() %>%
#     scenario_dtp3(baseline = 2019,
#                   targets = dtp3_input,
#                   target_year = 2030) %>%
#     mutate(scen_acceleration = scen_target_2030, scen_sdg = scen_acceleration)
# }
#
#
#
# add_pneumo_scenarios <- function(df) {
#   df %>% # updated aug to 2025 from 2030
#     scenario_bau() %>%
#     scenario_fixed_target(
#       target_value = 90,
#       target_year = 2025,
#       sname = "scen_90_2025",
#       small_is_best=FALSE) %>%
#     scenario_bestof(c("scen_bau", "scen_90_2025"), minv = FALSE) %>%
#     rename(scen_acceleration = scen_bestof)%>%
#     mutate( scen_sdg = scen_acceleration)
# }
#
#
# add_hwf_scenarios <- function(df) {
#   #TODO may need to set these back to no acceleration scenario
#   withscenario_df <- df %>%
#     mutate(glob_med=median(value[year==2018]))%>%
#     group_by(iso3) %>%
#     filter(any(value < glob_med & year==2018)) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     mutate("linear" = 4.54) %>%
#     scenario_lin(sname = "scen_linear") %>%
#     scenario_bestof(c("scen_bau", "scen_linear"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_linear, scen_sdg = scen_acceleration)
#   noscenario_df <- df %>%
#     mutate(glob_med=median(value[year==2018]))%>%
#     group_by(iso3) %>%
#     filter(!any(value < glob_med & year==2018)) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     mutate("scen_linear" = scen_bau) %>%
#     scenario_bestof(c("scen_bau", "scen_linear"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_acceleration)
#   withscenario_df %>% bind_rows(noscenario_df)
# }
#
#
# add_uhc_sanitation_scenarios <- function(df) {
#   df %>%
#     scenario_bau() %>%
#     scenario_quintile(
#       quintileyear = 2017,
#       baseline = 2018,
#       trim_min = 0,
#       trim_max = 99,
#       sname = "scen_quintile_2017"
#     ) %>%
#     mutate(scen_acceleration = scen_quintile_2017, scen_sdg = scen_acceleration)
# }
#
# add_itn_scenarios <- function(df) {
#   #check that non itn countries just end up with NAs
#   df %>%
#     scenario_bau() %>%
#     scenario_fixed_target(target_value = 80,
#                           target_year = 2030,
#                           sname = "scen_80_2030")  %>%
#     scenario_bestof(c("scen_bau", "scen_80_2030"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_bestof, scen_sdg = scen_acceleration)
# }
#
# add_art_scenarios <- function(df) {
#   withdata_df <- df %>%
#     group_by(iso3) %>%
#     filter(sum(type =="estimated"| type=="reported" & year >= 2000 & year <= 2018) > 1) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     scenario_fixed_target(target_value = 90.25,
#                           target_year = 2025,
#                           sname = "scen_90_25_2025")  %>%
#     mutate(scen_90_25_2025 = pmin(scen_90_25_2025, 95)) %>%
#     scenario_bestof(c("scen_bau", "scen_90_25_2025"), minv = FALSE) %>%
#     mutate(scen_acceleration = scen_bestof, scen_sdg = scen_acceleration)
#
#   withoutdata_df <- df %>%
#     group_by(iso3) %>%
#     filter(sum(type  %in% c("estimated","reported")  & year >= 2000 &
#                  year <= 2018) <= 1) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_acceleration)
#   withdata_df %>% bind_rows(withoutdata_df)
# }
#
# add_uhc_tobacco_scenarios <- function(df) {
#   #fudge factos to tob ratios need to be fixed
#   tobacco_ratio <-tobacco_ratio %>%
#     bind_rows(tobacco_ratio%>%filter(year==2023) %>%mutate(year=2024))%>%
#     bind_rows(tobacco_ratio%>%filter(year==2023) %>%mutate(year=2025))
#   tobm<-tobacco_ratio %>% group_by(year)%>% summarise(m=mean(ratio_agestd_over_crude))
#   without_df <- df %>%
#     group_by(iso3) %>%
#     filter(!any(type == "estimated")) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_acceleration)
#   with_df <- df %>%
#     group_by(iso3) %>%
#     filter(any(type == "estimated")) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     #convert to crude, run hpop scenario on crude, convert back to agestd
#     #NB cannot take hpop outputs because the imputed data (45 coutnries) is removed for hpop tobacco
#     left_join(tobacco_ratio) %>%
#     left_join(tobm) %>%
#     mutate(ratio_agestd_over_crude= ifelse(is.na(ratio_agestd_over_crude),m, ratio_agestd_over_crude))%>%
#     select(-m) %>%
#     mutate(crude = value / ratio_agestd_over_crude) %>%
#     scenario_bau(value = "crude", sname = "scen_crude_bau") %>%
#     #nonstandard because tobacco aims for 2025 to be on ncd trajectory
#     scenario_percent_baseline_tobacco(
#       percent = 30,
#       value = "crude",
#       baseline = 2018,
#       old_baseline = 2010,
#       end_year=2025,
#       target_year = 2025
#     ) %>%
#     scenario_bestof(c("scen_crude_bau", "scen_perc_catchup_30_2010")) %>%
#     mutate(scen_bestof = scen_bestof * ratio_agestd_over_crude) %>%
#     mutate(scen_acceleration = scen_bestof, scen_sdg = scen_acceleration) %>%
#     select(-crude, -ratio_agestd_over_crude, scen_crude_bau)
#   with_df %>% bind_rows(without_df)
#   print("here")
#   return(with_df)
# }
#
# add_beds_scenarios <- function(df) {
#   #only do scenarios where values are less than 18
#
#   withscenario_df <- df %>%
#     group_by(iso3) %>%
#     filter(any((value < 18 & year >=2018))) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     scenario_fixed_target(target_value = 18,
#                           target_year = 2025,
#                           sname = "scen_fixed_2025") %>%
#     mutate("linear" = 0.36) %>%
#     scenario_lin(sname = "scen_linear_36",trim_max=18) %>% #added trim sep 2021
#     scenario_bestof(c("scen_bau", "scen_linear_36"),minv=FALSE) %>%
#     mutate(scen_acceleration = scen_bestof,scen_sdg=scen_fixed_2025)
#   noscenario_df <- df %>%
#     group_by(iso3) %>%
#     filter(!any((value < 18 & year >= 2018))) %>%
#     ungroup() %>%
#     scenario_bau() %>%
#     mutate(scen_acceleration = scen_bau, scen_sdg = scen_acceleration)
#   withscenario_df %>% bind_rows(noscenario_df)
# }
#
#
#
#
# add_bp_scenarios <- function(df) {
#   bp_as_cr_ratio <-bp_as_cr_ratio %>%
#     bind_rows(bp_as_cr_ratio%>%filter(year==2023) %>%mutate(year=2024))%>%
#     bind_rows(bp_as_cr_ratio%>%filter(year==2023) %>%mutate(year=2025))
#
#   df %>%
#     scenario_bau() %>%
#     #convert to crude, run hpop scenario on crude, convert back to agrestd
#     #NB need to recalculate ratios????
#     left_join(bp_as_cr_ratio) %>%
#     mutate(ratio_agestd_over_crude= ifelse(is.na(ratio_agestd_over_crude),1,ratio_agestd_over_crude))%>%
#     mutate(crude = value / ratio_agestd_over_crude) %>%
#     scenario_percent_baseline(
#       percent_decrease = 25,
#       value = "crude",
#       old_baseline = 2010,
#       baseline = 2018,
#       target_year = 2025,
#       sname = "scen_perc_25_2010"
#     ) %>%
#     scenario_bau(value = "crude", sname = "scen_bau_crude") %>%
#     scenario_bestof(c("scen_bau_crude", "scen_perc_25_2010")) %>% select(-bestname) %>%
#     mutate(scen_perc_25_2010 = scen_perc_25_2010 * ratio_agestd_over_crude) %>%
#     mutate(scen_bestof = scen_bestof * ratio_agestd_over_crude) %>%
#     select(-scen_bau_crude,-crude, -ratio_agestd_over_crude) %>%
#     mutate(scen_acceleration = if_else(!is.na(scen_bestof), scen_bestof, scen_bau)) %>%
#     mutate(scen_sdg = scen_acceleration)
# }
#
#
#
# add_espar_scenarios <- function(df,baseline = 2018,targetyear=2025) {
#
#
#
#
#
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
#                         target_year = targetyear) %>%
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
#                         target_year = targetyear) %>%
#     group_by(iso3) %>%
#     mutate(fill_value = min(value[type == "reported" &
#                                     year >=2018 & year<= 2025],-1)) %>%
#     mutate(fill_value= ifelse(fill_value<0,value[year==2020],fill_value)) %>%
#     ungroup() %>%
#     arrange(ind, iso3, -year) %>%
#     mutate(scen_target = if_else(year == 2019 &
#                                    is.na(scen_target), fill_value, scen_target)) %>%
#     mutate(scen_target = if_else(year == 2018 &
#                                    is.na(scen_target), fill_value, scen_target)) %>%
#     rename(scen_acceleration = scen_target)
#
#   all2018 <- alle %>% filter(baseline == 2018| is.na(baseline)) %>%
#     mutate(baseline=2018) %>%
#     group_by(ind, iso3) %>%
#     scenario_target_col(baseline = 2018,
#                         target_col = "target",
#                         target_year = targetyear) %>%
#     arrange(ind, iso3, -year) %>%
#     rename(scen_acceleration = scen_target)
#
#   all2018 %>% bind_rows(all2019) %>% bind_rows(all2020) %>% bind_rows(missing)   %>%
#     select(-target,-hasdata, -fill_value, -scen_bau) %>% scenario_bau()
# }
