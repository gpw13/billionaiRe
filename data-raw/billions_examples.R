## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)
library(billionaiRe)

uhc_df <- read_csv("data-raw/uhc.csv")
usethis::use_data(uhc_df, overwrite = TRUE, internal = FALSE)

hpop_df <- read_csv("data-raw/hpop.csv")
usethis::use_data(hpop_df, overwrite = TRUE, internal = FALSE)

hep_df <- read_csv("data-raw/hep_df.csv")

usethis::use_data(hep_df, overwrite = TRUE)

# Creating test data set

all_data <- load_billion_data("all", "raw_data")

proj_data <- load_billion_data("all", "proj_data")

proj_data_those_isos <- proj_data %>%
  select(iso3, year, ind, value, type) %>%
  filter(
    iso3 %in% c("AFG", "AGO", "BOL", "BGD", "BDI", "UGA"),
    year >= 2000
  ) %>%
  mutate(scenario = case_when(
    type %in% c("estimated", "reported") ~ "none",
    TRUE ~ "tp"
  ))

all_data_those_isos <- all_data %>%
  select(iso3, year, ind, scenario, scenario_detail, value, type) %>%
  filter(
    iso3 %in% c("AFG", "AGO", "BOL", "BGD", "BDI", "UGA"),
    year >= 2000
  ) %>%
  anti_join(proj_data_those_isos, by = c("iso3", "ind", "year")) %>%
  bind_rows(proj_data_those_isos) %>%
  mutate(scenario = case_when(
    type %in% c("estimated", "reported") ~ "none",
    !is.na(scenario_detail) & scenario_detail != "precovid_bau" ~ scenario_detail,
    is.na(scenario) | scenario == "un_regional_median" | scenario_detail == "precovid_bau" ~ "pre_covid_bau",
    TRUE ~ scenario
  )) %>%
  select(-scenario_detail)

reported_2020_values <- all_data_those_isos %>%
  filter(scenario == "none" & year == 2020)

all_billions_transformed <- all_data_those_isos %>%
  anti_join(reported_2020_values, by = c("iso3", "year", "ind")) %>%
  bind_rows(reported_2020_values) %>%
  mutate(scenario = NA) %>%
  distinct() %>%
  transform_hep_data() %>%
  transform_hpop_data() %>%
  transform_uhc_data()

all_billions_transformed_types <- all_billions_transformed %>%
  select(iso3, year, ind, type)

# needs to import covid_scenario functions from:
# https://github.com/alicerobson/scenarios/blob/covid_proj/covid_scenario_functions.R
source("https://raw.githubusercontent.com/alicerobson/scenarios/covid_proj/covid_scenario_functions.R?token=AIYN4CQO4GWMGLFHHNP473TBRYVGG")

scenario_covid_dip_lag_same_aroc_only_2020values_df <- scenario_covid_dip_lag_same_aroc_only_2020values(all_billions_transformed, value = "transform_value") %>%
  select(-type) %>%
  left_join(all_billions_transformed_types, by = c("iso3", "year", "ind")) %>%
  select(-source, -baseline_value) %>%
  untransform_hpop_data() %>%
  untransform_uhc_data() %>%
  filter(year >= 2020 & !type %in% c("reported", "estimated")) %>%
  mutate(scenario = "covid_dip_lag")

test_data <- all_data_those_isos %>%
  bind_rows(scenario_covid_dip_lag_same_aroc_only_2020values_df) %>%
  select(-transform_value)
