## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)
library(billionaiRe)

uhc_df <- read_csv("data-raw/uhc.csv") %>%
  dplyr::mutate(type = case_when(
    year == 2018 ~ "Reported",
    year == 2023 ~ "Projected",
    TRUE ~ NA_character_
  ))

usethis::use_data(uhc_df, overwrite = TRUE, internal = FALSE)

uhc_calculated <- uhc_df %>%
  transform_uhc_data() %>%
  calculate_uhc_billion() %>%
  calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>%
  dplyr::filter(
    ind %in% c("uhc_sm", "asc", "fh"),
    year == 2023
  )

hpop_df <- read_csv("data-raw/hpop.csv") %>%
  dplyr::mutate(type = case_when(
    year == 2018 ~ "Reported",
    year == 2023 ~ "Projected",
    TRUE ~ NA_character_
  ))

usethis::use_data(hpop_df, overwrite = TRUE, internal = FALSE)

hpop_calculated <- hpop_df %>%
  transform_hpop_data() %>%
  add_hpop_populations(pop_year = 2023) %>%
  calculate_hpop_billion(end_year = 2023, pop_year = 2023)

hep_df <- read_csv("data-raw/hep_df.csv")

usethis::use_data(hep_df, overwrite = TRUE)

hep_calculated <- hep_df %>%
  transform_hep_data(extrapolate_to = 2023) %>%
  calculate_hep_components() %>%
  calculate_hep_billion(end_year = 2023, pop_year = 2023) %>%
  dplyr::filter(
    ind %in% c(
      "prevent",
      "espar",
      "detect_respond",
      "hep_idx"
    ),
    year == 2023
  )

basic_test_calculated <- uhc_calculated %>%
  bind_rows(hpop_calculated) %>%
  bind_rows(hep_calculated)

usethis::use_data(basic_test_calculated, overwrite = TRUE, internal = TRUE)


# Creating complete test data set

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
source("https://raw.githubusercontent.com/alicerobson/scenarios/covid_proj/covid_scenario_functions.R?token=AIYN4CR4CIME3K57TRZ3JE3BUD5ZW")

scenario_covid_dip_lag_same_aroc_only_2020values_df <- scenario_covid_dip_lag_same_aroc_only_2020values(all_billions_transformed, value = "transform_value") %>%
  select(-type) %>%
  left_join(all_billions_transformed_types, by = c("iso3", "year", "ind")) %>%
  select(-source, -baseline_value) %>%
  untransform_hpop_data() %>%
  untransform_uhc_data() %>%
  # filter(year >= 2020 & !type %in% c("reported", "estimated")) %>%
  mutate(
    scenario = "covid_dip_lag",
    value = case_when(
      year >= 2020 & !type %in% c("reported", "estimated") & str_detect(ind, "campaign") ~ NA_real_,
      TRUE ~ value
    )
  ) %>%
  filter(!str_detect(ind, "routine_num")) %>%
  filter(!is.na(value))

test_data <- all_data_those_isos %>%
  bind_rows(scenario_covid_dip_lag_same_aroc_only_2020values_df) %>%
  mutate(scenario = case_when(
    scenario == "pre_covid_bau" ~ "default",
    TRUE ~ scenario
  )) %>%
  select(-transform_value) %>%
  distinct() %>%
  filter(ind != "surviving_infants") %>%
  dplyr::distinct()

arrow::write_parquet(test_data, "data-raw/test_data.parquet")

test_data_calculated_hep <- test_data %>%
  transform_hep_data(scenario = "scenario", recycle = TRUE) %>%
  calculate_hep_components(scenario = "scenario") %>%
  calculate_hep_billion(scenario = "scenario")

test_data_calculated_hpop <- test_data %>%
  transform_hpop_data(recycle = TRUE) %>%
  add_hpop_populations() %>%
  calculate_hpop_billion(scenario = "scenario")

test_data_calculated_uhc <- test_data %>%
  transform_uhc_data(recycle = TRUE) %>%
  calculate_uhc_billion(scenario = "scenario") %>%
  calculate_uhc_contribution(scenario = "scenario")

test_data_calculated <- bind_rows(test_data_calculated_uhc, test_data_calculated_hep) %>%
  bind_rows(test_data_calculated_hpop)

arrow::write_parquet(test_data_calculated, "data-raw/test_data_calculated.parquet")
