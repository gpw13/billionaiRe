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
  transform_uhc_data(end_year = 2023)%>%
  calculate_uhc_billion() %>%
  calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>%
  dplyr::filter(
    ind %in% c("uhc_sm", "asc", "fh"),
    year == 2023
  ) %>%
  dplyr::mutate(source = dplyr::case_when(
    stringr::str_detect(source, "WHO DDI calculation") ~ "WHO DDI calculation, November 2021",
    TRUE ~ source
  ))


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
  ) %>%
  dplyr::mutate(source = dplyr::case_when(
    stringr::str_detect(source, "WHO DDI") ~ "WHO DDI, November 2021",
    TRUE ~ source
  ))

basic_test_calculated <- uhc_calculated %>%
  bind_rows(hpop_calculated) %>%
  bind_rows(hep_calculated)

usethis::use_data(basic_test_calculated, overwrite = TRUE, internal = TRUE)

# Creating complete test data set

all_data <- load_billion_data_legacy("all", "raw_data")

proj_data <- load_billion_data_legacy("all", "proj_data")

proj_data_those_isos <- proj_data %>%
  select(iso3, year, ind, value, type) %>%
  filter(
    iso3 %in% c("AFG", "AGO", "BOL", "BGD", "BDI", "UGA"),
    year >= 2000
  )

all_data_those_isos <- all_data %>%
  select(iso3, year, ind, scenario, scenario_detail, value, type) %>%
  filter(
    iso3 %in% c("AFG", "AGO", "BOL", "BGD", "BDI", "UGA"),
    year >= 2000
  ) %>%
  anti_join(proj_data_those_isos, by = c("iso3", "ind", "year")) %>%
  bind_rows(proj_data_those_isos) %>%
  mutate(scenario = case_when(
    type %in% c("reported", "estimated") & year < 2020 ~ "routine",
    type %in% c("reported", "estimated") & year %in% 2020:2021 ~ "covid_shock",
    type %in% c("projected", "imputed") & year <= 2020 ~ "reference_infilling",
    type %in% c("projected", "imputed") & year > 2020 ~ "pre_covid_trajectory",
    TRUE ~ scenario
  )) %>%
  select(-scenario_detail) %>%
  distinct()

reported_covid_shock <- all_data_those_isos %>%
  filter(scenario %in% c("covid_shock", "pre_covid_trajectory"))

all_billions_transformed <- all_data_those_isos %>%
  anti_join(reported_covid_shock, by = c("iso3", "year", "ind")) %>%
  bind_rows(reported_covid_shock) %>%
  mutate(scenario = NA) %>%
  distinct() %>%
  transform_hep_data() %>%
  transform_hpop_data() %>%
  transform_uhc_data()

all_billions_transformed_types <- all_billions_transformed %>%
  select(iso3, year, ind, type)

# needs to import covid_scenario functions from:
# https://github.com/alicerobson/scenarios/blob/covid_proj/covid_scenario_functions.R

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
  select(-transform_value) %>%
  distinct() %>%
  filter(ind != "surviving_infants") %>%
  dplyr::distinct()

test_data_calculated_hep <- test_data %>%
  filter(ind %in% billion_ind_codes("hep", include_calculated = TRUE)) %>%
  transform_hep_data(scenario_col = "scenario", recycle = TRUE) %>%
  calculate_hep_components(scenario_col = "scenario") %>%
  calculate_hep_billion(scenario_col = "scenario")

test_data_calculated_hpop <- test_data %>%
  filter(ind %in% billion_ind_codes("hpop", include_calculated = TRUE)) %>%
  transform_hpop_data(recycle = TRUE) %>%
  add_hpop_populations() %>%
  calculate_hpop_billion(scenario_col = "scenario")

test_data_calculated_uhc <- test_data %>%
  filter(ind %in% billion_ind_codes("uhc", include_calculated = TRUE)) %>%
  transform_uhc_data(recycle = TRUE) %>%
  calculate_uhc_billion(scenario_col = "scenario") %>%
  calculate_uhc_contribution(scenario_col = "scenario")

test_data_calculated <- bind_rows(test_data_calculated_uhc, test_data_calculated_hep) %>%
  bind_rows(test_data_calculated_hpop) %>%
  distinct()

time_stamp <- whdh::get_formatted_timestamp()

test_data_file_name <- glue::glue("test_data_{time_stamp}.parquet")

test_data_output_path <- glue::glue("data-raw/{test_data_file_name}")

test_data_destination_path <- glue::glue("3B/Bronze/misc_data/test_data/test_data/{test_data_file_name}")
test_data_destination_path_notimestamp <- glue::glue("3B/Bronze/misc_data/test_data/test_data/test_data.parquet")

arrow::write_parquet(test_data, test_data_output_path)

whdh::upload_to_data_lake(
  data_lake_name = get_data_lake_name(),
  container = "whdh",
  source_path = test_data_output_path,
  destination_path = test_data_destination_path_notimestamp
)

whdh::upload_to_data_lake(
  data_lake_name = get_data_lake_name(),
  container = "whdh",
  source_path = test_data_output_path,
  destination_path = test_data_destination_path
)

test_data_calculated_file_name <- glue::glue("test_data_calculated_{time_stamp}.parquet")

test_data_calculated_output_path <- glue::glue("data-raw/{test_data_calculated_file_name}")

test_data_calculated_destination_path <- glue::glue("3B/Bronze/misc_data/test_data/test_data_calculated/{test_data_calculated_file_name}")
test_data_destination_path_notimestamp <- glue::glue("3B/Bronze/misc_data/test_data/test_data_calculated/test_data_calculated.parquet")


arrow::write_parquet(test_data_calculated, test_data_calculated_output_path)

whdh::upload_to_data_lake(
  data_lake_name = get_data_lake_name(),
  container = "whdh",
  source_path = test_data_calculated_output_path,
  destination_path = test_data_calculated_destination_path
)

whdh::upload_to_data_lake(
  data_lake_name = get_data_lake_name(),
  container = "whdh",
  source_path = test_data_calculated_output_path,
  destination_path = test_data_destination_path_notimestamp
)

test_data_unofficial_20221114 <- load_billion_data("projected_data",
                                                   version = "2022-11-24",
                                                   experiment = "unofficial")

test_data_calculated_file_name <- "test_data_2022-11-24T12-07-52.parquet"

test_data_output_path <- glue::glue("data-raw/{test_data_calculated_file_name}")

arrow::write_parquet(test_data_unofficial_20221114,
                     test_data_output_path)

test_data_unofficial_destination_path <- glue::glue("3B/Bronze/misc_data/test_data/test_data/{test_data_calculated_file_name}")

whdh::upload_to_data_lake(
  data_lake_name = get_data_lake_name(),
  source_path = test_data_output_path,
  container = "whdh",
  destination_path = test_data_unofficial_destination_path
)

