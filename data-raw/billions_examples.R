## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)

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
