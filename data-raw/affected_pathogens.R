library(tidyverse)
library(usethis)

affected_pathogens <- readr::read_csv("data-raw/Prevent_CO_ReferenceTable.csv") %>%
  dplyr::transmute(
    iso3 = iso3,
    yellow_fever = dplyr::case_when(atrisk_yf == 1 ~ TRUE, TRUE ~ FALSE),
    cholera = dplyr::case_when(atrisk_cholera == 1 ~ TRUE, TRUE ~ FALSE),
    meningitis = dplyr::case_when(atrisk_cholera == 1 ~ TRUE, TRUE ~ FALSE)
  )

usethis::use_data(affected_pathogens, overwrite = TRUE, internal = FALSE)
