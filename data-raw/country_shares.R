## code to prepare `country_shares` goes here
library(tidyverse)

uhc_shares <- readxl::read_excel("data-raw/country_shares.xlsx",
                                 sheet = "UHC")
hpop_shares <- readxl::read_excel("data-raw/country_shares.xlsx",
                                  sheet = "HPOP")

hep_shares <- readxl::read_excel("data-raw/country_shares.xlsx",
                                 sheet = "HEP")

country_shares <- bind_rows(uhc_shares, hpop_shares) %>%
  bind_rows(hep_shares) %>%
  transmute(billion = tolower(Billion),
            iso3,
            share_n = Share_N,
            share_perc = Share_perc)

usethis::use_data(country_shares, overwrite = TRUE, internal = FALSE)
