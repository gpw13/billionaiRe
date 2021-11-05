## code to prepare `country_shares` goes here
library(tidyverse)

country_shares <- read_csv("data-raw/country_shares.csv") %>%
  transmute(
    billion = tolower(Billion),
    iso3,
    share_n = Share_N,
    share_perc = Share_perc
  )

usethis::use_data(country_shares, overwrite = TRUE, internal = FALSE)
