## code to prepare `indicator_df` dataset goes here
library(tidyverse)

indicator_df <- readr::read_csv("data-raw/indicator_df.csv") %>%
  arrange(hep, uhc, hpop, ind)

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
