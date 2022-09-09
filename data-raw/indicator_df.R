## code to prepare `indicator_df` dataset goes here
library(tidyverse)

indicator_df <- readr::read_delim("data-raw/indicator_df.csv", delim = ";") %>%
  arrange(hep, uhc, hpop, ind)

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
