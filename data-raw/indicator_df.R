## code to prepare `indicator_df` dataset goes here
library(tidyverse)

indicator_df <- readr::read_csv("data-raw/indicator_df.csv",
                                locale = locale()) %>%
  # HOTFIX: prevent is missing small is best value, needed for scenarios
  mutate(small_is_best = if_else(ind == "prevent", FALSE, small_is_best)) %>%
  arrange(hep, uhc, hpop, ind)

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
