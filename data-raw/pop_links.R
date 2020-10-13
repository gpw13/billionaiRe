## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)

pop_links <- read_csv("data-raw/pop_links.csv") %>%
  pivot_longer(-ind, names_to = "pop_group") %>%
  filter(value == 1) %>%
  select(-value)

usethis::use_data(pop_links, overwrite = TRUE, internal = FALSE)
