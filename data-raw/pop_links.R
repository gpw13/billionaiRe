## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)

pop_links <- read_csv("data-raw/pop_links.csv") %>%
  mutate(
    female = ifelse(sex %in% c("female", "both"),
      1,
      0
    ),
    male = ifelse(sex %in% c("male", "both"),
      1,
      0
    )
  ) %>%
  select(-sex) %>%
  pivot_longer(-c(ind, female, male), names_to = "pop_group") %>%
  pivot_longer(c(female, male), names_to = "sex", values_to = "sex_value") %>%
  filter(value == 1 & sex_value == 1) %>%
  mutate(pop_group = paste(pop_group, sex, sep = "_")) %>%
  select(-value, -sex_value, -sex)

usethis::use_data(pop_links, overwrite = TRUE, internal = FALSE)
