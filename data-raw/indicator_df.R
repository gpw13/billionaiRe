## code to prepare `indicator_df` dataset goes here
library(readxl)
library(tidyverse)

# Limited changes made, eventually going to live in xMart4

indicator_df <- read_excel("data-raw/indicator_df.xlsx") %>%
  mutate(across(c(input, calculated, covariate, uhc, hpop, hep, hep_prev, hep_prep, hep_dr),
                ~ifelse(is.na(.x), F, T)))

usethis::use_data(indicator_df, overwrite = TRUE, internal = TRUE)
