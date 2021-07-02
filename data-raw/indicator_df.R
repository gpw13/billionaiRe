## code to prepare `indicator_df` dataset goes here
library(readxl)
library(tidyverse)

# Limited changes made, eventually going to live in xMart4

indicator_df <- read_excel("data-raw/indicator_df.xlsx") %>%
  mutate(across(c(uhc, hpop, hep, covariate, calculated),
                ~ifelse(is.na(.x), F, T)))

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
