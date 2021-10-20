## code to prepare `indicator_df` dataset goes here
library(readxl)
library(tidyverse)

indicator_df <- read_excel("data-raw/indicator_df.xlsx") %>%
  arrange(order)

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
