## code to prepare `hep_df` dataset goes here

library(tidyverse)

hep_df <- read_csv("data-raw/hep_df.csv")

usethis::use_data(hep_df, overwrite = TRUE)
