## code to prepare `uhc_df`, `hpop_df`, and `hep_df` datasets goes here
library(tidyverse)

uhc_df <- read_csv("data-raw/uhc.csv")
usethis::use_data(uhc_df, overwrite = TRUE, internal = FALSE)

hpop_df <- read_csv("data-raw/hpop.csv")
usethis::use_data(hpop_df, overwrite = TRUE, internal = FALSE)

hep_df <- read_csv("data-raw/hep_df.csv")

usethis::use_data(hep_df, overwrite = TRUE)
