## code to prepare `sdi_ratio` goes here
library(tidyverse)

sdi_ratio <- read_csv("data-raw/sdi_ratio.csv")
usethis::use_data(sdi_ratio, overwrite = TRUE, internal = TRUE)
