## code to prepare `indicator_df` dataset goes here
library(readxl)
library(tidyverse)

# Limited changes made, eventually going to live in xMart4

indicator_order <- read_excel("data-raw/indicator_order.xlsx")
usethis::use_data(indicator_order, overwrite = TRUE, internal = FALSE)

