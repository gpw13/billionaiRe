## code to prepare `indicator_df` dataset goes here
library(readxl)
library(tidyverse)

# Limited changes made, eventually going to live in xMart4
indicator_order <- read_excel("data-raw/indicator_order.xlsx")


indicator_df <- read_excel("data-raw/indicator_df.xlsx") %>%
  mutate(across(
    c(uhc, hpop, hep, covariate, calculated),
    ~ ifelse(is.na(.x), F, T)
  )) %>%
  left_join(indicator_order, by = c("analysis_code" = "ind"), keep = TRUE) %>%
  arrange(order)

usethis::use_data(indicator_df, overwrite = TRUE, internal = FALSE)
