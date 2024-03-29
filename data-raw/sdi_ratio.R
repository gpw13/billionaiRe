## code to prepare `sdi_ratio` goes here
library(tidyverse)
library(whoville)

sdi_df <- readr::read_csv("data-raw/ihme_sdi_gbd_2019.csv")

sdi_df2 <- sdi_df %>%
  mutate(iso3 = names_to_iso3(Location,
    fuzzy_matching = "user_input"
  ))

sdi_ratio <- sdi_df2 %>%
  filter(is_who_member(iso3)) %>%
  # remove the US state of Georgia
  transmute(
    iso3 = iso3,
    value = paste0("0.", str_match(`2018`, "0\\·([0-9]+)$")[, 2]),
    value = as.numeric(value),
    value = ifelse(iso3 %in% c("SOM", "NER"),
      value[iso3 == "TCD"],
      value
    ),
    sdiratio = -35.69 + 497.40 * value - 1272.90 * (value^2) + 1075.56 * (value^3)
  ) %>%
  select(iso3, sdiratio)

usethis::use_data(sdi_ratio, overwrite = TRUE, internal = FALSE)
