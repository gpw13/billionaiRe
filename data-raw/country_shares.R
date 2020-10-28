## code to prepare `country_shares` goes here

country_shares <- readxl::read_excel("data-raw/country_shares.xlsx")

usethis::use_data(country_shares, overwrite = TRUE, internal = FALSE)
