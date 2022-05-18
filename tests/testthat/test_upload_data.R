testthat::test_that("upload_billion_data works as expected", {
  tstamp <- whdh::get_formatted_timestamp()
  gpw13_code <- "dtp3"

  df <- tibble::tibble(
    iso3 = "AFG", year = 2000:2019, ind = gpw13_code, scenario = "routine",
    value = as.double(1:20), lower = NA_real_, upper = NA_real_, use_dash = TRUE,
    use_calc = TRUE, source = "DDI", type = "reported", type_detail = NA_character_,
    other_detail = NA_character_, upload_detail = NA_character_,
    scenario_detail = NA_character_, upload_date = as.Date("2022-05-18")
  )

  upload_billion_data(
    df,
    data_type = "wrangled_data",
    billion = "uhc",
    ind_code = gpw13_code,
    version = tstamp,
    na_rm = TRUE,
    experiment = "unit_tests",
    silent = TRUE
  )

  Sys.sleep(30)

  whdh_files <- whdh::list_blobs_in_directory(
    get_data_lake_name(),
    "3B/Sandbox/unit_tests/Silver/wrangled_data/uhc_dtp3/",
    silent = TRUE
  ) %>%
    dplyr::pull(name)

  expect_true(any(stringr::str_detect(whdh_files, sprintf("%s_%s.parquet", gpw13_code, tstamp))))

  downloaded_df <- load_billion_data(
    data_type = "wrangled_data",
    billion = "uhc",
    ind_code = "dtp3",
    version = tstamp, na_rm = TRUE,
    experiment = "unit_tests",
    data_source = "whdh",
    silent = TRUE
  )

  expect_identical(df, downloaded_df, list_as_map = TRUE)
})
