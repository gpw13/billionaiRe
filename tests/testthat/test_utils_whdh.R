
# Download path tests -----------------------------------------------------

test_that("WHDH download path with no file_names is a directory", {
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", "alcohol"),
    "3B/Sandbox/Silver/wrangled_data/hpop_alcohol/"
  )
})

test_that("WHDH download path with file_names is a file", {
  expect_equal(
    get_whdh_path("download", "ingestion_data", "hpop", "alcohol", "AC Data 2020.xlsx"),
    "3B/Sandbox/Bronze/ingestion_data/hpop_alcohol/AC Data 2020.xlsx"
  )
})

test_that("Incorrect file extensions in WHDH paths raises errors", {
  # Raise error with wrong extension
  expect_error(
    get_whdh_path("download", "projected_data", "hpop", "pm25", "hpop_pm25.png"),
    "File extensions must be one of: {csv, xls, xlsx, parquet, feather}.",
    fixed = TRUE
  )
})

test_that("WHDH: File names with no extensions raise error", {
  expect_error(
    get_whdh_path("download", "projected_data", "hpop", "pm25", "hpop_pm25"),
    "One or more files do not have an extension.",
    fixed = TRUE
  )
})

# TODO: Should this also ignore the file_names argument?
test_that("final_data download paths are generated correctly", {
  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", "hpop", "alcohol", "hats.xlsx")),
    "3B/Sandbox/Gold/final_data/final_data/hats.xlsx"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", "hpop", "alcohol")),
    "3B/Sandbox/Gold/final_data/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", "all", "all")),
    "3B/Sandbox/Gold/final_data/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data")),
    "3B/Sandbox/Gold/final_data/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", file_names = "cars.csv")),
    "3B/Sandbox/Gold/final_data/final_data/cars.csv"
  )
})

test_that("get_whdh_path: NA and NULL not valid for billion", {
  expect_error(
    suppressMessages(get_whdh_path("download", "final_data", NULL, NULL)),
  )

  expect_error(
    suppressMessages(get_whdh_path("download", "final_data", NA, NA)),
  )

  expect_error(
    suppressMessages(get_whdh_path("download", "final_data", NA, "alcohol", "cars.csv"))
  )

  expect_error(
    suppressMessages(get_whdh_path("download", "final_data", "hpop", NA, "cars.csv"))
  )
})

test_that("final_data download paths raise billion and ind_code arguments ignored message", {
  expect_message(
    get_whdh_path("download", "final_data", "hpop", "alcohol", "hats.xlsx"),
    "`billion` and `ind_codes` arguments ignored when data_type = \"final_data\""
  )
})

# Upload path tests -------------------------------------------------------

test_that("Upload paths require file_names", {
  expect_error(
    get_whdh_path("upload", "projected_data", "hep", "espar"),
    "The file_names argument is required when uploading to the data lake.",
    fixed = TRUE
  )
})

test_that("Correct upload path is returned", {
  expect_equal(
    get_whdh_path("upload", "wrangled_data", "uhc", "uhc_tobacco", "uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"),
    "3B/Sandbox/Silver/wrangled_data/uhc_uhc_tobacco/uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"
  )
})

test_that("final_data upload paths are generated correctly", {
  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", "hpop", "alcohol", "cars.csv")),
    "3B/Sandbox/Gold/final_data/final_data/cars.csv"
  )

  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", "all", "all", "cars.csv")),
    "3B/Sandbox/Gold/final_data/final_data/cars.csv"
  )

  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", file_names = "cars.csv")),
    "3B/Sandbox/Gold/final_data/final_data/cars.csv"
  )
})

test_that("final_data upload paths raise billion and ind_code arguments ignored message", {
  expect_message(
    get_whdh_path("upload", "final_data", "hpop", "alcohol", "stats.parquet"),
    "`billion` and `ind_codes` arguments ignored when data_type = \"final_data\""
  )
})

test_that("get_whdh_path: sandbox argument is respected", {
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", "alcohol", sandbox = TRUE),
    "3B/Sandbox/Silver/wrangled_data/hpop_alcohol/"
  )

  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", "alcohol", sandbox = FALSE),
    "3B/Silver/wrangled_data/hpop_alcohol/"
  )
})

test_that("get_whdh_path: vector arguments are handled correctly", {
  # Vectorised ind_codes
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", c("alcohol", "hpop_sanitation")),
    c("3B/Sandbox/Silver/wrangled_data/hpop_alcohol/", "3B/Sandbox/Silver/wrangled_data/hpop_hpop_sanitation/")
  )

  # Vectorised files_names
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", "alcohol", c("cars.csv", "stars.parquet")),
    c("3B/Sandbox/Silver/wrangled_data/hpop_alcohol/cars.csv", "3B/Sandbox/Silver/wrangled_data/hpop_alcohol/stars.parquet")
  )

  # Vectorised ind_codes and file_names with 1-to-1 matching
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", c("alcohol", "pm25"), c("cars.csv", "stars.parquet")),
    c("3B/Sandbox/Silver/wrangled_data/hpop_alcohol/cars.csv", "3B/Sandbox/Silver/wrangled_data/hpop_pm25/stars.parquet")
  )

  # Vectorised ind_codes and files_names without 1-to-1 matching returns an error
  expect_error(
    get_whdh_path("download", "wrangled_data", "hpop", c("alcohol", "pm25", "fuel"), c("cars.csv", "stars.parquet")),
    "ind_codes, file_names must have the same length."
  )
})

test_that("billion = all is handled correctly", {

  expect_message(
    get_whdh_path("download", "wrangled_data", "all"),
    "`ind_codes` and `file_names` arguments ignored when billion = \"all\""
  )

  result = suppressMessages(get_whdh_path("download", "wrangled_data", "all"))

  # These tests are defunct since the function no longer returns a list
  # expect_equal(length(result), 3)
  # expect_equal(names(result), c("hep", "hpop", "uhc"))
  expect_equal(typeof(result), "character")
  expect_equal(
    length(result),
    c("hep", "hpop", "uhc") %>%
      purrr::map(~ get_valid_inds("wrangled_data", .x)) %>%
      unlist() %>%
      unique() %>% # to remove duplicates of espar
      length()
  )
})

test_that("ind_codes = all is handled correctly", {
  expect_message(
    get_whdh_path("download", "wrangled_data", "uhc", "all"),
    "`file_names` argument ignored when ind_codes = \"all\""
  )
  result = suppressMessages(get_whdh_path("download", "wrangled_data", "uhc", "all"))
  expect_equal(length(result), length(get_valid_inds("wrangled_data", "uhc")))
})
