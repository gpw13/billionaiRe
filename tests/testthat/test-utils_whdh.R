
# Download path tests -----------------------------------------------------

test_that("WHDH download path with no file_name is a directory", {
  expect_equal(
    get_whdh_path("download", "wrangled_data", "hpop", "alcohol"),
    "3B/Silver/wrangled_data/hpop_alcohol/"
  )
})

test_that("WHDH download path with file_name is a file", {
  expect_equal(
    get_whdh_path("download", "ingestion_data", "hpop", "alcohol", "AC Data 2020.xlsx"),
    "3B/Bronze/ingestion_data/hpop_alcohol/AC Data 2020.xlsx"
  )
})

test_that("Incorrect file extensions raise error", {
  # Raise error with wrong extension
  expect_error(
    get_whdh_path("download", "projected_data", "hpop", "pm25", "hpop_pm25.png"),
    "The file extension is png but needs to be one of: csv, xls, xlsx, parquet, feather",
    fixed = TRUE
  )
})

test_that("File names with no extensions raise error", {
  expect_error(
    get_whdh_path("download", "projected_data", "hpop", "pm25", "hpop_pm25"),
    "The file does not have an extension.",
    fixed = TRUE
  )
})

test_that("final_data download paths are generated correctly", {
  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", "hpop", "alcohol", "hats.xlsx")),
    "3B/Gold/final_data/hats.xlsx"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", "hpop", "alcohol")),
    "3B/Gold/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", NULL, NULL)),
    "3B/Gold/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", NA, NA)),
    "3B/Gold/final_data/"
  )

  expect_equal(
    suppressMessages(get_whdh_path("download", "final_data", NA, "alcohol", "cars.csv")),
    "3B/Gold/final_data/cars.csv"
  )
})

test_that("final_data download paths raise billion and ind_code arguments ignored message", {
  expect_message(
    get_whdh_path("download", "final_data", "hpop", "alcohol", "hats.xlsx"),
    "`billion` and `ind_code` arguments ignored when data_type = \"final_data\""
  )
})

# Upload path tests -------------------------------------------------------

test_that("Upload paths require a file_name", {
  expect_error(
    get_whdh_path("upload", "projected_data", "hep", "espar"),
    "The file_name argument is required when uploading to the data lake.",
    fixed = TRUE
  )
})

test_that("Correct upload path is returned", {
  expect_equal(
    get_whdh_path("upload", "wrangled_data", "uhc", "uhc_tobacco", "uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"),
    "3B/Silver/wrangled_data/uhc_uhc_tobacco/uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"
  )
})

test_that("final_data upload paths are generated correctly", {
  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", "hpop", "alcohol", "cars.csv")),
    "3B/Gold/final_data/cars.csv"
  )

  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", NULL, NULL, "cars.csv")),
    "3B/Gold/final_data/cars.csv"
  )

  expect_equal(
    suppressMessages(get_whdh_path("upload", "final_data", NA, NA, "cars.csv")),
    "3B/Gold/final_data/cars.csv"
  )
})


test_that("final_data upload paths raise billion and ind_code arguments ignored message", {
  expect_message(
    get_whdh_path("upload", "final_data", "hpop", "alcohol", "stats.parquet"),
    "`billion` and `ind_code` arguments ignored when data_type = \"final_data\""
  )
})

