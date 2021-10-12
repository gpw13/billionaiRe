
# Download path tests -----------------------------------------------------

test_that("WHDH path download path with no file_name is a directory", {
  expect_equal(
    get_whdh_path("download", "hpop", "alcohol", "wrangled_data"),
    "3B/Silver/wrangled_data/hpop/hpop_alcohol/"
  )
})

test_that("WHDH path with file_name is a file", {
  expect_equal(
    get_whdh_path("download", "hpop", "alcohol", "ingestion_data", "AC Data 2020.xlsx"),
    "3B/Bronze/ingestion_data/hpop/hpop_alcohol/AC Data 2020.xlsx"
  )
})

test_that("Incorrect file extensions raise error", {
  # Raise error with wrong extension
  expect_error(
    get_whdh_path("download", "hpop", "pm25", "projected_data", "hpop_pm25.png"),
    "The file extension is png but needs to be one of: csv, xls, xlsx, parquet, feather",
    fixed = TRUE
  )

  # Raise error with NA when no extension in filename
  expect_error(
    get_whdh_path("download", "hpop", "pm25", "projected_data", "hpop_pm25"),
    "The file extension is NA but needs to be one of: csv, xls, xlsx, parquet, feather",
    fixed = TRUE
  )
})

# Upload path tests -------------------------------------------------------

test_that("Correct upload path is returned", {
  expect_equal(
    get_whdh_path("upload", "uhc", "uhc_tobacco", "wrangled_data", "uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"),
    "3B/Silver/wrangled_data/uhc/uhc_uhc_tobacco/uhc_uhc_tobacco_2021-10-12T18-32-44.parquet"
  )
})

test_that("Upload paths require a file_name", {
  expect_error(
    get_whdh_path("upload", "hep", "espar", "projected_data"),
    "The file_name argument is required when uploading to the data lake.",
    fixed = TRUE
  )
})
