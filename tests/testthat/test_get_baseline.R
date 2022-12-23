value <- 10:15
year <- 2020:2025
type <- c(rep("reported", 2), rep("projected", 4))

testthat::test_that("get_baseline_value gets baseline_year value:",{

  purrr::walk2(c(2020, 2022, 2025), c(10, 12, 15),
               ~testthat::expect_equal(get_baseline_value(value, year, type, baseline_year = .x), .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value for reported values:",{
  purrr::walk2(c(2020, 2022, 2025), c(10, 11, 11),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "reported"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value for scenarios:",{
  values <- c(value, 20:25)
  years <- rep(year, 2)
  types <- rep(type, 2)
  scenario <- c(rep("a", 6), rep("b", 6))
  purrr::walk2(c(2020, 2022, 2025), c(20, 21, 21),
               ~testthat::expect_equal(
                 get_baseline_value(values, years, types, scenario, "b", baseline_year = .x, type_filter = "reported"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value looking after baseline:",{
  purrr::walk2(c(2020, 2022, 2025), c(10, 12, 15),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, direction = "after"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value looking after baseline:",{
  purrr::walk2(c(2020, 2022, 2025), c(10, NA, NA),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "reported", direction = "after"),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(12, 12, 15),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "projected", direction = "after"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value when type is NA:",{
  type <- rep(NA, 6)
  purrr::walk2(c(2020, 2022, 2025), c(NA_integer_, NA_integer_, NA_integer_),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "all", direction = "after"),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(10, 12, 15),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "all", direction = "after", na_rm = FALSE),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(NA_integer_, NA_integer_, NA_integer_),
               ~testthat::expect_equal(
                 get_baseline_value(value, year, type, baseline_year = .x, type_filter = "projected", direction = "after"),
                 .y))
})

#----- Test get_baseline_year -----#
type <- c(rep("reported", 2), rep("projected", 4))

testthat::test_that("get_baseline_value gets baseline_year value:",{

  purrr::walk2(c(2020, 2022, 2025), c(2020, 2022, 2025),
               ~testthat::expect_equal(get_baseline_year(year, type, baseline_year = .x), .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value for reported values:",{

  purrr::walk2(c(2020, 2022, 2025), c(2020, 2021, 2021),
               ~testthat::expect_equal(get_baseline_year(year, type, baseline_year = .x, type_filter = "reported"), .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value for scenarios:",{
  years <- rep(year, 2)
  types <- rep(type, 2)
  scenario <- c(rep("a", 6), rep("b", 6))
  purrr::walk2(c(2020, 2022, 2025), c(2020, 2021, 2021),
               ~testthat::expect_equal(
                 get_baseline_year(years, types, scenario, "b", baseline_year = .x, type_filter = "reported"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value looking after baseline:",{
  purrr::walk2(c(2020, 2022, 2025), c(2020, 2022, 2025),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, direction = "after"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value looking after baseline:",{
  purrr::walk2(c(2020, 2022, 2025), c(2020, NA, NA),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, type_filter = "reported", direction = "after"),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(2022, 2022, 2025),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, type_filter = "projected", direction = "after"),
                 .y))
})

testthat::test_that("get_baseline_value gets last baseline_year value when type is NA:",{
  type <- rep(NA, 6)

  purrr::walk2(c(2020, 2022, 2025), c(NA_integer_, NA_integer_, NA_integer_),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, type_filter = "all", direction = "after"),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(2020, 2022, 2025),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, type_filter = "all", direction = "after", na_rm = FALSE),
                 .y))

  purrr::walk2(c(2020, 2022, 2025), c(NA_integer_, NA_integer_, NA_integer_),
               ~testthat::expect_equal(
                 get_baseline_year(year, type, baseline_year = .x, type_filter = "projected", direction = "after"),
                 .y))
})
