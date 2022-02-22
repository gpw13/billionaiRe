# assert_equals -----------------------------------------------------------

testthat::test_that("assert_equals works as expected", {
  T <- TRUE
  F <- FALSE
  testthat::expect_error(assert_equals(1, 5, iden = F, rev = F), "must be equal")
  testthat::expect_error(assert_equals(1, 5, iden = T, rev = F), "must be identical")
  testthat::expect_error(assert_equals(1, "1", iden = T, rev = F), "must be identical")
  testthat::expect_error(assert_equals(1, 1, iden = F, rev = T), "must not be equal")
  testthat::expect_error(assert_equals(1, 1, iden = T, rev = T), "must not be identical")
  testthat::expect_error(assert_equals(1, 1.0, iden = F, rev = T), "must not be equal")
  testthat::expect_error(assert_equals(1, 1.0, iden = T, rev = T), "must not be identical")
})

# assert_type -------------------------------------------------------------

testthat::test_that("assert_type works as expected", {
  T <- TRUE
  F <- FALSE
  testthat::expect_error(assert_type("hello", "logical"))
  testthat::expect_error(assert_type("hello", "character", T))
  testthat::expect_error(assert_type("hello", c("logical", "double", "integer")))
  testthat::expect_error(assert_type("hello", c("character", "double", "integer"), T))
  testthat::expect_error(assert_type(NULL, "character"))
  testthat::expect_error(assert_type(NULL, c("NULL", "logical"), T))
})

# assert_class ------------------------------------------------------------

testthat::test_that("assert_class works as expected", {
  T <- TRUE
  F <- FALSE
  cars_df <- tibble::as_tibble(mtcars)
  # reverse = FALSE, how = "any"
  testthat::expect_silent(assert_class(cars_df, "data.frame", F, "any"))
  testthat::expect_silent(assert_class(cars_df, c("data.frame", "lm"), F, "any"))
  testthat::expect_error(assert_class(cars_df, "lm", F, "any"))
  testthat::expect_error(assert_class(cars_df, c("lm", "glm"), F, "any"))

  # reverse = TRUE, how = "any"
  testthat::expect_silent(assert_class(cars_df, "lm", T, "any"))
  testthat::expect_silent(assert_class(cars_df, c("lm", "glm"), T, "any"))
  testthat::expect_error(assert_class(cars_df, "data.frame", T, "any"))
  testthat::expect_error(assert_class(cars_df, c("data.frame", "lm"), T, "any"))

  # reverse = FALSE, how = "all"
  testthat::expect_silent(assert_class(cars_df, c("data.frame", "tbl", "tbl_df"), F, "all"))
  testthat::expect_error(assert_class(cars_df, "data.frame", F, "all"))
  testthat::expect_error(assert_class(cars_df, c("data.frame", "tbl_df"), F, "all"))

  # reverse = TRUE, how = "all"
  testthat::expect_silent(assert_class(cars_df, "data.frame", T, "all"))
  testthat::expect_silent(assert_class(cars_df, c("data.frame", "tbl_df"), T, "all"))
  testthat::expect_error(assert_class(cars_df, c("data.frame", "tbl", "tbl_df"), T, "all"))
})

# assert_timestamp --------------------------------------------------------

testthat::test_that("assert_timestamp works as expected", {
  testthat::expect_silent(assert_timestamp("2021-04-15T18-35-20"))
  testthat::expect_error(assert_timestamp("2021-04-15"))
  testthat::expect_error(assert_timestamp("2021-04-15T18-35"))
  testthat::expect_error(assert_timestamp("2021-04-15 18-35-20"))
})
