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

# assert_distinct_rows ------------------------------------------------------

testthat::test_that("assert_distinct_rows works as expected", {
  df <- tibble::tibble(
    x = c("a", "b", "c"),
    y = c("d", "e", "f"),
    z = "g"
  )

  testthat::expect_silent(assert_distinct_rows(df, c("x", "y")))
  testthat::expect_error(assert_distinct_rows(df, c("z")))
})

# assert_has_names ------------------------------------------------------------

testthat::test_that("assert_named works as expected", {
  named_vec <- c(x = 1, y = 2)
  named_list <- list(x = 1, y = 2)
  named_df <- as.data.frame(named_list)

  testthat::expect_silent(assert_has_names(named_vec))
  testthat::expect_silent(assert_has_names(named_list))
  testthat::expect_silent(assert_has_names(named_df))
  testthat::expect_error(assert_has_names(unname(named_vec)))
  testthat::expect_error(assert_has_names(unname(named_list)))
  testthat::expect_error(assert_has_names(unname(named_df)))
})

# assert_col_types --------------------------------------------------------

testthat::test_that("assert_col_types works as expected", {
  test_df <- tibble::tibble(x = c(1L, 2L), y = c(1.0, 2.0), z = c("a", "b"))

  testthat::expect_silent(assert_col_types(
    test_df,
    c(x = "integer", y = "double", z = "character")
  ))

  # Test that order doesn't matter if the types are correct
  testthat::expect_silent(assert_col_types(
    test_df,
    c(y = "double", z = "character", x = "integer")
  ))

  # expected is not named so raise an error
  testthat::expect_error(assert_col_types(test_df, c("integer", "double", "character")))

  # expected doesn't match the column types so raise an error
  testthat::expect_error(assert_col_types(
    test_df,
    c(x = "integer", y = "logical", z = "character")
  ))
})

# assert_col_paired_with --------------------------------------------------

testthat::test_that("assert_col_paired_with works as expected", {
  test_df <- tibble::tibble(x = c(1, 2, 3), y = c(4, NA, 5), z = 7)

  testthat::expect_silent(assert_col_paired_with(test_df, "x", "z"))
  testthat::expect_error(assert_col_paired_with(test_df, "x", "y"))
  testthat::expect_error(assert_col_paired_with(test_df, "x", c("y", "z")))
})
