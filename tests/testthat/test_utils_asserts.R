# assert_equals ---------------------------

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

# assert_type -----------------------------

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
