
testthat::test_that("surviving_infants added from wppdistro correctly:", {
  correct_data <- hep_df %>%
    transform_hep_data(extrapolate_to = 2023) %>%
    calculate_hep_components() %>%
    dplyr::filter(!ind %in% "surviving_infants")

  test_data <- hep_df %>%
    dplyr::filter(!ind %in% "surviving_infants") %>%
    transform_hep_data(extrapolate_to = 2023) %>%
    calculate_hep_components() %>%
    dplyr::filter(!ind %in% "surviving_infants")

  testthat::expect_equal(test_data,correct_data)

})
