
# test_data <- billionaiRe::load_billion_data("all", "test_data")

testthat::test_that("billion calculations are consistent", {
  uhc_basic_calculated <- uhc_df %>%
    transform_uhc_data() %>%
    calculate_uhc_billion() %>%
    calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>%
    dplyr::filter(
      ind %in% c("uhc_sm", "asc", "fh"),
      year == 2023
    )

  hpop_basic_calculated <- hpop_df %>%
    transform_hpop_data() %>%
    add_hpop_populations(pop_year = 2023) %>%
    calculate_hpop_billion(end_year = 2023, pop_year = 2023)

  hep_basic_calculated <- hep_df %>%
    transform_hep_data(extrapolate_to = 2023) %>%
    calculate_hep_components() %>%
    calculate_hep_billion(end_year = 2023, pop_year = 2023) %>%
    dplyr::filter(
      ind %in% c(
        "prevent",
        "espar",
        "detect_respond",
        "hep_idx"
      ),
      year == 2023
    )

  all_basic_calculated <- uhc_basic_calculated %>%
    dplyr::bind_rows(hpop_basic_calculated) %>%
    dplyr::bind_rows(hep_basic_calculated) %>%
    dplyr::mutate(source = dplyr::case_when(
      stringr::str_detect(source, "WHO DDI calculation") ~ "WHO DDI calculation, November 2021",
      stringr::str_detect(source, "WHO DDI,") ~ "WHO DDI, November 2021",
      TRUE ~ source
    ))

  testthat::expect_equal(all_basic_calculated, billionaiRe:::basic_test_calculated)
})
