# WHDH --------------------------------------------------------------------

testthat::test_that("WHDH: a single indicator is correctly downloaded", {
  wrangled_ind = load_billion_data("wrangled_data", "hpop", "alcohol") %>%
    dplyr::pull(ind) %>%
    unique()

  projected_ind = load_billion_data("projected_data", "hpop", "alcohol") %>%
    dplyr::pull(ind) %>%
    unique()

  testthat::expect_equal(wrangled_ind, "alcohol")
  testthat::expect_equal(projected_ind, "alcohol")
})

testthat::test_that("WHDH: load_billion_data downloads the right data_type", {
  df_wrangled = load_billion_data("wrangled_data", "hpop", "alcohol")
  df_projected = load_billion_data("projected_data", "hpop", "alcohol")

  testthat::expect_equal(unique(df_wrangled$type), "estimated")
  testthat::expect_true("projected" %in% unique(df_projected$type))
  testthat::expect_equal(names(df_wrangled), xmart_cols("wrangled_data"))
  # TODO: currently failing because of a bug in the data_pipeline
  testthat::expect_equal(names(df_projected), xmart_cols("projected_data"))
})

testthat::test_that("WHDH: uhc_espar is correctly downloaded (from the hep_espar folder)", {
  c("wrangled_data", "projected_data") %>%
    purrr::walk(~ {
      df_hep = load_billion_data(.x, "hep", "espar")
      df_uhc = load_billion_data(.x, "uhc", "espar")
      testthat::expect_equal(df_hep, df_uhc)
    })
})

testthat::test_that("WHDH: load_billion_data correctly handles ind_codes = \"all\"", {
  c("hep", "hpop", "uhc") %>%
    purrr::map(~ {
      df = suppressMessages(load_billion_data("wrangled_data", .x, "all"))
      inds = unique(df$ind) %>%
        # Remove sub-indicators like water_rural, water_urban, etc.
        stringr::str_replace("_rural|_urban|_num|_denom", "") %>%
        # Remove espar categories and sub-categories
        stringr::str_replace("espar.*", "espar") %>%
        unique() %>%
        sort()
      testthat::expect_equal(inds, unname(sort(get_valid_inds("wrangled_data", .x))))
    })
})

testthat::test_that("WHDH: load_billion_data correctly handles billion = \"all\"", {
  df = suppressMessages(load_billion_data("wrangled_data", "all"))

  inds = unique(df$ind) %>%
    # Remove sub-indicators like water_rural, water_urban, etc.
    stringr::str_replace("_rural|_urban|_num|_denom", "") %>%
    # Remove espar categories and sub-categories
    stringr::str_replace("espar.*", "espar") %>%
    unique() %>%
    sort()

  valid_inds = c("hep", "hpop", "uhc") %>%
    purrr::map(~ get_valid_inds("wrangled_data", .x)) %>%
    unlist() %>%
    unique() %>%
    unname() %>%
    sort()

  testthat::expect_equal(inds, valid_inds)
})

testthat::test_that("WHDH: load_billion_data correctly handles final_data", {
  df = suppressMessages(load_billion_data("final_data"))
  df2 = suppressMessages(load_billion_data("final_data", "all"))
  df3 = suppressMessages(load_billion_data("final_data", "all", "all"))

  inds = sort(unique(df$ind))
  expected_inds = c("hep", "hpop", "uhc") %>%
    purrr::map(
      ~ billion_ind_codes(.x, include_covariates = FALSE, include_calculated = TRUE, include_subindicators = TRUE)
    ) %>%
    unlist() %>%
    unname() %>%
    # Removing espar subindicators because those are not in final_data
    stringr::str_replace("espar.*", "espar") %>%
    unique() %>%
    sort()

  testthat::expect_equal(df, df2)
  testthat::expect_equal(df, df3)
  # TODO: currently failing due to issues with data_pipeline
  testthat::expect_equal(inds, expected_inds)
})

testthat::test_that("WHDH: load_billion_data correctly handles na_rm", {
  # Downloading all billions because some indicators might have a full timeseries
  # Using all indicators ensures there's at least one rows with an NA value
  df_na = load_billion_data("wrangled_data", "all", na_rm = FALSE) %>%
    dplyr::filter(is.na(value)) %>%
    suppressMessages()
  df_no_na = load_billion_data("wrangled_data", "all", na_rm = TRUE) %>%
    dplyr::filter(is.na(value)) %>%
    suppressMessages()

  testthat::expect_true(nrow(df_na) > 0)
  testthat::expect_true(nrow(df_no_na) == 0)
})





