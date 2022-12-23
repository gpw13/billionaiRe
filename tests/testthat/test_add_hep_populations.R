test_data_calculated <- load_misc_data("test_data/test_data_calculated/test_data_calculated_2022-10-13T17-10-12.parquet")

basic_hep_calculated <- hep_df %>%
  transform_hep_data(extrapolate_to = 2023) %>%
  calculate_hep_components() %>%
  calculate_hep_billion(end_year = 2023, pop_year = 2023)

testthat::test_that("add_hep_population does not generate population for non HEP indicator",{
  test_add_hep_population <- basic_hep_calculated %>%
    add_hep_populations(pop_year = 2023)

  # non HEP indicator don't get population figures
  non_hep_ind <- test_add_hep_population %>%
    dplyr::filter(!.data[["ind"]] %in% billion_ind_codes("hep", include_calculated = TRUE))

  testthat::expect_equal(sum(is.na(non_hep_ind[["population"]])), nrow(non_hep_ind))
})

testthat::test_that("add_hep_population does not generate population for non HEP indicator with complexe data",{
  test_add_hep_population <- test_data_calculated %>%
    add_hep_populations()

  # non HEP indicator don't get population figures
  non_hep_ind <- test_add_hep_population %>%
    dplyr::filter(!.data[["ind"]] %in% billion_ind_codes("hep", include_calculated = TRUE))

  testthat::expect_equal(sum(is.na(non_hep_ind[["population"]])), nrow(non_hep_ind))
})

testthat::test_that("add_hep_population generates accurate population figures",{
  test_add_hep_population <- basic_hep_calculated %>%
    add_hep_populations(pop_year = 2023)

  population_data <- test_add_hep_population %>%
    dplyr::filter(!is.na(population)) %>%
    dplyr::pull(population) %>%
    unique()

  testthat::expect_equal(population_data, c(wppdistro::get_population("AFG", 2023),
                                            wppdistro::get_population("AFG", 2023, age_range = "under_1"),
                                            wppdistro::get_population("AFG", 2023, age_range = "under_1")*2
  ))
})


test_fn <- function(df, indicator){
  testthat::test_that(glue::glue("add_hep_population generates accurate population figures with complexe data for {indicator}"),{

    inds <- df %>%
      dplyr::filter(iso3 == "UGA",
                    year == 2025,
                    stringr::str_detect(ind, indicator)) %>%
      dplyr::pull("ind") %>%
      unique()

    if(glue::glue("{indicator}_routine") %in% inds){
      surviving_infants <- wppdistro::get_population("UGA", 2025, age_range = "under_1")
    }else{
      surviving_infants <- NA
    }

    if(glue::glue("{indicator}_campaign_denom") %in% inds){
      uga_2025_denom <- df %>%
        dplyr::filter(iso3 == "UGA",
                      year == 2025,
                      ind == glue::glue("{indicator}_campaign_denom"),
                      scenario == "pre_covid_trajectory") %>%
        dplyr::pull("transform_value") %>%
        unique()
    }else{
      uga_2025_denom <- NA
    }

    if(glue::glue("{indicator}_routine") %in% inds){
      uga_routine_2025 <- df %>%
        dplyr::filter(iso3 == "UGA",
                      year == 2025,
                      ind == glue::glue("{indicator}_routine"),
                      scenario == "pre_covid_trajectory") %>%
        dplyr::pull(population) %>%
        unique()

      testthat::expect_equal(uga_routine_2025,
                             surviving_infants)
    }

    if(glue::glue("{indicator}_campaign") %in% inds){
      uga_campaign_2025 <- df %>%
        dplyr::filter(iso3 == "UGA",
                      year == 2025,
                      ind == glue::glue("{indicator}_campaign"),
                      scenario == "pre_covid_trajectory") %>%
        dplyr::pull(population) %>%
        unique()

      testthat::expect_equal(uga_campaign_2025,
                             sum(uga_2025_denom, na.rm = TRUE)
      )
    }

    uga_2025 <- df %>%
      dplyr::filter(iso3 == "UGA",
                    year == 2025,
                    ind == indicator,
                    scenario == "pre_covid_trajectory") %>%
      dplyr::pull(population) %>%
      unique()

    testthat::expect_equal(uga_2025,
                           sum(uga_2025_denom, surviving_infants, na.rm = TRUE))
  })
}

test_add_hep_population <- test_data_calculated %>%
  add_hep_populations(scenario_col = "scenario")

indicators <- c(
  "meningitis",
  "yellow_fever",
  "cholera",
  "polio",
  "measles"
)

purrr::walk(indicators, test_fn, df = test_add_hep_population)

testthat::test_that(glue::glue("add_hep_population generates accurate population figures with complexe data for prevent"),{
  surviving_infants <- wppdistro::get_population("UGA", 2025, age_range = "under_1")

  uga_2025 <- test_add_hep_population %>%
    dplyr::filter(iso3 == "UGA",
                  year == 2025,
                  ind == "prevent",
                  scenario == "pre_covid_trajectory") %>%
    dplyr::pull(population) %>%
    unique()

  campaign_denom <- billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "_campaign_denom$")]

  uga_2025_campaign <- test_add_hep_population %>%
    dplyr::filter(iso3 == "UGA",
                  year == 2025,
                  .data[["ind"]] %in% campaign_denom,
                  scenario == "pre_covid_trajectory") %>%
    dplyr::pull(transform_value) %>%
    unique() %>%
    sum()

  routine_num <- billion_ind_codes("hep")[stringr::str_detect(billion_ind_codes("hep"), "_routine_num$")]

  uga_2025_routine <- test_add_hep_population %>%
    dplyr::filter(iso3 == "UGA",
                  year == 2025,
                  .data[["ind"]] %in% routine_num,
                  scenario == "pre_covid_trajectory") %>%
    dplyr::pull(transform_value) %>%
    unique()


  testthat::expect_equal(uga_2025,
                         sum(uga_2025_campaign, surviving_infants * length(uga_2025_routine), na.rm = TRUE))
})
