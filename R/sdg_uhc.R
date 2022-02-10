#' Accelerate anc4 to SDG target
#'
#' Accelerate anc4 by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, the acceleration scenario is the same
#' as business as usual.
#' - For countries with reported data, scenarios with both a **fixed target of 95%
#' by 2030** and a **linear change of 2.6 per year till 2025** are tried, with the easiest
#' to achieve of the two selected. The selected scenario is then compared against
#' the business as usual scenario for reported data, and the best of the two chosen
#' as the acceleration scenario.
#'
#' @inheritParams transform_hpop_data
#' @inheritParams calculate_hpop_contributions
#' @inheritParams calculate_uhc_billion
#' @param ... additional parameters to be passed to scenario function
#'
#' @return data frame with acceleration scenario binded to `df`. `scenario` is
#' set to `acceleration`
#'
sdg_anc4 <- function(df,
                            ...) {

  df %>%
    sdg_hpop_sanitation(
      ind_ids = c("hpop_sanitation" = "anc4"),
      ...
    )

}

#' Accelerate art to SDG target
#'
#' Accelerate art by first dividing countries into those with reported data and
#' those without.
#' - For countries without reported data, business as usual is returned.
#' - For countries with reported data, the best of business as usual and **fixed
#'  target of 90.25% by 2025** is chosen.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#'
sdg_art <- function(df,
                    scenario = "scenario",
                    ind_ids = billion_ind_codes("uhc"),
                           ...) {
  this_ind <- ind_ids["art"]

  accelerate_art(df = df,
                          ind_ids = this_ind,
                          ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate beds to SDG target
#'
#' Accelerate beds by first dividing countries into two groups:
#' - For countries with 18 or more beds for all years after 2018, business
#' as usual is returned.
#' - For countries which have less than 18 beds for any of the years after 2018 (inclusive),
#' the best of business as usual and a **linear change of 0.36 per year up to 2025**,
#' with an upper limit of 18, is returned.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_beds <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario = "scenario",
                            ind = "ind",
                            year = "year",
                            value = "value",
                            iso3 = "iso3",
                            start_year = 2018,
                            ...) {
  this_ind <- ind_ids["beds"]

  accelerate_beds(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate bp to SDG target
#'
#' Accelerate bp by taking the best of business as usual and a **decrease of 25% from
#' 2010 to 2025**. These scenarios are run on the crude bp values, which
#' are then converted back to their age-standardised equivalents using an approximation.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_bp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          year = "year",
                          value = "value",
                          iso3 = "iso3",
                          type_col = "type",
                          start_year = 2018,
                          end_year = 2025,
                          ...) {
  this_ind <- ind_ids["bp"]

  accelerate_bp(df = df,
                  ind_ids = this_ind,
                  ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))

}

#' Accelerate doctors to SDG target
#'
#' Accelerate doctors using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
sdg_doctors <- function(df,
                               ind_ids = billion_ind_codes("uhc"),
                               scenario = "scenario",
                               ind = "ind",
                               ...) {
  this_ind <- ind_ids["doctors"]

  accelerate_doctors(df = df,
                ind_ids = this_ind,
                ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate nurses to SDG target
#'
#' Accelerate nurses using the business as usual scenario.
#'
#'
#' @inherit accelerate_anc4
#'
sdg_nurses <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario = "scenario",
                              ind = "ind",
                              ...) {
  this_ind <- ind_ids["nurses"]

  accelerate_nurses(df = df,
                     ind_ids = this_ind,
                     ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate hwf to SDG target
#'
#' Accelerate hwf by first dividing countries into two groups:
#' - For countries with a 2018 value greater than or equal to the 2018 global median,
#' business as usual is returned.
#' - For countries with a 2018 value less than the 2018 global median, a **linear change
#' of 4.54 per year from 2018 to 2025** is returned.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_hwf <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           iso3 = "iso3",
                           value = "value",
                           year = "year",
                           start_year = 2018,
                           ...) {
  this_ind <- ind_ids["hwf"]

  accelerate_hwf(df = df,
                     ind_ids = this_ind,
                     ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate dtp3 to SDG target
#'
#' Accelerate dtp3 using a customised version of scenario_fixed_target with the
#' following peculiarities:
#' - baseline_year = 2019;
#' - the 2020 value is kept identical to the 2019 (baseline) value;
#' - the target_year is 2030; and
#' - the scenario is then a straight line to the target_value and target_year
#' - the target values for each country are provided by the technical program.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_dtp3 <- function(df,
                            ind_ids = billion_ind_codes("uhc"),
                            scenario = "scenario",
                            ind = "ind",
                            iso3 = "iso3",
                            year = "year",
                            value = "value",
                            start_year = 2018,
                            end_year = 2025,
                            ...) {

  this_ind <- ind_ids["dtp3"]

  accelerate_dtp3(df = df,
                     ind_ids = this_ind,
                     ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}


#' Accelerate fh to SDG target
#'
#' Accelerate fh by taking the best of business as usual and halting upward trends
#' in the data to the 2018 value.
#'
#' @inherit accelerate_anc4
#'
sdg_fh <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["fh"]

  accelerate_fh(df = df,
                     ind_ids = this_ind,
                     ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate fp to SDG target
#'
#' Accelerate fp by dividing the countries into two groups:
#' - For BRN, CYP, FSM, ISL, LUX, and SYC, return business as usual.
#' - For all other countries, take the best of business as usual and the quantile
#' target for quantile_year = 2018 and 5 quantiles (capped by the maximum regional
#' value in 2018).
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_fp <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          iso3 = "iso3",
                          year = "year",
                          value = "value",
                          type_col = "type",
                          ...) {
  this_ind <- ind_ids["fp"]

  accelerate_fp(df = df,
                ind_ids = this_ind,
                ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate fpg to SDG target
#'
#' Accelerate fpg using the business as usual scenario.
#'
#' @inherit accelerate_anc4
#'
sdg_fpg <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["fpg"]

  accelerate_fpg(df = df,
                          ind_ids = this_ind,
                          ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))

}

#' Accelerate itn to SDG target
#'
#' Accelerate itn by taking the best of business as usual and a **fixed target of
#' 80 by 2030**.
#'
#' @inherit accelerate_anc4
#'
sdg_itn <- function(df,
                           ind_ids = billion_ind_codes("uhc"),
                           scenario = "scenario",
                           ind = "ind",
                           ...) {
  this_ind <- ind_ids["itn"]

  accelerate_itn(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}


#' Accelerate pneumo to SDG target
#'
#' Accelerate pneumo by taking the best of business as usual and a **fixed target
#' of 90 by 2025**.
#'
#' @inherit accelerate_anc4
#'
sdg_pneumo <- function(df,
                              ind_ids = billion_ind_codes("uhc"),
                              scenario = "scenario",
                              ind = "ind",
                              ...) {
  this_ind <- ind_ids["pneumo"]

  accelerate_pneumo(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate tb to SDG target
#'
#' Accelerate tb by using a **fixed target of 90 by 2025**.
#'
#' @inherit accelerate_anc4
#'
sdg_tb <- function(df,
                          ind_ids = billion_ind_codes("uhc"),
                          scenario = "scenario",
                          ind = "ind",
                          ...) {
  this_ind <- ind_ids["tb"]

  accelerate_tb(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}

#' Accelerate uhc_sanitation to SDG target
#'
#' Accelerate uhc_sanitation by encouraging the country to reach the mean (or upper
#' threshold) of the quantile it belongs to in 2017, with n = 5 quantiles. Lower
#' and upper limits of 0 and 99, respectively, are also imposed on the results.
#'
#' @inherit accelerate_anc4
#'
sdg_uhc_sanitation <- function(df,
                                      ind_ids = billion_ind_codes("uhc"),
                                      scenario = "scenario",
                                      ind = "ind",
                                      ...) {
  this_ind <- ind_ids["uhc_sanitation"]

  accelerate_uhc_sanitation(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}
# @Alice, there are no countries without data
# @Alice, why is scenario_bau called twice for withdata_df?
# @Alice, need explanation on following comments:
# NB cannot take hpop outputs because the imputed data (45 coutnries) is removed for hpop tobacco
# Is the input for this function hpop_tobacco, instead of uhc_tobacco due to the missing data for UHC?

#' Accelerate uhc_tobacco to SDG target
#'
#' Accelerate uhc_tobacco by first dividing countries into two groups:
#' - For countries without any routine (i.e., estimated) data, business as usual
#' is returned
#' - For countries with routine (i.e., estimated) data, the best of business as
#' usual and a **percent decrease of 30% between 2010 and 2025** is returned. Both
#' scenarios are run on the **crude tobacco usage** values, which are then converted
#' to their age-standardised equivalents using an approximation.
#'
#' @inherit accelerate_anc4
#' @inheritParams calculate_hpop_contributions
#' @inheritParams transform_hpop_data
#'
sdg_uhc_tobacco <- function(df,
                                   ind_ids = billion_ind_codes("uhc"),
                                   scenario = "scenario",
                                   ind = "ind",
                                   ...) {
  this_ind <- ind_ids["uhc_tobacco"]

  accelerate_uhc_tobacco(df = df,
                 ind_ids = this_ind,
                 ...) %>%
    dplyr::mutate("{scenario}" := dplyr::case_when(
      .data[[scenario]] == "acceleration" ~ "sdg",
      TRUE ~ as.character(.data[[scenario]])
    ))
}
