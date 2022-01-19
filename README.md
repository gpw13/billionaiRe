
<!-- README.md is generated from README.Rmd. Please edit that file -->

# billionaiRe <a href='https://github.com/gpw13/billionaiRe'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/gpw13/billionaiRe/workflows/R-CMD-check/badge.svg)](https://github.com/gpw13/billionaiRe/actions)
[![codecov](https://codecov.io/gh/gpw13/billionaiRe/branch/main/graph/badge.svg?token=IT9RI3OOKV)](https://codecov.io/gh/gpw13/billionaiRe)
<!-- badges: end -->

The goal of billionaiRe is to provide an easy interface for using long
format data to calculate the World Health Organization’s Triple
Billions.

## Installation

You can install billionaiRe from [GitHub](https://github.com/) with:

``` r
remotes::install_github("gpw13/billionaiRe", build_vignettes = TRUE)
```

You will need to have already installed the wppdistro package, which is
stored in a private repo and only made public upon request. Please
contact <kanjim@who.int> and <messeillere@who.int> to request access.

# Calculations

The package is built around a set of functions that calculate the
Billions for the three Billions separately:

-   Healthier Populations (HPOP)
-   Health Emergencies Protection (HEP)
-   Universal Health Coverage (UHC)

## HPOP Billion calculation

To calculate the HPOP Billion, there are a series of functions made
available through the billionaiRe package:

-   `transform_hpop_data()` to transform raw values into normalized
    values used within the calculations.
-   `add_hpop_populations()` to get relevant population groups for each
    country and indicator.
-   `calculate_hpop_contributions()` to calculate indicator level
    changes and contributions to the Billion.
-   `calculate_hpop_billion()` to calculate indicator level changes,
    country-level Billion, adjusting for double counting, and all
    contributions.

Run in sequence, these can calculate the entire HPOP Billion, or they
can be run separately to produce different outputs as required. Details
on the inputs of each function are available in their individual
documentation, but below you can see the quick and easy Billions
calculation done using the sample fake HPOP data provided in the
package, `hpop_df`.

``` r
library(billionaiRe)

hpop_df %>%
  transform_hpop_data() %>%
  add_hpop_populations() %>%
  calculate_hpop_billion() %>%
  dplyr::filter(stringr::str_detect(ind, "hpop_healthier"))
#> # A tibble: 6 x 9
#>   iso3   year ind                  value transform_value population contribution
#>   <chr> <dbl> <chr>                <dbl>           <dbl>      <dbl>        <dbl>
#> 1 AFG    2023 hpop_healthier_plus     NA              NA   41681232    25608812.
#> 2 AFG    2023 hpop_healthier_minus    NA              NA   41681232   -35897603.
#> 3 AFG    2023 hpop_healthier          NA              NA   41681232   -10288791.
#> 4 AFG    2023 hpop_healthier_plus~    NA              NA   41681232    30125312.
#> 5 AFG    2023 hpop_healthier_minu~    NA              NA   41681232   -69269569.
#> 6 AFG    2023 hpop_healthier_dbl_~    NA              NA   41681232   -39144257.
#> # ... with 2 more variables: contribution_percent <dbl>,
#> #   contribution_percent_total_pop <dbl>
```

## UHC Billion calculation

To calculate the UHC Billion, there are a series of functions made
available through the billionaiRe package:

-   `transform_uhc_data()` to transform raw values into normalized
    values used within the calculations.
-   `calculate_uhc_billion()` to calculate average service coverage,
    financial hardship, and the UHC single measure for each country and
    year in the data frame..
-   `calculate_uhc_contribution()` to calculate country-level Billion
    for specified beginning and end year.

Run in sequence, these can calculate the entire UHC Billion, or they can
be run separately to produce different outputs as required. Details on
the inputs of each function are available in their individual
documentation, but below you can see the quick and easy Billions
calculation done using the the sample fake UHC data provided in the
package, `uhc_df`.

``` r
library(billionaiRe)

uhc_df %>%
  transform_uhc_data() %>%
  calculate_uhc_billion() %>%
  calculate_uhc_contribution(end_year = 2023) %>%
  dplyr::filter(ind %in% c("uhc_sm", "asc", "fh"),
                year == 2023)
#> # A tibble: 3 x 9
#>   iso3   year ind    value transform_value type      source         contribution
#>   <chr> <dbl> <chr>  <dbl>           <dbl> <chr>     <chr>                 <dbl>
#> 1 AFG    2023 fh      25.4            25.4 <NA>      <NA>               2963536.
#> 2 AFG    2023 asc     45.6            45.6 projected WHO DDI calcu~     1607136.
#> 3 AFG    2023 uhc_sm  34.0            34.0 projected WHO DDI calcu~      -38238.
#> # ... with 1 more variable: contribution_percent <dbl>
```

## HEP Billion calculation

To calculate the HEP Billion, there are a series of functions made
available through the billionaiRe package:

-   `transform_hep_data()` to transform raw values into normalized
    values used within the calculations. For now, this is primarily
    calculating the total prevent numerators and denominators for
    campaign and routine data.
-   `calculate_hep_components()` to calculate component indicators
    (Prevent coverages), the HEP index, and levels for all components.
-   `calculate_hep_billion()` to calculate the change for the three HEP
    components (DNR, Prepare, and Prevent), their contribution to the
    Billion, and overall HEPI change and contribution.

Run in sequence, these can calculate the entire HEP Billion, or they can
be run separately to produce different outputs as required. Details on
the inputs of each function are available in their individual
documentation, but below you can see the quick and easy Billions
calculation done using the sample fake HEP data provided in the package,
`hep_df`.

``` r
library(billionaiRe)

hep_df %>%
  transform_hep_data() %>%
  calculate_hep_components() %>%
  calculate_hep_billion(end_year = 2023) %>%
  dplyr::filter(ind %in% c("prevent",
                           "espar",
                           "detect_respond",
                           "hep_idx"),
                year == 2023)
#> # A tibble: 4 x 10
#>   iso3   year ind            value type       source transform_value level contribution
#>   <chr> <dbl> <chr>          <dbl> <chr>      <chr>            <dbl> <dbl>        <dbl>
#> 1 AFG    2023 espar           51.2 Projection <NA>              51.2     3     4680802.
#> 2 AFG    2023 detect_respond  91   Projection <NA>              91       5     2084062.
#> 3 AFG    2023 prevent         NA   Projection WHO D~           100       5           0 
#> 4 AFG    2023 hep_idx         NA   Projection WHO D~            80.7     4     6764864.
#> # ... with 1 more variable: contribution_percent <dbl>
```
