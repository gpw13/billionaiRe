
<!-- README.md is generated from README.Rmd. Please edit that file -->

# billionaiRe <a href='https://github.com/caldwellst/billionaiRe'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/caldwellst/billionaiRe/workflows/R-CMD-check/badge.svg)](https://github.com/caldwellst/billionaiRe/actions)
<!-- badges: end -->

The goal of billionaiRe is to provide an easy interface for using long
format data to calculate the World Health Organization’s Triple
Billions.

## Installation

You can install billionaiRe from [GitHub](https://github.com/) with:

``` r
remotes::install_github("caldwellst/billionaiRe", build_vignettes = TRUE)
```

You will need to have already installed the wppdistro package, which is
stored in a private repo and only made public upon request. Please
contact <caldwellst@who.int> to request access.

# Calculations

The package is built around a set of functions that separately calculate
the Billions for the three Billions separately:

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
-   `calculate_hpop_billion()` to calculate country-level Billion,
    adjusting for double counting.

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
  calculate_hpop_contributions() %>%
  calculate_hpop_billion()
#> # A tibble: 1 x 5
#>   iso3  healthier unhealthier net_healthier perc_healthier
#> * <chr>     <dbl>       <dbl>         <dbl>          <dbl>
#> 1 AFG   25608812.  -35897603.    -10288791.          -24.7
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
  calculate_uhc_contribution()
#> # A tibble: 3 x 6
#>   iso3  ind   `2018` `2023` population contribution
#>   <chr> <chr>  <dbl>  <dbl>      <dbl>        <dbl>
#> 1 AFG   FH      18.3   25.4   41681232     2963536.
#> 2 AFG   ASC     40.9   43.0   41681232      863800.
#> 3 AFG   UHC     33.5   32.1   41681232     -568702.
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
  calculate_hep_billion()
#> # A tibble: 4 x 5
#>   iso3   year ind            change contribution
#>   <chr> <dbl> <chr>           <dbl>        <dbl>
#> 1 AFG    2023 detect_respond    5       2084062.
#> 2 AFG    2023 espar            11.2     4680802.
#> 3 AFG    2023 hep_idx          21.7    23326420.
#> 4 AFG    2023 prevent          39.7    16561556.
```
