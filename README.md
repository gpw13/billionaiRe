
<!-- README.md is generated from README.Rmd. Please edit that file -->

billionaiRe
===========

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/caldwellst/whoville.svg?branch=master)](https://travis-ci.com/caldwellst/billionaiRe)
[![R build
status](https://github.com/caldwellst/whoville/workflows/R-CMD-check/badge.svg)](https://github.com/caldwellst/billionaiRe/actions)
<!-- badges: end -->

The goal of billionaiRe is to provide an easy interface for using long
format data to calculate the World Health Organization’s Triple
Billions.

Installation
------------

You can install billionaiRe from [GitHub](https://github.com/) with:

    remotes::install_github("caldwellst/billionaiRe", build_vignettes = TRUE)

You will need to have already installed the wppdistro package, which is
stored in a private repo and only made public upon request. Please
contact
<a href="mailto:caldwellst@who.int" class="email">caldwellst@who.int</a>
to request access.

Calculations
============

The package is built around a set of functions that separately calculate
the Billions for the three Billions separately:

-   Healthier Populations (HPOP)
-   Health Emergencies Protection (HEP)
-   Universal Health Coverage (UHC)

HPOP Billion calculation
------------------------

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

    library(billionaiRe)

    hpop_df %>%
      transform_hpop_data() %>%
      add_hpop_populations() %>%
      calculate_hpop_contributions() %>%
      calculate_hpop_billion()
    #> # A tibble: 1 x 5
    #>   iso3  healthier unhealthier net_healthier perc_healthier
    #>   <chr>     <dbl>       <dbl>         <dbl>          <dbl>
    #> 1 AFG   34908262.  -35989503.     -1081241.          -2.59

UHC Billion calculation
-----------------------

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

    library(billionaiRe)

    uhc_df %>%
      transform_uhc_data() %>%
      calculate_uhc_billion() %>%
      calculate_uhc_contribution()
    #> # A tibble: 3 x 6
    #>   iso3  ind   `2018` `2023` population contribution
    #>   <chr> <chr>  <dbl>  <dbl>      <dbl>        <dbl>
    #> 1 AFG   FH      18.3   25.4   41681232    -2963536.
    #> 2 AFG   ASC     41.6   45.5   41681232     1596043.
    #> 3 AFG   UHC     34.0   33.9   41681232      -42994.
