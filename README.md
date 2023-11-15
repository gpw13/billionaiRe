
<!-- README.md is generated from README.Rmd. Please edit that file -->

# billionaiRe <a href='https://github.com/gpw13/billionaiRe'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R build
status](https://github.com/gpw13/billionaiRe/workflows/R-CMD-check/badge.svg)](https://github.com/gpw13/billionaiRe/actions)
<!-- badges: end -->

The goal of billionaiRe is to provide an easy interface for using long
format data to calculate the World Health Organization’s Triple
Billions.

## Installation

You can install billionaiRe from [GitHub](https://github.com/) with:

``` r
remotes::install_github("gpw13/billionaiRe", build_vignettes = TRUE)
```

You will need to have already installed the wppdistro and whdh packages,
which is stored in a private repo and only made public upon request from
valid WHO users. Please contact <trubetskoyv@who.int> to request access.

# Calculations

The package is built around a set of functions that calculate the
Billions for the three Billions separately:

- Healthier Populations (HPOP)
- Health Emergencies Protection (HEP)
- Universal Health Coverage (UHC)

## HPOP Billion calculation

To calculate the HPOP Billion, there are a series of functions made
available through the billionaiRe package:

- `transform_hpop_data()` to transform raw values into normalized values
  used within the calculations.
- `add_hpop_populations()` to get relevant population groups for each
  country and indicator.
- `calculate_hpop_contributions()` to calculate indicator level changes
  and contributions to the Billion.
- `calculate_hpop_billion()` to calculate indicator level changes,
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
#> # A tibble: 6 × 10
#>   iso3   year ind            value type  trans…¹ popul…² contr…³ contr…⁴ contr…⁵
#>   <chr> <dbl> <chr>          <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1 AFG    2023 hpop_healthie…    NA <NA>       NA  4.45e7  2.72e7    61.2      NA
#> 2 AFG    2023 hpop_healthie…    NA <NA>       NA  4.45e7 -3.84e7   -86.3      NA
#> 3 AFG    2023 hpop_healthier    NA <NA>       NA  4.45e7 -1.12e7   -25.1      NA
#> 4 AFG    2023 hpop_healthie…    NA <NA>       NA  4.45e7  3.20e7    71.9      NA
#> 5 AFG    2023 hpop_healthie…    NA <NA>       NA  4.45e7 -7.46e7  -168.       NA
#> 6 AFG    2023 hpop_healthie…    NA <NA>       NA  4.45e7 -4.26e7   -95.7      NA
#> # … with abbreviated variable names ¹​transform_value, ²​population,
#> #   ³​contribution, ⁴​contribution_percent, ⁵​contribution_percent_total_pop
```

## UHC Billion calculation

To calculate the UHC Billion, there are a series of functions made
available through the billionaiRe package:

- `transform_uhc_data()` to transform raw values into normalized values
  used within the calculations.
- `calculate_uhc_billion()` to calculate average service coverage,
  financial hardship, and the UHC single measure for each country and
  year in the data frame..
- `calculate_uhc_contribution()` to calculate country-level Billion for
  specified beginning and end year.

Run in sequence, these can calculate the entire UHC Billion, or they can
be run separately to produce different outputs as required. Details on
the inputs of each function are available in their individual
documentation, but below you can see the quick and easy Billions
calculation done using the the sample fake UHC data provided in the
package, `uhc_df`.

``` r
library(billionaiRe)

uhc_df %>%
  transform_uhc_data(end_year = 2023) %>%
  calculate_uhc_billion() %>%
  calculate_uhc_contribution(end_year = 2023, pop_year = 2023) %>% 
  dplyr::filter(ind %in% c("uhc_sm", "asc", "fh"),
                year == 2023)
#> # A tibble: 3 × 9
#>   iso3   year ind    value type      transform_value source      contr…¹ contr…²
#>   <chr> <dbl> <chr>  <dbl> <chr>               <dbl> <chr>         <dbl>   <dbl>
#> 1 AFG    2023 fh      25.4 Projected            74.6 <NA>        -3.00e6  -7.11 
#> 2 AFG    2023 asc     45.3 projected            45.3 WHO DDI ca…  1.72e6   4.06 
#> 3 AFG    2023 uhc_sm  33.8 projected            33.8 WHO DDI ca…  4.41e4   0.104
#> # … with abbreviated variable names ¹​contribution, ²​contribution_percent
```

## HEP Billion calculation

To calculate the HEP Billion, there are a series of functions made
available through the billionaiRe package:

- `transform_hep_data()` to transform raw values into normalized values
  used within the calculations. For now, this is primarily calculating
  the total prevent numerators and denominators for campaign and routine
  data.
- `calculate_hep_components()` to calculate component indicators
  (Prevent coverages), the HEP index, and levels for all components.
- `calculate_hep_billion()` to calculate the change for the three HEP
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
#> # A tibble: 4 × 12
#>   iso3   year ind       value type  source trans…¹ use_d…² use_c…³ level contr…⁴
#>   <chr> <dbl> <chr>     <dbl> <chr> <chr>    <dbl> <lgl>   <lgl>   <dbl>   <dbl>
#> 1 AFG    2023 espar      51.2 Proj… <NA>      51.2 NA      NA          3  5.00e6
#> 2 AFG    2023 detect_r…  91   Proj… <NA>      91   NA      NA          5  2.23e6
#> 3 AFG    2023 prevent    NA   proj… Unite…   100   NA      NA          5  0     
#> 4 AFG    2023 hep_idx    NA   proj… WHO D…    80.7 NA      NA          4  7.22e6
#> # … with 1 more variable: contribution_percent <dbl>, and abbreviated variable
#> #   names ¹​transform_value, ²​use_dash, ³​use_calc, ⁴​contribution
```

## Scenarios

In the Triple Billions and the billionaiRe package context, scenarios
are understood as alternative, plausible, description of how the future
may develop based on a set of defined assumptions.

Scenarios must:

1.  Be [tidy](https://r4ds.had.co.nz/tidy-data.html): each row is a
    unique combination of iso3 country-code, year, indicator, and
    scenario (if relevant).
2.  Contain **all** and **only** the data that is strictly needed for
    calculations with billionaiRe

Four main sets of scenarios can be identified, from the most basic to
the more complex:

1.  Basic scenarios: Those scenarios are the building blocks of the
    other scenarios, but they can also be called on their own. They can
    reach a fixed target at a specified year
    (`scenario_fixed_target()`), follow a specific rate of change
    (`scenario_aroc()`), etc. See [Basic
    scenario](./vignettes/basic-scenarios.html) for more details.
2.  Target scenarios: Target-based scenarios apply indicator-specific
    targets or trajectories to be reached by a specific date
    (e.g. Sustainable Development Goal (SDG) targets (called `sdg`
    scenarios)).
3.  Benchmarking scenarios: Benchmarking scenarios compare performance
    of countries given various grouping and aim at a specified sub-set
    of best performing countries.
4.  Mixed scenarios: Developped with indicator-level subject matter
    knowledge to pick realistic improvement scenarios
    (e.g. `acceleration` scenarios)

If billionaiRe require data that is missing in the scenario, they will
be recycled from other scenarios (see [Data
recycling](./vignettes/data-recycling.html)).

See [Scenarios vignette](./vignettes/basic-scenarios.html) for more
details.

### Quick start on billionaiRe scenarios

`add_scenario()` is the entry point function to all other scenario
functions. It essentially allow to pass a typical billionaiRe data frame
(`df`) and apply the selected scenario function.

``` r
library(billionaiRe)

df <- tibble::tibble(
    value = 60:80,
    year = 2010:2030,
    ind = "pm25",
    type = "reported",
    iso3 = "AFG",
    scenario = "default",
    source = NA_character_
  ) %>%
    dplyr::mutate(scenario = dplyr::case_when(
      year > 2021 ~ "historical",
      TRUE ~ scenario
    ),
    type = dplyr::case_when(
      year > 2021 ~ "projected",
      TRUE ~ type
    ))
```

The choice of scenario function to apply to the `df` is done through the
`scenario_function` parameter. Additional parameters can be passed
through the ellipsis (`...`).

For instance, to halt the rise to the 2010 value by `end_year` (2025 by
default), we can apply a simple function to `df`. This will apply the
`halt_rise` function to all unique combination of country and indicator.
In this case, there is just one combination:

``` r
df %>%
  add_scenario(
    scenario_function = "halt_rise",
    baseline_year = 2010
  )
```

To apply the SDG targets, we use the `sdg`:

``` r
df %>%
  add_scenario(
    scenario_function = "sdg"
  )
```

By default, the scenarios start from the last reported or estimated
value in the default scenario. This can be bypassed by setting
`start_scenario_last_default` to FALSE. The scenario will then start at
`start_year` (2018 by default):

``` r
df %>%
  add_scenario(
    scenario_function = "sdg",
    start_scenario_last_default = FALSE
  )
```
