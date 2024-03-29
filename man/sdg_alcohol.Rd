% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdg_hpop.R
\name{sdg_alcohol}
\alias{sdg_alcohol}
\title{Accelerate alcohol to SDG target}
\usage{
sdg_alcohol(
  df,
  ind_ids = billion_ind_codes("hpop"),
  scenario_col = "scenario",
  default_scenario = "default",
  ...
)
}
\arguments{
\item{df}{Data frame in long format, where 1 row corresponds to a specific
country, year, and indicator.}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}

\item{scenario_col}{Column name of column with scenario identifiers. Useful for
calculating contributions on data in long format rather than wide format.}

\item{default_scenario}{name of the default scenario.}

\item{...}{additional parameters to be passed to scenario function}
}
\description{
Put alcohol on SDG trajectory by -10 percent from 2010 value.
}
\seealso{
HPOP SDG scenarios
\code{\link{sdg_adult_obese}()},
\code{\link{sdg_child_obese}()},
\code{\link{sdg_child_viol}()},
\code{\link{sdg_devontrack}()},
\code{\link{sdg_fuel}()},
\code{\link{sdg_hpop_sanitation_rural}()},
\code{\link{sdg_hpop_sanitation_urban}()},
\code{\link{sdg_hpop_sanitation}()},
\code{\link{sdg_hpop_tobacco}()},
\code{\link{sdg_ipv}()},
\code{\link{sdg_overweight}()},
\code{\link{sdg_pm25}()},
\code{\link{sdg_road}()},
\code{\link{sdg_stunting}()},
\code{\link{sdg_suicide}()},
\code{\link{sdg_transfats}()},
\code{\link{sdg_wasting}()},
\code{\link{sdg_water_rural}()},
\code{\link{sdg_water_urban}()},
\code{\link{sdg_water}()}
}
\concept{hpop_sdg}
