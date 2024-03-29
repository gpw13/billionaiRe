% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate_hep.R
\name{accelerate_polio_routine}
\alias{accelerate_polio_routine}
\title{Accelerate polio}
\usage{
accelerate_polio_routine(
  df,
  ind_ids = billion_ind_codes("hep"),
  scenario_col = "scenario",
  default_scenario = "default",
  scenario_name = "acceleration",
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

\item{scenario_name}{name of scenario}

\item{...}{additional parameters to be passed to scenario function}
}
\value{
data frame with acceleration scenario binded to \code{df}. \code{scenario_col} is
set to \code{acceleration}
}
\description{
\code{accelerate_polio_routine()} accelerate polio routine by aiming at a +20\%
percent change between 2015 and 2025 AROC.
}
\details{
Runs:
\itemize{
\item \code{scenario_aroc(df, aroc_type = "percent_change", percent_change = 20, baseline_year = 2015, target_year = 2025, small_is_best = FALSE)}
}
}
\seealso{
HEP acceleration scenarios
\code{\link{accelerate_cholera_campaign}()},
\code{\link{accelerate_detect}()},
\code{\link{accelerate_espar}()},
\code{\link{accelerate_measles_routine}()},
\code{\link{accelerate_meningitis_campaign}()},
\code{\link{accelerate_yellow_fever_campaign}()}
}
\concept{hep_acceleration}
