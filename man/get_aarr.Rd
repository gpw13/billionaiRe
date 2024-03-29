% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-scenarios.R
\name{get_aarr}
\alias{get_aarr}
\title{Get Average Annual Rate of Reduction}
\usage{
get_aarr(year, value)
}
\arguments{
\item{year}{vector of years}

\item{value}{vector of values on which the AARR will be calculated. Must be
the same length as \code{year}}
}
\value{
a numeric being the beta value representing the AARR
}
\description{
Gets the \href{https://data.unicef.org/resources/technical-note-calculate-average-annual-rate-reduction-aarr-underweight-prevalence/}{Average Annual Rate of Reduction (AARC)}
by fitting the log of value by year with a linear regression.
}
\seealso{
General scenario functions
\code{\link{add_scenario}()},
\code{\link{calculate_aarc}()},
\code{\link{calculate_aroc}()},
\code{\link{exec_scenario}()},
\code{\link{fill_cols_scenario}()},
\code{\link{flat_extrapolation}()},
\code{\link{get_baseline_value}()},
\code{\link{get_baseline_year}()},
\code{\link{get_last_value}()},
\code{\link{get_last_year_scenario}()},
\code{\link{get_latest_aarc}()},
\code{\link{get_percent_change_aarc}()},
\code{\link{get_target_aarc}()},
\code{\link{remove_unwanted_scenarios}()},
\code{\link{trim_values}()},
\code{\link{trim_years}()}
}
\concept{scenarios}
\keyword{internal}
