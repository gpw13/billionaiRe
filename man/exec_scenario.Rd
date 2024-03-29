% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-scenarios.R
\name{exec_scenario}
\alias{exec_scenario}
\title{Execute a scenario}
\usage{
exec_scenario(df, fn, parameters)
}
\arguments{
\item{df}{(data.frame) containing the data.}

\item{fn}{function to be executed}

\item{parameters}{parameters to be passed to \code{fn}}
}
\description{
Simple wrapper around \link[rlang:exec]{rlang::exec} to pass \code{parameters} and \code{df} to \code{fn}
}
\seealso{
General scenario functions
\code{\link{add_scenario}()},
\code{\link{calculate_aarc}()},
\code{\link{calculate_aroc}()},
\code{\link{fill_cols_scenario}()},
\code{\link{flat_extrapolation}()},
\code{\link{get_aarr}()},
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
