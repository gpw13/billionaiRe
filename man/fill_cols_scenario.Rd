% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-scenarios.R
\name{fill_cols_scenario}
\alias{fill_cols_scenario}
\title{Infills scenarios with \code{values} when \code{cols} are missing values.}
\usage{
fill_cols_scenario(
  df,
  scenario_col = "scenario",
  cols = list("type", "source", "use_dash", "use_calc"),
  values = list("projected", NULL, TRUE, TRUE)
)
}
\arguments{
\item{df}{(data.frame) containing the data}

\item{scenario_col}{Column name of column with scenario identifiers.}

\item{cols}{(list) of columns to be infilled}

\item{values}{(list) of values to infill with.}
}
\description{
Infills scenarios with \code{values} when \code{cols} are missing values.
}
\seealso{
General scenario functions
\code{\link{add_scenario}()},
\code{\link{calculate_aarc}()},
\code{\link{calculate_aroc}()},
\code{\link{exec_scenario}()},
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
