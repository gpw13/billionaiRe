% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_hpop_billion.R
\name{calculate_hpop_billion_single}
\alias{calculate_hpop_billion_single}
\title{Calculate the HPOP Billion for one column of change}
\usage{
calculate_hpop_billion_single(
  change,
  contribution_col,
  df,
  pop_year,
  scenario_col
)
}
\arguments{
\item{change}{Column name of column with change value}

\item{contribution_col}{Column name of column(s) to store contribution (population)
values. Must be the same length as \code{transform_value_col}.}

\item{df}{Data frame in long format, where 1 row corresponds to a specific
country, year, and indicator.}

\item{pop_year}{Year used to pull in HPOP populations, defaults to 2025.}

\item{scenario_col}{Column name of column with scenario identifiers. Useful for
calculating contributions on data in long format rather than wide format.}
}
\description{
Calculate the HPOP Billion for one column of change
}
\keyword{internal}
