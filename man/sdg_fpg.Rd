% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdg_uhc.R
\name{sdg_fpg}
\alias{sdg_fpg}
\title{Accelerate fpg to SDG target}
\usage{
sdg_fpg(
  df,
  ind_ids = billion_ind_codes("uhc"),
  scenario = "scenario",
  ind = "ind",
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

\item{scenario}{Column name of column with scenario identifiers. Useful for
calculating contributions on data in long format rather than wide format.}

\item{ind}{Column name of column with indicator names.}

\item{...}{additional parameters to be passed to scenario function}
}
\value{
data frame with acceleration scenario binded to \code{df}. \code{scenario} is
set to \code{acceleration}
}
\description{
Accelerate fpg using the business as usual scenario.
}