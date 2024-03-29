% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_hep.R
\name{transform_prev_routine_data}
\alias{transform_prev_routine_data}
\title{Transform Prevent routine data}
\usage{
transform_prev_routine_data(
  df,
  value_col,
  transform_value_col,
  scenario_col,
  ind_ids
)
}
\arguments{
\item{df}{Data frame in long format, where 1 row corresponds to a specific
country, year, and indicator.}

\item{value_col}{Column name of column with indicator values.}

\item{transform_value_col}{Column name of column(s) with transformed indicator values,
used to calculate contributions.}

\item{scenario_col}{Column name of column with scenario identifiers. Useful for
calculating contributions on data in long format rather than wide format.}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}
}
\description{
Prevent routine data is now stored raw using the percent coverage of the indicator.
We want to transform this back into a numerator value for use within \code{pathogen_calc}.
}
\seealso{
HEP functions
\code{\link{add_hep_populations}()},
\code{\link{calculate_hep_billion}()},
\code{\link{calculate_hep_components}()},
\code{\link{hep_df}},
\code{\link{transform_hep_data}()},
\code{\link{transform_prev_cmpgn_data}()}

Other transform: 
\code{\link{transform_prev_cmpgn_data}()}
}
\concept{hep}
\concept{transform}
\keyword{internal}
