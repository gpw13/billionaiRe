% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hpop_df}
\alias{hpop_df}
\title{HPOP generated example data}
\format{
A data frame with 34 rows and 5 variables:
\describe{
\item{iso3}{Country ISO3 codes.}
\item{year}{Year.}
\item{ind}{HPOP indicator code.}
\item{value}{Raw indicator value.}
\item{type}{Data type.}
}
}
\usage{
hpop_df
}
\description{
Generated (fake) HPOP data used to test the Billions calculations code within the billionaiRe
package.
}
\details{
See the HPOP vignette for its example use:

\href{../doc/hpop.html}{\code{vignette("hpop", package = "billionaiRe")}}
}
\seealso{
Other hpop: 
\code{\link{add_hpop_populations}()},
\code{\link{calculate_hpop_billion_change}()},
\code{\link{calculate_hpop_billion}()},
\code{\link{calculate_hpop_contributions}()},
\code{\link{transform_hpop_data}()},
\code{\link{transform_hpop_single}()},
\code{\link{untransform_hpop_data}()},
\code{\link{untransform_hpop_single}()}
}
\concept{hpop}
\keyword{datasets}
