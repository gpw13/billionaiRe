% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate_target_uhc.R
\name{accelerate_target_uhc_sanitation}
\alias{accelerate_target_uhc_sanitation}
\title{Accelerate uhc_sanitation}
\usage{
accelerate_target_uhc_sanitation(df, ind_ids = billion_ind_codes("uhc"), ...)
}
\arguments{
\item{df}{Data frame in long format, where 1 row corresponds to a specific
country, year, and indicator.}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}

\item{...}{additional parameters to be passed to scenario function}
}
\description{
Accelerate uhc_sanitation by targeting to reach 95\% by 2030.
}
\seealso{
Target scenario
\code{\link{accelerate_target_anc4}()},
\code{\link{accelerate_target_art}()},
\code{\link{accelerate_target_beds}()},
\code{\link{accelerate_target_bp}()},
\code{\link{accelerate_target_doctors}()},
\code{\link{accelerate_target_dtp3}()},
\code{\link{accelerate_target_fh}()},
\code{\link{accelerate_target_fpg}()},
\code{\link{accelerate_target_fp}()},
\code{\link{accelerate_target_hwf}()},
\code{\link{accelerate_target_itn}()},
\code{\link{accelerate_target_nurses}()},
\code{\link{accelerate_target_pneumo}()},
\code{\link{accelerate_target_tb}()},
\code{\link{accelerate_target_uhc_tobacco}()}
}
\concept{accelerate_target}
