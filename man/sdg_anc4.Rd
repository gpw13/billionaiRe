% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdg_uhc.R
\name{sdg_anc4}
\alias{sdg_anc4}
\title{Accelerate anc4 to SDG target}
\usage{
sdg_anc4(
  df,
  scenario_col = "scenario",
  default_scenario = "default",
  bau_scenario = "historical",
  scenario_name = "sdg",
  ind_ids = billion_ind_codes("uhc"),
  end_year = 2030,
  ...
)
}
\arguments{
\item{df}{Data frame in long format, where 1 row corresponds to a specific
country, year, and indicator.}

\item{scenario_col}{Column name of column with scenario identifiers. Useful for
calculating contributions on data in long format rather than wide format.}

\item{default_scenario}{name of the default scenario.}

\item{bau_scenario}{name of scenario to be used for business as usual.
Default is \code{historical}.}

\item{scenario_name}{name of scenario}

\item{ind_ids}{Named vector of indicator codes for input indicators to the Billion.
Although separate indicator codes can be used than the standard, they must
be supplied as a named vector where the names correspond to the output of
\code{billion_ind_codes()}.}

\item{end_year}{End year(s) for contribution calculation, defaults to 2019 to 2025.}

\item{...}{additional parameters to be passed to scenario function}
}
\value{
data frame with sdg scenario binded to \code{df}. \code{scenario} is
set to \code{sdg}
}
\description{
Put \code{anc4} on SDG target trajectory by aiming at 100 by 2030 if there are 2 values or more
reported. Otherwise, business as usual.
}
\seealso{
UHC SDG scenarios
\code{\link{sdg_art}()},
\code{\link{sdg_beds}()},
\code{\link{sdg_bp}()},
\code{\link{sdg_doctors}()},
\code{\link{sdg_dtp3}()},
\code{\link{sdg_fh}()},
\code{\link{sdg_fpg}()},
\code{\link{sdg_fp}()},
\code{\link{sdg_hwf}()},
\code{\link{sdg_itn}()},
\code{\link{sdg_nurses}()},
\code{\link{sdg_pneumo}()},
\code{\link{sdg_tb}()},
\code{\link{sdg_uhc_sanitation}()},
\code{\link{sdg_uhc_tobacco}()}
}
\concept{uhc_sdg}
