% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_wrangling.R
\name{save_wrangled_output}
\alias{save_wrangled_output}
\title{Save the output to disk after ensuring column specs}
\usage{
save_wrangled_output(df, path)
}
\arguments{
\item{df}{data frame the output}

\item{path}{the path where the output should be saved}
}
\value{
a data frame. This is the modified dataframe that's saved to disk if
the data frame has all the columns expected by xMart. Otherwise, it simply return
the input data frame.
}
\description{
Helper functions that saves a wrangled output data frame to the disk if it has
the correct columns as required by the Triple Billions xMart tables.
}
\details{
The function returns a data frame (like \code{readr::write_csv()}) in order to allow
it to work with pipes better.
}