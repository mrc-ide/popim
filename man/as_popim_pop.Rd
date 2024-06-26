% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popim_population.R
\name{as_popim_pop}
\alias{as_popim_pop}
\title{Generate a \code{popim_population} object from a dataframe}
\usage{
as_popim_pop(df)
}
\arguments{
\item{df}{a dataframe with at least columns region, age, year and
pop_size.}
}
\value{
an object of class \code{popim_population}
}
\description{
Checks if the dataframe is suitable (i.e., contains appropriate
columns and data ranges), and if so converts it to a
\code{popim_population} object and returns this.
}
\details{
The input dataframe has to have at least the columns \code{region},
\code{age}, and \code{year}. The output popim_population object is generated
via expand.grid to have consecutive year and age ranges that are
identical for all regions.

If the input dataframe contains a column \code{pop_size}, this must be
numeric and non-negative. If it is missing, this column is
generated and initialised to NA.

If the input dataframe contains a colum \code{immunity}, this must be
numeric, with values between 0 and 1. If it is missing, this
column is generated and initialised to 0.

Any further colunms are simply carried over into the
popim_population object.
}
\examples{
## set up a minimal dataframe to convert to a popim_population object:
df <- expand.grid(region = "UK", age = 0:3, year = 2000:2004, stringsAsFactors = FALSE)
pop <- as_popim_pop(df)
}
\author{
Tini Garske
}
