% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitsort.R
\name{range_nanozero}
\alias{range_nanozero}
\title{Remove zeros and get range and number of NAs}
\usage{
range_nanozero(x)
}
\arguments{
\item{x}{an integer vector}
}
\value{
an integer vector without zeros and with an attribute \code{\link[=range_na]{range_na()}}  with three
elements:
\enumerate{
\item min integer
\item max integer
\item number of NAs
}
}
\description{
Remove zeros and get range and number of NAs
}
\examples{
range_nanozero(c(0L, 1L, 2L, NA))
}
\seealso{
\code{\link[=range_na]{range_na()}} and \code{\link[=range_sortna]{range_sortna()}}
}
