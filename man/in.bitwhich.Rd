% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\name{in.bitwhich}
\alias{in.bitwhich}
\title{Check existence of integers in table}
\usage{
in.bitwhich(x, table, is.unsorted = NULL)
}
\arguments{
\item{x}{a vector of integer}

\item{table}{a \code{\link[=bitwhich]{bitwhich()}} object or a vector of integer}

\item{is.unsorted}{logical telling the function whether the table is (un)sorted. With
the default \code{NULL} \code{FALSE} is assumed for \code{\link[=bitwhich]{bitwhich()}} tables, otherwise \code{TRUE}}
}
\value{
logical vector
}
\description{
If the table is sorted, this can be much faster than \code{\link[=match]{\%in\%}}
}
\examples{
x <- bitwhich(100)
x[3] <- TRUE
in.bitwhich(c(NA, 2, 3), x)
}
\seealso{
\code{\link[=match]{\%in\%}}
}
