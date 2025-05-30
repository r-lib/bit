% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\name{bitwhich_representation}
\alias{bitwhich_representation}
\title{Diagnose representation of bitwhich}
\usage{
bitwhich_representation(x)
}
\arguments{
\item{x}{a \code{\link[=bitwhich]{bitwhich()}} object}
}
\value{
a scalar, one of \code{logical()}, \code{FALSE}, \code{TRUE}, \code{-1} or \code{1}
}
\description{
Diagnose representation of bitwhich
}
\examples{
bitwhich_representation(bitwhich())
bitwhich_representation(bitwhich(12, FALSE))
bitwhich_representation(bitwhich(12, TRUE))
bitwhich_representation(bitwhich(12, -3))
bitwhich_representation(bitwhich(12, 3))
}
