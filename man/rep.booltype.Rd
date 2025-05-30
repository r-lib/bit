% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\name{rep.booltype}
\alias{rep.booltype}
\alias{rep.bit}
\alias{rep.bitwhich}
\title{Replicating bit and bitwhich vectors}
\usage{
\method{rep}{bit}(x, times = 1L, length.out = NA, ...)

\method{rep}{bitwhich}(x, times = 1L, length.out = NA, ...)
}
\arguments{
\item{x}{bit or bitwhich object}

\item{times}{number of replications}

\item{length.out}{final length of replicated vector (dominates times)}

\item{...}{not used}
}
\value{
An object of class 'bit' or 'bitwhich'
}
\description{
Creating new bit or bitwhich by recycling such vectors
}
\examples{

 rep(as.bit(c(FALSE, TRUE)), 2)
 rep(as.bit(c(FALSE, TRUE)), length.out=7)
 rep(as.bitwhich(c(FALSE, TRUE)), 2)
 rep(as.bitwhich(c(FALSE, TRUE)), length.out=1)
}
\seealso{
\code{\link[=rep]{rep()}}, \code{\link[=bit]{bit()}} , \code{\link[=bitwhich]{bitwhich()}}
}
\author{
Jens Oehlschlägel
}
\keyword{classes}
\keyword{logic}
