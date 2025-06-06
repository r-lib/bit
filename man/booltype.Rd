% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bit.R
\name{booltype}
\alias{booltype}
\title{Diagnosing boolean types}
\usage{
booltype(x)
}
\arguments{
\item{x}{an R object}
}
\value{
one scalar element of \code{\link[=booltypes]{booltypes()}} in case of 'nobool' it carries a name
attribute with the data type.
}
\description{
Specific methods for \code{booltype} are required, where non-unary methods can combine
multiple bollean types, particularly boolean binary operators.
}
\details{
Function \code{booltype} returns the boolean type of its argument.
There are currently six boolean types, \code{booltypes} is an \code{\link[=ordered]{ordered()}} vector with the
following ordinal \code{\link[=levels]{levels()}}:
\itemize{
\item nobool: non-boolean type
\item \code{\link[=logical]{logical()}}: for representing any boolean data including \code{NA}
\item \code{\link[=bit]{bit()}}: for representing dense boolean data
\item \code{\link[=bitwhich]{bitwhich()}}: for representing sparse (skewed) boolean data
\item \code{\link[=which]{which()}}: for representing sparse boolean data with few `TRUE
\item \code{\link[=ri]{ri()}}: range-indexing, for representing sparse boolean data with a single range of
\code{TRUE}
}
}
\note{
do not rely on the internal integer codes of these levels, we might add-in
\code{\link[ff:hi]{hi}} later
}
\examples{
unname(booltypes)
str(booltypes)
sapply(
  list(double(), integer(), logical(), bit(), bitwhich(), as.which(), ri(1, 2, 3)),
  booltype
)
}
\seealso{
\code{\link[=booltypes]{booltypes()}}, \code{\link[=is.booltype]{is.booltype()}}, \code{\link[=as.booltype]{as.booltype()}}
}
