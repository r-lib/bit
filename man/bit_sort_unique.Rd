% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitsort.R
\name{bit_sort_unique}
\alias{bit_sort_unique}
\title{bit sort unique}
\usage{
bit_sort_unique(
  x,
  decreasing = FALSE,
  na.last = NA,
  has.dup = TRUE,
  range_na = NULL
)
}
\arguments{
\item{x}{an integer vector}

\item{decreasing}{\code{FALSE} (ascending) or \code{TRUE} (descending)}

\item{na.last}{\code{NA} removes NAs, \code{FALSE} puts NAs at the beginning, \code{TRUE} puts NAs at
the end}

\item{has.dup}{TRUE (the default) assumes that \code{x} might have duplicates, set to
\code{FALSE} if duplicates are impossible}

\item{range_na}{\code{NULL} calls \code{\link[=range_na]{range_na()}}, optionally the result of \code{\link[=range_na]{range_na()}} can be
given here to avoid calling it again}
}
\value{
a sorted unique integer vector
}
\description{
fast combination of \code{\link[=sort]{sort()}} and \code{\link[=unique]{unique()}} for integers
}
\details{
determines the range of the integers and checks if the density justifies use
of a bit vector; if yes, creates the result using a bit vector; if no, falls back to
\code{sort(unique())}
}
\examples{
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L))
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), na.last=FALSE)
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), na.last=TRUE)
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE)
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE, na.last=FALSE)
bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE, na.last=TRUE)

\dontrun{
x <- sample(1e7, replace=TRUE)
system.time(bit_sort_unique(x))
system.time(sort(unique(x)))
x <- sample(1e7)
system.time(bit_sort_unique(x))
system.time(sort(x))
}
}
\seealso{
\code{\link[=sort]{sort()}}, \code{\link[=unique]{unique()}},
\code{\link[=bit_sort]{bit_sort()}}, \code{\link[=bit_unique]{bit_unique()}}
}
