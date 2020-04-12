# rle utilities for bit and ff
# (c) 2007-2009 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2007-09-03
# Last changed: 2007-10-25

# source("D:/mwp/eanalysis/bit/R/rle.R")

#' Hybrid Index, C-coded utilities
#' 
#' These C-coded utilitites speed up index preprocessing considerably. 
#' 
#' \code{intrle} is by factor 50 faster and needs less RAM (2x its input
#' vector) compared to \code{\link{rle}} which needs 9x the RAM of its input
#' vector.  This is achieved because we allow the C-code of \code{intrle} to
#' break when it turns out, that rle-packing will not achieve a compression
#' factor of 3 or better.  \cr \code{intisasc} is a faster version of
#' \code{\link{is.unsorted}}: it checks whether \code{x} is sorted.\cr 
#' \code{intisdesc} checks for being sorted descending and 
#' by default default assumes that the input \code{x} contains no NAs. 
#' \code{na.method="none"} treats \code{NAs} (the smallest integer) like every other integer and hence returns either \code{TRUE} or \code{FALSE}
#' \code{na.method="break"} checks for \code{NAs} and returns either \code{NA} as soon as  \code{NA} is encountered. 
#' \code{na.method="skip"} checks for \code{NAs} and skips over them, hence decides the return value only on the basis of non-NA values. 
#' 
#' 
#' @param x an integer vector
#' @param na.method one of "none","break","skip", see details. The strange defaults stem from the initial usage. 
#' @return \code{intrle} returns an object of class \code{\link{rle}} or NULL,
#' if rle-compression is not efficient (compression factor <3 or length(x)<3).
#' \cr \code{intisasc} returns one of \code{FALSE, NA, TRUE} \cr
#' \code{intisdesc} returns one of \code{FALSE, TRUE} (if the input contains
#' NAs, the output is undefined)
#' @author Jens Oehlschlägel
#' @seealso \code{\link[ff]{hi}}, \code{\link{rle}}, \code{\link{is.unsorted}},
#' \code{\link[ff]{is.sorted}}
#' @keywords IO data
#' @examples
#' 
#'   intrle(sample(1:10))
#'   intrle(diff(1:10))
#'   intisasc(1:10)
#'   intisasc(10:1)
#'   intisasc(c(NA, 1:10))
#'   intisdesc(1:10)
#'   intisdesc(c(10:1, NA))
#'   intisdesc(c(10:6, NA, 5:1))
#'   intisdesc(c(10:6, NA, 5:1), na.method="skip")
#'   intisdesc(c(10:6, NA, 5:1), na.method="break")
#' 
#' @export
# -- fast and efficient rle ------------------

# integer only
# returns rle object only if n>2 && rle is efficient (length(values)+lengths(lengths))<=length(x)
# returns NULL if n<3 || rle is inefficient
intrle <- function(x){
  stopifnot(is.integer(x))
  .Call(C_R_int_rle, x)
}


# -- check for sorting and NAs, 0s can be checked later when sorted ------------------

#' @describeIn intrle check whether integer vector is ascending
#' @export
intisasc <- function(x, na.method=c("none","break","skip")[2]){
  stopifnot(is.integer(x))
  if (na.method=="break")
    .Call(C_R_int_is_asc_break, x)
  else if (na.method=="none")
    .Call(C_R_int_is_asc_none, x)
  else
    .Call(C_R_int_is_asc_skip, x)
}

#' @describeIn intrle check whether integer vector is descending
#' @export
intisdesc <- function(x, na.method=c("none","break","skip")[1]){
  stopifnot(is.integer(x))
  if (na.method=="none")
    .Call(C_R_int_is_desc_none, x)
  else if (na.method=="break")
    .Call(C_R_int_is_desc_break, x)
  else
    .Call(C_R_int_is_desc_skip, x)
}




# -- basic sequence packing and unpacking ---------------------------------------------------

#' Hybrid Index, rle-pack utilities
#' 
#' Basic utilities for rle packing and unpacking and apropriate methods for
#' \code{\link{rev}} and \code{\link{unique}}.
#' 
#' 
#' @param x in 'rlepack' an integer vector, in the other functions an object of
#' class 'rlepack'
#' @param pack FALSE to suppress packing
#' @param incomparables just to keep R CMD CHECK quiet (not used)
#' @param \dots just to keep R CMD CHECK quiet (not used)
#' @return A list with components \item{ first }{ the first element of the
#' packed sequence } \item{ dat }{ either an object of class \code{\link{rle}}
#' or the complete input vector \code{x} if rle-packing is not efficient }
#' \item{ last }{ the last element of the packed sequence }
#' @author Jens Oehlschlägel
#' @seealso \code{\link[ff]{hi}}, \code{\link{intrle}}, \code{\link{rle}},
#' \code{\link{rev}}, \code{\link{unique}}
#' @keywords IO data
#' @examples
#' 
#'   x <- rlepack(rep(0L, 10))
#' \dontshow{
#'  for (x in list(10:1, 1:10, c(10:1,1:10), c(1:10,10:1), sample(100), sample(100, 100, TRUE), sample(10, 100, TRUE))){
#'    stopifnot(identical(rleunpack(rlepack(x)), x))
#'    stopifnot(identical(rleunpack(unique(rlepack(x))), unique(x)))
#'    stopifnot(identical(anyDuplicated(rlepack(x)), anyDuplicated(x)))
#'  }
#' }
#' 
#' @export
rlepack <- function(x, ...) 
UseMethod("rlepack")

#' @rdname rlepack
#' @export
rlepack.integer <- function(
  x
, pack = TRUE   # TRUE / FALSE
, ... # dummy to keep R CMD check quiet
){
  stopifnot(is.integer(x))
  n <- length(x)
  if (n>1){
    if (pack)
      r <- intrle(diff(x))  # returns NULL if rle is inefficient, old condition was 2*length(r$lengths)<n
    else
      r <- NULL
    structure(list(first=x[1], dat=if (is.null(r)) x else r, last=x[n]), class="rlepack")
  }else if (n==1){
    structure(list(first=x[1], dat=x, last=x[1]), class="rlepack")
  }else{
    structure(list(first=NA_integer_, dat=x, last=NA_integer_), class="rlepack")
  }
}

#' @rdname rlepack
#' @export
rleunpack <- function(x) 
UseMethod("rleunpack")

#' @rdname rlepack
#' @export
rleunpack.rlepack <- function(x){
  if (inherits(x$dat, "rle"))
    as.integer(cumsum(c(x$first, rep(x$dat$values, x$dat$lengths))))
  else
    x$dat
}


#' @rdname rlepack
#' @export
rev.rlepack <- function(x){
  if (inherits(x$dat,"rle")){
    x$dat$values <- -rev(x$dat$values)
    x$dat$lengths <- rev(x$dat$lengths)
  }else{
    x$dat <- rev(x$dat)
  }
  buf <- x$first
  x$first <- x$last
  x$last <- buf
  x
}


# beware: only for sorted input identical with unique()
# beware: rlepack(unique(x)) is faster than unique(rlepack(x))
# we use this only in hi() and as.hi.default()
#' @rdname rlepack
#' @export
unique.rlepack <- function(x
, incomparables = FALSE # dummy to keep R CMD check quiet
, ... # dummy to keep R CMD check quiet
){
  if (inherits(x$dat,"rle")){
    tab <- tabulate(sign(x$dat$values)+2L, nbins=3L)
    if (tab[1] && tab[3])
      x <- rlepack(unique(rleunpack(x)))
    else if (tab[2]){
      x$dat$lengths <- x$dat$lengths[x$dat$values != 0]
      x$dat$values <- x$dat$values[x$dat$values != 0]
    }
    # else nothing to do: no repeated values
  }else{
    x$dat <- unique(x$dat)
  }
  x
}


# beware: only for sorted input identical with unique()
# beware: returns TRUE/FALSE, not position of first duplicate
#' @rdname rlepack
#' @export
anyDuplicated.rlepack <- function(x
, incomparables = FALSE # dummy to keep R CMD check quiet
, ... # dummy to keep R CMD check quiet
){
  if (inherits(x$dat,"rle")){
    tab <- tabulate(sign(x$dat$values)+2L, nbins=3L)
    if (tab[1] && tab[3])
      anyDuplicated(rleunpack(x))
    else if (tab[2]){
      w <- .Call(C_R_first_zero, x$dat$values)
      if (w)
        if(w>1L)
          sum(x$dat$lengths[1:(w-1L)]) + 2L
        else 
          2L
      else
        0L
    }
    else
      0L
  }else{
    anyDuplicated(x$dat)
  }
}

