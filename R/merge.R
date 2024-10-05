# Fast methods for sorted integers
# (c) 2016-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk


# Attention:
#   must not use as.integer() on x and y here, otherwise we cannot pass bitwhich objects
#   (as.integer) would turn positions into 0/1

#' Fast functions for sorted sets of integer
#'
#' The \code{merge_} functions allow unary and binary operations
#' on (ascending) sorted vectors of \code{link{integer}}.
#' \code{merge_rev(x)} will do in one scan what costs two scans in \code{-\link{rev}(x)}, see also \code{\link{reverse_vector}(x)}.
#' Many of these \code{merge_} can optionally scan their input in reverse order (and switch the sign),
#' which again saves extra scans for calling \code{merge_rev(x)} first.
#'
#' @details These are low-level functions and hence do not check whether the set is actually sorted.
#' Note that the `merge_*` and `merge_range*` functions have no special treatment for `NA`.
#' If vectors with `NA` are sorted ith `NA` in the first positions (`na.last=FALSE`) and arguments `revx=` or `revy=` have not been used,
#' then `NAs` are treated like ordinary integers.
#' `NA` sorted elsewhere or using `revx=` or `revy=` can cause unexpected results
#' (note for example that `revx=` switches the sign on all integers but `NAs`).
#' \cr
#' \cr
#' The *binary* `merge_*` functions have a `method="exact"`
#' which in both sets treats consecutive occurrences of the same value as if they were different values,
#' more precisely they are handled as if the identity of ties were tuples of \code{ties, rank(ties)}.
#' \code{method="exact"} delivers unique output if the input is unique, and in this case works faster than \code{method="unique"}.
#'
#' @note xx OPTIMIZATION OPPORTUNITY These are low-level functions could be optimized with initial binary search (not findInterval, which coerces to double).
#'
#' @param x a sorted set
#' @param rx range of integers given as \code{\link{ri}} or as a two-element \code{\link{integer}}
#' @param y a sorted set
#' @param revx default \code{FALSE}, set to \code{TRUE} to reverse scan parameter 'x'
#' @param revy default \code{FALSE}, set to \code{TRUE} to reverse scan parameter 'y'
#' @param nomatch integer value returned for non-matched elements, see \code{\link{match}}
#' @param method one of "unique", "exact" (or "all") which governs how to treat ties, see the function descriptions
#'
#' @return \code{merge_rev(x)} returns \code{-\link{rev}(x)} for \code{\link{integer}} and \code{\link{double}} and \code{!\link{rev}(x)} for \code{\link{logical}}
#'
#' @examples
#' merge_rev(1:9)
#'
#' merge_match(1:7, 3:9)
#' #' merge_match(merge_rev(1:7), 3:9)
#' merge_match(merge_rev(1:7), 3:9, revx=TRUE)
#' merge_match(merge_rev(1:7), 3:9, revy=TRUE)
#' merge_match(merge_rev(1:7), merge_rev(3:9))
#'
#' merge_in(1:7, 3:9)
#' merge_notin(1:7, 3:9)
#'
#' merge_anyDuplicated(c(1L,1L,2L,3L))
#' merge_duplicated(c(1L,1L,2L,3L))
#' merge_unique(c(1L,1L,2L,3L))
#'
#' merge_union(c(1L,2L,2L,2L), c(2L,2L,3L))
#' merge_union(c(1L,2L,2L,2L), c(2L,2L,3L), method="exact")
#' merge_union(c(1L,2L,2L,2L), c(2L,2L,3L), method="all")
#'
#' merge_setdiff(c(1L,2L,2L,2L), c(2L,2L,3L))
#' merge_setdiff(c(1L,2L,2L,2L), c(2L,2L,3L), method="exact")
#' merge_setdiff(c(1L,2L,2L), c(2L,2L,2L,3L), method="exact")
#'
#' merge_symdiff(c(1L,2L,2L,2L), c(2L,2L,3L))
#' merge_symdiff(c(1L,2L,2L,2L), c(2L,2L,3L), method="exact")
#' merge_symdiff(c(1L,2L,2L), c(2L,2L,2L,3L), method="exact")
#'
#' merge_intersect(c(1L,2L,2L,2L), c(2L,2L,3L))
#' merge_intersect(c(1L,2L,2L,2L), c(2L,2L,3L), method="exact")
#'
#' merge_setequal(c(1L,2L,2L), c(1L,2L))
#' merge_setequal(c(1L,2L,2L), c(1L,2L,2L))
#' merge_setequal(c(1L,2L,2L), c(1L,2L), method="exact")
#' merge_setequal(c(1L,2L,2L), c(1L,2L,2L), method="exact")
#'
#' @export

merge_rev <- function(x){
  .Call(C_R_merge_rev, x)
}

#' @describeIn merge_rev returns integer positions of sorted set x in sorted set y, see \code{\link{match}(x, y, ...)}
#' @export
merge_match <- function(x, y, revx=FALSE, revy=FALSE, nomatch = NA_integer_){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_match, x, y, as.logical(revx), as.logical(revy), nomatch=as.integer(nomatch))
}

# xx OPTIMIZATION OPPORTUNITY this could be optimized with proper binary search (not findInterval, which coerces to double)
#' @describeIn merge_rev returns logical existence of sorted set x in sorted set y, see \code{x \link{\%in\%} y}
#' @export
merge_in <- function(x, y, revx=FALSE, revy=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_in, x, y, as.logical(revx), as.logical(revy))
}

# xx OPTIMIZATION OPPORTUNITY this could be optimized with proper binary search (not findInterval, which coerces to double)
#' @describeIn merge_rev returns logical in-existence of sorted set x in sorted set y, see \code{!(x \link{\%in\%} y)}
#' @export
merge_notin <- function(x, y, revx=FALSE, revy=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_notin, x, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev returns the duplicated status of a sorted set x, see \code{\link{duplicated}}
#' @export
merge_duplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_duplicated, x, as.logical(revx))
}

#' @describeIn merge_rev returns the anyDuplicated status of a sorted set x, see \code{\link{anyDuplicated}}
#' @export
merge_anyDuplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_anyDuplicated, x, as.logical(revx))
}

#' @describeIn merge_rev returns the sumDuplicated status of a sorted set x, see \code{\link{bit_sumDuplicated}}
#' @export
merge_sumDuplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_sumDuplicated, x, as.logical(revx))
}
#' @describeIn merge_rev returns unique elements of sorted set x, see \code{\link{unique}}
#' @export
merge_unique <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_unique, x, as.logical(revx))
}

#' @describeIn merge_rev returns union of two sorted sets.
#' Default \code{method='unique'} returns a unique sorted set, see \code{\link{union}};
#' \code{method='exact'} returns a sorted set with the maximum number of ties in either input set;
#' \code{method='all'} returns a sorted set with the sum of ties in both input sets.
#' @export
merge_union <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique","exact","all")){
  method <- match.arg(method)
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_union, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns sorted set x minus sorted set y
#' Default \code{method='unique'} returns a unique sorted set, see \code{\link{setdiff}};
#' \code{ethod='exact'} returns a sorted set with sum(x ties) minus sum(y ties);
#' @export
merge_setdiff <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique","exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_setdiff, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns those elements that are in sorted set \code{y} \code{\link{xor}} in sorted set \code{y}
#' Default \code{method='unique'} returns the sorted unique set complement, see \code{\link{symdiff}};
#' \code{method='exact'} returns a sorted set set complement with abs(sum(x ties) minus sum(y ties));
#' @export
merge_symdiff <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique","exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_symdiff, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns the intersection of two sorted sets x and y
#' Default \code{method='unique'} returns the sorted unique intersect, see \code{\link{intersect}};
#' \code{method='exact'} returns the intersect with the minium number of ties in either set;
#' @export
merge_intersect <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique","exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_intersect, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns \code{TRUE} for equal sorted sets and \code{FALSE} otherwise
#' Default \code{method='unique'} compares the sets after removing ties, see \code{\link{setequal}};
#' \code{method='exact'} compares the sets without removing ties;
#' @export
merge_setequal <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique","exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_setequal, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns logical existence of range rx in sorted set y, see \code{\link{merge_in}}
#' @export
merge_rangein <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_rangein, rx, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev returns logical in-existence of range rx in sorted set y, see \code{\link{merge_notin}}
#' @export
merge_rangenotin <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_rangenotin, rx, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev returns the intersection of range rx and sorted set y, see \code{\link{merge_intersect}}
#' @export
merge_rangesect <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_rangesect, rx, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev returns range rx minus sorted set y, see \code{\link{merge_setdiff}}
#' @export
merge_rangediff <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_rangediff, rx, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev quickly returns the first element of a sorted set x (or \code{NA} if x is empty), hence \code{x[1]} or \code{merge_rev(x)[1]}
#' @export
merge_first <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_first, x, as.logical(revx))
}
#' @describeIn merge_rev quickly returns the last element of a sorted set x, (or \code{NA} if x is empty), hence \code{x[n]} or \code{merge_rev(x)[n]}
#' @export
merge_last <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_last, x, as.logical(revx))
}

#' @describeIn merge_rev quickly returns the first common element of a range rx and a sorted set y, (or \code{NA} if the intersection is empty), hence \code{merge_first(merge_rangesect(rx,y))}
#' @export
merge_firstin <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_firstin, rx, y, as.logical(revx), as.logical(revy))
}
#' @describeIn merge_rev quickly returns the last common element of a range rx and a sorted set y, (or \code{NA} if the intersection is empty), hence \code{merge_last(merge_rangesect(rx,y))}
#' @export
merge_lastin <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_lastin, rx, y, as.logical(revx), as.logical(revy))
}
#' @describeIn merge_rev quickly returns the first element of a range rx which is not in a sorted set y (or \code{NA} if all rx are in y), hence \code{merge_first(merge_rangediff(rx,y))}
#' @export
merge_firstnotin <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_firstnotin, rx, y, as.logical(revx), as.logical(revy))
}
#' @describeIn merge_rev quickly returns the last element of a range rx which is not in a sorted set y (or \code{NA} if all rx are in y), hence \code{merge_last(merge_rangediff(rx,y))}
#' @export
merge_lastnotin <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_lastnotin, rx, y, as.logical(revx), as.logical(revy))
}



#' Symmetric set complement
#'
#' @param x a vector
#' @param y a vector
#' @return \code{union(setdiff(x,y), setdiff(y,x))}
#' @seealso \code{\link{merge_symdiff}} and \code{\link{xor}}
#' @note that \code{symdiff(x,y)} is not \code{\link{identical}}
#' as \code{symdiff(y,x)} without applying \code{\link{sort}} to the result
#' @examples
#' symdiff(c(1L,2L,2L), c(2L,3L))
#' symdiff(c(2L,3L), c(1L,2L,2L))
#' @export
symdiff <- function(x,y){
  union(setdiff(x,y), setdiff(y,x))
}
