# Fast methods for sorted integers
# (c) 2016-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk


# Attention:
#   must not use as.integer() on x and y here, otherwise we cannot pass bitwhich objects
#   (as.integer) would turn positions into 0/1

#' Fast functions for sorted sets of integer
#'
#' The `merge_` functions allow unary and binary operations on (ascending) sorted vectors
#'    of [integer()].
#' `merge_rev(x)` will do in one scan what costs two scans in [`-rev(x)`][rev], see also
#'   [reverse_vector()].
#' Many of these `merge_` can optionally scan their input in reverse order (and switch the
#'   sign), which again saves extra scans for calling `merge_rev(x)` first.
#'
#' @details These are low-level functions and hence do not check whether the set is
#'   actually sorted.
#' Note that the `merge_*` and `merge_range*` functions have no special treatment for
#'   `NA`.
#' If vectors with `NA` are sorted ith `NA` in the first positions (`na.last=FALSE`) and
#'   arguments `revx=` or `revy=` have not been used, then `NAs` are treated like ordinary
#'   integers. `NA` sorted elsewhere or using `revx=` or `revy=` can cause unexpected
#'   results (note for example that `revx=` switches the sign on all integers but `NAs`).
#'
#' The *binary* `merge_*` functions have a `method="exact"`
#' which in both sets treats consecutive occurrences of the same value as if they were
#'   different values, more precisely they are handled as if the identity of ties were
#'   tuples of `ties, rank(ties)`. `method="exact"` delivers unique output if the input is
#'   unique, and in this case works faster than `method="unique"`.
#'
#' @note xx OPTIMIZATION OPPORTUNITY These are low-level functions could be optimized with
#'   initial binary search (not findInterval, which coerces to double).
#'
#' @param x a sorted set
#' @param rx range of integers given as [ri()] or as a two-element [integer()]
#' @param y a sorted set
#' @param revx default `FALSE`, set to `TRUE` to reverse scan parameter 'x'
#' @param revy default `FALSE`, set to `TRUE` to reverse scan parameter 'y'
#' @param nomatch integer value returned for non-matched elements, see [match()]
#' @param method one of "unique", "exact" (or "all") which governs how to treat ties, see
#'   the function descriptions
#'
#' @return `merge_rev(x)` returns [`-rev(x)`][rev] for [integer()] and [double()] and
#'   [`!rev(x)`][rev] for [logical()]
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
#' merge_anyDuplicated(c(1L, 1L, 2L, 3L))
#' merge_duplicated(c(1L, 1L, 2L, 3L))
#' merge_unique(c(1L, 1L, 2L, 3L))
#'
#' merge_union(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L))
#' merge_union(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L), method="exact")
#' merge_union(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L), method="all")
#'
#' merge_setdiff(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L))
#' merge_setdiff(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L), method="exact")
#' merge_setdiff(c(1L, 2L, 2L), c(2L, 2L, 2L, 3L), method="exact")
#'
#' merge_symdiff(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L))
#' merge_symdiff(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L), method="exact")
#' merge_symdiff(c(1L, 2L, 2L), c(2L, 2L, 2L, 3L), method="exact")
#'
#' merge_intersect(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L))
#' merge_intersect(c(1L, 2L, 2L, 2L), c(2L, 2L, 3L), method="exact")
#'
#' merge_setequal(c(1L, 2L, 2L), c(1L, 2L))
#' merge_setequal(c(1L, 2L, 2L), c(1L, 2L, 2L))
#' merge_setequal(c(1L, 2L, 2L), c(1L, 2L), method="exact")
#' merge_setequal(c(1L, 2L, 2L), c(1L, 2L, 2L), method="exact")
#'
#' @export

merge_rev <- function(x){
  .Call(C_R_merge_rev, x)
}

#' @describeIn merge_rev returns integer positions of sorted set x in sorted set y, see
#'   [`match(x, y, ...)`][match]
#' @export
merge_match <- function(x, y, revx=FALSE, revy=FALSE, nomatch = NA_integer_){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_match, x, y, as.logical(revx), as.logical(revy), nomatch=as.integer(nomatch))
}

# xx OPTIMIZATION OPPORTUNITY: this could be optimized with proper binary search
#   (not findInterval, which coerces to double)
#' @describeIn merge_rev returns logical existence of sorted set x in sorted set y, see
#'   [`x %in% y`][match]
#' @export
merge_in <- function(x, y, revx=FALSE, revy=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_in, x, y, as.logical(revx), as.logical(revy))
}

# xx OPTIMIZATION OPPORTUNITY this could be optimized with proper binary search
#   (not findInterval, which coerces to double)
#' @describeIn merge_rev returns logical in-existence of sorted set x in sorted set y, see
#'   [`!(x %in% y)`][match]
#' @export
merge_notin <- function(x, y, revx=FALSE, revy=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_notin, x, y, as.logical(revx), as.logical(revy))
}

#' @describeIn merge_rev returns the duplicated status of a sorted set x, see
#'   [duplicated()]
#' @export
merge_duplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_duplicated, x, as.logical(revx))
}

#' @describeIn merge_rev returns the anyDuplicated status of a sorted set x, see
#'   [anyDuplicated()]
#' @export
merge_anyDuplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_anyDuplicated, x, as.logical(revx))
}

#' @describeIn merge_rev returns the sumDuplicated status of a sorted set x, see
#'   [bit_sumDuplicated()]
#' @export
merge_sumDuplicated <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_sumDuplicated, x, as.logical(revx))
}
#' @describeIn merge_rev returns unique elements of sorted set x, see [unique()]
#' @export
merge_unique <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_unique, x, as.logical(revx))
}

#' @describeIn merge_rev returns union of two sorted sets.
#' Default `method='unique'` returns a unique sorted set, see [union()];
#' `method='exact'` returns a sorted set with the maximum number of ties in either
#'   input set; `method='all'` returns a sorted set with the sum of ties in both input
#'   sets.
#' @export
merge_union <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique", "exact", "all")){
  method <- match.arg(method)
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_union, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns sorted set x minus sorted set y
#' Default `method='unique'` returns a unique sorted set, see [setdiff()];
#' `ethod='exact'` returns a sorted set with sum(x ties) minus sum(y ties);
#' @export
merge_setdiff <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique", "exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_setdiff, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns those elements that are in sorted set `y` [xor()] in
#'   sorted set `y`
#' Default `method='unique'` returns the sorted unique set complement, see [symdiff()];
#'   `method='exact'` returns a sorted set set complement with
#'   `abs(sum(x ties) - sum(y ties))`.
#' @export
merge_symdiff <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique", "exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_symdiff, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns the intersection of two sorted sets x and y
#' Default `method='unique'` returns the sorted unique intersect, see [intersect()];
#' `method='exact'` returns the intersect with the minium number of ties in either set;
#' @export
merge_intersect <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique", "exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_intersect, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns `TRUE` for equal sorted sets and `FALSE` otherwise
#' Default `method='unique'` compares the sets after removing ties, see [setequal()];
#' `method='exact'` compares the sets without removing ties;
#' @export
merge_setequal <- function(x, y, revx=FALSE, revy=FALSE, method=c("unique", "exact")){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  if (!(is.integer(y) || is.ordered(y)))
    stop("y must be integer (or ordered factor)")
  .Call(C_R_merge_setequal, x, y, as.logical(revx), as.logical(revy), method)
}

#' @describeIn merge_rev returns logical existence of range rx in sorted set y, see
#'   [merge_in()]
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

#' @describeIn merge_rev returns logical in-existence of range rx in sorted set y, see
#'   [merge_notin()]
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

#' @describeIn merge_rev returns the intersection of range rx and sorted set y, see
#'   [merge_intersect()]
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

#' @describeIn merge_rev returns range rx minus sorted set y, see [merge_setdiff()]
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

#' @describeIn merge_rev quickly returns the first element of a sorted set x (or `NA` if
#'   x is empty), hence `x[1]` or `merge_rev(x)[1]`
#' @export
merge_first <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_first, x, as.logical(revx))
}
#' @describeIn merge_rev quickly returns the last element of a sorted set x, (or `NA` if
#'   x is empty), hence `x[n]` or `merge_rev(x)[n]`
#' @export
merge_last <- function(x, revx=FALSE){
  if (!(is.integer(x) || is.ordered(x)))
    stop("x must be integer (or ordered factor)")
  .Call(C_R_merge_last, x, as.logical(revx))
}

#' @describeIn merge_rev quickly returns the first common element of a range rx and a
#'   sorted set y, (or `NA` if the intersection is empty), hence
#'   `merge_first(merge_rangesect(rx, y))`
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
#' @describeIn merge_rev quickly returns the last common element of a range rx and a
#'   sorted set y, (or `NA` if the intersection is empty), hence
#'   `merge_last(merge_rangesect(rx, y))`
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
#' @describeIn merge_rev quickly returns the first element of a range rx which is not in a
#'   sorted set y (or `NA` if all rx are in y), hence `merge_first(merge_rangediff(rx, y))`
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
#' @describeIn merge_rev quickly returns the last element of a range rx which is not in a
#'   sorted set y (or `NA` if all rx are in y), hence `merge_last(merge_rangediff(rx, y))`
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
#' @return `union(setdiff(x, y), setdiff(y, x))`
#' @seealso [merge_symdiff()] and [xor()]
#' @note that `symdiff(x, y)` is not [identical()]
#' as `symdiff(y, x)` without applying [sort()] to the result
#' @examples
#' symdiff(c(1L, 2L, 2L), c(2L, 3L))
#' symdiff(c(2L, 3L), c(1L, 2L, 2L))
#' @export
symdiff <- function(x, y){
  union(setdiff(x, y), setdiff(y, x))
}
