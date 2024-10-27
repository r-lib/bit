# Fast methods for integers leveraging bit vectors
# (c) 2016-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

#' Get range and number of NAs
#'
#' @param x an integer vector
#'
#' @return an integer vector with three elements:
#'   1. min integer
#'   2. max integer
#'   3. number of NAs
#'
#' @seealso [range_nanozero()] and [range_sortna()]
#'
#' @examples
#' range_na(c(0L, 1L, 2L, NA))
#' @export
range_na <- function(x){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  .Call(C_R_range_na, x)
}

#' Remove zeros and get range and number of NAs
#'
#' @param x an integer vector
#'
#' @return an integer vector without zeros and with an attribute [range_na()]  with three
#'   elements:
#'   1. min integer
#'   2. max integer
#'   3. number of NAs
#' @seealso [range_na()] and [range_sortna()]
#'
#' @examples
#' range_nanozero(c(0L, 1L, 2L, NA))
#' @export
range_nanozero <- function(x){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  .Call(C_R_range_nanozero, x)
}

#' Prepare for sorting and get range, number of NAs and unsortedness
#'
#' In one pass over the vector `NA`s are treated according to parameter
#' `na.last` exactly like [sort()] does, the [range()],
#' number of `NA`s and unsortedness is determined.
#'
#' @param x an integer vector
#' @inheritParams bit_sort
#'
#' @return an integer vector with `NA`s are treated and an `attribute` [range_na()]  with
#'   four elements:
#'
#'   1. min integer
#'   2. max integer
#'   3. number of NAs
#'   3. 0 for sorted vector and 1 for [is.unsorted()]
#'
#' @seealso [range_na()] and [range_nanozero()]
#'
#' @examples
#' range_sortna(c(0L, 1L, NA, 2L))
#' range_sortna(c(2L, NA, 1L, 0L))
#' range_sortna(c(0L, 1L, NA, 2L), na.last=TRUE)
#' range_sortna(c(2L, NA, 1L, 0L), na.last=TRUE)
#' range_sortna(c(0L, 1L, NA, 2L), na.last=FALSE)
#' range_sortna(c(2L, NA, 1L, 0L), na.last=FALSE)
#' @export
range_sortna <- function(x, decreasing = FALSE, na.last=NA){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  .Call(C_R_range_sortna, x, as.logical(decreasing), as.logical(na.last))
}


#' Low-level sorting: binary quicksort
#'
#' In one pass over the vector `NA`s are handled according to parameter
#' `na.last` by [range_sortna()], then, if the vector is unsorted,
#' binary quicksort is invoked.
#'
#' @param x an integer vector
#' @inheritParams bit_sort
#'
#' @return a sorted vector
#'
#' @examples
#' quicksort2(c(2L, 0L, 1L, NA, 2L))
#' quicksort2(c(2L, 0L, 1L, NA, 2L), na.last=TRUE)
#' quicksort2(c(2L, 0L, 1L, NA, 2L), na.last=FALSE)
#' @export
quicksort2 <- function(x, na.last=NA){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  x <- range_sortna(x, na.last=na.last)
  range_sortna <- getsetattr(x, "range_sortna", NULL)
  if (is.na(range_sortna[1]))  # no numbers to sort
    return(rep(NA_integer_, range_sortna[3]))
  if (range_sortna[4])  # is.unsorted
    .Call(C_R_int_quicksort2, x, range_sortna, as.logical(na.last))
  else
    x
}

#' Low-level sorting: threeway quicksort
#'
#' In one pass over the vector `NA`s are handled according to parameter
#' `na.last` by [range_sortna()], then, if the vector is unsorted,
#' threeway quicksort is invoked.
#'
#' @param x an integer vector
#' @inheritParams bit_sort
#'
#' @return a sorted vector
#'
#' @examples
#' countsort(c(2L, 0L, 1L, NA, 2L))
#' countsort(c(2L, 0L, 1L, NA, 2L), na.last=TRUE)
#' countsort(c(2L, 0L, 1L, NA, 2L), na.last=FALSE)
#' @export
quicksort3 <- function(x, na.last=NA){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  x <- range_sortna(x, na.last=na.last)
  range_sortna <- getsetattr(x, "range_sortna", NULL)
  if (is.na(range_sortna[1]))  # no numbers to sort
    return(rep(NA_integer_, range_sortna[3]))
  if (range_sortna[4])  # is.unsorted
    .Call(C_R_int_quicksort3, x, range_sortna, as.logical(na.last))
  else
    x
}


#' Low-level sorting: counting sort
#'
#' In one pass over the vector `NA`s are handled according to parameter
#' `na.last` by [range_sortna()], then, if the vector is unsorted,
#' counting sort is invoked.
#'
#' @param x an integer vector
#' @inheritParams bit_sort
#'
#' @return a sorted vector
#'
#' @examples
#' countsort(c(2L, 0L, 1L, NA, 2L))
#' countsort(c(2L, 0L, 1L, NA, 2L), na.last=TRUE)
#' countsort(c(2L, 0L, 1L, NA, 2L), na.last=FALSE)
#' @export
countsort <- function(x, na.last=NA){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  x <- range_sortna(x, na.last=na.last)
  range_sortna <- getsetattr(x, "range_sortna", NULL)
  if (is.na(range_sortna[1]))  # no numbers to sort
    return(rep(NA_integer_, range_sortna[3]))
  if (range_sortna[4])  # is.unsorted
    .Call(C_R_int_countsort, x, range_sortna, as.logical(na.last))
  else
    x
}

#' Low-level sorting: bit sort
#'
#' In one pass over the vector `NA`s are handled according to parameter
#' `na.last` by [range_sortna()], then, if the vector is unsorted,
#' bit sort is invoked.
#'
#' @param x an integer vector
#' @param depth an integer scalar giving the number of bit-passed before switching to
#'   quicksort
#' @inheritParams bit_sort
#'
#' @return a sorted vector
#'
#' @examples
#' bitsort(c(2L, 0L, 1L, NA, 2L))
#' bitsort(c(2L, 0L, 1L, NA, 2L), na.last=TRUE)
#' bitsort(c(2L, 0L, 1L, NA, 2L), na.last=FALSE)
#' @export
bitsort <- function(x, na.last=NA, depth=1){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  x <- range_sortna(x, na.last=na.last)
  range_sortna <- getsetattr(x, "range_sortna", NULL)
  if (is.na(range_sortna[1]))  # no numbers to sort
    return(rep(NA_integer_, range_sortna[3]))
  if (!range_sortna[4])  # already sorted
    return(x)
  n <- as.double(range_sortna[2])-as.double(range_sortna[1])+1
  .Call(C_R_bit_sort, tmp=bit(n), x, range_sortna, as.logical(na.last), depth=as.integer(depth))
}



# if (FALSE){
#   # time measure relative speeds (radixsort is not yet integrated)
#   require(microbenchmark)
#   require(bit)
#   require(ff)
#   x <- sample(2, 1e8, T)
#   system.time(bitsort(x))
#   x <- sample(1e8, 1e8, T)
#   system.time(bitsort(x))
#
#   D <- as.integer(2^(1:27))
#   N <- as.integer(2^(1:27))
#   M <- c("b", "c", "q") #, "r")
#   tim <- array(NA, dim=c(D=length(D), N=length(N), M=length(M)), dimnames=list(D=D, N=N, M=M))
#   for (ni in seq_along(N)){
#     n <- N[ni]
#     for (di in seq_along(D)){
#       d <- D[di]
#       x <- sample(d, n, T)
#       x[1] <- d
#       if (di > ni-5 && di < ni+10){
#         tim[di, ni, "b"] <- microbenchmark(bitsort(x), times = 1)$time
#         tim[di, ni, "c"] <- microbenchmark(countsort(x), times = 1)$time
#         tim[di, ni, "q"] <- microbenchmark(quicksort3(x), times = 1)$time
#         #tim[di, ni, "r"] <- microbenchmark(radixsort(copy_vector(x)), times = 1)$time
#       }
#     }
#     apply(tim[, 1:ni, , drop=FALSE], 1:2, \(x){i <- which.min(x); if (length(i))M[i] else "."}) |>
#       print(quote=FALSE)
#   }
#
#   round(tim[, , "b"] / tim[, , "c"], 1)
#   round(tim[, , "b"] / tim[, , "q"], 1)
#   round(tim[, , "c"] / tim[, , "q"], 1)
#   round(tim[, , "c"] / tim[, , "r"], 1)
#   round(tim[, , "q"] / tim[, , "r"], 1)
#
#   M <- dimnames(tim)[[3]]
#   r <- as.integer(rownames(tim[, , 1]))[row(tim[, , 1])]
#   n <- as.integer(colnames(tim[, , 1]))[col(tim[, , 1])]
#   d <- n/r
#   #x <- ifelse(d>=1, "c", "q")
#   x <- ifelse(d>=0.5, "c", ifelse(n<=1024, "q", ifelse(d<0.0625, "q", "b")))
#   x <- ifelse(n<=1024, ifelse(d<0.5, "q", "c"), ifelse(d<1, ifelse(d<0.0625, "q", "b"), "c"))
#   dim(x) <- dim(tim[, , 1])
#   dimnames(x) <- dimnames(tim[, , 1])
#   y <- tim[cbind(as.vector(row(x)), as.vector(col(x)), match(as.vector(x), M))]
#   attributes(y) <- attributes(x)
#   print(apply(tim, 1:2, function(x){i <- which.min(x); if (length(i))M[i] else "."}), quote=FALSE)
#   print(x, quote=F)
#   round(y / apply(tim[, , -4], 1:2, min, na.rm=T), 1)
#   round(y / tim[, , 1], 1)
#
#   f <- function(n, d=1, k=1){
#     x <- dbinom(1:log2(n), n, 1/(d*n))*n
#     r <- rev(cumsum(rev(x)))
#     rr <- c(r[-1], 0)
#     c <- r*log2(r)
#     b <- n*k+r+rr*log2(rr)
#     w <- as.integer(!is.na(b) & b<c)
#     #print(round(data.frame(x=x, r=r, rr=rr, c=c, b=b, w=w)))
#     sum(w)
#   }
#   sapply(2^(1:30), f, d=8, k=1)
#   sapply(2^(1:30), f, k=2)
#   sapply(2^(1:30), f, k=4)
#   sapply(2^(1:30), f, k=8)
#   sapply(2^(1:30), f, k=16)
#   sapply(2^(1:30), f, k=32)
#
#   require(microbenchmark)
#   require(bit)
#   n <- as.integer(2^27)
#   x <- sample(n*4, n, T)
#   times <- 1
#   microbenchmark(countsort(x), times=times)
#   microbenchmark(quicksort3(x), times=times)
#   microbenchmark(bitsort(x, depth=1), times=times)
#   microbenchmark(bitsort(x, depth=2), times=times)
#   microbenchmark(bitsort(x, depth=3), times=times)
#   microbenchmark(bitsort(x, depth=4), times=times)
#   microbenchmark(bitsort(x, depth=5), times=times)
#
#   microbenchmark(quicksort2(x), times=times)
#   microbenchmark(bit_sort_unique(x), times=times)
#
#   # ergo: ab 2^16 zwischen 1/4 und 1/32
#
# }


#' bit sort
#'
#' fast sorting of integers
#'
#' determines the range of the integers and checks if the density justifies use
#' of a bit vector; if yes, sorts the first occurences of each integer in the
#' range using a bit vector, sorts the rest and merges; if no, falls back to quicksort.
#'
#' @param x an integer vector
#' @param decreasing  (currently only `FALSE` is supported)
#' @param na.last `NA` removes NAs, `FALSE` puts NAs at the beginning,
#'   `TRUE` puts NAs at the end
#' @param has.dup TRUE (the default) assumes that `x` might have
#'   duplicates, set to `FALSE` if duplicates are impossible
#'
#' @return a sorted vector
#' @seealso [sort()], [ramsort()],
#'   [bit_sort_unique()]
#'
#' @examples
#' bit_sort(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_sort(c(2L, 1L, NA, NA, 1L, 2L), na.last=FALSE)
#' bit_sort(c(2L, 1L, NA, NA, 1L, 2L), na.last=TRUE)
#'
#' \dontrun{
#' x <- sample(1e7, replace=TRUE)
#' system.time(bit_sort(x))
#' system.time(sort(x))
#' }
#' @export
bit_sort <- function(x, decreasing = FALSE, na.last=NA, has.dup = TRUE){
  if (decreasing)
    stop("decreasing=TRUE not implemented")
  if (!has.dup)
    return(bit_sort_unique(x, decreasing = decreasing, na.last=na.last, has.dup=FALSE))
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  x <- range_sortna(x, decreasing = decreasing, na.last=na.last)
  range_sortna <- getsetattr(x, "range_sortna", NULL)
  if (is.na(range_sortna[1]))  # no numbers to sort
    return(rep(NA_integer_, range_sortna[3]))
  if (!range_sortna[4])  # already sorted
    return(x)
  nr <- as.double(range_sortna[2])-as.double(range_sortna[1])+1
  d <- length(x) / nr
  if (nr<=65536) {
    if (d<0.5)
      ret <- .Call(C_R_int_quicksort3, x, range_sortna, as.logical(na.last))
    else
      ret <- .Call(C_R_int_countsort, x, range_sortna, as.logical(na.last))
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism.
    if (d < 0.03125 || nr > .Machine$integer.max) {
      ret <- .Call(C_R_int_quicksort3, x, range_sortna, as.logical(na.last))
    } else if (d <= 0.25) {
      ret <- .Call(C_R_bit_sort, tmp=bit(nr), x, range_sortna, as.logical(na.last), depth=1L)
    } else {
      ret <- .Call(C_R_int_countsort, x, range_sortna, as.logical(na.last))
    }
  }
  ret
}


#' bit sort unique
#'
#' fast combination of [sort()] and [unique()] for integers
#'
#' determines the range of the integers and checks if the density justifies use
#' of a bit vector; if yes, creates the result using a bit vector; if no, falls back to
#'   `sort(unique())`
#'
#' @param x an integer vector
#' @param decreasing `FALSE` (ascending) or `TRUE` (descending)
#' @param na.last `NA` removes NAs, `FALSE` puts NAs at the beginning, `TRUE` puts NAs at
#'   the end
#' @param has.dup TRUE (the default) assumes that `x` might have duplicates, set to
#'   `FALSE` if duplicates are impossible
#' @param range_na `NULL` calls [range_na()], optionally the result of [range_na()] can be
#'   given here to avoid calling it again
#'
#' @return a sorted unique integer vector
#' @seealso [sort()], [unique()],
#'   [bit_sort()], [bit_unique()]
#'
#' @examples
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), na.last=FALSE)
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), na.last=TRUE)
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE)
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE, na.last=FALSE)
#' bit_sort_unique(c(2L, 1L, NA, NA, 1L, 2L), decreasing = TRUE, na.last=TRUE)
#'
#' \dontrun{
#' x <- sample(1e7, replace=TRUE)
#' system.time(bit_sort_unique(x))
#' system.time(sort(unique(x)))
#' x <- sample(1e7)
#' system.time(bit_sort_unique(x))
#' system.time(sort(x))
#' }
#' @export
bit_sort_unique <- function(x, decreasing = FALSE, na.last=NA, has.dup=TRUE, range_na=NULL){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (is.null(range_na))
    range_na <- range_na(x)
  else
    range_na <- as.integer(range_na)
  if (is.na(range_na[1])) {
    if (is.na(na.last) || length(range_na)<3L || range_na[3]==0)
      return(integer())
    else
      return(NA_integer_)
  }
  nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- length(x) / nr
  if (nr > .Machine$integer.max || d < 0) {
    if (has.dup) {
      ret <- quicksort3(x, na.last = xor(na.last, decreasing))
      ret <- merge_unique(ret)
    } else {
      ret <- quicksort2(x, na.last = xor(na.last, decreasing))
    }
    if (decreasing)
      reverse_vector(ret)
    else
      ret
  } else
    .Call(C_R_bit_sort_unique,
      tmp=bit(nr), x, range_na,
      as.logical(na.last),
      as.logical(decreasing)
    )
}


#' bit %in%
#'
#' fast [`%in%`][match] for integers
#'
#' determines the range of the integers and checks if the density justifies use
#' of a bit vector; if yes, maps `x` or `table` -- whatever is smaller
#' -- into a bit vector and searches the other of `table` or `x` in
#' the it vector; if no, falls back to [`%in%`][match]
#'
#' @param x an integer vector of values to be looked-up
#' @param table an integer vector used as lookup-table
#' @param retFUN a function that coerces [bit()] and [logical()] vectors
#'
#' @return a boolean vector coerced to `retFUN`
#' @seealso [`%in%`][match]
#' @examples
#' bit_in(1:2, 2:3)
#' bit_in(1:2, 2:3, retFUN=as.logical)
#' @export
bit_in <- function(x, table, retFUN=as.bit){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(table) || is.factor(table)))
    stop("table must be integer (or factor)")
  nx <- length(x)
  nt <- length(table)
  reverse <- nx<nt
  if (reverse){
    range_na <- range_na(x)
  }else{
    range_na <- range_na(table)
  }
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+nt) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/64) {
    ret <- !is.na(match(x, table))
  } else if (reverse) {
    ret <- .Call(C_R_bit_table_in, tmp=bit(nr), x, table, range_na, ret=bit(length(x)))
  } else {
    ret <- .Call(C_R_bit_in_table, tmp=bit(nr), x, table, range_na, ret=bit(length(x)))
  }
  retFUN(ret)
}

#' bit unique and duplicated
#'
#' Fast versions of [unique()], [duplicated()] ,
#' [anyDuplicated()] and `sum(duplicated(x))` for integers.
#'
#' determines the range of the integers and checks if the density justifies use
#' of a bit vector; if yes, uses a bit vector for finding duplicates; if no,
#' falls back to [unique()], [duplicated()], [anyDuplicated()] and `sum(duplicated(x))`
#'
#' @name bit_unidup
#' @param x an integer vector
#' @param na.rm `NA` treats NAs like other integers, `TRUE` treats
#'   _all_ NAs as duplicates, `FALSE` treats _no_ NAs as
#'   duplicates
#' @inheritParams bit_in
#' @inheritParams bit_sort_unique
#'
#' @return
#'   - `bit_unique` returns a vector of unique integers,
#'   - `bit_duplicated` returns a boolean vector coerced to `retFUN`,
#'   - `bit_anyDuplicated` returns the position of the first duplicate (or zero if no
#'      duplicates)
#'   - `bit_sumDuplicated` returns the number of duplicated values (as.integer)
#' @seealso [bit_sort_unique()]
#'
#' @examples
#' bit_unique(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_unique(c(2L, 1L, NA, NA, 1L, 2L), na.rm=FALSE)
#' bit_unique(c(2L, 1L, NA, NA, 1L, 2L), na.rm=TRUE)
#'
#' bit_duplicated(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_duplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=FALSE)
#' bit_duplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=TRUE)
#'
#' bit_anyDuplicated(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_anyDuplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=FALSE)
#' bit_anyDuplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=TRUE)
#'
#' bit_sumDuplicated(c(2L, 1L, NA, NA, 1L, 2L))
#' bit_sumDuplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=FALSE)
#' bit_sumDuplicated(c(2L, 1L, NA, NA, 1L, 2L), na.rm=TRUE)
NULL

#' @describeIn bit_unidup extracts unique elements
#' @export
bit_unique <- function(x, na.rm = NA, range_na=NULL){
  na.rm <- as.logical(na.rm)
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(x)
  if (is.null(range_na))
    range_na <- range_na(x)
  else
    range_na <- as.integer(range_na)
  if (is.na(range_na[1])) {
    if (is.na(na.rm)) {
      return(NA_integer_)
    } else if (na.rm) {
      return(integer())
    } else {
      return(rep(NA_integer_, range_na[3]))
    }
  }
  nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- length(x) / nr
  if (nr > .Machine$integer.max || d < 1/64) {
    if (is.na(na.rm)) {
      unique(x, incomparables = FALSE)
    } else if (na.rm) {
      x <- unique(x, incomparables = FALSE)
      x <- x[!is.na(x)]
      x
    } else {
      unique(x, incomparables = NA)
    }
  } else {
    .Call(C_R_bit_unique, tmp=bit(nr), x, range_na, na.rm)
  }
}

#' @describeIn bit_unidup determines duplicate elements
#' @export
bit_duplicated <- function(x, na.rm = NA, range_na=NULL, retFUN=as.bit){
  na.rm <- as.logical(na.rm)
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(retFUN(NULL))
  if (is.null(range_na))
    range_na <- range_na(x)
  else
    range_na <- as.integer(range_na)
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- length(x) / nr
  if (nr > .Machine$integer.max || d < 1/64) {
    if (is.na(na.rm)) {
      ret <- duplicated(x, incomparables = FALSE)
    } else if (na.rm) {
      ret <- duplicated(x, incomparables = FALSE) | is.na(x)
    } else {
      ret <- duplicated(x, incomparables = NA)
    }
  } else {
    ret <-
      .Call(C_R_bit_duplicated, tmp=bit(nr), x, range_na, ret=bit(length(x)), as.logical(na.rm))
  }
  retFUN(ret)
}

#' @describeIn bit_unidup checks for existence of duplicate elements
#' @export
bit_anyDuplicated <- function(x, na.rm = NA, range_na=NULL){
  na.rm <- as.logical(na.rm)
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(0L)
  if (is.null(range_na))
    range_na <- range_na(x)
  else
    range_na <- as.integer(range_na)
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- length(x) / nr
  if (nr > .Machine$integer.max || d < 1/64) {
    if (is.na(na.rm)){
      anyDuplicated(x, incomparables = FALSE)
    }else if (na.rm){
      max(0L, min(anyDuplicated(x, incomparables = NA), firstNA(x)))
    }else{
      anyDuplicated(x, incomparables = NA)
    }
  }else{
    .Call(C_R_bit_anyDuplicated, bit(nr), x, range_na=range_na, as.logical(na.rm))
  }
}

#' @describeIn bit_unidup counts duplicate elements
#' @export
bit_sumDuplicated <- function(x, na.rm = NA, range_na=NULL){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!length(x))
    return(0L)
  if (is.null(range_na))
    range_na <- range_na(x)
  else
    range_na <- as.integer(range_na)
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- length(x) / nr
  if (nr > .Machine$integer.max || d < 1/64) {
    if (is.na(na.rm)){
      sum(duplicated(x, incomparables = FALSE))
    }else if (na.rm){
      sum(duplicated(x, incomparables = NA)) + sum(is.na(x))
    }else{
      sum(duplicated(x, incomparables = NA))
    }
  }else{
    .Call(C_R_bit_sumDuplicated, bit(nr), x, range_na=range_na, as.logical(na.rm))
  }
}




#' bit set operations
#'
#' Fast versions of [union()], [intersect()],
#' [setdiff()], symmetric difference and [setequal()]
#' for integers.
#'
#' determines the range of the integers and checks if the density justifies use
#' of a bit vector; if yes, uses a bit vector for finding duplicates; if no,
#' falls back to [union()], [intersect()],
#' [setdiff()], `union(setdiff(x, y), setdiff(y, x))` and [setequal()]
#'
#' @name bit_setops
#' @param x an integer vector
#' @param y an integer vector
#'
#' @return an integer vector
#' @seealso [bit_in()], [bit_rangediff()]
#'
#' @examples
#' bit_union(1:2, 2:3)
#' bit_intersect(1:2, 2:3)
#' bit_setdiff(1:2, 2:3)
#' bit_symdiff(1:2, 2:3)
#' bit_setequal(1:2, 2:3)
#' bit_setequal(1:2, 2:1)
NULL

#' @describeIn bit_setops union
#' @export
bit_union <- function(x, y){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(y) || is.factor(y)))
    stop("y must be integer (or factor)")
  nx <- length(x)
  ny <- length(y)
  rx <- range_na(x)
  ry <- range_na(y)
  range_na <- c(min(rx[1], ry[1]), max(rx[2], ry[2]), rx[3]+ry[3])
  if (max(rx[1], ry[1])>min(rx[2], ry[2])) {
    if (range_na[3]>0 && (rx[3]>0) == (rx[3]>0))
      return(bit_unique(c(x, y), range_na=range_na))
    else
      return(c(bit_unique(x, range_na=rx), bit_unique(y, range_na=ry)))
  }
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+ny) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/64) {
    ret <- union(x, y)
  } else {
    ret <- .Call(C_R_bit_union, tmp=bit(nr), x, y, range_na)
  }
  ret
}

#' @describeIn bit_setops intersection
#' @export
bit_intersect <- function(x, y){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(y) || is.factor(y)))
    stop("y must be integer (or factor)")
  nx <- length(x)
  ny <- length(y)
  rx <- range_na(x)
  ry <- range_na(y)
  range_na <- c(max(rx[1], ry[1]), min(rx[2], ry[2]), rx[3]+ry[3])
  if (range_na[1]>range_na[2])
    return(integer())
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+ny) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/64) {
    ret <- intersect(x, y)
  }else{
    ret <- .Call(C_R_bit_intersect, tmp=bit(nr), x, y, range_na)
  }
  ret
}

#' @describeIn bit_setops asymmetric difference
#' @export
bit_setdiff <- function(x, y){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(y) || is.factor(y)))
    stop("y must be integer (or factor)")
  nx <- length(x)
  ny <- length(y)
  range_na <- range_na(x)
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+ny) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/64) {
    ret <- setdiff(x, y)
  }else{
    ret <- .Call(C_R_bit_setdiff, tmp=bit(nr), x, y, range_na)
  }
  ret
}

#' @describeIn bit_setops symmetricx difference
#' @export
bit_symdiff <- function(x, y){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(y) || is.factor(y)))
    stop("y must be integer (or factor)")
  nx <- length(x)
  ny <- length(y)
  rx <- range_na(x)
  ry <- range_na(y)
  range_na <- c(min(rx[1], ry[1]), max(rx[2], ry[2]), rx[3]+ry[3])
  if (max(rx[1], ry[1])>min(rx[2], ry[2])) {
    if (range_na[3]>0 && (rx[3]>0) == (rx[3]>0))
      return(bit_unique(c(x, y), range_na=range_na))
    else
      return(c(bit_unique(x, range_na=rx), bit_unique(y, range_na=ry)))
  }
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+ny) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/64) {
    ret <- union(setdiff(x, y), setdiff(y, x))
  } else {
    ret <- .Call(C_R_bit_symdiff, bit(nr), bit(nr), x, y, range_na, rx[3]>0, ry[3]>0)
  }
  ret
}

#' @describeIn bit_setops equality
#' @export
bit_setequal <- function(x, y){
  if (!(is.integer(x) || is.factor(x)))
    stop("x must be integer (or factor)")
  if (!(is.integer(y) || is.factor(y)))
    stop("y must be integer (or factor)")
  nx <- length(x)
  ny <- length(y)
  rx <- range_na(x)
  ry <- range_na(y)
  rx[3] <- as.logical(rx[3])  # map NA count to 0/1
  ry[3] <- as.logical(ry[3])  # map NA count to 0/1
  if (!identical(rx, ry))
    return(FALSE)
  range_na <- rx
  if (is.na(range_na[1]))
    nr <- 0L  # no numbers, no temp bit vector
  else
    nr <- as.double(range_na[2])-as.double(range_na[1])+1
  d <- (nx+ny) / nr
  if (nr==0L || nr > .Machine$integer.max || d < 1/256) {
    ret <- setequal(x, y)
  } else {
    ret <- .Call(C_R_bit_setequal, bit(nr), bit(nr), x, y, range_na)
  }
  ret
}


#' bit range difference
#'
#' Fast version of `setdiff(rx[1]:rx[2], y)`.
#'
#' determines the range of the integers `y` and checks if the density justifies use
#' of a bit vector; if yes, uses a bit vector for the set operation; if no,
#' falls back to a quicksort and [merge_rangediff()]
#'
#' @param y an integer vector of elements to exclude
#' @param revx `FALSE` as is, `TRUE` to reverse the direction and sign of `rx`
#' @param revy `FALSE` as is, `TRUE` to reverse the direction and sign of `y`
#' @inheritParams merge_rev
#'
#' @return an integer vector
#' @seealso [bit_setdiff()], [merge_rangediff()]
#'
#' @examples
#' bit_rangediff(c(1L, 6L), c(3L, 4L))
#' bit_rangediff(c(6L, 1L), c(3L, 4L))
#' bit_rangediff(c(6L, 1L), c(3L, 4L), revx=TRUE)
#' bit_rangediff(c(6L, 1L), c(3L, 4L), revx=TRUE, revy=TRUE)
#' @export
bit_rangediff <- function(rx, y, revx=FALSE, revy=FALSE){
  if (!is.integer(y))
    stop("y must be integer")
  if (!is.ri(rx)){
    stopifnot(length(rx)==2)
    rx <- as.integer(rx)
  }
  if (rx[1]>rx[2])
    ox <- as.double(rev(rx))
  else
    ox <- as.double(rx)
  n <- ox[2]-ox[1]+1L
  d <- length(y) / n
  if (n > .Machine$integer.max || d < 1/64) {
    ox <- as.integer(ox)
    y <- .Call(C_R_int_quicksort2, copy_vector(y), c(ox, 0L), na.last=NA)
    ret <- merge_rangediff(ox, y, revx=revx, revy=revy)
    if (rx[1]>rx[2])
      ret <- rev(ret)
    ret
  }else{
    .Call(C_R_bit_rangediff, tmp=bit(n), as.integer(rx), y, as.logical(revx), as.logical(revy))
  }
}


# /* some experiments - just ignore
# bit_sample1 <- function (x, size, replace = FALSE, prob = NULL){
#   if (length(x)==1L){
#     x <- seq_len(x)
#   } else{
#     x <- as.integer(x)
#   }
#   size <- as.integer(size)
#   if (replace)
#     stop("replace not implemented")
#   else{
#     stopifnot(size <= length(x))
#     .Call(C_R_bit_sample, x, bit(length(x)), size)
#   }
# }
# bit_sample <- function (x, size=length(x), replace = FALSE, prob = NULL){
#   if (length(x)==1L){
#     x <- seq_len(x)
#   } else{
#     x <- as.integer(x)
#   }
#   size <- as.integer(size)
#   replace <- as.logical(replace)
#   if (!replace && size>length(x))
#     stop("size > length(x) without replace")
#   .Call(C_R_bit_sample, x, size, replace)
# }
# */
