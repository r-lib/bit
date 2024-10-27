# Chunking utilities for bit and ff
# (c) 2007-2009 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2007-09-03
# Last changed: 2007-10-25

# source("D:/mwp/eanalysis/bit/R/chunkutil.R")




# non-vectorized
#bbatch <-
#function(N,B){
#  N <- as.integer(N)
#  B <- as.integer(B)
#  RB <- N %% B
#  NB <- N %/% B
#  if (RB){
#    cc <- min((B - RB) %/% NB, (B - RB) %/% (NB + 1L))
#    if (cc){
#      rb <- RB + cc * NB
#      b <- B - cc
#      if (rb==b){
#        return(list(b=b, nb=NB+1L, rb=0L))
#      }else{
#        return(list(b=b, nb=NB, rb=rb))
#      }
#    }else{
#      return(list(b=B, nb=NB, rb=RB))
#    }
#  }else{
#    return(list(b=B, nb=NB, rb=RB))
#  }
#}

#' Balanced Batch sizes
#'
#' `bbatch` calculates batch sizes in 1..N so that they have rather balanced
#' sizes than very different sizes.
#'
#' Tries to have `rb==0` or `rb` as close to `b` as possible
#' while guaranteeing that `rb < b && (b - rb) <= min(nb, b)`
#'
#' @param N total size in 0..integer_max
#' @param B desired batch size in 1..integer_max
#' @return a list with components:
#'  - b: the batch size
#'  - nb: the number of batches
#'  - rb: the size of the rest
#'
#' @author Jens Oehlschlägel
#' @seealso [repfromto()], [ff::ffvecapply()]
#' @keywords IO data
#' @examples
#'
#'   bbatch(100, 24)
#'
#' @export
bbatch <- function(N,B){
  if (any(B<1))
    stop("B too small")
  if (any(N<0))
    stop("N too small")
  N <- as.integer(N)
  B <- pmin(pmax(1L,N), as.integer(B))
  RB <- N %% B
  NB <- N %/% B
  cc <- pmin((B - RB) %/% NB, (B - RB) %/% (NB + 1L))
  cc[RB==0 | NB == 0] <- 0L
  i <- cc > 0
  RB[i] <- RB[i] + cc[i] * NB[i]
  B[i] <- B[i] - cc[i]
  j <- i & (RB == B)
  NB[j] <- NB[j] + 1L
  RB[j] <- 0L
  i <- (RB>0) & (NB == 0)
  B[i] <- RB[i]
  NB[i] <- 1L
  RB[i] <- 0L
  list(b=B, nb=NB, rb=RB)
}



#' Virtual recycling
#'
#' `repfromto` virtually recylcles object `x` and cuts out
#' positions `from .. to`
#'
#' `repfromto` is a generalization of [rep()], where
#' `rep(x, n) == repfromto(x, 1, n)`.  You can see this as an R-side
#' (vector) solution of the `mod_iterate` macro in arithmetic.c
#'
#' @param x an object from which to recycle
#' @param from first position to return
#' @param to last position to return
#' @param value value to assign
#' @return a vector of length `from - to + 1`
#' @author Jens Oehlschlägel
#' @seealso [rep()], [ff::ffvecapply()]
#' @keywords IO data
#' @examples
#'
#'   message("a simple example")
#'   repfromto(0:9, 11, 20)
#'
#' @export
repfromto <- function(x, from, to){
  nx <- length(x)
  if (!nx) {
    return(NA[from:to])
  }
  from <- as.integer(from)
  to <- as.integer(to)
  if (to>nx) {
    N <- to - from + 1L
    from <- (from-1L)%%nx + 1L
    to <- to%%nx
    # NOTE: fetch in sequence pre-main-post in case is.ff(x)
    if (from<=to && N<nx) {
      ret <- x[from:to]
    } else {
      pre <- x[from:nx]
      nrep <- (N - length(pre) - to) %/%nx
      main <- if (nrep) rep(x[1:nx], nrep)
      post <- if (to) x[1:to]
      ret <- c(pre, main, post)
    }
  } else {
    ret <- x[from:to]
  }
  a <- attributes(x[1])
  a$names <- NULL
  attributes(ret) <- a
  ret
}

#' @rdname repfromto
#' @export
`repfromto<-` <- function(x, from, to, value){
  x[from:to] <- value
  x
}


if (FALSE){
  x <- 1:10
  for (n in 1:20)
  for (i1 in 1:30){
    i2 <- i1+n-1
    cat(i1,i2,"|",repfromto(x,i1,i2), "\n")
  }
}

if (FALSE){
  intseq <- function(from=NULL, to=NULL, by=NULL, length.out=NULL, along.with=NULL){
    if (is.null(from)){
      if (is.null(to))
        stop("need 'from' or 'to'")
      else
        to <- as.integer(to)
      if (is.null(by))
        by <- 1L
      else
        by <- as.integer(by)
    }else{
      from <- as.integer(from)
      if (is.null(to)){
        if (is.null(by))
          by <- 1L
        else
          by <- as.integer(by)
      }else{
        to <- as.integer(to)
        n <- to - from
        if (is.null(by)){
          if (to<from)
            by = -1L
          else
            by = 1L
        }else{
          by <- as.integer(by)
          if (n){
            if (sign(n) != sign(by))
              stop("wrong sign of by")
          }else
            return(from)  # to == from
        }
      }
    }

    if (is.null(length.out)){
      if (is.null(along.with)){
        if (is.null(to) || is.null(from))
          stop("not enough info to guess the length.out")
        else{
          length.out <- n %/% by + 1L
        }
      }else{
        length.out <- length(along.with)
      }
    }else{
      length.out <- as.integer(length.out)
    }
    if (length.out){
      if (length.out==1L)
        from
      else
        cumsum(c(from, rep(by, length.out-1L)))
    }else
      integer()
  }
}



#' Function for chunked range index
#'
#' creates a sequence of range indexes using a syntax not completely unlike
#' 'seq'
#'
#' @param from the starting value of the sequence.
#' @param to the (maximal) end value of the sequence.
#' @param by increment of the sequence
#' @param length.out desired length of the sequence.
#' @param along.with take the length from the length of this argument.
#' @param overlap number of values to overlap (will lower the starting value of
#' the sequence, the first range becomes smaller
#' @param method default 'bbatch' will try to balance the chunk size, see
#' [bbatch()], 'seq' will create chunks like [`seq()`][base::seq]
#' @param maxindex passed to [ri()]
#' @return returns a named list of [ri()] objects
#' representing chunks of subscripts
#' @author Jens Oehlschlägel
#' @seealso generic [chunk()], [ri()], [`seq()`][base::seq], [bbatch()]
#' @keywords data
#' @examples
#'
#'   chunks(1, 100, by=30)
#'   chunks(1, 100, by=30, method="seq")
#'    \dontrun{
#' require(foreach)
#' m <- 10000
#' k <- 1000
#' n <- m*k
#' message("Four ways to loop from 1 to n. Slowest foreach to fastest chunk is 1700:1
#' on a dual core notebook with 3GB RAM\n")
#' z <- 0L;
#' print(k*system.time({it <- icount(m); foreach (i = it) %do% { z <- i; NULL }}))
#' z
#'
#' z <- 0L
#' print(system.time({i <- 0L; while (i<n) {i <- i + 1L; z <- i}}))
#' z
#'
#' z <- 0L
#' print(system.time(for (i in 1:n) z <- i))
#' z
#'
#' z <- 0L; n <- m*k;
#' print(system.time(for (ch in chunks(1, n, by=m)){for (i in ch[1]:ch[2])z <- i}))
#' z
#'
#' message("Seven ways to calculate sum(1:n).
#'  Slowest foreach to fastest chunk is 61000:1 on a dual core notebook with 3GB RAM\n")
#' print(k*system.time({it <- icount(m); foreach (i = it, .combine="+") %do% { i }}))
#'
#' z <- 0;
#' print(k*system.time({it <- icount(m); foreach (i = it) %do% { z <- z + i; NULL }}))
#' z
#'
#' z <- 0; print(system.time({i <- 0L;while (i<n) {i <- i + 1L; z <- z + i}})); z
#'
#' z <- 0; print(system.time(for (i in 1:n) z <- z + i)); z
#'
#' print(system.time(sum(as.double(1:n))))
#'
#' z <- 0; n <- m*k
#' print(system.time(for (ch in chunks(1, n, by=m)){for (i in ch[1]:ch[2])z <- z + i}))
#' z
#'
#' z <- 0; n <- m*k
#' print(system.time(for (ch in chunks(1, n, by=m)){z <- z+sum(as.double(ch[1]:ch[2]))}))
#' z
#'    }
#'
#' @export

chunks <- function(
  from = NULL
  , to = NULL
  , by = NULL
  , length.out = NULL
  , along.with = NULL
  , overlap = 0L
  , method=c("bbatch","seq")
  , maxindex = NA
)
{
  method <- match.arg(method)
  if (!is.null(along.with)){
    if (is.null(from))
      from <- 1L
    else if (length(from)==1)
      from <- as.integer(from)
    else
      stop("'from' must be scalar")
    if (is.null(to))
      to <- length(along.with)
    else if (length(to)==1)
      to <- as.integer(to)
    else
      stop("'to' must be scalar")
  }
  if (length(from)==1)
    from <- as.integer(from)
  else
    stop("'from' must be scalar")
  if (length(to)==1)
    to <- as.integer(to)
  else
    stop("'to' must be scalar")

  if (to<from)
    stop("to < from")
  N <- to - from + 1L

  if (is.null(by)) {
    if (is.null(length.out))
      stop("need either 'by' or 'length.out'")
    if (length(length.out) !=1)
      stop("'length.out' must be scalar")
    length.out <- as.integer(length.out)
    if (length.out>N)
      length.out <- N
    by <- N %/% length.out
  } else if (length(by)==1) {
    by <- as.integer(by)
    if (by<1)
      stop("'by' must be > 0")
    length.out <- (N - 1L) %/% by + 1L
  } else
    stop("'by' must be scalar")

  if (method=="bbatch")
    by <- as.integer(bbatch(N, by)$b)

  if (length.out>1L) {
    from <- cumsum(c(from, rep(by, length.out - 1L)))
    to <- c(from[-1], from[1] + N) - 1L  # fixed by Edwin de Jonge, 18.1.2011
    if (overlap>0)
      from[-1] <- from[-1] - overlap
  }
  n <- length(from)
  s <- seq_len(n)
  ret <- vector("list", n)
  for (i in s) {
    ret[[i]] <- ri(from[i], to[i], maxindex)
  }
  names(ret) <- paste(from, to, sep=":")
  ret
}



#' Methods for chunked range index
#'
#' Calls [chunks()] to create a sequence of range indexes along the object which causes
#'   the method dispatch.
#'
#' `chunk` is generic, the default method is described here, other methods
#'   that automatically consider RAM needs are provided with package 'ff', see
#'   for example [ff::chunk.ffdf()]
#'
#' @param x the object along we want chunks
#' @param RECORDBYTES integer scalar representing the bytes needed to process a single
#'   element of the boolean vector (default 4 bytes for logical)
#' @param BATCHBYTES  integer scalar limiting the number of bytes to be processed in one
#'   chunk, default from `getOption("ffbatchbytes")` if not null, otherwise 16777216
#' @param ... further arguments passed to [chunks()]
#' @return returns a named list of [ri()] objects
#'   representing chunks of subscripts
#' @section available methods: `chunk.default`, [ff::chunk.ff_vector()],
#'   [ff::chunk.ffdf()]
#' @author Jens Oehlschlägel
#' @seealso [chunks()], [ri()], [`seq()`][base::seq], [bbatch()]
#' @keywords data
#' @examples
#'   chunk(complex(1e7))
#'   chunk(raw(1e7))
#'   chunk(raw(1e7), length=3)
#'
#'   chunks(1,10,3)
#'   # no longer do
#'   chunk(1,100,10)
#'   # but for bckward compatibility this works
#'   chunk(from=1,to=100,by=10)
#'
#' @export
chunk <- function(x = NULL, ...){
  if (is.null(x))
    return(chunks(...))
  UseMethod("chunk")
}
#' @describeIn chunk default vector method
#' @export
chunk.default <- function(x = NULL, ..., RECORDBYTES = NULL, BATCHBYTES = NULL) {
  if (is.null(BATCHBYTES)) {
    BATCHBYTES <- getOption("ffbatchbytes", 16777216L)
  }
  if (is.null(RECORDBYTES)) {
    if ("ff" %in% loadedNamespaces())
      RECORDBYTES = get(".rambytes")[get("vmode")(x)]
    else
      RECORDBYTES =
        switch(typeof(x), raw=1L, character=4L, logical=4L, integer=4L, double=8L, complex=16L)
  }
  RECORDBYTES <- sum(RECORDBYTES)
  if (is.na(RECORDBYTES) || RECORDBYTES == 0L)
    stop("RECORDBYTES not above zero, probably due to unknown type resp. vmode")
  n <- length(x)
  if (n){
    l <- list(...)
    if (is.null(l$from))
      l$from <- 1L
    if (is.null(l$to))
      l$to <- n
    if (is.null(l$by) && is.null(l$len)){
      b <- pmin(BATCHBYTES %/% RECORDBYTES, .Machine$integer.max)
      if (b==0L){
        b <- 1L
        warning("single record does not fit into BATCHBYTES")
      }
      l$by <- b
    }
    l$maxindex <- n
    ret <- do.call("chunks", l)
  }else{
    ret <- list()
  }
  ret
}

#' Vectorized Sequences
#'
#' `vecseq` returns concatenated multiple sequences
#'
#' This is a generalization of [sequence()] in that you can choose
#' sequence starts other than 1 and also have options to no concat and/or
#' return a call instead of the evaluated sequence.
#'
#' @param x vector of sequence start points
#' @param y vector of sequence end points (if `is.null(y)` then `x`
#' are taken as endpoints, all starting at 1)
#' @param concat vector of sequence end points (if `is.null(y)` then
#' `x` are taken as endpoints, all starting at 1)
#' @param eval vector of sequence end points (if `is.null(y)` then
#' `x` are taken as endpoints, all starting at 1)
#' @return
#'  - if `concat==FALSE` and `eval==FALSE` a list with n calls that generate sequences
#'  - if `concat==FALSE` and `eval==TRUE ` a list with n sequences
#'  - if `concat==TRUE ` and `eval==FALSE` a single call generating the concatenated
#'    sequences
#'  - if `concat==TRUE` and `eval==TRUE ` an integer vector of concatentated sequences
#' @author Angelo Canty, Jens Oehlschlägel
#' @seealso [`:`][:], [seq()], [sequence()]
#' @keywords manip
#' @examples
#'
#'   sequence(c(3,4))
#'   vecseq(c(3,4))
#'   vecseq(c(1,11), c(5, 15))
#'   vecseq(c(1,11), c(5, 15), concat=FALSE, eval=FALSE)
#'   vecseq(c(1,11), c(5, 15), concat=FALSE, eval=TRUE)
#'   vecseq(c(1,11), c(5, 15), concat=TRUE, eval=FALSE)
#'   vecseq(c(1,11), c(5, 15), concat=TRUE, eval=TRUE)
#'
#' @export
vecseq <- function(x, y=NULL, concat=TRUE, eval=TRUE){
  if (missing(y)) {
    y <- x
    x <- 1L
  }
  if (concat) {
    if (eval) {
      # pure R version was: eval(parse(text=paste("c(",paste(x,y,sep=":",collapse=","),")")))
      # now calling C-code
      nx <- length(x)
      ny <- length(y)
      if (nx<ny)
        x <- rep(as.integer(x), length.out=ny)
      if (ny<nx)
        y <- rep(as.integer(y), length.out=nx)
      .Call(C_R_bit_vecseq, as.integer(x),  as.integer(y))
    }else
      parse(text=paste("c(",paste(x,y,sep=":",collapse=","),")"))[[1]]
  } else {
    # nolint next: unnecessary_nesting_linter. Good parallelism.
    if (eval)
      eval(parse(text=paste("list(",paste(x,y,sep=":",collapse=","),")")))
    else
      as.list(parse(text=paste(x,y,sep=":")))
  }
}
