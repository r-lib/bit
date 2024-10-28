test_that("Can create zero length bit objects", {
  expect_identical(bit(), bit(0))
  expect_identical(as.bit(), bit(0))
  expect_identical(as.bit(NULL), bit(0))
  expect_identical(as.bit(logical()), bit(0))
  expect_identical(as.bit(integer()), bit(0))
  expect_error(as.bit(factor()))
})

test_that("length<-.bit does set unused bits to FALSE", {
  b <- !bit(.BITS)
  length(b) <- 7
  b2 <- !bit(7)
  expect_identical(b, b2)
  length(b) <- .BITS
  length(b2) <- .BITS
  expect_identical(b, b2)
  b <- !bit(2*.BITS)
  length(b) <- 7
  b2 <- !bit(7)
  expect_identical(b, b2)
  length(b) <- 2*.BITS
  length(b2) <- 2*.BITS
  expect_identical(b, b2)
})

test_that("length<-.bit does set new bits to FALSE", {
  b <- !bit(1)
  length(b) <- .BITS
  b2 <- bit(.BITS)
  b2[1] <- TRUE
  expect_identical(b, b2)
  b <- !bit(1)
  length(b) <- 2*.BITS
  b2 <- bit(2*.BITS)
  b2[1] <- TRUE
  expect_identical(b, b2)
})

test_that("c.bit does set unused bits to FALSE", {
  b <- !bit(.BITS-1)
  b <- c(b, b)
  b2 <- !bit(2*.BITS-2)
  expect_identical(b, b2)
})

test_that("Can create zero length bitwhich objects", {
  expect_identical(bitwhich(), bitwhich(0))
  expect_identical(as.bitwhich(), bitwhich(0))
  expect_identical(as.bitwhich(NULL), bitwhich(0))
  expect_identical(as.bitwhich(logical()), bitwhich(0))
  expect_identical(as.bitwhich(integer()), bitwhich(0))
  expect_error(as.bitwhich(factor()))
})

# nolint start: undesirable_function_linter. structure() seems OK here.
test_that("bitwhich creates correctly", {
  # to check whether we properly obtain integer
  n <- 12
  x <- rep(3:10, 2)
  y <- c(-12L, -11L, -2L, -1L)
  attr(y, "maxindex") = 12L
  attr(y, "poslength") = 8L
  class(y) = c("booltype", "bitwhich")
  eval(substitute(expect_identical(bitwhich(n, x), y), list(n=n, x=x, y=y)))
  eval(substitute(expect_identical(bitwhich(n, unique(x), has.dup=FALSE), y), list(n=n, x=x, y=y)))
  eval(substitute(
    expect_identical(bitwhich(n, sort(x), is.unsorted = FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, sort(unique(x)), is.unsorted = FALSE, has.dup=FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, c(-12L, -11L, -2L, -1L), poslength=8), y),
    list(n=n, x=x, y=y)
  ))

  x <- -rev(x)
  y <- c(1L, 2L, 11L, 12L)
  attr(y, "maxindex") = 12L
  attr(y, "poslength") = 4L
  class(y) = c("booltype", "bitwhich")
  eval(substitute(expect_identical(bitwhich(n, x), y), list(n=n, x=x, y=y)))
  eval(substitute(expect_identical(bitwhich(n, unique(x), has.dup=FALSE), y), list(n=n, x=x, y=y)))
  eval(substitute(
    expect_identical(bitwhich(n, sort(x), is.unsorted = FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, sort(unique(x)), is.unsorted = FALSE, has.dup=FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, c(1L, 2L, 11L, 12L), poslength=4), y),
    list(n=n, x=x, y=y)
  ))

  x <- rep(5:6, 2)
  y <- structure(5:6, maxindex = 12L, poslength = 2L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n, x), y), list(n=n, x=x, y=y)))
  eval(substitute(expect_identical(bitwhich(n, unique(x), has.dup=FALSE), y), list(n=n, x=x, y=y)))
  eval(substitute(
    expect_identical(bitwhich(n, sort(x), is.unsorted = FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, sort(unique(x)), is.unsorted = FALSE, has.dup=FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(expect_identical(bitwhich(n, 5:6, poslength=2), y), list(n=n, x=x, y=y)))

  x <- -rev(x)
  y <- structure(-6:-5, maxindex = 12L, poslength = 10L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n, x), y), list(n=n, x=x, y=y)))
  eval(substitute(expect_identical(bitwhich(n, unique(x), has.dup=FALSE), y), list(n=n, x=x, y=y)))
  eval(substitute(
    expect_identical(bitwhich(n, sort(x), is.unsorted = FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(
    expect_identical(bitwhich(n, sort(unique(x)), is.unsorted = FALSE, has.dup=FALSE), y),
    list(n=n, x=x, y=y)
  ))
  eval(substitute(expect_identical(bitwhich(n, -6:-5, poslength=10), y), list(n=n, x=x, y=y)))

  y <- structure(TRUE, maxindex = 12L, poslength = 12L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n, TRUE), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, poslength=n), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, integer(), poslength=n), y), list(n=n, y=y)))

  y <- structure(FALSE, maxindex = 12L, poslength = 0L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n, FALSE), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, poslength=0), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, integer(), poslength=0), y), list(n=n, y=y)))

  y <- structure(FALSE, maxindex = 12L, poslength = 0L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, poslength=0), y), list(n=n, y=y)))

  y <- structure(FALSE, maxindex = 12L, poslength = 0L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(n), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n), y), list(n=n, y=y)))
  eval(substitute(expect_identical(bitwhich(n, poslength=0), y), list(n=n, y=y)))

  y <- structure(logical(0), maxindex = 0L, poslength = 0L, class = c("booltype", "bitwhich"))
  eval(substitute(expect_identical(bitwhich(0), y), list(y=y)))
  eval(substitute(expect_identical(bitwhich(0, poslength=0), y), list(y=y)))
})
# nolint end: undesirable_function_linter.

test_that("length<-.bitwhich does set new bits according to the rules given in details", {
  w <- bitwhich(0)
  length(w) <- 2
  w2 <- bitwhich(2, FALSE)
  expect_identical(w, w2)

  w <- bitwhich(1, FALSE)
  length(w) <- 2
  w2 <- bitwhich(2, FALSE)
  expect_identical(w, w2)

  w <- bitwhich(1, TRUE)
  length(w) <- 2
  w2 <- bitwhich(2, TRUE)
  expect_identical(w, w2)

  w <- bitwhich(6, 1:2)
  length(w) <- 12
  w2 <- bitwhich(12, 1:2)
  expect_identical(w, w2)

  w <- bitwhich(6, -(2:1))
  length(w) <- 12
  w2 <- bitwhich(12, -(2:1))
  expect_identical(w, w2)
})

test_that("c() works", {
  l <- b <- w <- list()
  for (k in 1:12) {
    l[[k]] <- rep_len(c(FALSE, TRUE), k)
    b[[k]] <- as.bit(l[[k]])
    w[[k]] <- as.bitwhich(l[[k]])
  }
  l <- do.call("c", l)
  b <- as.logical(do.call("c", b))
  w <- as.logical(do.call("c", w))
  expect_identical(b, l)
  expect_identical(w, l)
})

# nolint start: rep_len_linter. Specifically testing rep().
test_that("rep() works", {
  l <- c(FALSE, TRUE)
  b <- as.bit(l)
  w <- as.bitwhich(l)
  for (k in 1:(3*.BITS)) {
    expect_identical(as.logical(rep(b, length.out=k)), rep(l, length.out=k))
    expect_identical(as.logical(rep(w, length.out=k)), rep(l, length.out=k))
  }
  for (k in 1:(2*.BITS)) {
    expect_identical(as.logical(rep(b, k)), rep(l, k))
    expect_identical(as.logical(rep(w, k)), rep(l, k))
  }
})
# nolint end: rep_len_linter.

test_that("NAs are coerced to FALSE", {
  expect_identical(as.logical(as.bit(c(NA, FALSE, TRUE))), c(FALSE, FALSE, TRUE))
  expect_identical(as.logical(as.bitwhich(c(NA, FALSE, TRUE))), c(FALSE, FALSE, TRUE))
})

test_that("coercions work", {
  for (i in c(FALSE, TRUE)) {
    for (j in c(FALSE, TRUE)) {
      for (k in c(FALSE, TRUE)) {
        l <- c(i, j, k)
        expect_identical(as.logical(as.bit(l)), l)
        expect_identical(as.logical(as.bitwhich(l)), l)
        expect_identical(as.logical(as.bit(as.bit(l))), l)
        expect_identical(as.logical(as.bitwhich(as.bitwhich(l))), l)
        expect_identical(as.logical(as.bit(as.bitwhich(l))), l)
        expect_identical(as.logical(as.bitwhich(as.bit(l))), l)
        set.seed(1)
        for (m in 1:ifelse(sum(l) %in% c(0L, 3L), 1, 24)) {
          l <- sample(l, 3*.BITS, TRUE)
          expect_identical(as.logical(as.bit(l)), l)
          expect_identical(as.logical(as.bitwhich(l)), l)
          expect_identical(as.logical(as.bit(as.bit(l))), l)
          expect_identical(as.logical(as.bitwhich(as.bitwhich(l))), l)
          expect_identical(as.logical(as.bit(as.bitwhich(l))), l)
          expect_identical(as.logical(as.bitwhich(as.bit(l))), l)
        }
      }
    }
  }

  set.seed(1)
  for (k in 0:(3*.BITS)) {
    l <- sample(c(FALSE, TRUE), k, TRUE)
    expect_identical(as.logical(as.bit(l)), l)
    expect_identical(as.logical(as.bit(as.bit(l))), l)
    expect_identical(as.logical(as.bit(as.bitwhich(l))), l)
    expect_identical(as.logical(as.bitwhich(as.bit(l))), l)
    expect_identical(as.logical(as.bitwhich(as.bitwhich(l))), l)
  }
  set.seed(1)
  for (k in 0:(3*.BITS)) {
    l <- sample(c(NA, FALSE, TRUE), k, TRUE)
    b <- as.bit(l)
    expect_identical(as.bit(as.bit(l)), b)
    expect_identical(as.bit(as.bit(as.bit(l))), b)
    expect_identical(as.bit(as.bit(as.bitwhich(l))), b)
    expect_identical(as.bit(as.bitwhich(as.bit(l))), b)
    expect_identical(as.bit(as.bitwhich(as.bitwhich(l))), b)
  }
  set.seed(1)
  for (k in 0:(3*.BITS)) {
    i <- sample(-2:2, k, TRUE)
    expect_identical(as.logical(as.bit(i)), as.logical(i))
    expect_identical(as.logical(as.bitwhich(i)), as.logical(i))
    i <- as.double(i)
    expect_identical(as.logical(as.bit(i)), as.logical(i))
    expect_identical(as.logical(as.bitwhich(i)), as.logical(i))
  }

}
)


test_that("boolean operations work", {
  N <- c(1L, .BITS-1L, .BITS, .BITS+1L)
  X <- c(rev(-N), N)
  N <- c(0L, N)
  fx <- function(x, n)as.integer(sign(x))*sample(n, abs(x))
  for (n in N) {
    for (x1 in X[abs(X)<=abs(n)]) {
      for (x2 in X[abs(X)<=abs(n)]) {
        set.seed(1)
        w1 <- bitwhich(n, fx(x1, n))
        w2 <- bitwhich(n, fx(x2, n))
        l1 <- as.logical(w1)
        l2 <- as.logical(w2)
        b1 <- as.bit(l1)
        b2 <- as.bit(l2)
        fun <- function(x1, x2, f) {
          eval(substitute(
            expect_identical(f(!x1), !f(x1)),
            list(x1=x1, f=f)
          ))
          eval(substitute(
            expect_identical(f(x1 & x2), f(x1) & f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(f(x1 | x2), f(x1) | f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(f(x1 == x2), f(x1) == f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(f(x1 != x2), f(x1) != f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(f(xor(x1, x2)), xor(f(x1), f(x2))),
            list(x1=x1, x2=x2, f=f)
          ))
        }
        fun(w1, w2, as.logical)
        fun(b1, b2, as.logical)
        fun <- function(x1, x2, f) {
          eval(substitute(
            expect_identical(x1 & x2, f(x1) & f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(x1 | x2, f(x1) | f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(x1 == x2, f(x1) == f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(x1 != x2, f(x1) != f(x2)),
            list(x1=x1, x2=x2, f=f)
          ))
          eval(substitute(
            expect_identical(xor(x1, x2), xor(f(x1), f(x2))),
            list(x1=x1, x2=x2, f=f)
          ))
        }
        fun(b1, w2, as.bit)
        fun(w1, b2, as.bit)
        fun(b1, l2, as.logical)
        fun(l1, b2, as.logical)
        fun(l1, w2, as.logical)
        fun(w1, l2, as.logical)
      }
    }
  }
})


test_that("promotion is correct in boolean operations and concatenation", {
  N2 <- N1 <- c(2L, 4L)
  T2 <- T1 <- c("logical", "bit", "bitwhich")
  FUN <- list(logical=logical, bit=bit, bitwhich=bitwhich)
  for (t1 in T1) {
    for (t2 in T2) {
      for (n1 in N1) {
        for (n2 in N2) {
          x1 <- FUN[[t1]](n1)
          x2 <- FUN[[t2]](n2)
          eval(substitute(
            expect_identical(booltype(x1 & x2), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
          eval(substitute(
            expect_identical(booltype(x1 | x2), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
          eval(substitute(
            expect_identical(booltype(x1 == x2), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
          eval(substitute(
            expect_identical(booltype(x1 != x2), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
          eval(substitute(
            expect_identical(booltype(xor(x1, x2)), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
          if (t1!="logical") { # c with first argument logical does not dispatch
            eval(substitute(
              expect_identical(booltype(c(x1, x2)), min(booltypes[[t1]], booltypes[[t2]])),
              list(x1=x1, x2=x2, t1=t1, t2=t2)
            ))
          }
          eval(substitute(
            expect_identical(booltype(c.booltype(x1, x2)), min(booltypes[[t1]], booltypes[[t2]])),
            list(x1=x1, x2=x2, t1=t1, t2=t2)
          ))
        }
      }
    }
  }
})


test_that("subscript operations work", {
  N <- c(1L, .BITS-1L, .BITS, .BITS+1L)
  X <- c(rev(-N), N)
  J <- c(rev(-N), 0L, N)
  N <- c(0L, N)
  R <- 1
  fx <- function(x, n) as.integer(sign(x))*sample(n, abs(x), TRUE)
  fi <- function(x, n) as.integer(sign(x))*sample(0:n, abs(x), FALSE)

  for (r in 1:R) {
    for (n in N) {
      for (x in X[abs(X)<=abs(n)]) {
        set.seed(r)
        w <- bitwhich(n, fx(x, n))
        l <- as.logical(w)
        b <- as.bit(l)
        I <- J[abs(J)<=n]
        I <- lapply(I, fi, n=n)
        I <- c(list(FALSE, TRUE), I, lapply(I, as.which, maxindex=n))
        for (i in I) {
          v <- l[i]
          eval(substitute(expect_identical(b[i], v, ignore_attr="vmode"), list(b=b, i=i, v=v)))
          eval(substitute(expect_identical(w[i], v, ignore_attr="vmode"), list(w=w, i=i, v=v)))
          l2 <- l
          l2[i] <- !v
          eval(substitute(env = list(b=b, i=i, v=v, l2=l2), {
            b2 <- b
            b2[i] <- !v
            expect_identical(b2, as.bit(l2))
          }))
          eval(substitute(env = list(w=w, i=i, v=v, l2=l2), {
            w2 <- w
            w2[i] <- !v
            expect_identical(w2, as.bitwhich(l2))
          }))
          if (length(v)>1 && (length(v)%%2L) == 0) {
            v2 <- !v[seq_len(ceiling(length(v)/2))]
            l2 <- l
            l2[i] <- v2
            eval(substitute(env = list(b=b, i=i, v=v, l2=l2), {
              b2 <- b
              b2[i] <- v2
              expect_identical(b2, as.bit(l2))
            }))
            eval(substitute(env = list(w=w, i=i, v=v, l2=l2), {
              w2 <- w
              w2[i] <- v2
              expect_identical(w2, as.bitwhich(l2))
            }))
          }
        }
        I <- lapply(1:1, function(x) {
          i <- quicksort2(sample(n, 2, TRUE))
          ri(i[1], i[2], n)
        })
        for (i in I) {
          i2 <- i[1]:i[2]
          v <- l[i2]
          eval(substitute(expect_identical(b[i], v, ignore_attr="vmode"), list(b=b, i=i, v=v)))
          eval(substitute(expect_identical(w[i], v, ignore_attr="vmode"), list(w=w, i=i, v=v)))
          # debugonce(get("[.bitwhich"))
          l2 <- l
          l2[i2] <- !v
          eval(substitute(env = list(b=b, i=i, v=v, l2=l2), {
            b2 <- b
            b2[i] <- !v
            expect_identical(b2, as.bit(l2))
          }))
          eval(substitute(env = list(w=w, i=i, v=v, l2=l2), {
            w2 <- w
            w2[i] <- !v
            expect_identical(w2, as.bitwhich(l2))
          }))
          if (length(v)>1 && (length(v)%%2L) == 0) {
            v2 <- !v[seq_len(ceiling(length(v)/2))]
            l2[i2] <- v2
            eval(substitute(env = list(b=b, i=i, v2=v2, l2=l2), {
              b2 <- b
              b2[i] <- v2
              expect_identical(b2, as.bit(l2))
            }))
            eval(substitute(env = list(w=w, i=i, v2=v2, l2=l2), {
              w2 <- w
              w2[i] <- v2
              expect_identical(w2, as.bitwhich(l2))
            }))
          }
        }
        i <- sample(n, 1)
        v <- l[[i]]
        eval(substitute(expect_identical(b[[i]], v, ignore_attr="vmode"), list(b=b, i=i, v=v)))
        eval(substitute(expect_identical(w[[i]], v, ignore_attr="vmode"), list(w=w, i=i, v=v)))
        l2 <- l
        l2[[i]] <- !v
        eval(substitute(env = list(b=b, i=i, v=v, l2=l2), {
          b2 = b
          b2[[i]] = !v
          expect_identical(b2, as.bit(l2))
        }))
        eval(substitute(env = list(w=w, i=i, v=v, l2=l2), {
          w2 = w
          w2[[i]] = !v
          expect_identical(w2, as.bitwhich(l2))
        }))

      }
    }
  }

})


test_that("aggregation functions work", {
  D <- list(
    full_range=ri(1, 128, 128),
    begin_range=ri(1, 5, 128),
    end_range=ri(99, 128, 128),
    begin_scalar=ri(1, 1, 128),
    end_scalar=ri(128, 128, 128)
  )
  R <- list(
    norange=NULL,
    begin_range=ri(1, 64, 128),
    end_range=ri(65, 128, 128),
    mid_range=ri(32, 96, 128),
    full_range=ri(1, 128, 128)
  )
  I <- list(id=function(x) x, not=function(x) !x)
  A <- list(
    logical=as.logical,
    bit=as.bit,
    bitwhich=as.bitwhich,
    which=as.which,
    ri=function(x)x
  )
  S1 <- list(any=any, all=all, sum=sum)
  S2 <- list(
    min=function(x) {
      if (booltype(x) != "logical") return(min(x))
      if (!any(x)) return(NA_integer_)
      which.max(x)
    },
    max=function(x) {
      if (booltype(x) != "logical") return(max(x))
      if (!any(x)) return(NA_integer_)
      length(x) - which.max(rev(x)) + 1L
    },
    range=function(x) if (booltype(x)=="logical") range.booltype(x) else range(x),
    summary=function(x) if (booltype(x)=="logical") summary.booltype(x) else summary(x)
  )
  S3 <- list(
    sum=sum.booltype,
    min=min.booltype,
    max=max.booltype,
    range=range.booltype,
    summary=summary.booltype
  )
  for (d in names(D)) {
    for (i in names(I)) {
      x <- I[[i]](as.logical(D[[d]]))
      for (a in names(A)) if (! (i=="not" && a %in% c("which", "ri"))) {
        y <- I[[i]](A[[a]](D[[d]]))
        for (s in names(S1)) {
          expect_identical(S1[[s]](x), S1[[s]](y))
        }
        if (a != "which") for (s in names(S2)) {
          expect_identical(S2[[s]](x), S2[[s]](y))
        }
        if (a != "which") for (s in names(S3)) {
          expect_identical(S3[[s]](x), S3[[s]](y))
        }
      }
    }
  }

})

test_that("rev() methods work", {
  x = c(FALSE, TRUE)
  expect_identical(rev(as.bit(x)), as.bit(rev(x)))
  expect_identical(rev(as.bitwhich(x)), as.bitwhich(rev(x)))
})

test_that("as.integer method for bit works", {
  expect_identical(as.integer(as.bit(0:1)), 0:1)
})
