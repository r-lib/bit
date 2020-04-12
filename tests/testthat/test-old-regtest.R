library("testthat")
library("bit")

regtest.bit <- function(
  N = 50  # number of repetitions for random regression tests
)
{
  #.BITS <- bit:::.BITS  # available in package namespace
  OK <- TRUE
  pool <- c(FALSE, TRUE)
  
  if (!identical(unattr(as.bit(c(FALSE,NA,TRUE))[]), c(FALSE,FALSE,TRUE))){
    message("bit error: wrong coercion of triboolean to (bi)boolean")
    OK <- FALSE
  }
  
  l <- TRUE
  b <- as.bit(l)
  
  i <- -c(1, 0, 1, NA)
  if (!inherits(try(b[i], silent=TRUE), "try-error")){
    message("bit error: did not throw on mixing zero with negative subscripts")
    OK <- FALSE
  }
  
  i <- c(2, 1, 0, 1, NA)
  if (!identical(l[i],unattr(b[i]))){
    message("\nregression test difference between b[i] and l[i]")
    message(l[i])
    message(unattr(b[i]))
    OK <- FALSE
  }
  
  l[0] <- TRUE
  b[0] <- TRUE
  if (!identical(l,unattr(b[]))){
    message("\nregression test difference after assigning at R position zero")
    message(l)
    message(unattr(b[]))
    OK <- FALSE
  }
  
  l[2] <- TRUE
  b[2] <- TRUE
  if (!identical(ifelse(is.na(l), FALSE, l),unattr(b[]))){
    message("\nregression test difference after assigning after vector length (at 2)")
    message(l)
    message(unattr(b[]))
    OK <- FALSE
  }
  
  l[.BITS+1] <- FALSE
  b[.BITS+1] <- NA
  if (!identical(ifelse(is.na(l), FALSE, l),unattr(b[]))){
    message("\nregression test difference after assigning after vector length (at .BITS+1)")
    message(l)
    message(unattr(b[]))
    OK <- FALSE
  }
  
  if (!identical(ifelse(is.na(l[TRUE]), FALSE, l[TRUE]),unattr(b[TRUE]))){
    message("\nregression test difference after subscripting with scalar TRUE")
    message(l)
    message(unattr(b[]))
    OK <- FALSE
  }
  
  if (!identical(ifelse(is.na(l[FALSE]), FALSE, l[FALSE]),unattr(b[FALSE]))){
    message("\nregression test difference after subscripting with scalar FALSE")
    message(l)
    message(unattr(b[]))
    OK <- FALSE
  }
  
  
  
  for (i in 1:N){
    n <- sample(1:(2*.BITS), 1)
    l <- sample(pool, n, TRUE)
    # check direct coercion
    b <- as.bit(l)
    l2 <- as.logical(b)
    if (!identical(l,l2)){
      message("\nregression test difference between logical")
      message(l)
      message("and as.logical(as.bit(logical))")
      message(l2)
      OK <- FALSE
    }
    
    
    # summary functions with logical return
    s <- c(all=all(l), any=any(l))
    s2 <- c(all=all(b), any=any(b))
    if (!identical(s,s2)){
      message("\nregression test difference between logical summaries")
      message(s)
      message("and bit summaries")
      message(s2)
      OK <- FALSE
    }
    # summary functions with integer return
    if (any(l)){
      s <- c(min=min(as.which(l)), max=max(as.which(l)), range=range(as.which(l)), sum=sum(l), summary=c("FALSE"=length(l)-sum(l), "TRUE"=sum(l), "Min."=min(as.which(l)), "Max."=max(as.which(l))))
    }else{
      s <- c( min=NA_integer_, max=NA_integer_, range=c(NA_integer_, NA_integer_), sum=sum(l), summary=c("FALSE"=length(l)-sum(l), "TRUE"=sum(l), "Min."=as.integer(NA), "Max."=as.integer(NA)) )
    }
    s2 <- c(min=min(b), max=max(b), range=range(b), sum=sum(b), summary=summary(b))
    if (!identical(s,s2)){
      message("\nregression test difference between logical summaries")
      message(s)
      message("and bit summaries")
      message(s2)
      OK <- FALSE
    }
    # check positive whichs
    w <- as.which(l)
    w2 <- as.which(as.bit(w, n))
    if (!identical(w,w2)){
      message("\nregression test difference between which")
      message(w)
      message("and as.which(as.bit.which(which))")
      message(w2)
      OK <- FALSE
    }
    # check automatic whichs (pos or neg whatever shorter)
    s <- sum(l)
    if (s==0){
      w <- FALSE
    }else if (s==n){
      w <- TRUE
    }else if (s>(n%/%2L)){
      w <- -rev(which(!l))
    }else{
      w <- which(l)
    }
    w2 <- as.vector(as.bitwhich(as.bit(l)))
    if (!identical(w,w2)){
      message("\nregression test difference between which")
      message(w)
      message("and as.which(as.bit.which(which))")
      message(w2)
      OK <- FALSE
    }
    # check boolean operators
    l2 <- sample(c(FALSE, TRUE), n, TRUE)
    b2 <- as.bit(l2)
    ops <- c(
      NOT = identical(!l, as.logical(!b))
      , AND = identical(l&l2, as.logical(b&b2))
      , OR = identical(l|l2, as.logical(b|b2))
      , XOR = identical(xor(l,l2), as.logical(xor(b,b2)))
      , NEQ = identical(l!=l2, as.logical(b!=b2))
      , EQ = identical(l==l2, as.logical(b==b2))
    )
    if (!all(ops)){
      message("\nbit differs for boolean operators(s)")
      message(ops)
      message(cbind(l=l, l2=l))
      OK <- FALSE
    }
    w <- as.bitwhich(l)
    w2 <- as.bitwhich(l2)
    ops <- c(
      NOT = identical(!l, as.logical(!w))
      , AND = identical(l&l2, as.logical(w&w2))
      , OR = identical(l|l2, as.logical(w|w2))
      , XOR = identical(xor(l,l2), as.logical(xor(w,w2)))
      , NEQ = identical(l!=l2, as.logical(w!=w2))
      , EQ = identical(l==l2, as.logical(w==w2))
    )
    if (!all(ops)){
      message("\nbitwhich differs for boolean operators(s)")
      message(ops)
      message(cbind(l=l, l2=l))
      OK <- FALSE
    }
    rm(l2,b2,w2)
    # check extractors
    n2 <- sample(1:n, 1)
    j <- sample(1:n, n2)
    if (!identical(l[j], unattr(b[j]))){
      message("\nregression test difference when extracting")
      OK <- FALSE
    }
    # check replacement (index)
    new <- sample(pool, n2, TRUE)
    l[j] <- new
    b[j] <- new
    if (!identical(l, unattr(b[]))){
      message("\nregression test difference when replacing with index")
      OK <- FALSE
    }
    # check replacement (recycle)
    if (n%%2){
      new <- sample(pool, 1)
      l[] <- new
      b[] <- new
    }else{
      l[] <- pool
      b[] <- pool
    }
    if (!identical(l, as.logical(b))){
      message("\nregression test difference when replacing with recylcling")
      OK <- FALSE
    }
  }
  
  l0 <- c(FALSE, FALSE, FALSE)
  l1 <- c(FALSE, FALSE, TRUE)
  l2 <- c(FALSE, TRUE, TRUE)
  l3 <- c(TRUE, TRUE, TRUE)
  
  bw0 <- as.bitwhich(l0)
  bw1 <- as.bitwhich(l1)
  bw2 <- as.bitwhich(l2)
  bw3 <- as.bitwhich(l3)
  
  OK <- OK && identical(l0, as.logical(bw0))
  OK <- OK && identical(l1, as.logical(bw1))
  OK <- OK && identical(l2, as.logical(bw2))
  OK <- OK && identical(l3, as.logical(bw3))
  
  OK <- OK && identical(l0 & l0, as.logical(bw0 & bw0))
  OK <- OK && identical(l0 & l1, as.logical(bw0 & bw1))
  OK <- OK && identical(l0 & l2, as.logical(bw0 & bw2))
  OK <- OK && identical(l0 & l3, as.logical(bw0 & bw3))
  
  OK <- OK && identical(l1 & l0, as.logical(bw1 & bw0))
  OK <- OK && identical(l1 & l1, as.logical(bw1 & bw1))
  OK <- OK && identical(l1 & l2, as.logical(bw1 & bw2))
  OK <- OK && identical(l1 & l3, as.logical(bw1 & bw3))
  
  OK <- OK && identical(l2 & l0, as.logical(bw2 & bw0))
  OK <- OK && identical(l2 & l1, as.logical(bw2 & bw1))
  OK <- OK && identical(l2 & l2, as.logical(bw2 & bw2))
  OK <- OK && identical(l2 & l3, as.logical(bw2 & bw3))
  
  OK <- OK && identical(l3 & l0, as.logical(bw3 & bw0))
  OK <- OK && identical(l3 & l1, as.logical(bw3 & bw1))
  OK <- OK && identical(l3 & l2, as.logical(bw3 & bw2))
  OK <- OK && identical(l3 & l3, as.logical(bw3 & bw3))
  
  
  OK <- OK && identical(l0 | l0, as.logical(bw0 | bw0))
  OK <- OK && identical(l0 | l1, as.logical(bw0 | bw1))
  OK <- OK && identical(l0 | l2, as.logical(bw0 | bw2))
  OK <- OK && identical(l0 | l3, as.logical(bw0 | bw3))
  
  OK <- OK && identical(l1 | l0, as.logical(bw1 | bw0))
  OK <- OK && identical(l1 | l1, as.logical(bw1 | bw1))
  OK <- OK && identical(l1 | l2, as.logical(bw1 | bw2))
  OK <- OK && identical(l1 | l3, as.logical(bw1 | bw3))
  
  OK <- OK && identical(l2 | l0, as.logical(bw2 | bw0))
  OK <- OK && identical(l2 | l1, as.logical(bw2 | bw1))
  OK <- OK && identical(l2 | l2, as.logical(bw2 | bw2))
  OK <- OK && identical(l2 | l3, as.logical(bw2 | bw3))
  
  OK <- OK && identical(l3 | l0, as.logical(bw3 | bw0))
  OK <- OK && identical(l3 | l1, as.logical(bw3 | bw1))
  OK <- OK && identical(l3 | l2, as.logical(bw3 | bw2))
  OK <- OK && identical(l3 | l3, as.logical(bw3 | bw3))
  
  
  OK <- OK && identical(xor(l0,l0), as.logical(xor(bw0,bw0)))
  OK <- OK && identical(xor(l0,l1), as.logical(xor(bw0,bw1)))
  OK <- OK && identical(xor(l0,l2), as.logical(xor(bw0,bw2)))
  OK <- OK && identical(xor(l0,l3), as.logical(xor(bw0,bw3)))
  
  OK <- OK && identical(xor(l1,l0), as.logical(xor(bw1,bw0)))
  OK <- OK && identical(xor(l1,l1), as.logical(xor(bw1,bw1)))
  OK <- OK && identical(xor(l1,l2), as.logical(xor(bw1,bw2)))
  OK <- OK && identical(xor(l1,l3), as.logical(xor(bw1,bw3)))
  
  OK <- OK && identical(xor(l2,l0), as.logical(xor(bw2,bw0)))
  OK <- OK && identical(xor(l2,l1), as.logical(xor(bw2,bw1)))
  OK <- OK && identical(xor(l2,l2), as.logical(xor(bw2,bw2)))
  OK <- OK && identical(xor(l2,l3), as.logical(xor(bw2,bw3)))
  
  OK <- OK && identical(xor(l3,l0), as.logical(xor(bw3,bw0)))
  OK <- OK && identical(xor(l3,l1), as.logical(xor(bw3,bw1)))
  OK <- OK && identical(xor(l3,l2), as.logical(xor(bw3,bw2)))
  OK <- OK && identical(xor(l3,l3), as.logical(xor(bw3,bw3)))
  
  
  OK <- OK && identical(c(l0,l0), as.logical(c(bw0,bw0)))
  OK <- OK && identical(c(l0,l1), as.logical(c(bw0,bw1)))
  OK <- OK && identical(c(l0,l2), as.logical(c(bw0,bw2)))
  OK <- OK && identical(c(l0,l3), as.logical(c(bw0,bw3)))
  
  OK <- OK && identical(c(l1,l0), as.logical(c(bw1,bw0)))
  OK <- OK && identical(c(l1,l1), as.logical(c(bw1,bw1)))
  OK <- OK && identical(c(l1,l2), as.logical(c(bw1,bw2)))
  OK <- OK && identical(c(l1,l3), as.logical(c(bw1,bw3)))
  
  OK <- OK && identical(c(l2,l0), as.logical(c(bw2,bw0)))
  OK <- OK && identical(c(l2,l1), as.logical(c(bw2,bw1)))
  OK <- OK && identical(c(l2,l2), as.logical(c(bw2,bw2)))
  OK <- OK && identical(c(l2,l3), as.logical(c(bw2,bw3)))
  
  OK <- OK && identical(c(l3,l0), as.logical(c(bw3,bw0)))
  OK <- OK && identical(c(l3,l1), as.logical(c(bw3,bw1)))
  OK <- OK && identical(c(l3,l2), as.logical(c(bw3,bw2)))
  OK <- OK && identical(c(l3,l3), as.logical(c(bw3,bw3)))
  
  N <- 2L*.BITS
  l <- logical(N)
  b <- bit(N)
  for (i in 1:N){
    l[i] <- TRUE
    b[i] <- TRUE
    if (!identical(l,as.logical(b))){
      message("\nregression test difference when replacing at position", i, "")
      OK <- FALSE
    }
  }
  
  OK
}


context("old regression test")

test_that("old regtest is still OK", {
  expect_identical(regtest.bit(), TRUE)
})

test_that("some old regression tests are also OK for bitwhich", {
  expect_error(TRUE[c(-1, 1)], label="positive mixed with zeros")
  expect_error(as.bit(TRUE)[c(-1, 1)], label="positive mixed with zeros")
  expect_error(as.bitwhich(TRUE)[c(-1, 1)], label="positive mixed with zeros")
  
  expect_error(TRUE[c(-1, NA)], label="NA mixed with zeros")
  expect_error(as.bit(TRUE)[c(-1, NA)], label="NA mixed with zeros")
  expect_error(as.bitwhich(TRUE)[c(-1, NA)], label="NA mixed with zeros")
  
  expect_equivalent(as.bit(TRUE)[c(2, 1, 0, 1, NA)], TRUE[c(2, 1, 0, 1, NA)])
  expect_equivalent(as.bitwhich(TRUE)[c(2, 1, 0, 1, NA)], TRUE[c(2, 1, 0, 1, NA)])
  
  expect_identical({b <- as.bit(FALSE); b[0] <- TRUE; as.logical(b)}, {l <- FALSE; l[0] <- TRUE; l})
  expect_identical({w <- as.bitwhich(FALSE); w[0] <- TRUE; as.logical(w)}, {l <- FALSE; l[0] <- TRUE; l})
  
  expect_identical({b <- as.bit(FALSE); b[2] <- TRUE; as.logical(b)}, {l <- FALSE; l[2] <- TRUE; l})
  expect_identical({w <- as.bitwhich(FALSE); w[2] <- TRUE; as.logical(w)}, {l <- FALSE; l[2] <- TRUE; l})
  
  expect_identical({b <- as.bit(FALSE); b[bit:::.BITS+1] <- NA; as.logical(b)}, {l <- FALSE; l[bit:::.BITS+1] <- FALSE; l[is.na(l)]<- FALSE; l})
  expect_identical({w <- as.bitwhich(FALSE); w[bit:::.BITS+1] <- NA; as.logical(w)}, {l <- FALSE; l[bit:::.BITS+1] <- FALSE; l[is.na(l)]<- FALSE; l})
  
  expect_equivalent(as.bit(rep(c(FALSE,TRUE), bit:::.BITS))[TRUE], rep(c(FALSE,TRUE), bit:::.BITS)[TRUE], label="subscripting with scalar TRUE")  
  expect_equivalent(as.bitwhich(rep(c(FALSE,TRUE), bit:::.BITS))[TRUE], rep(c(FALSE,TRUE), bit:::.BITS)[TRUE], label="subscripting with scalar TRUE")  
  
  expect_equivalent(as.bit(rep(c(FALSE,TRUE), bit:::.BITS))[FALSE], rep(c(FALSE,TRUE), bit:::.BITS)[FALSE], label="subscripting with scalar FALSE")  
  expect_equivalent(as.bitwhich(rep(c(FALSE,TRUE), bit:::.BITS))[FALSE], rep(c(FALSE,TRUE), bit:::.BITS)[FALSE], label="subscripting with scalar FALSE")  
})  

