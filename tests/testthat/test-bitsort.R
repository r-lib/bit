library("testthat")
library("bit")

context("bitsort")

# expect_identical <- function(x, y, ...){
#  stopifnot(identical(x,y))
# }

test_that("bitsort function work on several inputs", {
  nums <- list(
      integer()
    , as.integer(9:0)
    , as.integer(0:9)
    , as.integer(0:(-9))
    , as.integer((-9):0)
    , as.integer((-4):4)
    , as.integer(4:(-4))
  )
  nas <- list(
      integer()
    , NA_integer_
    , c(NA_integer_,NA_integer_)
  )
  set.seed(1)
  for (num in nums){
    for (na in nas){
      x <- c(num,na)
      if (length(x)>1)
        x <- sample(x)
      #dput(x)
      
      range_result <- suppressWarnings( as.integer(c( range(x, na.rm=TRUE), sum(is.na(x)) )) )
      expect_identical(
        range_na(x)
      , range_result
      , label=paste("range_na(c(", paste(x, collapse=","), "))", sep="")
      )
      
      range_result <- suppressWarnings( as.integer(c( range(x[is.na(x) | x!=0], na.rm=TRUE), sum(is.na(x)) )) )
      expect_identical(
        attr(range_nanozero(x),"range_na")
      , range_result
      , label=paste("range_na(c(", paste(x, collapse=","), ")", sep=""))
      
      for (na.last in c(NA,FALSE,TRUE)){
        expect_identical(
            {y <- range_sortna(x, na.last=na.last); y[!is.na(y)] <- 1L; y}
          , {y <- sort.int(x, na.last=na.last, method="quick"); r <- range_na(y); r[4] <- is.unsorted(x, na.rm=TRUE); y[!is.na(y)] <- 1L; attr(y, "range_sortna") <- r; y}
          , label=paste("range_sortna(c(", paste(x, collapse=","), "), na.last=", na.last,")", sep="")
        )

        expect_identical(
           bit_sort_unique(x, na.last=na.last, has.dup = TRUE)
           , sort.int(unique(x), na.last=na.last, method="quick")
           , label=paste("bit_sort_unique(c(", paste(x, collapse=","), "), na.last=", na.last,", has.dup=TRUE)", sep="")
        )
        
        y <- unique(x)
        expect_identical(
            bit_sort_unique(y, na.last=na.last, has.dup = FALSE)
          , sort.int(y, na.last=na.last, method="quick")
          , label=paste("bit_sort_unique(c(", paste(x, collapse=","), "), na.last=", na.last,", has.dup=FALSE)", sep="")
        )
        
        expect_identical(
          bit_sort(x, na.last=na.last)
        , sort.int(x, na.last=na.last, method="quick")
        , label=paste("bit_sort(c(", paste(x, collapse=","), "), na.last=", na.last,")", sep="")
        )
      }

      expect_identical(  
        bit_unique(x, na.rm=NA)
      , unique(x, incomparables=FALSE)
      , label=paste("bit_unique(c(", paste(x, collapse=","), "), na.rm=", NA, ")", sep="")
      )
      expect_identical(  
        bit_unique(x, na.rm=FALSE)
      , unique(x, incomparables=NA)
      , label=paste("bit_unique(c(", paste(x, collapse=","), "), na.rm=", FALSE, ")", sep="")
      )
      expect_identical(  
        bit_unique(x, na.rm=TRUE)
      , unique(x[!is.na(x)])
      , label=paste("bit_unique(c(", paste(x, collapse=","), "), na.rm=", TRUE, ")", sep="")
      )

      expect_identical(  
        bit_duplicated(x, na.rm=NA)
      , as.bit(duplicated(x, incomparables=FALSE))
      , label=paste("bit_duplicated(c(", paste(x, collapse=","), "), na.rm=", NA, ")", sep="")
      )
      expect_identical(  
        bit_duplicated(x, na.rm=FALSE)
      , as.bit(duplicated(x, incomparables=NA))
      , label=paste("bit_duplicated(c(", paste(x, collapse=","), "), na.rm=", FALSE, ")", sep="")
      )
      expect_identical(  
        bit_duplicated(x, na.rm=TRUE)
      , as.bit(duplicated(x) | is.na(x))
      , label=paste("bit_duplicated(c(", paste(x, collapse=","), "), na.rm=", TRUE, ")", sep="")
      )
      
      expect_identical(
        bit_anyDuplicated(x, na.rm=NA)
      , anyDuplicated(x, incomparables=FALSE)
      , label=paste("bit_anyDuplicated(c(", paste(x, collapse=","), "), na.rm=", NA, ")", sep="")
        )
      expect_identical(
        bit_anyDuplicated(x, na.rm=FALSE)
      , anyDuplicated(x, incomparables=NA)
      , label=paste("bit_anyDuplicated(c(", paste(x, collapse=","), "), na.rm=", FALSE, ")", sep="")
      )
      expect_identical(
        bit_anyDuplicated(x, na.rm=TRUE)
      , max(0L, head(which(is.na(x)|duplicated(x)), 1))
      , label=paste("bit_anyDuplicated(c(", paste(x, collapse=","), "), na.rm=", TRUE, ")", sep="")
      )

      expect_identical(
        bit_sumDuplicated(x, na.rm=NA)
      , sum(duplicated(x, incomparables=FALSE))
      , label=paste("bit_sumDuplicated(c(", paste(x, collapse=","), "))", sep="")
      )
      expect_identical(
        bit_sumDuplicated(x, na.rm=FALSE)
      , sum(duplicated(x, incomparables=NA))
      , label=paste("bit_sumDuplicated(c(", paste(x, collapse=","), "))", sep="")
      )
      expect_identical(
        bit_sumDuplicated(x, na.rm=TRUE)
      , sum(duplicated(x, incomparables=NA)) + sum(is.na(x))
      , label=paste("bit_sumDuplicated(c(", paste(x, collapse=","), "))", sep="")
      )
      
    }
  }

})
  

test_that("bit_sort and bit_sort_unique are OK", {
    n <- 6L
    for (s in 1:10){
      set.seed(s)
      x <- 2L*sample(n, replace=TRUE)
      for (napos in c(-1L,0L,1L)){
        if (napos == -1){
          y <- c(NA,x)
        } else if (napos == 1){
          y <- c(x,NA)
        } else {
          y <- c(x[1:(n%/%2)],NA,x[(n%/%2+1L):n])
        }
        for (has.dup in c(TRUE,FALSE)){
          if (!has.dup)
            y <- unique(y)
        for (na.last in c(NA, FALSE, TRUE)){
          eval(
            substitute(
              expect_identical(
                bit_sort(y, na.last=na.last, has.dup = has.dup)
              , sort(y, na.last = na.last)
              )
              , list(
                  y=y
                , na.last=na.last
                , has.dup = has.dup
              )
            )
          )
         for (decreasing in c(FALSE, TRUE)){
              eval(
              substitute(
                expect_identical(
                    bit_sort_unique(y, decreasing=decreasing, na.last=na.last, has.dup = has.dup)
                  , sort(unique(y), decreasing=decreasing, na.last = na.last)
                )
                , list(
                    y=y
                  , decreasing=decreasing
                  , na.last=na.last
                  , has.dup = has.dup
                )
              )
            )
            range_na <- range_na(y)
            eval(
              substitute(
                expect_identical(
                  bit_sort_unique(y, decreasing=decreasing, na.last=na.last, has.dup = has.dup, range_na=range_na)
                  , sort(unique(y), decreasing=decreasing, na.last = na.last)
                )
                , list(
                  y=y
                  , decreasing=decreasing
                  , na.last=na.last
                  , has.dup = has.dup
                  , range_na = range_na
                )
              )
            )
          }
        }
      }
      }
    }
  })
