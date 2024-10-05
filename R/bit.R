# Functions for boolean vectors
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

# source("C:/mwp/eanalysis/bit/R/bit.R")

# Configuration: set this to 32L or 64L and keep in sync with BITS in bit.c
#' @rdname bit_init
#' @export
.BITS <- 32L

#' Initializing bit masks
#'
#' Functions to allocate (and de-allocate) bit masks
#'
#' The C-code operates with bit masks.  The memory for these is allocated
#' dynamically.  \code{bit_init} is called by \code{\link{.First.lib}} and
#' \code{bit_done} is called by \code{\link{.Last.lib}}.  You don't need to
#' care about these under normal circumstances.
#'
#' @return NULL
#' @author Jens Oehlschlägel
#' @seealso \code{\link{bit}}
#' @keywords classes logic
#' @examples
#'
#'   bit_done()
#'   bit_init()
#'
#' @export
bit_init <- function()
  .Call(C_R_bit_init, .BITS)

#' @rdname bit_init
#' @export
bit_done <- function()
  .Call(C_R_bit_done)




#' Create empty bit vector
#'
#' Bit vectors are a boolean type wihout \code{NA} that requires by factor 32 less RAM than \code{\link{logical}}.
#' For details on usage see the \href{../doc/bit-usage.html}{usage-vignette} and for details on performance see \href{../doc/bit-performance.html}{performance-vignette}
#'
#' @param length length in bits
#' @return \code{bit} returns a vector of integer sufficiently long to store 'length' bits
#' @seealso  \code{\link{booltype}}, \code{\link{bitwhich}}, \code{\link{logical}}
#' @keywords classes logic
#' @examples
#' bit(12)
#' !bit(12)
#' str(bit(128))
#' @export
bit <- function(length=0L){
  length <- as.integer(length)
  if (length %% .BITS)
    n <- length %/% .BITS + 1L
  else
    n <- length %/% .BITS
  if (.BITS==64L)
    x <- integer(2L*n)
  else
    x <- integer(n)
  #physical(x) <- list(vmode="boolean")
  #virtual(x)  <- list(Length=length)
  #class(x) <- "bit"
  # tuning
  p <- list()
  v <- list()
  attributes(p) <- list(vmode="boolean", class="physical")
  attributes(v) <- list(Length=length, class="virtual")
  setattributes(x, list(physical=p, virtual=v, class=c("booltype","bit")))
  x
}



#' Print method for bit
#'
#' @param x a bit vector
#' @param ... passed to print
#' @return a character vector showing first and last elements of the bit vector
#' @examples
#' print(bit(120))
#' @export
print.bit <- function(x, ...){
  n <- length(x)
  cat("bit length=", n, " occupying only ", length(unclass(x)), " int32\n", sep="")
  if (n>16){
    y <- c(x[1:8], "..", x[(n-7L):n])
    names(y) <- c(1:8, "", (n-7L):n)
    print(y, quote=FALSE, ...)
  }else if(n){
    y <- c(x[])
    names(y) <- c(1:n)
    print(y, quote=FALSE, ...)
  }
}

#' Coerce bit to character
#'
#' @param x a \code{\link{bit}} vector
#' @param ... ignored
#' @return a character vector of zeroes and ones
#' @examples
#' as.character(bit(12))
#' @export
as.character.bit <- function(x, ...){
  c("0","1")[1+as.logical(x)]
}


#' Str method for bit
#'
#' To actually view the internal structure use \code{str(unclass(bit))}
#'
#' @inheritParams utils::str
#' @return \code{\link{invisible}}
#' @importFrom utils strOptions
#' @examples
#' str(bit(120))
#' @export
str.bit <- function(object
, vec.len  = strO$vec.len
, give.head = TRUE
, give.length = give.head
, ...
)
  {
  strO <- strOptions()
  vec.len <- 8*vec.len
  n <- length(object)
  if (n>vec.len)
    object <- as.bit(object[seq_len(vec.len)])
  cat(if (give.head)paste("bit ", if (give.length && n>1) paste(" [1:",n,"] ",sep=""), sep=""), paste(as.character(object), collapse=" ")," \n", sep="")
  invisible()
}


#' Coerce bitwhich to character
#'
#' @param x a \code{\link{bitwhich}} vector
#' @param ... ignored
#' @return a character vector of zeroes and ones
#' @examples
#' as.character(bitwhich(12))
#' @export
as.character.bitwhich <- function(x, ...)c("0","1")[1+as.logical(x)]


#' Str method for bitwhich
#'
#' To actually view the internal structure use \code{str(unclass(bitwhich))}
#'
#' @inheritParams utils::str
#' @return \code{\link{invisible}}
#' @examples
#' str(bitwhich(120))
#' @export
str.bitwhich <- function(object
                    , vec.len  = strO$vec.len
                    , give.head = TRUE
                    , give.length = give.head
                    , ...
){
  strO <- strOptions()
  vec.len <- 8*vec.len
  n <- length(object)
  if (n>vec.len)
    object <- as.bitwhich(object[seq_len(vec.len)])
  cat(if (give.head)paste("bitwhich ", if (give.length && n>1) paste(" [1:",n,"] ",sep=""), sep=""), paste(as.character(object), collapse=" ")," \n", sep="")
  invisible()
}



#' Create bitwhich vector (skewed boolean)
#'
#' A bitwhich object represents a boolean filter like a \code{\link{bit}} object (NAs are not allowed)
#' but uses a sparse representation suitable for very skewed (asymmetric) selections.
#' Three extreme cases are represented with logical values, no length via logical(),
#' all TRUE with TRUE and all FALSE with FALSE. All other selections are represented with
#' positive or negative integers, whatever is shorter.
#' This needs less RAM compared to \code{\link{logical}} (and often less than \code{\link{bit}} or \code{\link[=as.which]{which}}).
#' Logical operations are fast if the selection is asymetric (only few or almost all selected).
#'
#' @param maxindex length of the vector
#' @param x Information about which positions are FALSE or TRUE: either \code{logical()} or \code{TRUE} or \code{FALSE} or a integer vector of positive or of negative subscripts.
#' @param xempty what to assume about parameter \code{x} if \code{x=integer(0)}, typically \code{TRUE} or \code{FALSE}.
#' @param poslength tuning: \code{poslength} is calculated automatically, you can give \code{poslength} explicitely, in this case it must be correct and \code{x} must be sorted and not have duplicates.
#' @param is.unsorted tuning: FALSE implies that \code{x} is already sorted and sorting is skipped
#' @param has.dup tuning: FALSE implies that \code{x} has no duplicates
#' @return an object of class 'bitwhich' carrying two attributes
#' \describe{
#'   \item{maxindex}{ see above }
#'   \item{poslength}{ see above }
#' }
#' @seealso \code{\link{bitwhich_representation}},  \code{\link{as.bitwhich}}, \code{\link{bit}}
#' @examples
#' bitwhich()
#' bitwhich(12)
#' bitwhich(12, x=TRUE)
#' bitwhich(12, x=3)
#' bitwhich(12, x=-3)
#' bitwhich(12, x=integer())
#' bitwhich(12, x=integer(), xempty=TRUE)
#' @export
bitwhich <- function(maxindex=0L, x=NULL, xempty=FALSE, poslength=NULL, is.unsorted=TRUE, has.dup=TRUE){
  maxindex <- as.integer(maxindex)
  if (maxindex==0L){
    if ((!is.null(poslength) && poslength) || (length(x) && (!is.logical(x) || x[[1]]==TRUE)))
      stop("maxindex=0 given with poslength or x")
    poslength <- 0L
    ret <- logical()
  }else{
    stopifnot(maxindex>0L)
    if (length(x)){
      if (is.logical(x)){
        if (length(x)!=1L || is.na(x)){
          stop("logical x should be scalar FALSE or TRUE")
        }else if (x){
          if (is.null(poslength))
            poslength <- maxindex
          else if (poslength!=maxindex)
            stop("x==TRUE implies poslength==maxindex")
          ret <- copy_vector(TRUE)
        }else{
          if (is.null(poslength))
            poslength <- 0L
          else if (poslength!=0L)
            stop("x==FALSE implies poslength==0")
          ret <- copy_vector(FALSE)
        }
      }else{
        x <- as.integer(x)
        if (is.null(poslength)){
          ret <- range_nanozero(x)
          r <- getsetattr(ret, "range_na", NULL)
          if (r[3]>0L)
            stop("NA positions not allowed (neither positive nor negative)")
          if (r[1]<0L){
            if (r[2]>0L)
              stop("mixed negative and positive subscripts not allowed")
            if (-r[1] > maxindex)
              stop("index value outside -maxindex..-1")
          }else{
            if (r[2] > maxindex)
              stop("index value outside 1..maxindex")
          }
          if (is.unsorted)
            ret <- bit_sort_unique(ret, na.last=NA, range_na=r)
          else if (has.dup)
            ret <- bit_unique(ret, na.rm = FALSE, range_na=r)
          if (ret[1]<0){
            poslength <- maxindex - length(ret)
            if (poslength){
              if (poslength <= maxindex%/%2L)
                ret <- merge_rangediff(c(1L,maxindex), ret, revx=FALSE, revy=TRUE)
            }else{
              ret <- copy_vector(FALSE)
            }
          }else{
            poslength <- length(ret)
            if (poslength < maxindex){
              if (poslength > maxindex%/%2L)
                ret <- merge_rangediff(c(1L,maxindex), ret, revx=TRUE, revy=TRUE)
            }else{
              ret <- copy_vector(TRUE)
            }
          }
        }else{
          poslength <- as.integer(poslength)
          if (poslength==0L)
            ret <- copy_vector(FALSE)
          else if (poslength==maxindex)
            ret <- copy_vector(TRUE)
          else{
            if (length(x) > 2 && x[1] >= x[2])
              stop("x is not sorted unique")
            if ( x[1]<0L ){
              if ( poslength != maxindex - length(x) )
                stop("wrong poslength")
              if (poslength <= maxindex%/%2L)
                ret <- merge_rangediff(c(1L,maxindex), x, revx=FALSE, revy=TRUE)
              else
                ret <- copy_vector(x)
            }else{
              if ( poslength != length(x) )
                stop("wrong poslength")
              if (poslength > maxindex%/%2L)
                ret <- merge_rangediff(c(1L,maxindex), x, revx=TRUE, revy=TRUE)
              else
                ret <- copy_vector(x)
            }

          }
        }
      }
    }else{
      if (is.null(poslength)){
          if (!is.logical(xempty) || length(xempty)!=1 || is.na(xempty))
            stop("xempty must be FALSE or TRUE")
          if (xempty)
            poslength <- maxindex
          else
            poslength <- 0L
          ret <- copy_vector(xempty)
      }else{
        poslength <- as.integer(poslength)
        if (poslength==0)
          ret <- copy_vector(FALSE)
        else if (poslength==maxindex)
          ret <- copy_vector(TRUE)
        else
          stop("need x with extreme poslength")
      }
    }
  }
  setattributes(ret, list("maxindex" = maxindex, "poslength" = poslength, "class" = c("booltype","bitwhich")))
  ret
}



#' Diagnose representation of bitwhich
#'
#' @param x a \code{\link{bitwhich}} object
#' @return a scalar, one of \code{logical()}, \code{FALSE}, \code{TRUE}, \code{-1} or \code{1}
#' @examples
#' bitwhich_representation(bitwhich())
#' bitwhich_representation(bitwhich(12,FALSE))
#' bitwhich_representation(bitwhich(12,TRUE))
#' bitwhich_representation(bitwhich(12, -3))
#' bitwhich_representation(bitwhich(12, 3))
#' @export
bitwhich_representation <- function(x)
{
  .Call(C_R_bitwhich_representation, x)
}



#' Print method for bitwhich
#'
#' @param x a \code{\link{bitwhich}} object
#' @param ... ignored
#' @export
print.bitwhich <- function(x, ...){
  n <- length(x)
  cat("bitwhich: ", sum(x), "/", n, " occupying only ", length(unclass(x)), " int32 in ", bitwhich_representation(x), " representation\n", sep="")
  if (n>16){
    y <- c(x[1:8], "..", x[(n-7L):n])
    names(y) <- c(1:8, "", (n-7L):n)
    print(y, quote=FALSE, ...)
  }else if(n){
    y <- c(x[])
    names(y) <- c(1:n)
    print(y, quote=FALSE, ...)
  }
}


#' Boolean types
#'
#' The \code{\link{ordered}} factor \code{booltypes} ranks the boolean types.
#'
#' There are currently six boolean types, \code{booltypes} is an \code{\link{ordered}} vector with the following ordinal \code{\link{levels}} \describe{
#' \item{nobool}{non-boolean types}
#' \item{\code{\link{logical}}}{for representing any boolean data including \code{NA} }
#' \item{\code{\link{bit}}}{for representing dense boolean data }
#' \item{\code{\link{bitwhich}}}{for representing sparse (skewed) boolean data  }
#' \item{\code{\link{which}}}{for representing sparse boolean data with few \code{TRUE}}
## \item{\code{\link[ff]{hi}}}{hybrid-indexing, implemented in package \code{\link[ff]{ff}} }
#' \item{\code{\link{ri}}}{range-indexing, for representing sparse boolean data with a single range of \code{TRUE} }
#' }
#' \code{booltypes} has a \code{\link{names}} attribute such that elements can be selected by name.
#'
#' @note do not rely on the internal integer codes of these levels, we might add-in \code{\link[ff]{hi}} later
#' @seealso \code{\link{booltype}}, \code{\link{is.booltype}}, \code{\link{as.booltype}}
#' @export
booltypes <- c("nobool","logical","bit","bitwhich","which","ri")
booltypes <- ordered(booltypes, levels=booltypes)
names(booltypes) <- booltypes


#' Diagnosing boolean types
#'
#' Specific methods for \code{booltype} are required, where non-unary methods can combine multiple bollean types, particularly boolean binary operators.
#'
#' Function \code{booltype} returns the boolean type of its argument.
#' There are currently six boolean types, \code{booltypes} is an \code{\link{ordered}} vector with the following ordinal \code{\link{levels}} \describe{
#' \item{nobool}{non-boolean types}
#' \item{\code{\link{logical}}}{for representing any boolean data including \code{NA} }
#' \item{\code{\link{bit}}}{for representing dense boolean data }
#' \item{\code{\link{bitwhich}}}{for representing sparse (skewed) boolean data  }
#' \item{\code{\link{which}}}{for representing sparse boolean data with few \code{TRUE}}
## \item{\code{\link[ff]{hi}}}{hybrid-indexing, implemented in package \code{\link[ff]{ff}} }
#' \item{\code{\link{ri}}}{range-indexing, for representing sparse boolean data with a single range of \code{TRUE} }
#' }
#' @param x an R object
#'
#' @return one scalar element of \code{\link{booltypes}} in case of 'nobool' it carries a name attribute with the data type.
#' @note do not rely on the internal integer codes of these levels, we might add-in \code{\link[ff]{hi}} later
#' @seealso \code{\link{booltypes}}, \code{\link{is.booltype}}, \code{\link{as.booltype}}
#'
#' @examples
#' unname(booltypes)
#' str(booltypes)
#' sapply(list(double(),integer(),logical(),bit(),bitwhich(),as.which(),ri(1,2,3)), booltype)
#' @export
booltype <- function(x){
  if (is.ri(x))
    booltypes[["ri"]]
  else if (is.hi(x))
    booltypes[["hi"]]
  else if (is.which(x))
    booltypes[["which"]]
  else if (is.bitwhich(x))
    booltypes[["bitwhich"]]
  else if (is.bit(x))
    booltypes[["bit"]]
  else if (is.logical(x))
    booltypes[["logical"]]
  else {
    ret <- booltypes[["nobool"]]
    names(ret) <- typeof(x)
    ret
  }
}




#' Testing for boolean types
#'
#' All \code{\link{booltypes}} including  \code{\link{logical}} except 'nobool' types are considered 'is.booltype'.
#'
#' @param x an R object
#'
#' @return logical scalar
#' @seealso \code{\link{booltypes}}, \code{\link{booltype}}, \code{\link{as.booltype}}
#'
#' @examples
#' sapply(list(double(),integer(),logical(),bit(),bitwhich(),as.which(),ri(1,2,3)), is.booltype)
#' @export
is.booltype <- function(x){
  inherits(x, "booltype") || is.logical(x)
}

#' @describeIn is.booltype tests for \code{\link{bit}}
#' @export
is.bit <- function(x)
  inherits(x, "bit")

#' @describeIn is.booltype tests for \code{\link{bitwhich}}
#' @export
is.bitwhich <- function(x)
  inherits(x, "bitwhich")

#' @describeIn is.booltype tests for \code{\link[=as.which]{which}}
#' @export
is.which <- function(x)
  inherits(x, "which")

#' @describeIn is.booltype tests for \code{\link[ff]{hi}}
#' @export
is.hi <- function(x)
  inherits(x, "hi")

#' @describeIn is.booltype tests for \code{\link{ri}}
#' @export
is.ri <- function(x)
  inherits(x, "ri")


#' @describeIn as.booltype default method for as.booltype
#' @export
as.booltype.default <- function(x, booltype="logical", ...){
  bt <- match.arg(as.character(booltype), as.character(booltypes))
  do.call(switch(bt
    , logical = "as.logical"
    , bit     = "as.bit"
    , bitwhich= "as.bitwhich"
    , which= "as.which"
    , hi= stop("not implemented for booltype hi")
    , ri= "as.ri"
    ), c(list(x, ...)))
}


#' @describeIn as.ri method to coerce \code{\link{ri}} to \code{\link{ri}}
#' @export
as.ri.ri <- function(x, ...)x

#' @describeIn as.ri default method to coerce to \code{\link{ri}}
#' @export
as.ri.default <- function(x, ...){
  r <- range.booltype(x)
  n <- maxindex(x)
  ri(r[[1]], r[[2]], n)
}



#' @describeIn maxindex default method for \code{maxindex}
#' @export
maxindex.default <-
  function (x, ...)
  {
    mi <- attr(x, "maxindex")
    if (is.null(mi))
      NA_integer_
    else mi
  }

#' @describeIn maxindex default method for \code{poslength}
#' @export
poslength.default <-
  function (x, ...)
  {
    pl <- attr(x, "poslength")
    if (is.null(pl))
      NA_integer_
    else pl
  }


#' @describeIn maxindex \code{maxindex} method for class \code{\link{logical}}
#' @export
maxindex.logical <- function(x, ...){
  length(x)
}

#' @describeIn maxindex \code{poslength} method for class \code{\link{logical}}
#' @export
poslength.logical <- function(x, ...){
  sum(x)
}

#' @describeIn maxindex \code{maxindex} method for class \code{\link{bit}}
#' @export
maxindex.bit <- function(x, ...){
  length(x)
}

#' @describeIn maxindex \code{poslength} method for class \code{\link{bit}}
#' @export
poslength.bit <- function(x, ...)
  sum(x, ...)

#' @describeIn maxindex \code{maxindex} method for class \code{\link{bitwhich}}
#' @export
maxindex.bitwhich <- function(x, ...)
  length(x, ...)

#' @describeIn maxindex \code{poslength} method for class \code{\link{bitwhich}}
#' @export
poslength.bitwhich <- function(x, ...)
  sum(x, ...)

#' @describeIn maxindex \code{maxindex} method for class \code{\link[=as.which]{which}}
#' @export
maxindex.which <- function(x, ...){
  attr(x, "maxindex")
}

#' @describeIn maxindex \code{poslength} method for class \code{\link[=as.which]{which}}
#' @export
poslength.which <- function(x, ...){
  length(x)
}

#' @describeIn maxindex \code{maxindex} method for class \code{\link{ri}}
#' @export
maxindex.ri <- function(x, ...){
  x[[3]]
}

#' @describeIn maxindex \code{poslength} method for class \code{\link{ri}}
#' @export
poslength.ri <- function(x, ...){
  x[[2]] - x[[1]] + 1L
}



#' Getting and setting length of bit, bitwhich and ri objects
#'
#' Query the number of bits in a \code{\link{bit}} vector or change the number
#' of bits in a bit vector. \cr Query the number of bits in a
#' \code{\link{bitwhich}} vector or change the number of bits in a bit vector.
#' \cr
#'
#' NOTE that the length does NOT reflect the number of selected (\code{TRUE})
#' bits, it reflects the sum of both, \code{TRUE} and \code{FALSE} bits.
#' Increasing the length of a \code{\link{bit}} object will set new bits to
#' \code{FALSE}.  The behaviour of increasing the length of a
#' \code{\link{bitwhich}} object is different and depends on the content of the
#' object: \itemize{
#' \item TRUE -- all included, new bits are set to \code{TRUE}
#' \item positive integers -- some included, new bits are set to \code{FALSE}
#' \item negative integers -- some excluded, new bits are set to \code{TRUE}
#' \item FALSE -- all excluded:, new bits are set to \code{FALSE} } Decreasing the
#' length of bit or bitwhich removes any previous information about the status
#' bits above the new length.
#'
#' @name length.bit
#' @param x a \code{\link{bit}}, \code{\link{bitwhich}} or \code{\link{ri}}
#' object
#' @param value the new number of bits
#' @return the length A bit vector with the new length
#' @author Jens Oehlschlägel
#' @seealso \code{\link{length}}, \code{\link[=sum.bit]{sum}},
#' \code{\link{poslength}}, \code{\link{maxindex}}
#' @keywords classes logic
#' @examples
#'
#'   stopifnot(length(ri(1, 1, 32))==32)
#'
#'   x <- as.bit(ri(32, 32, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==0)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==0)
#'
#'   x <- as.bit(ri(1, 1, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'
#'   x <- as.bitwhich(bit(32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==0)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==0)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==0)
#'
#'   x <- as.bitwhich(!bit(32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==32)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==16)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==32)
#'
#'   x <- as.bitwhich(ri(32, 32, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==0)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==0)
#'
#'   x <- as.bitwhich(ri(2, 32, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==31)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==15)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==31)
#'
#'   x <- as.bitwhich(ri(1, 1, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==1)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==1)
#'
#'   x <- as.bitwhich(ri(1, 31, 32))
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==31)
#'   message("NOTE the change from 'some excluded' to 'all excluded' here")
#'   length(x) <- 16
#'   stopifnot(length(x)==16)
#'   stopifnot(sum(x)==16)
#'   length(x) <- 32
#'   stopifnot(length(x)==32)
#'   stopifnot(sum(x)==32)
#'
#' @export
length.bit <- function(x)
  virtual(x)$Length

#' @rdname length.bit
#' @export
"length<-.bit" <- function(x, value){
  value <- as.integer(value)
  vattr <- attr(x, "virtual")
  oldvalue <- attr(vattr, "Length")
  if (value!=oldvalue){
    pattr <- attr(x, "physical")
    cl <- oldClass(x)
    oldn <- get_length(x)
    dn <- value %% .BITS
    if (dn){
      n <- value %/% .BITS + 1L
    }else{
      n <- value %/% .BITS
    }
    if (oldn<n){
      ret <- integer(n)
      ret[seq_len(oldn)] <- x
    }else if (n<oldn){
      ret <- unclass(x)[seq_len(n)]
    }else{
      ret <- copy_vector(x)
    }
    if (dn && value<oldvalue){
      .Call(C_R_bit_set_logical, ret, FALSE, c(value+1L, n*.BITS))
    }
    attr(vattr, "Length") <- value
    setattributes(ret, list("physical" = pattr, "virtual" = vattr, "class" = cl))
    ret
  }else
    x
}


#' @rdname length.bit
#' @export
length.bitwhich <- function(x)
  attr(x, "maxindex")

#' @rdname length.bit
#' @export
"length<-.bitwhich" <- function(x, value){
  if (value!=length(x)){
    value <- as.integer(value)
    a <- attributes(x)
    if (value){
      if (is.integer(x)){
          oldClass(x) <- NULL
          if (x[1]>0){
            ret <- x[x <= value]
            l <- length(ret)
            if (l==0)
              ret <- copy_vector(FALSE)
            else if (l==value)
              ret <- copy_vector(TRUE)
            else if (l>(value%/%2L))
              ret <- merge_rangediff(c(-value,-1L), ret, revy=TRUE)
          }else{
            ret <- x[x >= -value]
            l <- length(ret)
            if (l==0)
              ret <- copy_vector(TRUE)
            else if (l==value)
              ret <- copy_vector(FALSE)
            else if (!((value-l)>(value%/%2L)))
              ret <- merge_rangediff(c(1L,value), ret, revy=TRUE)
            l <- value - l
          }
        }else{
          if (length(x) && x){
            ret <- bitwhich(value, x=TRUE, poslength=value)
            l <- value
          }else{
            ret <- bitwhich(value, x=FALSE, poslength=0L)
            l <- 0L
          }
        }
    }else{
      ret <- bitwhich()
      l <- 0L
    }
  }
  a$maxindex <- value
  a$poslength <- l
  setattributes(ret, a)
  ret
}



#' Concatenating booltype vectors
#'
#' Creating new boolean vectors by concatenating boolean vectors
#'
#' @param \dots \code{\link{booltype}} vectors
#' @return a vector with the lowest input \code{\link{booltype}} (but not lower than\code{\link{logical}})
#' @author Jens Oehlschlägel
#' @seealso \code{\link{c}}, \code{\link{bit}} , \code{\link{bitwhich}},  , \code{\link{which}}
#' @keywords classes logic
#' @examples
#'  c(bit(4), !bit(4))
#'  c(bit(4), !bitwhich(4))
#'  c(bitwhich(4), !bit(4))
#'  c(ri(1,2,4), !bit(4))
#'  c(bit(4), !logical(4))
#'  message("logical in first argument does not dispatch: c(logical(4), bit(4))")
#'  c.booltype(logical(4), !bit(4))
#'
#' @export c.booltype
#' @export
c.booltype <- function(...){
  l <- list(...)
  bt <- sapply(l, booltype)
  # xx TEMPORARY WORKAROND: work around a bug in sapply which destroys ordered levels
  class(bt) <- c("ordered", "factor")
  bt <- max(booltypes[["logical"]], min(bt, booltypes[["bitwhich"]]))
  bt <- as.character(bt)
  f <- list(logical=as.logical, bit=as.bit, bitwhich=as.bitwhich, which=as.which)[[bt]]
  l <- lapply(l, f)
  do.call(switch(bt
 , logical="c"
 , bit="c.bit"
 , bitwhich="c.bitwhich"
  ),  l)
}

#' @rdname c.booltype
#' @export
c.bit <- function(...){
  l <- list(...)
  nl <- length(l)
  nold <- sapply(l, length)
  nnew <- sum(nold)
  ncum <- cumsum(nold)
  offsets <- c(0L, ncum[-length(ncum)])
  x <- bit(nnew)
  for (i in seq_len(nl)){
    b <- as.bit(l[[i]])
    .Call(C_R_bit_shiftcopy, bsource_=b, btarget_=x, otarget_=offsets[i], n_=nold[i])
  }
  x
}

#' @rdname c.booltype
#' @export
c.bitwhich <- function(...){
  l <- list(...)
  if (length(l)==1)
    l[[1]]
  else
    as.bitwhich(do.call("c", lapply(l, as.bit)))
}


#' Reversing bit and bitwhich vectors
#'
#' Creating new bit or bitwhich by reversing such vectors
#'
#' @name rev.booltype
#' @param x bit or bitwhich object
#' @return An object of class 'bit' or 'bitwhich'
#' @author Jens Oehlschlägel
#' @seealso \code{\link{rev}}, \code{\link{bit}} , \code{\link{bitwhich}}
#' @keywords classes logic
#' @examples
#'
#'  rev(as.bit(c(FALSE,TRUE)))
#'  rev(as.bitwhich(c(FALSE,TRUE)))
NULL

#' @rdname rev.booltype
#' @export
rev.bit <- function(x){
  if (length(x)){
    x <- .Call(C_R_bit_reverse, x, bit(length(x)))
  }
  x
}

#' @rdname rev.booltype
#' @export
rev.bitwhich <- function(x){
  n <- length(x)
  if (is.logical(x)){
    ret <- bitwhich(n, copy_vector(x), poslength=sum(x))
  }else{
    y <- bitwhich_representation(x)
    if (n < .Machine$integer.max){
      if (y[1]<0)
        ret <- bitwhich(n, -(n+1L)-reverse_vector(x), poslength=sum(x))
      else
        ret <- bitwhich(n, (n+1L)-reverse_vector(x), poslength=sum(x))
    }else{
      if (y[1]<0)
        ret <- bitwhich(n, -n-reverse_vector(x)-1L, poslength=sum(x))
      else
        ret <- bitwhich(n, n-reverse_vector(x)+1L, poslength=sum(x))
    }
  }
  ret
}


#' Replicating bit and bitwhich vectors
#'
#' Creating new bit or bitwhich by recycling such vectors
#'
#' @name rep.booltype
#' @param x bit or bitwhich object
#' @param times number of replications
#' @param length.out final length of replicated vector (dominates times)
#' @param \dots not used
#' @return An object of class 'bit' or 'bitwhich'
#' @author Jens Oehlschlägel
#' @seealso \code{\link{rep}}, \code{\link{bit}} , \code{\link{bitwhich}}
#' @keywords classes logic
#' @examples
#'
#'  rep(as.bit(c(FALSE,TRUE)), 2)
#'  rep(as.bit(c(FALSE,TRUE)), length.out=7)
#'  rep(as.bitwhich(c(FALSE,TRUE)), 2)
#'  rep(as.bitwhich(c(FALSE,TRUE)), length.out=1)
NULL

#' @rdname rep.booltype
#' @export
rep.bit <- function(x, times = 1L, length.out = NA, ...){
  if (length(times)>1L)
    stop("only scalar times supported")
  if (is.na(length.out))
    length.out <- length(x)*as.integer(times)
  else
    length.out <- as.integer(length.out)
  ret <- bit(length.out)
  .Call(C_R_bit_recycle, ret, x)
}

#' @rdname rep.booltype
#' @export
rep.bitwhich <- function(x, times = 1L, length.out = NA, ...){
  as.bitwhich(rep(as.bit(x), times=times, length.out=as.integer(length.out), ...))
}


#' @describeIn as.bit method to coerce to \code{\link{bit}} (zero length) from \code{\link{NULL}}
#' @export
as.bit.NULL <- function(x, ...){
  bit(0L)
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from \code{\link{bit}}
#' @export
as.bit.bit <- function(x, ...)
  x

#' @describeIn as.bit method to coerce to \code{\link{bit}} from \code{\link{logical}}
#' @export
as.bit.logical <- function(x, ...){
  n <- length(x)
  b <- bit(n)
  .Call(C_R_bit_set_logical, b, x, c(1L, n))
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from
#'   \code{\link{integer}} (\code{0L} and \code{NA} become \code{FALSE},
#'   everthing else becomes \code{TRUE})
#' @examples as.bit(c(0L,1L,2L,-2L,NA))
#' @export
as.bit.integer <- function(x, ...){
  n <- length(x)
  b <- bit(n)
  .Call(C_R_bit_set_integer, b, x, c(1L, n))
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from
#'   \code{\link{double}} (\code{0} and \code{NA} become \code{FALSE}, everthing
#'   else becomes \code{TRUE})
#' @examples as.bit(c(0,1,2,-2,NA))
#' @export
as.bit.double <- function(x, ...){
  n <- length(x)
  b <- bit(n)
  .Call(C_R_bit_set_integer, b, as.integer(x), c(1L, n))
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from \code{\link{bitwhich}}
#' @export
as.bit.bitwhich <- function(x, ...){
  if (length(x)){
    b <- bit(length(x))
    if (is.logical(x)){
      if (unclass(x))
        !b
      else
        b
    }else{
      .Call(C_R_bit_replace, b, x, TRUE)
    }
  }else{
    bit()
  }
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from \code{\link[=as.which]{which}}
#' @export
as.bit.which <- function(x, length=attr(x, "maxindex"), ...){
  if (is.na(length))
    stop("cannot coerce to bit from which object with unknown maxindex")
  b <- bit(length)
  .Call(C_R_bit_replace, b, x, TRUE)
}

#' @describeIn as.bit method to coerce to \code{\link{bit}} from \code{\link{ri}}
#' @export
as.bit.ri <- function(x, ...){
  if (is.na(x[3]))
    stop("cannot coerce to bit from ri object with unknown maxindex")
  b <- bit(x[3])
  .Call(C_R_bit_set_logical, b, TRUE, x)
}



#' Coercion from bit, bitwhich, which and ri to logical, integer, double
#'
#' Coercion from bit is quite fast because we use a double loop that fixes each
#' word in a processor register.
#'
#' @name CoercionToStandard
#' @param x an object of class \code{\link{bit}}, \code{\link{bitwhich}} or
#' \code{\link{ri}}
#' @param length length of the boolean vector (required for \code{as.logical.which})
#' @param \dots ignored
#' @return \code{\link{as.logical}} returns a vector of \code{FALSE, TRUE},
#' \code{\link{as.integer}} and \code{\link{as.double}} return a vector of
#' \code{0,1}.
#' @author Jens Oehlschlägel
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
#' , \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
#' @keywords classes logic
#' @examples
#'
#'   x <- ri(2, 5, 10)
#'   y <- as.logical(x)
#'   y
#'   stopifnot(identical(y, as.logical(as.bit(x))))
#'   stopifnot(identical(y, as.logical(as.bitwhich(x))))
#'
#'   y <- as.integer(x)
#'   y
#'   stopifnot(identical(y, as.integer(as.logical(x))))
#'   stopifnot(identical(y, as.integer(as.bit(x))))
#'   stopifnot(identical(y, as.integer(as.bitwhich(x))))
#'
#'   y <- as.double(x)
#'   y
#'   stopifnot(identical(y, as.double(as.logical(x))))
#'   stopifnot(identical(y, as.double(as.bit(x))))
#'   stopifnot(identical(y, as.double(as.bitwhich(x))))
NULL

#' @rdname CoercionToStandard
#' @export
as.logical.bit <- function(x, ...){
  l <- logical(length(x))
  .Call(C_R_bit_get_logical, x, l, c(1L, length(x)))
}

#' @rdname CoercionToStandard
#' @export
as.integer.bit <- function(x, ...){
  l <- integer(length(x))
  .Call(C_R_bit_get_integer, x, l, c(1L, length(x)))
}

#' @rdname CoercionToStandard
#' @export
as.double.bit <- function(x, ...){
  l <- integer(length(x))
  as.double(.Call(C_R_bit_get_integer, x, l, c(1L, length(x))))
}

#' @rdname CoercionToStandard
#' @export
as.integer.bitwhich <- function(x, ...){
  n <- length(x)
  if (is.logical(x)){
    if (sum(x)==n)
      rep(1L, n)
    else
      rep(0L, n)
  }else{
    ret <- integer(n)
    ret[x] <- 1L
    ret
  }
}

#' @rdname CoercionToStandard
#' @export
as.double.bitwhich <- function(x, ...){
  n <- length(x)
  if (is.logical(x)){
    if (sum(x)==n)
      rep(1, n)
    else
      rep(0, n)
  }else{
    ret <- double(n)
    ret[x] <- 1
    ret
  }
}


#' @rdname CoercionToStandard
#' @export
as.logical.bitwhich <- function(x, ...){
  n <- length(x)
  p <- sum(x)
  if (p==0){
    rep(FALSE, length(x))
  }else if (p==n){
    rep(TRUE, length(x))
  }else{
    ret <- logical(length(x))
    ret[x] <- TRUE
    ret
  }
}

#' @rdname CoercionToStandard
#' @export
as.logical.ri <- function(x, ...){
  if (is.na(x[3]))
    stop("cannot coerce to logical from ri object with unknown maxindex")
  ret <- logical(x[3])
  ret[x[1]:x[2]] <- TRUE
  ret
}

#' @rdname CoercionToStandard
#' @export
as.integer.ri <- function(x, ...){
  if (is.na(x[3]))
    stop("cannot coerce to integer from ri object with unknown maxindex")
  ret <- integer(x[3])
  ret[x[1]:x[2]] <- 1L
  ret
}

#' @rdname CoercionToStandard
#' @export
as.double.ri <- function(x, ...){
  if (is.na(x[3]))
    stop("cannot coerce to integer from ri object with unknown maxindex")
  ret <- double(x[3])
  ret[x[1]:x[2]] <- 1
  ret
}


#' @rdname CoercionToStandard
#' @export
as.logical.which <- function(x, length=attr(x, "maxindex"), ...){
  if (is.na(length))
    stop("cannot coerce to logical from which object with unknown maxindex")
  l <- logical(length)
  l[x] <- TRUE
  l
}


#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link[=as.which]{which}}
#' @export
as.which.which <- function(x, maxindex=NA_integer_, ...)x

#' @describeIn as.which method to coerce to zero length \code{\link[=as.which]{which}} from \code{\link{NULL}}
#' @export
as.which.NULL <- function(x, ...)structure(integer(), maxindex=0L, class=c("booltype", "which"))

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{numeric}}
#' @export
as.which.numeric <- function(x, maxindex=NA_integer_, ...){
  as.which(as.integer(x), maxindex=maxindex, ...)
}

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{integer}}
#' @export
as.which.integer <- function(x, maxindex=NA_integer_, is.unsorted=TRUE, has.dup=TRUE, ...){
  ret <- range_nanozero(as.integer(x))
  r <- getsetattr(ret, "range_na", NULL)
  if (length(ret)){
    if (r[3]>0L)
      stop("NA positions not allowed (neither positive nor negative)")
    if (r[1]<0L){
      if (r[2]>0L)
        stop("mixed negative and positive subscripts not allowed")
      if (is.na(maxindex))
        stop("need maxindex with negative subscripts")
      else if (-r[1] > maxindex)
        stop("index value outside -maxindex..-1")
    }else{
      if (!is.na(maxindex) && r[2] > maxindex)
        stop("index value outside 1..maxindex")
    }
    if (is.unsorted)
      ret <- bit_sort_unique(ret, na.last=NA, range_na=r, has.dup=has.dup)
    else if (has.dup)
      ret <- bit_unique(ret, na.rm = FALSE, range_na=r)
    if (r[1]<0L)
      ret <- merge_rangediff(c(1L,maxindex), ret, revx=FALSE, revy=TRUE)
  }
  setattributes(ret, list("maxindex" = maxindex, "class" = c("booltype", "which")))
  ret
}

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{logical}}
#' @export
as.which.logical <- function(x, ...){
  ret <- which(x)
  setattributes(ret, list("maxindex" = as.integer(length(x)), "class" = c("booltype", "which")))
  ret
}

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{ri}}
#' @export
as.which.ri <- function(x, ...){
  ret <- x[1]:x[2]
  setattributes(ret, list("maxindex" = as.integer(x[3]), "class" = c("booltype", "which")))
  ret
}

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{bit}}
#' @export
as.which.bit <- function(x, range=NULL, ...){
  maxindex <- length(x)
  if (is.null(range))
    range <- c(1L, maxindex)
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>maxindex)
      stop("illegal range")
  }
  s <- sum(x, range=range)
  n <- range[2] - range[1] + 1L
  if (s==0L){
    ret <- integer()
  }else if (s==n){
    #ret <- as.integer(seq.int(from=range[1], to=range[2], by=1))
    ret <- merge_rangediff(range, integer())
  }else
    ret <- .Call(C_R_bit_which, x, s, range, negative=FALSE)
  setattributes(ret, list("maxindex" = as.integer(maxindex), "class" = c("booltype", "which")))
  ret
}

#' @describeIn as.which method to coerce to \code{\link[=as.which]{which}} from \code{\link{bitwhich}}
#' @export
as.which.bitwhich <- function(x, ...){
  maxindex <- length(x)
  if (is.logical(x)){
    if (maxindex && unclass(x))
      ret <- seq_len(maxindex)
    else
      ret <- integer()
  }else{
    if (unclass(x)[[1]]<0)
      ret <- merge_rangediff(c(1L,maxindex), x, revx=FALSE, revy=TRUE)
    else
      ret <- copy_vector(x)
  }
  setattributes(ret, list("maxindex" = as.integer(maxindex), "class" = c("booltype", "which")))
  ret
}


#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} (zero length) from \code{\link{NULL}}
#' @export
as.bitwhich.NULL <- function(x, ...){
  bitwhich()
}

#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from \code{\link{bitwhich}}
#' @export
as.bitwhich.bitwhich <- function(x, ...){
  x
}

#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from \code{\link[=as.which]{which}}
#' @export
as.bitwhich.which <- function(x, maxindex=attr(x, "maxindex"), ...){
  if (is.na(maxindex))
    stop("need maxindex")
  if (maxindex==0)
    bitwhich()
  else{
    poslength <- length(x)
    if (poslength==0)
      bitwhich(maxindex, FALSE, poslength)
    else if (poslength==maxindex)
      bitwhich(maxindex, TRUE, poslength)
    else if (poslength>(maxindex%/%2L)){
      bitwhich(maxindex, merge_rangediff(c(1L,maxindex), x, revx=TRUE, revy=TRUE), poslength=poslength)
    }else{
      bitwhich(maxindex, x, poslength=poslength)
    }
  }
}

#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from \code{\link{ri}}
#' @export
as.bitwhich.ri <- function(x, ...){
  maxindex <- length(x)
  if (is.na(maxindex))
    stop("you must provide maxindex with ri() in as.bitwhich.ri()")
  # ri selects at least one element,
  # hence maxindex>0 and poslength>0
  poslength <- sum(x)
  if (poslength==maxindex)
    bitwhich(maxindex, TRUE, poslength=poslength)
  else if (poslength>(maxindex%/%2L)){
    if (x[1]>1L) b <- (-x[1]+1L):(-1) else b <- integer()
    if (x[2]<maxindex) a <- (-maxindex):(-x[2]-1L) else a <- integer()
    bitwhich(maxindex, c(a,b), poslength=poslength)
  }else{
    bitwhich(maxindex, x[1]:x[2], poslength=poslength)
  }
}



#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from
#'   \code{\link{integer}} (\code{0} and \code{NA} become \code{FALSE}, everthing
#'   else becomes \code{TRUE})
#' @examples as.bitwhich(c(0L,1L,2L,-2L,NA))
#' @export
as.bitwhich.integer <- function(x, poslength=NULL, ...)
  as.bitwhich(as.logical(x), poslength=poslength, ...)


#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from
#'   \code{\link{double}} (\code{0} and \code{NA} become \code{FALSE}, everthing
#'   else becomes \code{TRUE})
#' @examples as.bitwhich(c(0,1,2,-2,NA))
#' @export
as.bitwhich.double <- as.bitwhich.integer


#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from \code{\link{logical}}
#' @export
as.bitwhich.logical <- function(x, poslength=NULL, ...){
  maxindex <- length(x)
  if (maxindex==0)
    bitwhich()
  else{
    if (is.null(poslength))
      poslength <- sum(x, na.rm=TRUE)
    else
      if(poslength>maxindex)
        stop("poslength > maxindex")
    if (poslength==0)
      bitwhich(maxindex, FALSE, poslength=poslength)
    else if (poslength==maxindex)
      bitwhich(maxindex, TRUE, poslength=poslength)
    else if (poslength>(maxindex%/%2L)){
      as.bitwhich(as.bit(x), poslength=poslength)
    }else{
      bitwhich(maxindex, which(x), poslength=poslength)
    }
  }
}


#' @describeIn as.bitwhich method to coerce to \code{\link{bitwhich}} from \code{\link{bit}}
#' @export
as.bitwhich.bit <- function(x, range=NULL, poslength=NULL, ...){
  maxindex <- length(x)
  if (maxindex){
    if (is.null(range))
      range <- c(1L, maxindex)
    else{
      range <- as.integer(range[1:2])
      if (range[1]<1L || range[2]>maxindex)
        stop("illegal range")
    }
    if (is.null(poslength))
      poslength <- sum(x, range=range, na.rm=TRUE)
    else
      if(poslength>maxindex)
        stop("poslength > maxindex")
    if (poslength==0)
      bitwhich(maxindex, FALSE, poslength=poslength)
    else if (poslength==maxindex)
      bitwhich(maxindex, TRUE, poslength=poslength)
    else{
      if (poslength>(maxindex%/%2L)){
        bitwhich(maxindex, .Call(C_R_bit_which, x, maxindex - poslength, range=range, negative=TRUE), poslength=poslength)
      }else{
        bitwhich(maxindex, .Call(C_R_bit_which, x, poslength, range=range, negative=FALSE), poslength=poslength)
      }
    }
  }else bitwhich()
}


#' Test for NA in bit and bitwhich
#'
#' @name is.na.bit
#' @param x a \code{\link{bit}} or  \code{\link{bitwhich}} vector
#'
#' @return vector of same type with all elements \code{FALSE}
#' @seealso \code{\link{is.na}}
#'
#' @examples
#' is.na(bit(6))
#' is.na(bitwhich(6))
is.na.bit <- function(x)bit(length(x))

#' @describeIn is.na.bit method for \code{\link{is.na}} from \code{\link{bitwhich}}
#' @export
is.na.bitwhich <- function(x)bitwhich(length(x))


#' @describeIn xor default method for \code{\link{xor}}
#' @export
xor.default <- function (x, y)
{
  cat("default\n")
  (x | y) & !(x & y)
}

#' @describeIn xor \code{\link{logical}} method for \code{\link{xor}}
#' @export
xor.logical <- function(x,y){
    as.logical(x) != as.logical(y)
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{!}}
#' @export
"!.bit" <- function(x){
  if (length(x)){
    ret <- copy_vector(x)
    setattributes(ret, attributes(x))
    .Call(C_R_bit_not, ret)
  }else{
    x
  }
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{&}}
#' @export
"&.bit" <- function(e1, e2){
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bit())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(n[1])
  .Call(C_R_bit_and, e1, e2, ret)
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{|}}
#' @export
"|.bit" <- function(e1, e2){
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bit())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(n[1])
  .Call(C_R_bit_or, e1, e2, ret)
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{==}}
#' @export
"==.bit" <- function(e1, e2){
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bit())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(n[1])
  .Call(C_R_bit_equal, e1, e2, ret)
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{!=}}
#' @export
"!=.bit" <- function(e1, e2){
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bit())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  e1 <- as.bit(e1)
  e2 <- as.bit(e2)
  ret <- bit(n[1])
  .Call(C_R_bit_xor, e1, e2, ret)
}

#' @describeIn xor \code{\link{bit}} method for \code{\link{xor}}
#' @export
"xor.bit" <- function(x, y){
  n <- c(length(x), length(y))
  if (any(n==0L))
    return(bit())
  if(n[1]!=n[2])
    stop("length(x) != length()")
  x <- as.bit(x)
  y <- as.bit(y)
  ret <- bit(n[1])
  .Call(C_R_bit_xor, x, y, ret)
}


#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{!}}
#' @export
"!.bitwhich" <- function(x){
  n <- length(x)
  p <- sum(x)
  if (is.logical(x)){
    if (n==0)
      bitwhich()
    else if (p==n){
      bitwhich(maxindex=n, FALSE, poslength=0L)
    }else{
      bitwhich(maxindex=n, TRUE, poslength=n)
    }
  }else{
    #bitwhich(maxindex=n, -rev(unclass(x)), poslength=n-p, is.unsorted = FALSE, has.dup=FALSE)
    bitwhich(maxindex=n, copy_vector(x, revx=TRUE), poslength=n-p, is.unsorted = FALSE, has.dup=FALSE)
  }
}

#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{&}}
#' @export
"&.bitwhich" <- function(e1, e2){
  e1 <- as.bitwhich(e1)
  e2 <- as.bitwhich(e2)
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bitwhich())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  p <- c(sum(e1), sum(e2))
  if (p[1]==0 || p[2]==0)
    return(bitwhich(n[1], FALSE, 0L))
  if (p[1]==n[1])
    return(e2)
  if (p[2]==n[2])
    return(e1)
  #negative <- p>(n%/%2L)
  negative <- c(unclass(e1)[1]<0, unclass(e2)[1]<0)
  if (negative[1]){
    if (negative[2]){
      ret <- merge_union(e1, e2, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }else{
      ret <- merge_setdiff(e2, e1, revy=TRUE, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }
  }else{
    if (negative[2]){
      ret <- merge_setdiff(e1, e2, revy=TRUE, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }else{
      ret <- merge_intersect(e1, e2, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }
  }
}


#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{|}}
#' @export
"|.bitwhich" <- function(e1, e2){
  e1 <- as.bitwhich(e1)
  e2 <- as.bitwhich(e2)
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bitwhich())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  p <- c(sum(e1), sum(e2))
  if (p[1]==n[1] || p[2]==n[2])
    return(bitwhich(n[1], TRUE, n[1]))
  if (p[1]==0)
    return(e2)
  if (p[2]==0)
    return(e1)
  #negative <- p>(n%/%2L)
  negative <- c(unclass(e1)[1]<0, unclass(e2)[1]<0)
  if (negative[1]){
    if (negative[2]){
      ret <- merge_intersect(e1, e2, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }else{
      ret <- merge_setdiff(e1, e2, revy=TRUE, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }
  }else{
    if (negative[2]){
      ret <- merge_setdiff(e2, e1, revy=TRUE, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }else{
      ret <- merge_union(e1, e2, method="exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }
  }
}

#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{==}}
#' @export
"==.bitwhich" <- function(e1, e2){
  e1 <- as.bitwhich(e1)
  e2 <- as.bitwhich(e2)
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bitwhich())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  p <- c(sum(e1), sum(e2))
  if (p[1]==0)
    return(!e2)
  if (p[1]==n[1])
    return(e2)
  if (p[2]==0)
    return(!e1)
  if (p[2]==n[2])
    return(e1)
  #negative <- p>(n%/%2L)
  negative <- c(unclass(e1)[1]<0, unclass(e2)[1]<0)
  if (negative[1]){
    if (negative[2]){
      ret <- merge_symdiff(e1, e2, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }else{
      ret <- merge_symdiff(e1, e2, revx=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }
  }else{
    if (negative[2]){
      ret <- merge_symdiff(e1, e2, revy=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }else{
      ret <- merge_symdiff(e1, e2, revx=TRUE, revy=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }
  }
}

#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{!=}}
#' @export
"!=.bitwhich" <- function(e1, e2){
  e1 <- as.bitwhich(e1)
  e2 <- as.bitwhich(e2)
  n <- c(length(e1), length(e2))
  if (any(n==0L))
    return(bitwhich())
  if(n[1]!=n[2])
    stop("length(e1) != length(e2)")
  p <- c(sum(e1), sum(e2))
  if (p[1]==0)
    return(e2)
  if (p[1]==n[1])
    return(!e2)
  if (p[2]==0)
    return(e1)
  if (p[2]==n[2])
    return(!e1)
  #negative <- p>(n%/%2L)
  negative <- c(unclass(e1)[1]<0, unclass(e2)[1]<0)
  if (negative[1]){
    if (negative[2]){
      ret <- merge_symdiff(e1, e2, revx=TRUE, revy=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }else{
      ret <- merge_symdiff(e1, e2, revy=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }
  }else{
    if (negative[2]){
      ret <- merge_symdiff(e1, e2, revx=TRUE, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=n[1]-length(ret))
    }else{
      ret <- merge_symdiff(e1, e2, method = "exact")
      bitwhich(maxindex=n[1], ret, poslength=length(ret))
    }
  }
}

#' @describeIn xor \code{\link{bitwhich}} method for \code{\link{xor}}
#' @export
"xor.bitwhich" <- function(x, y) x != y

#' @describeIn xor \code{\link{booltype}} method for \code{\link{&}}
#' @export &.booltype
#' @export
"&.booltype" <- function(e1, e2){
  # align booltype between logical and bitwhich
  b1 <- booltype(e1)
  b2 <- booltype(e2)
  b <- min(max(booltypes[["logical"]], min(b1,b2)), booltypes[["bitwhich"]])
  e1 <- as.booltype(e1, b)
  e2 <- as.booltype(e2, b)
  # align length
  n1 <- length(e1)
  n2 <- length(e2)
  if (n1 && n2){
    if (n1 < n2){
      if (n2%%n1)
        warning("longer object length is not a multiple of shorter object length")
      e1 <- rep(e1, length.out=n2)
      n1 <- n2
    }else if (n2 < n1){
      if (n1%%n2)
        warning("longer object length is not a multiple of shorter object length")
      e2 <- rep(e2, length.out=n1)
    }
  }
  # do the operation
  switch(  as.character(b)
         , "logical" = e1 & e2
         , "bit" = "&.bit"(e1, e2)
         , "bitwhich" = "&.bitwhich"(e1, e2)
  )
}

#' @describeIn xor \code{\link{booltype}} method for \code{\link{|}}
#' @export |.booltype
#' @export
"|.booltype" <- function(e1, e2){
  # align booltype between logical and bitwhich
  b1 <- booltype(e1)
  b2 <- booltype(e2)
  b <- min(max(booltypes[["logical"]], min(b1,b2)), booltypes[["bitwhich"]])
  e1 <- as.booltype(e1, b)
  e2 <- as.booltype(e2, b)
  # align length
  n1 <- length(e1)
  n2 <- length(e2)
  if (n1 && n2){
    if (n1 < n2){
      if (n2%%n1)
        warning("longer object length is not a multiple of shorter object length")
      e1 <- rep(e1, length.out=n2)
      n1 <- n2
    }else if (n2 < n1){
      if (n1%%n2)
        warning("longer object length is not a multiple of shorter object length")
      e2 <- rep(e2, length.out=n1)
    }
  }
  # do the operation
  switch(  as.character(b)
           , "logical" = e1 | e2
           , "bit" = "|.bit"(e1, e2)
           , "bitwhich" = "|.bitwhich"(e1, e2)
  )
}

#' @describeIn xor \code{\link{booltype}} method for \code{\link{==}}
#' @export ==.booltype
#' @export
"==.booltype" <- function(e1, e2){
  # align booltype between logical and bitwhich
  b1 <- booltype(e1)
  b2 <- booltype(e2)
  b <- min(max(booltypes[["logical"]], min(b1,b2)), booltypes[["bitwhich"]])
  e1 <- as.booltype(e1, b)
  e2 <- as.booltype(e2, b)
  # align length
  n1 <- length(e1)
  n2 <- length(e2)
  if (n1 && n2){
    if (n1 < n2){
      if (n2%%n1)
        warning("longer object length is not a multiple of shorter object length")
      e1 <- rep(e1, length.out=n2)
      n1 <- n2
    }else if (n2 < n1){
      if (n1%%n2)
        warning("longer object length is not a multiple of shorter object length")
      e2 <- rep(e2, length.out=n1)
    }
  }
  # do the operation
  switch(  as.character(b)
           , "logical" = e1 == e2
           , "bit" = "==.bit"(e1, e2)
           , "bitwhich" = "==.bitwhich"(e1, e2)
  )
}

#' @describeIn xor \code{\link{booltype}} method for \code{\link{!=}}
#' @export !=.booltype
#' @export
"!=.booltype" <- function(e1, e2){
  # align booltype between logical and bitwhich
  b1 <- booltype(e1)
  b2 <- booltype(e2)
  b <- min(max(booltypes[["logical"]], min(b1,b2)), booltypes[["bitwhich"]])
  e1 <- as.booltype(e1, b)
  e2 <- as.booltype(e2, b)
  # align length
  n1 <- length(e1)
  n2 <- length(e2)
  if (n1 && n2){
    if (n1 < n2){
      if (n2%%n1)
        warning("longer object length is not a multiple of shorter object length")
      e1 <- rep(e1, length.out=n2)
      n1 <- n2
    }else if (n2 < n1){
      if (n1%%n2)
        warning("longer object length is not a multiple of shorter object length")
      e2 <- rep(e2, length.out=n1)
    }
  }
  # do the operation
  switch(  as.character(b)
           , "logical" = e1 != e2
           , "bit" = "!=.bit"(e1, e2)
           , "bitwhich" = "!=.bitwhich"(e1, e2)
  )
}

#' @describeIn xor \code{\link{booltype}} method for \code{\link{xor}}
#' @export xor.booltype
#' @export
"xor.booltype" <- function(x, y){
  x != y
}



#' Summaries of boolean vectors
#'
#' Fast aggregation functions for \code{\link{booltype}} vectors. namely \code{\link{bit}}, \code{\link{all}}, \code{\link{any}}, \code{\link{anyNA}},
#' \code{\link{min}}, \code{\link{max}}, \code{\link{range}},
#' \code{\link{sum}} and \code{\link{summary}}.
#' Now all boolean summaries (except for \code{anyNA} because the generic does not allow it) have an optional \code{range} argument to restrict the range of evalution.
#' Note that the boolean summaries have meaning and return values differing from logical aggregation functions: they treat \code{NA} as \code{FALSE},
#' \code{min}, \code{max} and \code{range} give the minimum and maximum positions of \code{TRUE}, \code{summary} returns counts of \code{FALSE},  \code{TRUE} and the \code{range}.
#' Note that you can force the boolean interpretation by calling the booltype method explicitely on any \code{\link{booltypes}} input, e.g. \code{min.booltype()}, see the examples.
#'
#' Summaries of \code{\link{bit}} vectors are quite fast because we use a double loop that fixes each
#' word in a processor register.  Furthermore we break out of looping as soon
#' as possible. Summaries of \code{\link{bitwhich}} vectors are even faster, if the selection is very skewed.
#'
#' @name Summaries
#' @param x an object of class bit or bitwhich
#' @param object an object of class bit
#' @param range a \code{\link{ri}} or an integer vector of length==2 giving a
#' range restriction for chunked processing
#' @param recursive formally required but not used
#' @param \dots formally required but not used
#' @return as expected
#' @author Jens Oehlschlägel
#' @seealso \code{\link{length}}
#' @keywords classes logic
#' @examples
#'
#'   l <- c(NA, FALSE, TRUE)
#'   b <- as.bit(l)
#'
#'   all(l)
#'   all(b)
#'   all(b, range=c(3,3))
#'   all.booltype(l, range=c(3,3))
#'
#'   min(l)
#'   min(b)
#'
#'   sum(l)
#'   sum(b)
#'
#'   summary(l)
#'   summary(b)
#'   summary.booltype(l)
NULL

# xx MEMO: R CMD check --no-tests  --no-manual --no-vignettes bit

#' @rdname Summaries
#' @export
all.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  .Call(C_R_bit_all, x, range)
}

#' @rdname Summaries
#' @export
any.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  .Call(C_R_bit_any, x, range)
}

#' @rdname Summaries
#' @export
anyNA.bit <- function(x
                      #, range=NULL
                      , recursive = FALSE)FALSE

#' @rdname Summaries
#' @export
sum.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  .Call(C_R_bit_sum, x, range)
}

#' @rdname Summaries
#' @export
min.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  .Call(C_R_bit_min, x, range)
}

#' @rdname Summaries
#' @export
max.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  .Call(C_R_bit_max, x, range)
}

#' @rdname Summaries
#' @export
range.bit <- function(x, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(x))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(x))
      stop("illegal range")
  }
  ret <- integer(2)
  ret[1] <- .Call(C_R_bit_min, x, range)
  if (is.na(ret[1]))
    ret[2] <- NA_integer_
  else
    ret[2] <- .Call(C_R_bit_max, x, range)
  ret
}

#' @rdname Summaries
#' @export
summary.bit <- function(object, range=NULL, ...){
  if (is.null(range))
    range <- c(1L, length(object))
  else{
    range <- as.integer(range[1:2])
    if (range[1]<1L || range[2]>length(object))
      stop("illegal range")
  }
  s <- sum(object, range=range)
  r <- range(object, range=range)
  c("FALSE"=range[2]-range[1]+1L-s, "TRUE"=s, "Min."=r[1], "Max."=r[2])
}



#' @rdname Summaries
#' @export
all.bitwhich <- function(x, range=NULL, ...){
  if (is.null(range))
    attr(x, "poslength") == attr(x, "maxindex")
  else{
    y <- bitwhich_representation(x)
    range <- as.integer(range)
    if (is.logical(y)){
      if (y)
        TRUE
      else
        FALSE
    }else{
      if (y<0){
        all(merge_rangenotin(rx=range, y=x, revx=FALSE, revy=TRUE))
      }else{
        all(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
      }
    }
  }
}

#' @rdname Summaries
#' @export
any.bitwhich <- function(x, range=NULL, ...){
  if (is.null(range))
    attr(x, "poslength") > 0L
  else{
    y <- bitwhich_representation(x)
    range <- as.integer(range)
    if (is.logical(y)){
      if (y)
        TRUE
      else
        FALSE
    }else{
      if (y<0){
        any(merge_rangenotin(rx=range, y=x, revx=FALSE, revy=TRUE))
      }else{
        any(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
      }
    }
  }
}

#' @rdname Summaries
#' @export
anyNA.bitwhich <- function(x
                           #, range=NULL
                           , recursive = FALSE)FALSE

#' @rdname Summaries
#' @export
sum.bitwhich <- function(x, range=NULL, ...){
  if (is.null(range))
    attr(x, "poslength")
  else{
    y <- bitwhich_representation(x)
    range <- as.integer(range)
    if (is.logical(y)){
      if (y)
        range[2] - range[1] + 1L
      else
        0L
    }else{
      if (y<0){
        sum(merge_rangenotin(rx=range, y=x, revx=FALSE, revy=TRUE))
      }else{
        sum(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
      }
    }
  }
}

#' @rdname Summaries
#' @export
min.bitwhich <- function(x, range=NULL, ...){
  y <- bitwhich_representation(x)
  if (is.logical(y)){
    if (length(y) && y)
      1L
    else
      NA_integer_
  }else{
    if (is.null(range)){
      if (y<0L){
        merge_firstnotin(c(1L,length(x)), x, revy=TRUE)
      }else{
        merge_first(x)
      }
    }else{
      range <- as.integer(range)
      if (y<0L){
        merge_firstnotin(range, x, revy=TRUE)
      }else{
        merge_firstin(range, x)
      }
    }
  }
}

#' @rdname Summaries
#' @export
max.bitwhich <- function(x, range=NULL, ...){
  y <- bitwhich_representation(x)
  if (is.logical(y)){
    if (length(y) && y)
      length(x)
    else
      NA_integer_
  }else{
    if (is.null(range)){
      if (y<0L){
        merge_lastnotin(c(1L,length(x)), x, revy=TRUE)
      }else{
        merge_last(x)
      }
    }else{
      range <- as.integer(range)
      if (y<0L){
        merge_lastnotin(range, x, revy=TRUE)
      }else{
        merge_lastin(range, x)
      }
    }
  }
}

#' @rdname Summaries
#' @export
range.bitwhich <- function(x, range=NULL, ...){
  c(min(x, range=range, ...), max(x, range=range, ...))
}

#' @rdname Summaries
#' @export
summary.bitwhich <- function(object, range=NULL, ...){
  n <- attr(object, "maxindex")
  p <- attr(object, "poslength")
  r <- range(object)
  c("FALSE"=n-p, "TRUE"=p, "Min."=r[1], "Max."=r[2])
}




#' @rdname Summaries
#' @export
all.which <- function(x, range=NULL, ...){
  if (is.null(range))
    length(x) == attr(x, "maxindex")
  else{
    range <- as.integer(range)
    all(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
  }
}

#' @rdname Summaries
#' @export
any.which <- function(x, range=NULL, ...){
  if (is.null(range))
    length(x) > 0L
  else{
    range <- as.integer(range)
    any(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
  }
}

#' @rdname Summaries
#' @export
anyNA.which <- function(x
                        #, range=NULL
                        , recursive = FALSE)FALSE

#' @rdname Summaries
#' @export
sum.which <- function(x, range=NULL, ...){
  if (is.null(range))
    length(x)
  else{
    sum(merge_rangein(rx=range, y=x, revx=FALSE, revy=FALSE))
  }
}

#' @rdname Summaries
#' @export
min.which <- function(x, range=NULL, ...){
  if (is.null(range)){
    merge_first(x)
  }else{
    range <- as.integer(range)
    merge_firstin(range, x)
  }
}

#' @rdname Summaries
#' @export
max.which <- function(x, range=NULL, ...){
  if (is.null(range)){
    merge_last(x)
  }else{
    range <- as.integer(range)
    merge_lastin(range, x)
  }
}

#' @rdname Summaries
#' @export
range.which <- function(x, range=NULL, ...){
  c(min(x, range=range, ...), max(x, range=range, ...))
}

#' @rdname Summaries
#' @export
summary.which <- function(object, range=NULL, ...){
  n <- attr(object, "maxindex")
  p <- attr(object, "poslength")
  r <- range(object)
  c("FALSE"=n-p, "TRUE"=p, "Min."=r[1], "Max."=r[2])
}

#' @rdname Summaries
#' @export all.booltype
#' @export
all.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=all.bit(as.bit(x), range=range, ...)
         , logical=all.bit(as.bit(x), range=range, ...)
         , bit=all.bit(x, range=range, ...)
         , bitwhich=all.bitwhich(x, range=range, ...)
         , which=all.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=all.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export any.booltype
#' @export
any.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=any.bit(as.bit(x), range=range, ...)
         , logical=any.bit(as.bit(x), range=range, ...)
         , bit=any.bit(x, range=range, ...)
         , bitwhich=any.bitwhich(x, range=range, ...)
         , which=any.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=any.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export anyNA.booltype
#' @export
anyNA.booltype <- function(x
                           #, range=NULL
                           , ...){
  switch(as.character(booltype(x))
         , nobool=anyNA.bit(as.bit(x)
                            #, range=range
                            , ...)
         , logical=anyNA.bit(as.bit(x)
                             #, range=range
                             , ...)
         , bit=anyNA.bit(x
                         #, range=range
                         , ...)
         , bitwhich=anyNA.bitwhich(x
                                   #, range=range
                                   , ...)
         , which=anyNA.bit(as.bit(x)
                           #, range=range
                           , ...)
         , hi=stop("not implemented")
         , ri=anyNA.ri(x
                       #, range=range
                       , ...)
  )
}


#' @rdname Summaries
#' @export sum.booltype
#' @export
sum.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=sum.bit(as.bit(x), range=range, ...)
         , logical=sum.bit(as.bit(x), range=range, ...)
         , bit=sum.bit(x, range=range, ...)
         , bitwhich=sum.bitwhich(x, range=range, ...)
         , which=sum.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=sum.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export min.booltype
#' @export
min.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=min.bit(as.bit(x), range=range, ...)
         , logical=min.bit(as.bit(x), range=range, ...)
         , bit=min.bit(x, range=range, ...)
         , bitwhich=min.bitwhich(x, range=range, ...)
         , which=min.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=min.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export max.booltype
#' @export
max.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=max.bit(as.bit(x), range=range, ...)
         , logical=max.bit(as.bit(x), range=range, ...)
         , bit=max.bit(x, range=range, ...)
         , bitwhich=max.bitwhich(x, range=range, ...)
         , which=max.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=max.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export range.booltype
#' @export
range.booltype <- function(x, range=NULL, ...){
  switch(as.character(booltype(x))
         , nobool=range.bit(as.bit(x), range=range, ...)
         , logical=range.bit(as.bit(x), range=range, ...)
         , bit=range.bit(x, range=range, ...)
         , bitwhich=range.bitwhich(x, range=range, ...)
         , which=range.bit(as.bit(x), range=range, ...)
         , hi=stop("not implemented")
         , ri=range.ri(x, range=range, ...)
  )
}

#' @rdname Summaries
#' @export summary.booltype
#' @export
summary.booltype <- function(object, range=NULL, ...){
  switch(as.character(booltype(object))
         , nobool=summary.bit(as.bit(object), range=range, ...)
         , logical=summary.bit(as.bit(object), range=range, ...)
         , bit=summary.bit(object, range=range, ...)
         , bitwhich=summary.bitwhich(object, range=range, ...)
         , which=summary.bit(as.bit(object), range=range, ...)
         , hi=stop("not implemented")
         , ri=summary.ri(object, range=range, ...)
  )
}


#' Extract or replace part of an boolean vector
#'
#' Operators acting on \code{\link{bit}} or \code{\link{bitwhich}} objects to extract or replace parts.
#'
#' The typical usecase for for '[' and '[<-' is subscripting with positive integers,
#' negative integers are allowed but slower,
#' as logical subscripts only scalars are allowed.
#' The subscript can be given as a \code{\link{bitwhich}} object.
#' Also \code{\link{ri}} can be used as subscript.
#'
#' Extracting from \code{\link{bit}} and \code{\link{bitwhich}} is faster than from  \code{\link{logical}} if positive subscripts are used.
#' integer subscripts make sense.  Negative subscripts are converted to
#' positive ones, beware the RAM consumption.
#'
#' @name Extract
#' @param x a \code{\link{bit}} or \code{\link{bitwhich}} object
#' @param i preferrably a positive integer subscript or a \code{\link{ri}}, see text
#' @param value new logical or integer values
#' @return The extractors \code{[[} and \code{[} return a logical scalar or
#' vector.  The replacment functions return an object of \code{class(x)}.
#' @author Jens Oehlschlägel
#' @seealso \code{\link{bit}}, \code{\link[base]{Extract}}
#' @keywords classes logic
#' @examples
#'
#'   x <- as.bit(c(FALSE, NA, TRUE))
#'   x[] <- c(FALSE, NA, TRUE)
#'   x[1:2]
#'   x[-3]
#'   x[ri(1,2)]
#'   x[as.bitwhich(c(TRUE,TRUE,FALSE))]
#'   x[[1]]
#'   x[] <- TRUE
#'   x[1:2] <- FALSE
#'   x[[1]] <- TRUE
#'
NULL

#' @rdname Extract
#' @export
"[[.bit" <- function(x, i){
  if (length(i)!=1L)
    stop("subscript length not 1")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L || i>length(x))
      stop("subscript must be positive integer (or double) within length")
    ret <- .Call(C_R_bit_extract, x, i)
    setattr(ret, "vmode", "boolean")
    ret
  }else
    stop("subscript must be positive integer (or double) within length")
}

#' @rdname Extract
#' @export
"[[<-.bit" <- function(x, i, value){
  if (length(i)!=1L)
    stop("subscript length not 1")
  if (length(value)!=1)
    stop("value length not 1")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L)
      stop("subscript must be positive integer (or double)")
    if ((mi <- max(i))>length(x))
      length(x) <- mi
    value <- as.logical(value)
    .Call(C_R_bit_replace, x, i, value)
  }else
    stop("subscript must be positive integer (or double) within length")
}


#' @rdname Extract
#' @export
"[.bit" <- function(x, i){
  nx <- length(x)
  if ( missing(i) ){
    ret <- logical(nx)
    .Call(C_R_bit_get_logical, x, ret, range=c(1L, nx))
  }else{
    if (inherits(i, "bit")){
      i <- as.bitwhich(i)
    }
    if (inherits(i, "bitwhich")){
      i <- unclass(i)
    }
    if(is.numeric(i)){
      if (inherits(i, "ri")){
        if (i[1]<1L || i[2]>nx )
          stop("illegal range index 'ri'")
        ret <- logical(i[2]-i[1]+1L)
        .Call(C_R_bit_get_logical, x, ret, range=i)
      }else{
        i <- as.integer(i)
        r <- range_na(i)
        if (is.na(r[1])){
          ret <- logical()
        }else if (r[1]<0L){
          # check for positive or NA mixed with negative
          if (r[2]>0L || r[3]>0L)
            stop("only 0's may be mixed with negative subscripts")
          isasc <- intisasc(i, "none") # NAs checked already, early terminate on FALSE
          if (!isasc){
            if((length(i) / (r[2]-r[1])) < 0.05)
              i <- sort.int(i, method="quick")
            else
              i <- bit_sort_unique(i)
          }
        } # is positive, hence no sorting
        ret <- .Call(C_R_bit_extract, x, i)
      }
    }else if(is.logical(i)){
      if (poslength(i)==0L){
         ret <- logical()
      }else{
        if (inherits(i, "bitwhich")){
          i <- unclass(i)
        }else{
          if (length(i)!=1 || is.na(i))
            stop("only scalar TRUE or FALSE allowed")
        }
        if (i){
          ret <- logical(nx)
          .Call(C_R_bit_get_logical, x, ret, range=c(1L, nx))
        }else{
          ret <- logical()
        }
      }
    }else
      stop("subscript must be ri or integer (or double) or  TRUE (or missing) or FALSE")
  }
  setattr(ret, "vmode", "boolean")
  ret
}


#' @rdname Extract
#' @export
"[<-.bit" <- function(x, i, value){
  nx <- length(x)
  value <- as.logical(value)
  nv <- length(value)
  if (missing(i))
    i <- TRUE
  if (inherits(i, "bit")){
    i <- as.bitwhich(i)
  }
  if (inherits(i, "bitwhich")){
    i <- unclass(i)
  }
  if (is.logical(i)){
    if (length(i)!=1L || is.na(i))
      stop("logical only scalar TRUE or FALSE allowed")
    if (i){
      if (nv==0L)
        stop("replacement has length zero")
      if (nx %% nv)
        warning("number of items to replace is not a multiple of replacement length")
      .Call(C_R_bit_set_logical, x, value, range=c(1L, nx))
    }else{
      x
    }
  }else if(is.numeric(i)){
    if (inherits(i, "ri")){
      if (i[1]<1L)
        stop("illegal range index 'ri'")
      if (i[2]>nx)
        length(x) <- i[2]
      ni <- i[2] - i[1] + 1L
      if (nv==0L)
        stop("replacement has length zero")
      if (ni %% nv)
        warning("number of items to replace is not a multiple of replacement length")
      .Call(C_R_bit_set_logical, x, value, range=i)
    }else{
      if (inherits(i, "which")){
        ni <- length(i)
        if (ni && i[ni] > nx)
          length(x) <- i[ni]
      }else{
        i <- range_nanozero(as.integer(i))
        r <- getsetattr(i, "range_na", NULL)
        ni <- length(i)
        if (ni){
          if (r[3]>0L)
             stop("NAs are not allowed in subscripted assignments")
          if (r[1]>0L){
            if (r[2] > nx)
              length(x) <- r[2]
          }else{
            if (r[2] > 0L)
              stop("only 0's may be mixed with negative subscripts")
            # R_bit_replace expects sorted i if i is negative
            i <- bit_sort_unique(i, range_na = r)
            ni <- nx - length(i)
          }
        }
      }
      if (nv != ni){
        if (nv==0L)
          stop("replacement has length zero")
        if (ni %% nv)
          warning("number of items to replace is not a multiple of replacement length")

      }
      .Call(C_R_bit_replace, x, i, value)
    }
  }else  stop("subscript must be integer (or double) or ri or bitwhich or TRUE or FALSE or missing")
}


#' Check existence of integers in table
#'
#' If the table is sorted, this can be much faster than \code{\link{\%in\%}}
#'
#' @param x a vector of integer
#' @param table a \code{\link{bitwhich}} object or a vector of integer
#' @param is.unsorted logical telling the function whether the table is (un)sorted. With the defautl \code{NULL} \code{FALSE} is assumed for \code{\link{bitwhich}} tables, otherwise \code{TRUE}
#'
#' @return logical vector
#' @seealso \code{\link{\%in\%}}
#'
#' @examples
#' x <- bitwhich(100)
#' x[3] <- TRUE
#' in.bitwhich(c(NA,2,3), x)
#' @export
in.bitwhich <- function(x, table, is.unsorted=NULL){
  x <- as.integer(x)
  if (is.null(is.unsorted))
    is.unsorted <- !is.bitwhich(table)
  if (is.logical(table)){
    if (length(table) && table){
      1L <= x & x <= length(table)
    }else{
      rep(FALSE, length(x))
    }
  }else{
    y <- bitwhich_representation(table)
    if (length(x)>1L && is.unsorted){
      if (y[1]>0L)
        !is.na(match(x,table))
      else
        is.na(match(-x,table))

    }else{
      if (y[1]>0L)
        merge_in(x,table)
      else
        merge_notin(x,table, revy=TRUE)
    }
  }
}


#' @rdname Extract
#' @export
"[[.bitwhich" <- function(x, i){
  if (length(i)!=1L)
    stop("subscript length not 1")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L || i>length(x))
      stop("subscript must be positive integer (or double) within length")
    y <- bitwhich_representation(x)
    if (is.logical(y))
      ret <- as.vector(x)
    else{
      ret <- in.bitwhich(i, x)
    }
    setattr(ret, "vmode", "boolean")
    ret
  }else
    stop("subscript must be positive integer (or double) within length")
}



#' @rdname Extract
#' @export
"[[<-.bitwhich" <- function(x, i, value){
  if (length(i)!=1L)
    stop("subscript length not 1")
  if (length(value)!=1L)
    stop("value length not 1")
  value <- as.logical(value)
  if (is.na(value))
    value <- FALSE
  n <- length(x)
  if (i>n)
    warning("increasing length of bitwhich, which has non-standard semantics")
  if (is.numeric(i)){
    i <- as.integer(i)
    if (is.na(i) || i<1L || i>.Machine$integer.max)
      stop("subscript must be positive integer (or double)")
    y <- bitwhich_representation(x)
    if (is.logical(y)){
      if (length(y)){
        if (value == y){
          if (i>n)
            length(x) <- i
          return(x)
        }else if (value)
          ret <- bitwhich(max(n,i), i, poslength=1L)
        else
          ret <- bitwhich(max(n,i), -i, poslength=n-1L)
      }else{
        if (value)
          ret <- bitwhich(i, i, poslength=1L)
        else
          ret <- bitwhich(i, -i, poslength=n-1L)
      }
    }else{
      if (i>n){
        n <- i
        length(x) <- i
      }
      oldvalue <- in.bitwhich(i, x)
      if (value == oldvalue)
        return(x)
      else{
        if (value == (y>0)){
          ret <- bitwhich(n, merge_union(x, y*i, method = "all"), poslength=attr(x, "poslength")+y)
        }else{
          ret <- bitwhich(n, merge_setdiff(x, y*i, method = "exact"), poslength=attr(x, "poslength")-y)
        }
      }
    }
  }else
    stop("subscript must be positive integer (or double) within length")
  a <- attributes(x)
  a$poslength <- attr(ret, "poslength")
  setattributes(ret, a)
  ret
}


#' @rdname Extract
#' @export
"[.bitwhich" <- function(x, i){
  nx <- length(x)
  if ( missing(i) ){
    ret <- as.logical(x)
  }else{
    if (inherits(i, "bit"))
      stop("please use as.which(bit) for subscripting with bit")
    if (inherits(i, "bitwhich"))
      stop("please use unclass(bitwhich) or as.which(bitwhich) to clarify what you want")
    if (length(i)==0){
      ret <- logical()
    }else{
      if(is.logical(i)){
        if (length(i)!=1L || is.na(i))
          stop("only scalar TRUE or FALSE allowed")
        if (i){
          ret <- as.logical(x)
        }else{
          ret <- logical()
        }
      }else if(is.numeric(i)){
        if (inherits(i, "ri")){
          if (i[1]<1L || i[2]>nx )
            stop("illegal range index 'ri'")
          if (is.logical(x)){
            if (length(x))
              ret <- rep(copy_vector(x), i[2]-i[1]+1L)
            else
              ret <- rep(NA, i[2]-i[1]+1L)
          }else{
            #y <- unclass(x)
            y <- bitwhich_representation(x)
            if (y[1]>0L){
              #ret <- rep(FALSE, i[2]-i[1]+1L)
              #ret[y[i[1]<=y & y<=i[2]] - i[1] + 1L] <- TRUE
              ret <- merge_rangein(c(i[1], i[2]), x)
            }else{
              # ret <- rep(TRUE, i[2]-i[1]+1L)
              # ret[-y[(-i[1])>=y & y>=(-i[2])] - i[1] + 1L] <- FALSE
              ret <- merge_rangenotin(c(i[1], i[2]), x, revy=TRUE)
            }
          }
        }else{
          i <- range_nanozero(as.integer(i))
          r <- getsetattr(i, "range_na", NULL)
          n <- length(i)
          if (r[3]==n) # if allNA
            ret <- rep(NA, n)
          else{
            if (r[1] < 0L && r[2] > 0L)
              stop("only 0's may be mixed with negative subscripts")
            ret <- as.bit(x)[i]
          }
        }
      }else stop("subscript must be integer (or double) or ri or bitwhich or TRUE or FALSE or missing")
    }
  }
  setattr(ret, "vmode", "boolean")
  ret
}


#' @rdname Extract
#' @export
"[<-.bitwhich" <- function(x, i, value){
  nx <- length(x)
  value <- as.logical(value)
  if (anyNA(value))
    value[is.na(value)] <- FALSE
  nv <- length(value)
  if (missing(i))
    i <- TRUE
  if (inherits(i, "bit"))
    stop("please use as.which(bit) for subscripting with bit")
  if (inherits(i, "bitwhich"))
    stop("please use unclass(bitwhich) or as.which(bitwhich) to clarify what you want")
  if (length(i)){
    if (is.logical(i)){
      if (length(i)!=1L || is.na(i))
        stop("logical only scalar TRUE or FALSE allowed")
      if (i){
        if (nv==1L){
          ret <- bitwhich(nx, value)
        }else{
          b <- as.bit(value)
          if (nv==nx){
            ret <- as.bitwhich(b)
          }else{
            if (nv==0L)
              stop("replacement has length zero")
            if (nx%%nv)
              warning("number of items to replace is not a multiple of replacement length")
            ret <- as.bitwhich(rep(b, length.out=nx))
          }
        }
      }else{
        return(x)
      }
    }else if(is.numeric(i)){
      if (nv>1L){
        b <- as.bit(x)
        b[i] <- value
        ret <- as.bitwhich(b)
      }else{
        if (inherits(i, "ri")){
          if (i[1]<1L)
            stop("illegal range index 'ri'")
          biggest_mentioned_index <- max(abs(i[1:2]))
          i <- i[1]:i[2]
          ni <- length(i)
        }else{
          if (inherits(i, "which")){
            ni <- length(i)
            biggest_mentioned_index <- i[length(i)]
          }else{
            i <- range_nanozero(as.integer(i))
            r <- getsetattr(i, "range_na", NULL)
            if (length(i)){
              if (r[3]>0L)
                stop("NAs are not allowed in subscripted assignments")
              if (r[1]>0L){
                # since value is a scalar removing duplicates does not harm and speeds up
                i <- bit_sort_unique(i, range_na = r)
                ni <- length(i)
              }else{
                if (r[2] > 0L)
                  stop("only 0's may be mixed with negative subscripts")
                i <- bit_sort_unique(i, range_na = r)
                ni <- nx - length(i)
              }
              # since value is a scalar removing duplicates does not harm and speeds up
              biggest_mentioned_index <- max(abs(i[1:2]), na.rm=TRUE)
            }else{
              ni <- 0L
              biggest_mentioned_index <- 0L
            }
          }
        }
        if (!ni){
          return(x)
        }else{
          if (nv==0L)
            stop("replacement has length zero")
          if (biggest_mentioned_index>nx){
            length(x) <- biggest_mentioned_index
            nx <- biggest_mentioned_index
          }
          y <- bitwhich_representation(x)
          if (is.logical(y)){
            if (value == y){
              # assignment doesn't change anything
              return(x)
            }else{
              if (value){
                # assignment has first inclusions
                if (i[1]<0){
                  # assignment enumerates those not included
                  ret <- bitwhich(nx, i, poslength=nx-length(i))
                }else{
                  # assignment enumerates those included
                  ret <- bitwhich(nx, i, poslength=length(i))
                }
              }else{
                # assignment has first exclusions
                if (i[1]<0){
                  # assignment enumerates those not excluded
                  ret <- bitwhich(nx, copy_vector(i, revx=TRUE), poslength=length(i))
                }else{
                  # assignment enumerates those excluded
                  ret <- bitwhich(nx, copy_vector(i, revx=TRUE), poslength=nx-length(i))
                }
              }
            }
          }else{
            if (y<0){
              # object maintains exclusions
              if (value){
                # assignment has inclusions
                if (i[1]<0){
                  # assignment enumerates those not included
                  # w2 <- w <- bitwhich(12, -(1:3)); w2[-(3:5)] <- TRUE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_intersect(x,i, method='exact'), xempty=TRUE, is.unsorted = FALSE, has.dup = FALSE) #done
                }else{
                  # assignment enumerates those included
                  # w2 <- w <- bitwhich(12, -(1:3)); w2[(3:5)] <- TRUE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_setdiff(x,i,revy=TRUE, method='exact'), xempty=TRUE, is.unsorted = FALSE, has.dup = FALSE)  #done
                }
              }else{
                # assignment has exclusions
                if (i[1]<0){
                  # assignment enumerates those not excluded
                  # w2 <- w <- bitwhich(12, -(1:3)); w2[-(3:5)] <- FALSE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_setdiff(i, x, revx=TRUE, revy=TRUE, method='exact'), xempty=FALSE, is.unsorted = FALSE, has.dup = FALSE) #done
                }else{
                  # assignment enumerates those excluded
                  # w2 <- w <- bitwhich(12, -(1:3)); w2[(3:5)] <- FALSE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_union(x,i,revy=TRUE, method='exact'), is.unsorted = FALSE, has.dup = FALSE) #done
                }
              }
            }else{
              # object maintains inclusions
              if (value){
                # assignment has inclusions
                if (i[1]<0){
                  # assignment enumerates those not included
                  # w2 <- w <- bitwhich(12, (1:3)); w2[-(3:5)] <- TRUE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_setdiff(i,x, revy = TRUE, method='exact'), xempty=TRUE, is.unsorted = FALSE, has.dup = FALSE) #done
                }else{
                  # assignment enumerates those included
                  # w2 <- w <- bitwhich(12, (1:3)); w2[(3:5)] <- TRUE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_union(x,i, method='exact'), is.unsorted = FALSE, has.dup = FALSE) #done
                }
              }else{
                # assignment has exclusions
                if (i[1]<0){
                  # assignment enumerates those not excluded
                  # w2 <- w <- bitwhich(12, (1:3)); w2[-(3:5)] <- FALSE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_intersect(x, i, revy=TRUE, method='exact'), xempty=FALSE, is.unsorted = FALSE, has.dup = FALSE) #done
                }else{
                  # assignment enumerates those excluded
                  # w2 <- w <- bitwhich(12, (1:3)); w2[(3:5)] <- FALSE; cbind(as.logical(w), as.logical(w2))
                  ret <- bitwhich(nx, merge_setdiff(x,i, method='exact'), xempty=FALSE, is.unsorted = FALSE, has.dup = FALSE)
                }
              }
            }
          }
        }
      }
    }else  stop("subscript must be integer (or double) or ri or bitwhich or TRUE or FALSE or missing")
    a <- attributes(x)
    a$poslength <- sum(ret)
    setattributes(ret, a)
    ret
  }else
    x
}



#' Range index
#'
#' A range index can be used to extract or replace a continuous ascending part
#' of the data
#'
#' @param from first position
#' @param to last posistion
#' @param x an object of class 'ri'
#' @param maxindex the maximal length of the object-to-be-subscripted (if
#' known)
#' @param \dots further arguments
#' @return A two element integer vector with class 'ri'
#' @author Jens Oehlschlägel
#' @seealso \code{\link[ff]{as.hi}}
#' @keywords classes logic
#' @examples
#'
#'  bit(12)[ri(1,6)]
#'
#' @export ri
ri <- function(from, to=NULL, maxindex=NA){
  if (is.null(to)){
    x <- as.integer(c(from, maxindex))
  }else{
    x <- as.integer(c(from, to, maxindex))
  }
  maxindex = maxindex
  if (length(x)!=3 )
    stop("range must have exactly three elements")
  if (x[[1]]<1L)
    stop("range must at least select one element")
  if (x[[1]]>x[[2]])
    stop("lower bound must be smaller or equal than upper bound")
  if (!is.na(x[[3]]) && x[[2]]>x[[3]])
    stop("lower and upper bound must be smaller or equal to maxindex")
  oldClass(x) <- c("booltype","ri")
  x
}

#' @rdname ri
#' @export
print.ri <- function(x, ...)
  cat("range index (ri) from", x[[1]], "to", x[[2]], "maxindex",  x[[3]], "\n")

#' @rdname length.bit
#' @export
length.ri <- function(x)x[[3]]


#' @rdname Summaries
#' @export
all.ri <- function(x, range=NULL, ...){
  if (is.null(range)){
    range[[1]] <- 1L
    range[[2]] <- x[[3]]
  }
  x[[1]]<=range[[1]] && x[[2]]>=range[[2]]
}

#' @rdname Summaries
#' @export
any.ri <- function(x, range=NULL, ...){
  if (is.null(range)){
    range[[1]] <- 1L
    range[[2]] <- x[[3]]
  }
  range[[1]]<=x[[1]] && range[[2]]>=x[[2]]
}

#' @rdname Summaries
#' @export
anyNA.ri <- function(x
                     #, range=NULL
                     , recursive = FALSE)FALSE

#' @rdname Summaries
#' @export
sum.ri <- function(x, ...){
  if (any(names(match.call(expand.dots = TRUE))=="range") && !is.null(list(...)$range))
    stop("parameter 'range' allowed only for 'bit' but not for 'ri'")
  x[[2]] - x[[1]] + 1L
}

#' @rdname Summaries
#' @export
min.ri <- function(x, ...){
  if (any(names(match.call(expand.dots = TRUE))=="range") && !is.null(list(...)$range))
    stop("parameter 'range' allowed only for 'bit' but not for 'ri'")
  x[[1]]
}

#' @rdname Summaries
#' @export
max.ri <- function(x, ...){
  if (any(names(match.call(expand.dots = TRUE))=="range") && !is.null(list(...)$range))
    stop("parameter 'range' allowed only for 'bit' but not for 'ri'")
  x[[2]]
}

#' @rdname Summaries
#' @export
range.ri <- function(x, ...){
  if (any(names(match.call(expand.dots = TRUE))=="range") && !is.null(list(...)$range))
    stop("parameter 'range' allowed only for 'bit' but not for 'ri'")
  x[1:2]
}

#' @rdname Summaries
#' @export
summary.ri <- function(object, ...){
  if (any(names(match.call(expand.dots = TRUE))=="range") && !is.null(list(...)$range))
    stop("parameter 'range' allowed only for 'bit' but not for 'ri'")
  s <- object[[2]] - object[[1]] + 1L
   c(`FALSE` = object[[3]] - s, `TRUE` = s, Min. = object[[1]], Max. = object[[2]])
}



# this version without vmode() will be overwritte by the version in package ff
#' @rdname PhysVirt
#' @export
physical.default <- function(x){
  p <- attributes(attr(x, "physical"))
  p <- p[is.na(match(names(p), "class"))]
  p
}
#' @rdname PhysVirt
#' @export
"physical<-.default" <- function(x, value){
  attributes(attr(x, "physical")) <- c(value, list(class="physical"))
  x
}


#' @rdname PhysVirt
#' @export
virtual.default <- function(x){
  v <- attributes(attr(x, "virtual"))
  v[is.na(match(names(v), "class"))]
}
#' @rdname PhysVirt
#' @export
"virtual<-.default" <- function(x, value){
  attributes(attr(x, "virtual")) <- c(value, list(class="virtual"))
  x
}


#' @rdname PhysVirt
#' @export
print.physical <- function(x, ...){
  cat("(hidden, use physical(x) to access the physical attributes and vmode(x) for accessing vmode)\n")
  invisible()
}

#' @rdname PhysVirt
#' @export
print.virtual <- function(x, ...){
  cat("(hidden, use virtual(x) to access the virtual attributes)\n")
  invisible()
}




# not exported - just here to avoid cross calling the dll from ff
R_bit_as_hi <- function(x, range, offset)
.Call(C_R_bit_as_hi, x, range, offset)
