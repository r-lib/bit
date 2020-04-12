# Package generics
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

# source("D:/mwp/eanalysis/bit/R/generics.R")


#' Get maxindex (length of boolean vector) and poslength (number of 'selected' elements)
#' 
#' For \code{\link{is.booltype}} objects the term \code{\link{length}} is ambiguous. 
#' For example the length of \code{\link{which}} corresponds to the sum of \code{\link{logical}}.
#' The generic \code{maxindex} gives \code{length(logical)} for all \code{\link{booltype}s}.
#' The generic \code{poslength} gives the number of positively selected elements, i.e. \code{sum(logical)} for all \code{\link{booltype}s} 
#' (and gives \code{NA} if \code{NAs} are present).
#' 
#' @name maxindex
#' @param x an R object, typically a \code{\link{is.booltype}} object.
#' @param ... further arguments (ignored)
#' @return an integer scalar
#' @examples
#' r <- ri(1,2,12)
#' i <- as.which(r)
#' w <- as.bitwhich(r)
#' b <- as.bit(r)
#' l <- as.logical(r)
#' u <- which(l)      # unclassed which
#' 
#' sapply(list(r=r,u=u,i=i,w=w,b=b,l=l), function(x){
#'   c(length=length(x), sum=sum(x), maxindex=maxindex(x), poslength=poslength(x))
#' })
#' @export
maxindex <- function(x, ...)
{
  UseMethod("maxindex", x)
}

#' @name maxindex
#' @export
poslength <- function(x, ...)
{
  UseMethod("poslength")
}



#' Coerce to booltype (generic)
#'
#' @param x object to coerce
#' @param booltype target \code{\link{booltype}} given as integer or as character
#' @param ... further arguments
#'
#' @return \code{x} coerced to \code{booltype}
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{booltypes}}, \code{\link{booltype}}, \code{\link{is.booltype}}
#'
#' @examples
#' as.booltype(0:1)
#' as.booltype(0:1, "logical")
#' as.booltype(0:1, "bit")
#' as.booltype(0:1, "bitwhich")
#' as.booltype(0:1, "which", maxindex=2)
#' as.booltype(0:1, "ri")
#' @export
as.booltype <- function (x, booltype, ...){
  UseMethod("as.booltype")
}


#' Coercing to bit
#' 
#' Coercing to bit vector
#' 
#' Coercing to bit is quite fast because we use a double loop that fixes each
#' word in a processor register
#' 
#' @param x an object of class \code{\link{bit}}, \code{\link{logical}},
#' \code{\link{integer}}, \code{\link{bitwhich}} or an integer from
#' \code{\link{as.which}} or a boolean \code{\link[ff:vmode]{ff}}
#' @param length the length of the new bit vector
#' @param \dots further arguments
#' @return \code{is.bit} returns FALSE or TRUE, \code{as.bit} returns a vector
#' of class 'bit'
#' @note Zero is coerced to FALSE, all other numbers including NA are coerced
#' to TRUE.  This differs from the NA-to-FALSE coercion in package ff and may
#' change in the future.
#' @author Jens Oehlschlägel
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
#' , \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
#' @keywords classes logic
#' @examples
#' 
#' as.bit(c(FALSE, NA, TRUE))
#' 
#' @export
as.bit <- function(x=NULL, ...)
  UseMethod("as.bit", x)


#' Coercing to bitwhich
#' 
#' Functions to coerce to bitwhich
#' 
#' @param x An object of class 'bitwhich', 'integer', 'logical' or 'bit' or an
#' integer vector as resulting from 'which'
#' @param maxindex the length of the new bitwhich vector
#' @param poslength the number of selected elements
#' @param range a \code{\link{ri}} or an integer vector of length==2 giving a
#' range restriction for chunked processing
#' @param \dots further arguments
#' @return a value of class \code{\link{bitwhich}}
#' @author Jens Oehlschlägel
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
#' , \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
#' @keywords classes logic
#' @examples
#' 
#'  as.bitwhich(c(NA,NA,NA))
#'  as.bitwhich(c(FALSE, FALSE, FALSE))
#'  as.bitwhich(c(FALSE, FALSE, TRUE))
#'  as.bitwhich(c(FALSE, TRUE, TRUE))
#'  as.bitwhich(c(TRUE, TRUE, TRUE))
#' 
#' @export
as.bitwhich <- function(x=NULL, ...)
  UseMethod("as.bitwhich")


#' Coercion to (positive) integer positions
#' 
#' Coercing to something like the result of which \code{\link{which}}
#' 
#' \code{as.which.bit} returns a vector of subscripts with class 'which'
#' 
#' @param x an object of classes \code{\link{bit}}, \code{\link{bitwhich}},
#' \code{\link{ri}} or something on which \code{\link{which}} works
#' @param range a \code{\link{ri}} or an integer vector of length==2 giving a
#' range restriction for chunked processing
#' @param maxindex the length of the boolean vector which is represented 
#' @param is.unsorted a logical scalar indicating whether the data may be unsorted 
#' @param has.dup a logical scalar indicating whether the data may have duplicates
#' @param \dots further arguments (passed to \code{\link{which}} for the
#' default method, ignored otherwise)
#' @return a vector of class 'logical' or 'integer'
#' @author Jens Oehlschlägel
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
#' , \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
#' @keywords classes logic
#' @examples
#' 
#'   r <- ri(5, 20, 100)
#'   x <- as.which(r)
#'   x
#' 
#'   stopifnot(identical(x, as.which(as.logical(r))))
#'   stopifnot(identical(x, as.which(as.bitwhich(r))))
#'   stopifnot(identical(x, as.which(as.bit(r))))
#' 
#' @export
as.which <- function (x, ...)
  UseMethod("as.which")


#' Coerce to ri
#'
#' @param x object to coerce
#' @param ... further arguments
#'
#' @return an \code{\link{ri}} object
#' @author Jens Oehlschlägel
#' @seealso \code{\link{CoercionToStandard}}, \code{\link{as.booltype}}, \code{\link{as.bit}}, \code{\link{as.bitwhich}}
#' , \code{\link{as.which}}, \code{\link{as.ri}}, \code{\link[ff]{as.hi}},  \code{\link[ff]{as.ff}}
#' @keywords classes logic
#'
#' @examples
#' as.ri(c(FALSE, TRUE, FALSE, TRUE))
#' @export
as.ri <- function (x, ...)
{
  UseMethod("as.ri")
}



#' Boolean operators and functions
#' 
#' Boolean NEGATION '!', AND '&', OR '|' and EXCLUSIVE OR xor', see \code{\link[base]{Logic}}.
#' 
#' The binary operators and function \code{xor} can now combine any \code{\link{is.booltype}} vectors. 
#' They now recycle if vectors have different length. If the two arguments have different \code{\link{booltypes}} the return value corresponds to the lower \code{\link{booltype}} of the two. \cr
#'
#' Boolean operations on \code{\link{bit}} vectors are extremely fast because they are
#' implemented using C's bitwise operators. Boolean operations on or \code{\link{bitwhich}}  
#' vectors are even faster, if they represent very skewed selections.  \cr
#' 
#' The \code{xor} function has been made generic and \code{xor.default} has
#' been implemented much faster than R's standard \code{\link[base:Logic]{xor}}.
#' This was possible because actually boolean function \code{xor} and
#' comparison operator \code{!=} do the same (even with NAs), and \code{!=} is
#' much faster than the multiple calls in \code{(x | y) & !(x & y)}
#' 
#' @param x a \code{\link{is.booltype}} vector
#' @param y a \code{\link{is.booltype}} vector
#' @param e1 a \code{\link{is.booltype}} vector
#' @param e2 a \code{\link{is.booltype}} vector
#' @return An object of class \code{\link{booltype}} or \code{\link{logical}}
#' @author Jens Oehlschlägel
#' @seealso \code{\link{booltypes}}, \code{\link{Logic}}
#' @keywords classes logic
#' @examples
#' 
#'   x <- c(FALSE, FALSE, FALSE, NA, NA, NA, TRUE, TRUE, TRUE)
#'   y <- c(FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE)
#'   
#'   x|y
#'   x|as.bit(y)
#'   x|as.bitwhich(y)
#'   x|as.which(y)
#'   x|ri(1,1,9)
#'   
#'   
#' @export
xor <- function(x, y)
  UseMethod("xor", x)



#' Physical and virtual attributes
#' 
#' Compatibility functions (to package ff) for getting and setting physical and
#' virtual attributes.
#' 
#' ff objects have physical and virtual attributes, which have different
#' copying semantics: physical attributes are shared between copies of ff
#' objects while virtual attributes might differ between copies.
#' \code{\link[ff:as.ff]{as.ram}} will retain some physical and virtual atrributes in
#' the ram clone, such that \code{\link[ff]{as.ff}} can restore an ff object
#' with the same attributes.
#' 
#' @name PhysVirt
#' @param x a ff or ram object
#' @param value a list with named elements
#' @param \dots further arguments
#' @return \command{physical} and \command{virtual} returns a list with named
#' elements
#' @author Jens Oehlschlägel
#' @seealso \code{\link[ff]{physical.ff}}, \code{\link[ff]{physical.ffdf}}
#' @keywords IO data attribute
#' @examples
#' 
#'   physical(bit(12))
#'   virtual(bit(12))
NULL

#' @rdname PhysVirt
#' @export
physical <- function(x)UseMethod("physical")

#' @rdname PhysVirt
#' @export
"physical<-" <- function(x, value)UseMethod("physical<-")

#' @rdname PhysVirt
#' @export
virtual <- function(x)UseMethod("virtual")

#' @rdname PhysVirt
#' @export
"virtual<-" <- function(x, value)UseMethod("virtual<-")


#' Generics for in-RAM sorting and ordering
#' 
#' These are generic stubs for low-level sorting and ordering methods
#' implemented in packages 'bit64' and 'ff'.  The \code{..sortorder} methods do
#' sorting and ordering at once, which requires more RAM than ordering but is
#' (almost) as fast as as sorting.
#' 
#' The \code{sort} generics do sort their argument 'x', some methods need
#' temporary RAM of the same size as 'x'.  The \code{order} generics do order
#' their argument 'i' leaving 'x' as it was, some methods need temporary RAM of
#' the same size as 'i'.  The \code{sortorder} generics do sort their argument
#' 'x' and order their argument 'i', this way of ordering is much faster at the
#' price of requiring temporary RAM for both, 'x' and 'i', if the method
#' requires temporary RAM.  The \code{ram} generics are high-level functions
#' containing an optimizer that chooses the 'best' algorithms given some
#' context.
#' 
#' @name Sorting
#' @param x a vector to be sorted by \code{\link{ramsort}} and
#' \code{\link{ramsortorder}}, i.e. the output of \code{\link{sort}}
#' @param i integer positions to be modified by \code{\link{ramorder}} and
#' \code{\link{ramsortorder}}, default is 1:n, in this case the output is
#' similar to \code{\link{order}}
#' @param \dots further arguments to the sorting methods
#' @return These functions return the number of \code{NAs} found or assumed
#' during sorting
#' @note Note that these methods purposely violate the functional programming
#' paradigm: they are called for the side-effect of changing some of their
#' arguments.  The rationale behind this is that sorting is very RAM-intensive
#' and in certain situations we might not want to allocate additional memory if
#' not necessary to do so.  The \code{sort}-methods change \code{x}, the
#' \code{order}-methods change \code{i}, and the \code{sortoder}-methods change
#' both \code{x} and \code{i} You as the user are responsible to create copies
#' of the input data 'x' and 'i' if you need non-modified versions.
#' @section Index of implemented methods: \tabular{rrl}{ \bold{generic} \tab
#' \bold{ff} \tab \bold{bit64} \cr \code{ramsort} \tab
#' \code{\link[ff]{ramsort.default}} \tab
#' \code{\link[bit64]{ramsort.integer64}} \cr \code{shellsort} \tab
#' \code{\link[ff:ramsort.default]{shellsort.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{shellsort.integer64}} \cr \code{quicksort} \tab \tab
#' \code{\link[bit64:ramsort.integer64]{quicksort.integer64}} \cr \code{mergesort} \tab
#' \code{\link[ff:ramsort.default]{mergesort.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{mergesort.integer64}} \cr \code{radixsort} \tab
#' \code{\link[ff:ramsort.default]{radixsort.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{radixsort.integer64}} \cr \code{keysort} \tab
#' \code{\link[ff:ramsort.default]{keysort.default}} \tab \cr \cr \bold{generic} \tab \bold{ff}
#' \tab \bold{bit64} \cr \code{ramorder} \tab
#' \code{\link[ff]{ramorder.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{ramorder.integer64}} \cr \code{shellorder} \tab
#' \code{\link[ff:ramorder.default]{shellorder.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{shellorder.integer64}} \cr \code{quickorder} \tab \tab
#' \code{\link[bit64:ramsort.integer64]{quickorder.integer64}} \cr \code{mergeorder} \tab
#' \code{\link[ff:ramorder.default]{mergeorder.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{mergeorder.integer64}} \cr \code{radixorder} \tab
#' \code{\link[ff:ramorder.default]{radixorder.default}} \tab
#' \code{\link[bit64:ramsort.integer64]{radixorder.integer64}} \cr \code{keyorder} \tab
#' \code{\link[ff:ramorder.default]{keyorder.default}} \tab \cr \cr \bold{generic} \tab
#' \bold{ff} \tab \bold{bit64} \cr \code{ramsortorder} \tab \tab
#' \code{\link[bit64:ramsort.integer64]{ramsortorder.integer64}} \cr \code{shellsortorder} \tab
#' \tab \code{\link[bit64:ramsort.integer64]{shellsortorder.integer64}} \cr \code{quicksortorder}
#' \tab \tab \code{\link[bit64:ramsort.integer64]{quicksortorder.integer64}} \cr
#' \code{mergesortorder} \tab \tab
#' \code{\link[bit64:ramsort.integer64]{mergesortorder.integer64}} \cr \code{radixsortorder} \tab
#' \tab \code{\link[bit64:ramsort.integer64]{radixsortorder.integer64}} \cr \code{keysortorder}
#' \tab \tab \cr }
#' @author Jens Oehlschlägel <Jens.Oehlschlaegel@@truecluster.com>
#' @seealso \code{\link{sort}} and \code{\link{order}} in base R, \code{\link{bitsort}} for faster inteer sorting
#' @keywords univar manip arith
NULL

#' @rdname Sorting
#' @export
ramsort <- function(x, ...)UseMethod("ramsort")
#' @rdname Sorting
#' @export
ramorder <- function(x, i, ...)UseMethod("ramorder")
#' @rdname Sorting
#' @export
ramsortorder <- function(x, i, ...)UseMethod("ramsortorder")
#' @rdname Sorting
#' @export
mergesort <- function(x, ...)UseMethod("mergesort")
#' @rdname Sorting
#' @export
mergeorder <- function(x, i, ...)UseMethod("mergeorder")
#' @rdname Sorting
#' @export
mergesortorder <- function(x, i, ...)UseMethod("mergesortorder")
#' @rdname Sorting
#' @export
quicksort <- function(x, ...)UseMethod("quicksort")
#' @rdname Sorting
#' @export
quickorder <- function(x, i, ...)UseMethod("quickorder")
#' @rdname Sorting
#' @export
quicksortorder <- function(x, i, ...)UseMethod("quicksortorder")
#' @rdname Sorting
#' @export
shellsort <- function(x, ...)UseMethod("shellsort")
#' @rdname Sorting
#' @export
shellorder <- function(x, i, ...)UseMethod("shellorder")
#' @rdname Sorting
#' @export
shellsortorder <- function(x, i, ...)UseMethod("shellsortorder")
#' @rdname Sorting
#' @export
radixsort <- function(x, ...)UseMethod("radixsort")
#' @rdname Sorting
#' @export
radixorder <- function(x, i, ...)UseMethod("radixorder")
#' @rdname Sorting
#' @export
radixsortorder <- function(x, i, ...)UseMethod("radixsortorder")
#' @rdname Sorting
#' @export
keysort <- function(x, ...)UseMethod("keysort")
#' @rdname Sorting
#' @export
keyorder <- function(x, i, ...)UseMethod("keyorder")
#' @rdname Sorting
#' @export
keysortorder <- function(x, i, ...)UseMethod("keysortorder")


#' Generics related to cache access
#' 
#' These generics are packaged here for methods in packages \code{bit64} and
#' \code{ff}.
#' 
#' see help of the available methods
#' 
#' @name Metadata
#' @param x some object
#' @param value value assigned on responsibility of the user
#' @param \dots ignored
#' @return see help of the available methods
#' @author Jens Oehlschlägel <Jens.Oehlschlaegel@@truecluster.com>
#' @seealso \code{\link[bit64]{is.sorted.integer64}},
#' \code{\link[bit64:is.sorted.integer64]{na.count.integer64}},
#' \code{\link[bit64:is.sorted.integer64]{nvalid.integer64}},
#' \code{\link[bit64:is.sorted.integer64]{nunique.integer64}}, \code{\link[bit64:is.sorted.integer64]{nties.integer64}}
#' \cr
#' @keywords environment methods
#' @examples
#' 	methods("na.count")
NULL

#' @rdname Metadata
#' @export
is.sorted <- function(x, ...)UseMethod("is.sorted")

#' @rdname Metadata
#' @export
"is.sorted<-" <- function(x, ..., value)UseMethod("is.sorted<-")

#' @rdname Metadata
#' @export
na.count <- function(x, ...)UseMethod("na.count")

#' @rdname Metadata
#' @export
"na.count<-" <- function(x, ..., value)UseMethod("na.count<-")

#' @rdname Metadata
#' @export
nvalid <- function(x, ...)UseMethod("nvalid")

#' @rdname Metadata
#' @export
nunique <- function(x, ...)UseMethod("nunique")

#' @rdname Metadata
#' @export
"nunique<-" <- function(x, ..., value)UseMethod("nunique<-")

#' @rdname Metadata
#' @export
nties <- function(x, ...)UseMethod("nties")

#' @rdname Metadata
#' @export
"nties<-" <- function(x, ..., value)UseMethod("nties<-")

