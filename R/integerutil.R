# Integer utilities
# (c) 2016-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

## Get C reference count of a vector
##
## Gets C reference count of a vector
##
## Queries the vector reference count using C-macro `NAMED`
##
## @param x a vector
##
## @return integer scalar
## @seealso [get_parent_refcnt()], [set_refcnt()]
##
## @examples
## x <- integer()
## get_refcnt(x)
## y <- x
## get_refcnt(x)
## z <- x
## get_refcnt(x)
##
#get_refcnt <- function(x){
#  y <- substitute(x)
#  if (is.name(y)){
#    .Call(C_R_get_refcnt, get(as.character(y), envir=parent.frame()))
#  } else
#    stop("x must be the name of an object")
#}

## Set C reference count of a vector
##
## Sets C reference count of a vector
##
## Manipulates the vector reference count using C-macro `SET_NAMED`
##
## @note Do use this only if you know what you do! Otherwise unexpected behaviour can result!
##
## @param x a vector
## @param refcnt integer scalar to be assigned (must be 1L or 2L)
##
## @return integer scalar
## @seealso @seealso [get_refcnt()], [get_parent_refcnt()]
##
## @examples
## x <- integer()
## get_refcnt(x)
## y <- x
## get_refcnt(x)
## z <- x
## get_refcnt(x)
#set_refcnt <- function(x, refcnt=1L){
#  invisible(.Call(C_R_set_refcnt, x, as.integer(refcnt)))
#}

## Get C pareet reference count of a vector
##
## Gets C reference count of a vector in parent frame
##
## Queries the vector reference count using C-macro `NAMED`
##
## @param x a vector
##
## @return integer scalar
## @seealso [get_refcnt()], [set_refcnt()]
##
## @examples
## f <- function(x)c(refcnt=get_refcnt(x), parent_refcnt=get_parent_refcnt(x))
## x <- integer()
## f(x)
## y <- x
## f(x)
## z <- x
## f(x)
## f(0)
## try(get_parent_refcnt(0))
#get_parent_refcnt <- function(x){
#  y <- substitute(x)
#  if (is.name(y)){
#    z <- eval(call("substitute", y), parent.frame())
#    if (is.name(z)){
#      .Call(C_R_get_refcnt, get(as.character(z), envir=parent.frame(2)))
#    }else
#      0L
#  } else
#    stop("x must be the name of an object")
#}

# named <- function(x)
# .Call(C_R_bit_named, x)



#' Get C length of a vector
#'
#' Gets C length of a vector ignoring any length-methods dispatched by classes
#'
#' Queries the vector length using C-macro `LENGTH`, this can be substantially faster than
#'   `length(unclass(x))`
#'
#' @param x a vector
#' @return integer scalar
#' @examples
#' length(bit(12))
#' get_length(bit(12))
#' @export
get_length <- function(x){
  .Call(C_R_get_length, x)
}


#' Test for C-level identity of two atomic vectors
#'
#' @param x an atomic vector
#' @param y an atomic vector
#' @return logical scalar
#' @examples
#' x <- 1:2
#' y <- x
#' z <- copy_vector(x)
#' still.identical(y,x)
#' still.identical(z,x)
#' @export
still.identical <- function(x, y){
  .Call(C_R_still_identical, x = x, y = y)
}

#' Copy atomic R vector
#'
#' Creates a true copy of the underlying C-vector -- dropping all attributes -- and
#'   optionally reverses the direction of the elements.
#'
#' This can be substantially faster than `duplicate(as.vector(unclass(x)))`
#'
#' @param x an R vector
#' @param revx default `FALSE`, set to `TRUE` to reverse the elements in 'x'
#' @return copied R vector
#' @seealso [clone()], [still.identical()],  [reverse_vector()]
#' @examples
#' x <- factor(letters)
#' y <- x
#' z <- copy_vector(x)
#' still.identical(x,y)
#' still.identical(x,z)
#' str(x)
#' str(y)
#' str(z)
#' @export
copy_vector <- function(x, revx=FALSE){
  .Call(C_R_copy_vector, x, as.logical(revx))
}


#' Reverse atomic vector
#'
#' Returns a reversed copy -- with attributes retained.
#'
#' This is substantially faster than [rev()]
#'
#' @param x an R vector
#' @return a reversed vector
#' @seealso [rev()], [copy_vector()]
#' @examples
#' x <- factor(letters)
#' rev(x)
#' reverse_vector(x)
#' \dontrun{
#' x <- 1:1e7
#' system.time(rev(x))
#' system.time(reverse_vector(x))
#' }
#' @export
reverse_vector <- function(x){
  .Call(C_R_reverse_vector, x)
}

#' Position of first NA
#'
#' This is substantially faster than `which.max(is.na(x))`
#'
#' @param x an R vector
#' @return a reversed vector
#' @seealso [which.max()], [is.na()], [anyNA()], [anyDuplicated()], [bit_anyDuplicated()]
#' @examples
#' x <- c(FALSE,NA,TRUE)
#' firstNA(x)
#' reverse_vector(x)
#' \dontrun{
#' x <- 1:1e7
#' system.time(rev(x))
#' system.time(reverse_vector(x))
#' }
#' @export
firstNA <- function(x){
  .Call(C_R_firstNA, x)
}
