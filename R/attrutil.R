# attribute utilities for ff and bit
# (c) 2010 Jens Oehlschlägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2007-08-24
# Last changed: 2007-10-25

# WARNING: these functions are called for the side-effect of changing their arguments
# this can save RAM and avoid unnecessary copying
# but it can easily have unexpected effects
# Only use them if you know what you do - and even then think twice

#' Attribute setting by reference
#' 
#' Function \code{setattr} sets a singe attribute and function
#' \code{setattributes} sets a list of attributes.
#' 
#' The attributes of 'x' are changed in place without copying x. function
#' \code{setattributes} does only change the named attributes, it does not
#' delete the non-names attributes like \code{\link{attributes}} does.
#' 
#' @param x an R object
#' @param which name of the attribute
#' @param value value of the attribute, use NULL to remove this attribute
#' @param attributes a named list of attribute values
#' @return invisible(), we do not return the changed object to remind you of
#' the fact that this function is called for its side-effect of changing its
#' input object.
#' @author Jens Oehlschlägel
#' @seealso \code{\link{attr}} \code{\link{unattr}}
#' @references Writing R extensions -- System and foreign language interfaces
#' -- Handling R objects in C -- Attributes (Version 2.11.1 (2010-06-03 ) R
#' Development)
#' @keywords attributes
#' @examples
#' 
#'   x <- as.single(runif(10))
#'   attr(x, "Csingle")
#' 
#'   f <- function(x)attr(x, "Csingle") <- NULL
#'   g <- function(x)setattr(x, "Csingle", NULL)
#' 
#'   f(x)
#'   x
#'   g(x)
#'   x
#' 
#'  \dontrun{
#' 
#'   # restart R
#'   library(bit)
#' 
#'   mysingle <- function(length = 0){
#'     ret <- double(length)
#'     setattr(ret, "Csingle", TRUE)
#'     ret
#'   }
#' 
#'   # show that mysinge gives exactly the same result as single
#'   identical(single(10), mysingle(10))
#' 
#'   # look at the speedup and memory-savings of mysingle compared to single
#'   system.time(mysingle(1e7))
#'   memory.size(max=TRUE)
#'   system.time(single(1e7))
#'   memory.size(max=TRUE)
#' 
#'   # look at the memory limits
#'   # on my win32 machine the first line fails beause of not enough RAM, the second works
#'   x <- single(1e8)
#'   x <- mysingle(1e8)
#' 
#'   # .g. performance with factors
#'   x <- rep(factor(letters), length.out=1e7)
#'   x[1:10]
#'   # look how fast one can do this
#'   system.time(setattr(x, "levels", rev(letters)))
#'   x[1:10]
#'   # look at the performance loss in time caused by the non-needed copying
#'   system.time(levels(x) <- letters)
#'   x[1:10]
#' 
#' 
#'   # restart R
#'   library(bit)
#' 
#'   simplefactor <- function(n){
#'     factor(rep(1:2, length.out=n))
#'   }
#' 
#'   mysimplefactor <- function(n){
#'     ret <- rep(1:2, length.out=n)
#'     setattr(ret, "levels", as.character(1:2))
#'     setattr(ret, "class", "factor")
#'     ret
#'   }
#' 
#'   identical(simplefactor(10), mysimplefactor(10))
#' 
#'   system.time(x <- mysimplefactor(1e7))
#'   memory.size(max=TRUE)
#'   system.time(setattr(x, "levels", c("a","b")))
#'   memory.size(max=TRUE)
#'   x[1:4]
#'   memory.size(max=TRUE)
#'   rm(x)
#'   gc()
#' 
#'   system.time(x <- simplefactor(1e7))
#'   memory.size(max=TRUE)
#'   system.time(levels(x) <- c("x","y"))
#'   memory.size(max=TRUE)
#'   x[1:4]
#'   memory.size(max=TRUE)
#'   rm(x)
#'   gc()
#' 
#' }
#' 
#' 
#' @export
getsetattr <- function(x, which, value)
{
  ret <- copy_vector(attr(x, which))
  .Call(C_R_bit_set_attr, x, which, value)
  ret
}

#' @describeIn getsetattr  
#' @export
setattr <- function(x, which, value)
{
  .Call(C_R_bit_set_attr, x, which, value)
  invisible()
}

#' @describeIn getsetattr  
#' @export
setattributes <- function(x, attributes)
{
  nam <- names(attributes)
  for (i in seq_len(length(attributes))){
    .Call(C_R_bit_set_attr, x, nam[[i]], attributes[[i]])
  }
  invisible()
}


#' Attribute removal
#' 
#' Returns object with attributes removed
#' 
#' attribute removal copies the object as usual
#' 
#' @param x any R object
#' @return a similar object with attributes removed
#' @author Jens Oehlschlägel
#' @seealso \code{\link{attributes}}, \code{\link{setattributes}},
#' \code{\link{unclass}}
#' @keywords attribute
#' @examples
#' 
#'   bit(2)[]
#'   unattr(bit(2)[])
#' 
#' @export

unattr <- function(x){
  attributes(x) <- NULL
  x
}

