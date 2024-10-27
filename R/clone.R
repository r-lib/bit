# clone utilities for bit,bit64,ff
# (c) 2014 Jens Oehlschlägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2014-03-02


#' Cloning ff and ram objects
#'
#' `clone` physically duplicates objects and can additionally change
#' some features, e.g. length.
#'
#' `clone` is generic.  `clone.default` handles ram objects.
#' Further methods are provided in package 'ff'.
#' `still.identical` returns TRUE if the two atomic arguments still
#' point to the same memory.
#'
#' @param x `x` an R object
#' @param ... further arguments to the generic
#' @return an object that is a deep copy of x
#' @author Jens Oehlschlägel
#' @seealso `clone.ff`, [copy_vector()]
#' @keywords IO data
#' @examples
#'
#'   x <- 1:12
#'   y <- x
#'   still.identical(x, y)
#'   y[1] <- y[1]
#'   still.identical(x, y)
#'   y <- clone(x)
#'   still.identical(x, y)
#'   rm(x, y); gc()
#'
#' @export
clone  <- function(x, ...) UseMethod("clone")

#' @describeIn clone default method uses R's C-API 'duplicate()'
#' @export
clone.default <- function(x, ...) .Call(C_R_duplicate, x)
