# timing utilities for ff and bit
# (c) 2012 Jens Oehlschlägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2012-05-28

#' Adaptive timer
#'
#' Repeats timing expr until minSec is reached
#'
#'
#' @param expr Valid expression to be timed.
#' @param gcFirst Logical - should a garbage collection be performed
#' immediately before the timing?  Default is `TRUE`.
#' @param minSec number of seconds to repeat at least
#' @param envir the environment in which to evaluate `expr` (by default
#' the calling frame)
#' @return A object of class `"proc_time"`: see [proc.time()]
#' for details.
#' @author Jens Oehlschlägel <Jens.Oehlschlaegel@@truecluster.com>
#' @seealso [system.time()]
#' @keywords utilities
#' @examples
#'
#'   system.time(1 + 1)
#'   repeat.time(1 + 1)
#'   system.time(sort(runif(1e6)))
#'   repeat.time(sort(runif(1e6)))
#'
#' @export
repeat.time <- function(expr, gcFirst = TRUE, minSec=0.5, envir=parent.frame()) {
  ppt <- function(y) {
    if (!is.na(y[4L]))
      y[1L] <- y[1L] + y[4L]
    if (!is.na(y[5L]))
      y[2L] <- y[2L] + y[5L]
    y[1L:3L]
  }
  if (!exists("proc.time"))
    return(rep(NA_real_, 5L))
  if (gcFirst)
    gc(FALSE)
  time0 <- proc.time()
  on.exit(cat("Timing stopped at:", ppt(proc.time() - time0), "\n"))
  r <- 0L
  while ((proc.time()[3L] - time0[3L]) < minSec) {
    r <- r + 1L
    eval(substitute(expr), envir=envir)
  }
  new.time <- proc.time()
  on.exit()
  out = (new.time - time0) / r
  class(out) = "proc_time"
  out
}
