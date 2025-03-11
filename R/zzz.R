# Package intialization
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

# nolint next: coalesce_linter. obv ;-)
if (!exists("%||%", "package:base")) `%||%` = function(x, y) if (is.null(x)) y else x

#' @useDynLib bit, .registration = TRUE, .fixes = "C_"
#' @importFrom utils packageDescription
.onLoad <- function(lib, pkg) {
  bit_init()
}

.onUnload <- function(libpath) {
  bit_done()
  library.dynam.unload("bit", libpath)
}
