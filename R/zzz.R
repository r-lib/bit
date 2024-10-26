# Package intialization
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

#' @useDynLib bit, .registration = TRUE, .fixes = "C_"
#' @importFrom utils packageDescription
.onLoad <- function(lib, pkg) {
  bit_init()
}

.onUnload <- function(libpath){
   #packageStartupMessage("Unloading package bit")
   bit_done()
   library.dynam.unload("bit", libpath)
}
