# Package intialization
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk


#' @useDynLib bit, .registration = TRUE, .fixes = "C_"
#' @importFrom utils packageDescription
.onLoad <- function(lib, pkg) {
  ##library.dynam("bit", pkg, lib) use useDynLib(bit, .registration = TRUE, .fixes = "C_") in NAMESPACE instead
  ##packageStartupMessage("Loading package bit ", packageDescription("bit", fields="Version"))
  bit_init()
}

# .onAttach <- function(libname, pkgname){
#   packageStartupMessage("Attaching package bit")
  # packageStartupMessage("package:bit (c) 2008-2017 Jens Oehlschlaegel (GPL-2)")
  # packageStartupMessage("creators: bit bitwhich")
  # packageStartupMessage("coercion: as.logical as.integer as.bit as.bitwhich which")
  # packageStartupMessage("operator: ! & | xor != ==")
  # packageStartupMessage("querying: print length any all min max range sum summary")
  # packageStartupMessage("bit access: length<- [ [<- [[ [[<-")
  # packageStartupMessage("for more help type ?bit")
# }

.onUnload <- function(libpath){
   #packageStartupMessage("Unloading package bit")
   bit_done()
   library.dynam.unload("bit", libpath)
}


# require(rhub)
# rhub_bit_4.5.0 <- check_for_cran(
#   path = "../bit_4.5.0.tar.gz"
# , email = "Jens.Oehlschlaegel@truecluster.com"
# , check_args = "--as-cran"
# , env_vars = c('_R_CHECK_FORCE_SUGGESTS_'= "false",'_R_CHECK_CRAN_INCOMING_USE_ASPELL_'= "true", '_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_'="true")
# , platforms = NULL
# , show_status = FALSE
# )

# > require(rhub)
# > rhub_setup()
# > rhub_doctor()
# > rhub_check(platforms = c("linux", "macos", "windows", "ubuntu-clang", "ubuntu-gcc12"))
# > rhub_check(platforms = c("linux", "macos", "windows"))
# ✔ Found git repository at /home/jo/SIK/truecluster/bit.
# ✔ Found GitHub PAT.                                 
# ✔ Check started: linux, macos, windows, ubuntu-clang, ubuntu-gcc12 (aspherical-sphinx).
# See <https://github.com/truecluster/bit/actions> for live output!
