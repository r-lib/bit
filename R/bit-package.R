# Package documentation
# (c) 2008-2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk

#' bit: Classes and methods for fast memory-efficient boolean selections
#' 
#' Provided are classes for boolean and skewed boolean vectors, fast boolean 
#' methods, fast unique and non-unique integer sorting, fast set operations on 
#' sorted and unsorted sets of integers, and foundations for ff (range indices, 
#' compression, chunked processing).
#' 
#' For details view the vignettes \url{../doc/bit-usage.pdf} and
#' \url{../doc/bit-performance.pdf}
#'
#'@name bit-package
NULL

# devtools::use_vignette("bit-usage")
# devtools::use_vignette("bit-performance")
require(rhub)
rhub_bit_4.0.3 <- check_for_cran(
  path = "../bit_4.0.3.tar.gz"
, email = "Jens.Oehlschlaegel@truecluster.com"
, check_args = "--as-cran"
, env_vars = c('_R_CHECK_FORCE_SUGGESTS_'= "false",'_R_CHECK_CRAN_INCOMING_USE_ASPELL_'= "true", '_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_'="true")
, platforms = NULL
, show_status = FALSE
)


