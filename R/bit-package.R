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

# require(rhub)
# rhub_bit_4.0.5 <- check_for_cran(
#   path = "../bit_4.0.5.tar.gz"
# , email = "Jens.Oehlschlaegel@truecluster.com"
# , check_args = "--as-cran"
# , env_vars = c('_R_CHECK_FORCE_SUGGESTS_'= "false",'_R_CHECK_CRAN_INCOMING_USE_ASPELL_'= "true", '_R_CHECK_XREFS_MIND_SUSPECT_ANCHORS_'="true")
# , platforms = NULL
# , show_status = FALSE
# )
# 
# ─  Uploading package
# ─  Preparing build, see status at
# https://builder.r-hub.io/status/bit_4.0.5.tar.gz-ca2c830386114704b24680ea90fdeb54
# https://builder.r-hub.io/status/bit_4.0.5.tar.gz-a404334be2774d65ae824aaea7b4e25f
# https://builder.r-hub.io/status/bit_4.0.5.tar.gz-988366f4387f4a72b8cd3dfab9f6c005
# https://builder.r-hub.io/status/bit_4.0.5.tar.gz-95a6561611774578b9a4d957f8b61739


# olddir <- "../revdepold"
# newdir <- "../revdepnew"
# tools::check_packages_in_dir(olddir,
#                              check_args = c("--as-cran", ""),
#                              reverse = list(repos = getOption("repos")["CRAN"]))
# tools::check_packages_in_dir(newdir, old=olddir
#                              check_args = c("--as-cran", ""),
#                              reverse = list(repos = getOption("repos")["CRAN"]))
# tools::summarize_check_packages_in_dir_results(newdir, all = FALSE, full = TRUE)
# tools::check_packages_in_dir_changes(newdir, olddir, outputs = TRUE, sources = FALSE)
