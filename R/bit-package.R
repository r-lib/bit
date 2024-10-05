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
