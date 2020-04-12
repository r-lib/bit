library("testthat")
library("bit")

context("rle")

test_that("intisasc is correct", {
  expect_identical(intisasc(c(as.integer(NA,NA)), "none"), TRUE)
  expect_identical(intisasc(c(NA,1L), "none"), TRUE)
  expect_identical(intisasc(c(1L,1L), "none"), TRUE)
  expect_identical(intisasc(c(1L,NA), "none"), FALSE)
  expect_identical(intisasc(c(NA,1L,NA,2L,NA), "skip"), TRUE)
  expect_identical(intisasc(c(NA,1L,NA,1L,NA), "skip"), TRUE)
  expect_identical(intisasc(c(NA,2L,NA,1L,NA), "skip"), FALSE)
  expect_identical(intisasc(c(1L,NA,2L), "break"), NA)
  expect_identical(intisasc(c(1L,2L), "break"), TRUE)
  expect_identical(intisasc(c(1L,1L), "break"), TRUE)
  expect_identical(intisasc(c(2L,1L), "break"), FALSE)
  expect_identical(intisasc(c(1L,2L)), TRUE)
  expect_identical(intisasc(c(1L,1L)), TRUE)
  expect_identical(intisasc(c(2L,1L)), FALSE)
})

test_that("intisdesc is correct", {
  expect_identical(intisdesc(c(as.integer(NA,NA)), "none"), TRUE)
  expect_identical(intisdesc(c(NA,1L), "none"), FALSE)
  expect_identical(intisdesc(c(1L,1L), "none"), TRUE)
  expect_identical(intisdesc(c(1L,NA), "none"), TRUE)
  expect_identical(intisdesc(c(NA,1L,NA,2L,NA), "skip"), FALSE)
  expect_identical(intisdesc(c(NA,1L,NA,1L,NA), "skip"), TRUE)
  expect_identical(intisdesc(c(NA,2L,NA,1L,NA), "skip"), TRUE)
  expect_identical(intisdesc(c(1L,NA,2L), "break"), NA)
  expect_identical(intisdesc(c(1L,2L), "break"), FALSE)
  expect_identical(intisdesc(c(1L,1L), "break"), TRUE)
  expect_identical(intisdesc(c(2L,1L), "break"), TRUE)
  expect_identical(intisdesc(c(as.integer(NA,NA))), TRUE)
  expect_identical(intisdesc(c(NA,1L)), FALSE)
  expect_identical(intisdesc(c(1L,1L)), TRUE)
  expect_identical(intisdesc(c(1L,NA)), TRUE)
})

# takes_less_than has been deprecated
# test_that("intisasc and intisdesc early terminate correctly", {
#   x <- y <- integer(1e7)
#   x[1] <- NA
#   expect_that(intisasc(x, "break"), takes_less_than(max(0.01,system.time(intisasc(y, "break"))[3])))
#   expect_that(intisdesc(x, "break"), takes_less_than(max(0.01,system.time(intisasc(y, "break"))[3])))
#   x[1] <- 1L
#   expect_that(intisasc(x, "skip"), takes_less_than(max(0.01,system.time(intisasc(y, "skip"))[3])))
#   expect_that(intisasc(x, "none"), takes_less_than(max(0.01,system.time(intisasc(y, "none"))[3])))
#   x[1] <- -1L
#   expect_that(intisdesc(x, "skip"), takes_less_than(max(0.01,system.time(intisasc(y, "skip"))[3])))
#   expect_that(intisdesc(x, "none"), takes_less_than(max(0.01,system.time(intisasc(y, "none"))[3])))
# })

test_that("intrle is correct", {
  expect_identical(intrle(c(rep(1L,60), 1:30))
  , structure(list(lengths = c(61L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), values = 1:30)
  , .Names = c("lengths", "values"), class = "rle"))
  expect_identical(intrle(c(rep(1L,60), 1:31)), NULL)
})

test_that("rlepack, rleunpack rev.rlepack, unique.rlepack and anyDuplicated.rlepack work correctly", {
  for (x in list(10:1, 1:10, c(10:1,1:10), c(1:10,10:1), sample(100), sample(100, 100, TRUE), sample(10, 100, TRUE))){
    expect_identical(rleunpack(rlepack(x)), x)
    expect_identical(rleunpack(rev(rlepack(x))), rev(x))
    expect_identical(rleunpack(unique(rlepack(x))), unique(x))
    expect_identical(anyDuplicated(rlepack(x)), anyDuplicated(x))
  }
})

