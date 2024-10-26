test_that("intisasc is correct", {
  expect_true(intisasc(c(as.integer(NA,NA)), "none"))
  expect_true(intisasc(c(NA,1L), "none"))
  expect_true(intisasc(c(1L,1L), "none"))
  expect_false(intisasc(c(1L,NA), "none"))
  expect_true(intisasc(c(NA,1L,NA,2L,NA), "skip"))
  expect_true(intisasc(c(NA,1L,NA,1L,NA), "skip"))
  expect_false(intisasc(c(NA,2L,NA,1L,NA), "skip"))
  expect_identical(intisasc(c(1L,NA,2L), "break"), NA)
  expect_true(intisasc(c(1L,2L), "break"))
  expect_true(intisasc(c(1L,1L), "break"))
  expect_false(intisasc(c(2L,1L), "break"))
  expect_true(intisasc(c(1L,2L)))
  expect_true(intisasc(c(1L,1L)))
  expect_false(intisasc(c(2L,1L)))
})

test_that("intisdesc is correct", {
  expect_true(intisdesc(c(as.integer(NA,NA)), "none"))
  expect_false(intisdesc(c(NA,1L), "none"))
  expect_true(intisdesc(c(1L,1L), "none"))
  expect_true(intisdesc(c(1L,NA), "none"))
  expect_false(intisdesc(c(NA,1L,NA,2L,NA), "skip"))
  expect_true(intisdesc(c(NA,1L,NA,1L,NA), "skip"))
  expect_true(intisdesc(c(NA,2L,NA,1L,NA), "skip"))
  expect_identical(intisdesc(c(1L,NA,2L), "break"), NA)
  expect_false(intisdesc(c(1L,2L), "break"))
  expect_true(intisdesc(c(1L,1L), "break"))
  expect_true(intisdesc(c(2L,1L), "break"))
  expect_true(intisdesc(c(as.integer(NA,NA))))
  expect_false(intisdesc(c(NA,1L)))
  expect_true(intisdesc(c(1L,1L)))
  expect_true(intisdesc(c(1L,NA)))
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
  x = c(rep(1L, 60), 1:30)
  expect_identical(intrle(x), rle(x))
  expect_null(intrle(c(rep(1L,60), 1:31)))
})

test_that("rlepack, rleunpack rev.rlepack, unique.rlepack and anyDuplicated.rlepack work correctly", {
  for (x in list(10:1, 1:10, c(10:1,1:10), c(1:10,10:1), sample(100), sample(100, 100, TRUE), sample(10, 100, TRUE))){
    expect_identical(rleunpack(rlepack(x)), x)
    expect_identical(rleunpack(rev(rlepack(x))), rev(x))
    expect_identical(rleunpack(unique(rlepack(x))), unique(x))
    expect_identical(anyDuplicated(rlepack(x)), anyDuplicated(x))
  }
})
