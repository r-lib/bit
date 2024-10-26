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

test_that("intrle is correct", {
  x = c(rep(1L, 60), 1:30)
  expect_identical(intrle(x), rle(x))
  expect_null(intrle(c(rep(1L,60), 1:31)))
})

test_that("rlepack, rleunpack, and rlepack methods for rev, uniqu and anyDuplicated work", {
  inputs = list(
    10:1,
    1:10,
    c(10:1,1:10),
    c(1:10,10:1),
    sample(100),
    sample(100, 100, TRUE),
    sample(10, 100, TRUE)
  )
  for (x in inputs){
    expect_identical(rleunpack(rlepack(x)), x)
    expect_identical(rleunpack(rev(rlepack(x))), rev(x))
    expect_identical(rleunpack(unique(rlepack(x))), unique(x))
    expect_identical(anyDuplicated(rlepack(x)), anyDuplicated(x))
  }
})
