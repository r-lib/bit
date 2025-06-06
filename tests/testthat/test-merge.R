# map NAs to position 1
NAtab <- function(x, nbins=max(1L, x, na.rm = TRUE)) {
  force(nbins)
  x <- x + 1L
  x[is.na(x)] <- 1L
  tabulate(x, nbins + 1L)
}

test_that("positive merging is OK with NAs", {
  xs <- list(
    rep(3:7, rep(1L, 5L)),
    rep(3:7, rep(2L, 5L)),
    rep(c(3L, 5L, 7L), rep(1L, 3L)),
    rep(c(3L, 5L, 7L), rep(2L, 3L))
  )
  ys <- list(
    rep(1:9, rep(1L, 9L)),
    rep(1:9, rep(2L, 9L)),
    rep(2:8, rep(1L, 7L)),
    rep(2:8, rep(2L, 7L)),
    rep(3:7, rep(1L, 5L)),
    rep(3:7, rep(2L, 5L)),
    rep(c(3L, 5L, 7L), rep(1L, 3L)),
    rep(c(3L, 5L, 7L), rep(2L, 3L)),
    rep(4:6, rep(1L, 3L)),
    rep(4:6, rep(2L, 3L)),
    rep(5L, 1L),
    rep(5L, 2L),
    rep(5L, 3L),
    rep(1L, 1L),
    rep(1L, 1L),
    rep(1L, 2L),
    rep(1:2, rep(1L, 2L)),
    rep(1:2, rep(2L, 2L)),
    rep(1:3, rep(1L, 3L)),
    rep(1:3, rep(2L, 3L)),
    rep(1:4, rep(1L, 4L)),
    rep(1:4, rep(2L, 4L)),
    rep(1:5, rep(1L, 5L)),
    rep(1:5, rep(2L, 5L)),
    rep(5:9, rep(1L, 5L)),
    rep(5:9, rep(2L, 5L)),
    rep(6:9, rep(1L, 4L)),
    rep(6:9, rep(2L, 4L)),
    rep(7:9, rep(1L, 3L)),
    rep(7:9, rep(2L, 3L)),
    rep(8:9, rep(1L, 2L)),
    rep(8:9, rep(2L, 2L)),
    rep(9L, 1L),
    rep(9L, 1L),
    rep(9L, 2L)
  )
  for (xi in seq_along(xs)) {
    for (yi in seq_along(ys)) {
      x <- sort.int(xs[[xi]], na.last=FALSE, method="quick")
      y <- sort.int(ys[[yi]], na.last=FALSE, method="quick")
      env = list(x=x, y=y)
      eval(substitute(env=env, expect_identical(
        merge_union(x, y, method="all"),
        sort.int(c(x, y), na.last=FALSE, method="quick")
      )))
      eval(substitute(env=env, expect_identical(
        merge_union(x, y, method="exact"),
        rep(c(NA, 1:9), pmax(NAtab(x, 9), NAtab(y, 9)))
      )))
      eval(substitute(env=env, expect_identical(
        merge_intersect(x, y, method="exact"),
        rep(c(NA, 1:9), pmin(NAtab(x, 9), NAtab(y, 9)))
      )))
      eval(substitute(env=env, expect_identical(
        merge_setdiff(x, y, method="exact"),
        rep(c(NA, 1:9), pmax(0L, NAtab(x, 9) - NAtab(y, 9)))
      )))
      eval(substitute(env=env, expect_identical(
        merge_symdiff(x, y, method="exact"),
        rep(c(NA, 1:9), abs(NAtab(x, 9) - NAtab(y, 9)))
      )))
      eval(substitute(env=env, expect_identical(
        merge_union(x, y),
        sort.int(union(x, y), na.last=FALSE, method="quick")
      )))
      eval(substitute(env=env, expect_identical(
        merge_intersect(x, y),
        sort.int(intersect(x, y), na.last=FALSE, method="quick")
      )))
      eval(substitute(env=env, expect_identical(
        merge_setdiff(x, y),
        sort.int(setdiff(x, y), na.last=FALSE, method="quick")
      )))
      eval(substitute(env=env, expect_identical(
        merge_symdiff(x, y),
        sort.int(union(setdiff(x, y), setdiff(y, x)), na.last=FALSE, method="quick")
      )))
    }
  }
})


test_that("reversed merging is OK (without NAs)", {
  set.seed(1)
  for (i in 1:24) {
    x <- sort.int(sample(9, replace=TRUE), na.last=FALSE, method="quick")
    y <- sort.int(sample(9, replace=TRUE), na.last=FALSE, method="quick")
    for (revx in c(FALSE, TRUE)) {
      if (revx)
        rx <- rev(-x)
      else
        rx <- x
      for (revy in c(FALSE, TRUE)) {
        if (revy)
          ry <- rev(-y)
        else
          ry <- y
        eval(substitute(expect_identical(
          merge_union(rx, ry, revx=revx, revy=revy, method="all")
          , sort.int(c(x, y), na.last=FALSE, method="quick")
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_union(rx, ry, revx=revx, revy=revy, method="exact")
          , rep(c(NA, 1:9), pmax(NAtab(x, 9), NAtab(y, 9)))
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_intersect(rx, ry, revx=revx, revy=revy, method="exact")
          , rep(c(NA, 1:9), pmin(NAtab(x, 9), NAtab(y, 9)))
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_setdiff(rx, ry, revx=revx, revy=revy, method="exact")
          , rep(c(NA, 1:9), pmax(0L, NAtab(x, 9) - NAtab(y, 9)))
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_symdiff(rx, ry, revx=revx, revy=revy, method="exact")
          , rep(c(NA, 1:9), abs(NAtab(x, 9) - NAtab(y, 9)))
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_union(rx, ry, revx=revx, revy=revy)
          , sort.int(union(x, y), na.last=FALSE, method="quick")
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_intersect(rx, ry, revx=revx, revy=revy)
          , sort.int(intersect(x, y), na.last=FALSE, method="quick")
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_setdiff(rx, ry, revx=revx, revy=revy)
          , sort.int(setdiff(x, y), na.last=FALSE, method="quick")
        ), list(x=x, y=y, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_symdiff(rx, ry, revx=revx, revy=revy)
          , sort.int(union(setdiff(x, y), setdiff(y, x)), na.last=FALSE, method="quick")
        ), list(x=x, y=y, revx=revx, revy=revy)))

      }
    }
  }

})


test_that("for-looped merging is OK (without NAs)", {
  nx <- 9
  x <- 1:nx
  set.seed(1)
  for (i in 1:12) {
    y <- sort.int(sample(nx, replace=TRUE), na.last=FALSE, method="quick")
    for (revx in c(FALSE, TRUE)) {
      if (revx) {
        rx <- rev(-x)
        rnx <- c(-nx, -1L)
      } else {
        rx <- x
        rnx <- c(1L, nx)
      }
      for (revy in c(FALSE, TRUE)) {
        if (revy) {
          ry <- rev(-y)
        } else {
          ry <- y
        }
        eval(substitute(expect_identical(
          merge_rangesect(rnx, ry, revx=revx, revy=revy)
          , merge_intersect(rx, ry, revx=revx, revy=revy)
        ), list(rnx=rnx, rx=rx, ry=ry, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_rangediff(rnx, ry, revx=revx, revy=revy)
          , merge_setdiff(rx, ry, revx=revx, revy=revy)
        ), list(rnx=rnx, rx=rx, ry=ry, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_rangein(rnx, ry, revx=revx, revy=revy)
          , copy_vector(rx, revx=revx) %in% copy_vector(ry, revx=revy)
        ), list(rnx=rnx, rx=rx, ry=ry, revx=revx, revy=revy)))
        eval(substitute(expect_identical(
          merge_rangenotin(rnx, ry, revx=revx, revy=revy)
          , !(copy_vector(rx, revx=revx) %in% copy_vector(ry, revx=revy))
        ), list(rnx=rnx, rx=rx, ry=ry, revx=revx, revy=revy)))
      }
    }
  }

})
