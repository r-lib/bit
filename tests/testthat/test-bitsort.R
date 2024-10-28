test_that("bitsort function work on several inputs", {
  nums <- list(
    integer(),
    as.integer(9:0),
    as.integer(0:9),
    as.integer(0:(-9)),
    as.integer((-9):0),
    as.integer((-4):4),
    as.integer(4:(-4))
  )
  nas <- list(
    integer(),
    NA_integer_,
    c(NA_integer_, NA_integer_)
  )
  set.seed(1)
  for (num in nums) {
    for (na in nas) {
      x <- c(num, na)
      if (length(x) > 1)
        x <- sample(x)

      range_result <- suppressWarnings(as.integer(c(range(x, na.rm=TRUE), sum(is.na(x)))))
      expect_identical(
        range_na(x),
        range_result,
        label=paste0("range_na(c(", toString(x), "))")
      )

      range_result <- suppressWarnings(as.integer(c(
        range(x[is.na(x) | x != 0], na.rm=TRUE),
        sum(is.na(x))
      )))
      expect_identical(
        attr(range_nanozero(x), "range_na"),
        range_result,
        label=paste0("range_na(c(", toString(x), ")")
      )

      for (na.last in c(NA, FALSE, TRUE)) {
        expected = range_sortna(x, na.last=na.last)
        expected[!is.na(expected)] = 1L

        actual = sort.int(x, na.last=na.last, method='quick')
        r = range_na(actual)
        r[4L] = is.unsorted(x, na.rm=TRUE)
        actual[!is.na(actual)] = 1L
        attr(actual, "range_sortna") = r
        expect_identical(
          expected, actual,
          label=sprintf("range_sortna(c(%s), na.last=%s)", toString(x), na.last)
        )

        expect_identical(
          bit_sort_unique(x, na.last=na.last, has.dup = TRUE),
          sort.int(unique(x), na.last=na.last, method="quick"),
          label=sprintf("bit_sort_unique(c(%s), na.last=%s, has.dup=TRUE)", toString(x), na.last)
        )

        y <- unique(x)
        expect_identical(
          bit_sort_unique(y, na.last=na.last, has.dup = FALSE),
          sort.int(y, na.last=na.last, method="quick"),
          label=sprintf("bit_sort_unique(c(%s), na.last=%s, has.dup=FALSE)", toString(x), na.last)
        )

        expect_identical(
          bit_sort(x, na.last=na.last),
          sort.int(x, na.last=na.last, method="quick"),
          label=paste0("bit_sort(c(", toString(x), "), na.last=", na.last, ")")
        )
      }

      expect_identical(
        bit_unique(x, na.rm=NA),
        unique(x, incomparables=FALSE),
        label=paste0("bit_unique(c(", toString(x), "), na.rm=", NA, ")")
      )
      expect_identical(
        bit_unique(x, na.rm=FALSE),
        unique(x, incomparables=NA),
        label=paste0("bit_unique(c(", toString(x), "), na.rm=", FALSE, ")")
      )
      expect_identical(
        bit_unique(x, na.rm=TRUE),
        unique(x[!is.na(x)]),
        label=paste0("bit_unique(c(", toString(x), "), na.rm=", TRUE, ")")
      )

      expect_identical(
        bit_duplicated(x, na.rm=NA),
        as.bit(duplicated(x, incomparables=FALSE)),
        label=paste0("bit_duplicated(c(", toString(x), "), na.rm=", NA, ")")
      )
      expect_identical(
        bit_duplicated(x, na.rm=FALSE),
        as.bit(duplicated(x, incomparables=NA)),
        label=paste0("bit_duplicated(c(", toString(x), "), na.rm=", FALSE, ")")
      )
      expect_identical(
        bit_duplicated(x, na.rm=TRUE),
        as.bit(duplicated(x) | is.na(x)),
        label=paste0("bit_duplicated(c(", toString(x), "), na.rm=", TRUE, ")")
      )

      expect_identical(
        bit_anyDuplicated(x, na.rm=NA),
        anyDuplicated(x, incomparables=FALSE),
        label=paste0("bit_anyDuplicated(c(", toString(x), "), na.rm=", NA, ")")
      )
      expect_identical(
        bit_anyDuplicated(x, na.rm=FALSE),
        anyDuplicated(x, incomparables=NA),
        label=paste0("bit_anyDuplicated(c(", toString(x), "), na.rm=", FALSE, ")")
      )
      expect_identical(
        bit_anyDuplicated(x, na.rm=TRUE),
        max(0L, head(which(is.na(x) | duplicated(x)), 1)),
        label=paste0("bit_anyDuplicated(c(", toString(x), "), na.rm=", TRUE, ")")
      )

      expect_identical(
        bit_sumDuplicated(x, na.rm=NA),
        sum(duplicated(x, incomparables=FALSE)),
        label=paste0("bit_sumDuplicated(c(", toString(x), "))")
      )
      expect_identical(
        bit_sumDuplicated(x, na.rm=FALSE),
        sum(duplicated(x, incomparables=NA)),
        label=paste0("bit_sumDuplicated(c(", toString(x), "))")
      )
      expect_identical(
        bit_sumDuplicated(x, na.rm=TRUE),
        sum(duplicated(x, incomparables=NA)) + sum(is.na(x)),
        label=paste0("bit_sumDuplicated(c(", toString(x), "))")
      )

    }
  }

})


test_that("bit_sort and bit_sort_unique are OK", {
  n <- 6L
  for (s in 1:10) {
    set.seed(s)
    x <- 2L * sample(n, replace=TRUE)
    for (napos in c(-1L, 0L, 1L)) {
      if (napos == -1) {
        y <- c(NA, x)
      } else if (napos == 1) {
        y <- c(x, NA)
      } else {
        y <- c(x[1:(n %/% 2)], NA, x[(n %/% 2 + 1L):n])
      }
      for (has.dup in c(TRUE, FALSE)) {
        if (!has.dup)
          y <- unique(y)
        for (na.last in c(NA, FALSE, TRUE)) {
          eval(substitute(
            expect_identical(
              bit_sort(y, na.last=na.last, has.dup = has.dup),
              sort(y, na.last = na.last)
            ),
            list(
              y=y,
              na.last=na.last,
              has.dup = has.dup
            )
          ))
          for (decreasing in c(FALSE, TRUE)) {
            eval(substitute(
              expect_identical(
                bit_sort_unique(y, decreasing=decreasing, na.last=na.last, has.dup = has.dup),
                sort(unique(y), decreasing=decreasing, na.last = na.last)
              ),
              list(
                y=y,
                decreasing=decreasing,
                na.last=na.last,
                has.dup = has.dup
              )
            ))
            range_na = range_na(y)
            env = list(
              y=y,
              decreasing=decreasing,
              na.last=na.last,
              has.dup = has.dup,
              range_na = range_na
            )
            eval(substitute(env = env, {
              expect_identical(
                bit_sort_unique(y,
                  decreasing=decreasing,
                  na.last=na.last,
                  has.dup=has.dup,
                  range_na=range_na
                ),
                sort(unique(y),
                  decreasing=decreasing,
                  na.last=na.last
                )
              )
            }))
          }
        }
      }
    }
  }
})

test_that("bit_setops work", {
  x = 1:5
  y = 2:6
  x_na = c(x, NA_integer_)
  y_na = c(y, NA_integer_)

  expect_identical(bit_setdiff(x, y), 1L)
  expect_identical(bit_setdiff(x_na, y), c(1L, NA_integer_))
  expect_identical(bit_setdiff(x, y_na), 1L)
  expect_identical(bit_setdiff(x_na, y_na), 1L)

  expect_identical(bit_symdiff(x, y), c(1L, 6L))
  expect_identical(bit_symdiff(x_na, y), c(1L, NA_integer_, 6L))
  expect_identical(bit_symdiff(x, y_na), c(1L, 6L, NA_integer_))
  expect_identical(bit_symdiff(x_na, y_na), c(1L, 6L))

  expect_identical(bit_intersect(x, y), 2:5)
  expect_identical(bit_intersect(x_na, y), 2:5)
  expect_identical(bit_intersect(x, y_na), 2:5)
  expect_identical(bit_intersect(x_na, y_na), c(2:5, NA_integer_))

  expect_identical(bit_union(x, y), 1:6)
  expect_identical(bit_union(x_na, y), c(1:5, NA_integer_, 6L))
  expect_identical(bit_union(x, y_na), c(1:6, NA_integer_))
  expect_identical(bit_union(x_na, y_na), c(1:5, NA_integer_, 6L))

  expect_false(bit_setequal(x, y))
  expect_false(bit_setequal(x_na, y))
  expect_false(bit_setequal(x, y_na))
  expect_false(bit_setequal(x_na, y_na))
  expect_true(bit_setequal(x, x))
  expect_true(bit_setequal(x_na, x_na))

  expect_identical(bit_rangediff(c(1L, 6L), c(3L, 4L)), c(1:2, 5:6))
  expect_identical(bit_rangediff(c(1L, 6L), c(3L, 4L), revx=TRUE), -(6:1))
  expect_identical(bit_rangediff(c(1L, 6L), c(3L, 4L), revy=TRUE), 1:6)
  expect_identical(bit_rangediff(c(1L, 6L), c(3L, 4L), revx=TRUE, revy=TRUE), -c(6:5, 2:1))
  expect_identical(bit_rangediff(c(6L, 1L), c(3L, 4L), revx=TRUE), -(1:6))
  expect_identical(bit_rangediff(c(6L, 1L), c(3L, 4L), revy=TRUE), 6:1)
  expect_identical(bit_rangediff(c(6L, 1L), c(3L, 4L), revx=TRUE, revy=TRUE), -c(1:2, 5:6))
})

test_that("bitsort works", {
  expect_identical(bitsort(c(2L, 0L, 1L, NA, 2L)), c(0:2, 2L))
  expect_identical(bitsort(c(2L, 0L, 1L, NA, 2L), na.last=TRUE), c(0:2, 2L, NA))
})

test_that("bit_in works", {
  x = 1:5
  y = 3:6
  x_na = c(x, NA)
  y_na = c(y, NA)

  expect_identical(bit_in(x, y), as.bit(x %in% y))
  expect_identical(bit_in(y, x), as.bit(y %in% x))
  expect_identical(bit_in(x_na, y), as.bit(x_na %in% y))
  expect_identical(bit_in(y, x), as.bit(y %in% x_na))
  expect_identical(bit_in(x, y_na), as.bit(x %in% y_na))
  expect_identical(bit_in(y_na, x), as.bit(y_na %in% x))
  expect_identical(bit_in(x_na, y_na), as.bit(x_na %in% y_na))
  expect_identical(bit_in(y_na, x_na), as.bit(y_na %in% x_na))
})
