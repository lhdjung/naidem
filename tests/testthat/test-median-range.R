
# Example vectors ---------------------------------------------------------

x12 <- c(7, 7, 7, 8, 9, 9, NA, NA)
x13 <- c(7, 7, 7, 8, NA, NA)
x14 <- c(7, 7, 8, 9, NA)
x15 <- c(6, 7, 9, 9, NA)
x16 <- c(1, 2, 3, 3, 5, 9)
x17 <- c(1, 1, 2, NA, NA, NA)


# Testing -----------------------------------------------------------------

test_that("`median_bounds()` works correctly with `x12`", {
  expect_equal(median_bounds(x12), c(7, 8.5))
  expect_equal(median_bounds(c(x12, NA)), c(7, 9))
  expect_equal(median_bounds(c(x12, rep(NA, 2))), c(7, 9))
  expect_equal(median_bounds(c(x12, rep(NA, 3))), c(7, 9))
  expect_equal(median_bounds(c(x12, rep(NA, 4))), c(-Inf, Inf))
  expect_equal(median_bounds(c(x12, rep(NA, 5))), c(-Inf, Inf))
})

test_that("`median_bounds()` works correctly with `x13`", {
  expect_equal(median_bounds(x13), c(7, 7.5))
  expect_equal(median_bounds(c(x13, NA)), c(7, 8))
  expect_equal(median_bounds(c(x13, rep(NA, 2))), c(-Inf, Inf))
  expect_equal(median_bounds(c(x13, rep(NA, 3))), c(-Inf, Inf))
})

test_that("`median_bounds()` works correctly with `x14`", {
  expect_equal(median_bounds(x14), c(7, 8))
  expect_equal(median_bounds(c(x14, NA)), c(7, 8.5))
  expect_equal(median_bounds(c(x14, rep(NA, 2))), c(7, 9))
  expect_equal(median_bounds(c(x14, rep(NA, 3))), c(-Inf, Inf))
})

test_that("`median_bounds()` works correctly with `x15`", {
  expect_equal(median_bounds(x15), c(7, 9))
  expect_equal(median_bounds(c(x15, NA)), c(6.5, 9))
  expect_equal(median_bounds(c(x15, rep(NA, 2))), c(6, 9))
  expect_equal(median_bounds(c(x15, rep(NA, 3))), c(-Inf, Inf))
  expect_equal(median_bounds(c(x15, rep(NA, 4))), c(-Inf, Inf))
  expect_equal(median_bounds(c(x15, 5)), c(6.5, 8))
  expect_equal(median_bounds(c(x15, 5), even = "low"), c(6, 7))
  expect_equal(median_bounds(c(x15, 5), even = "high"), c(7, 9))
})

test_that("`median_bounds()` works correctly with `x16`", {
  expect_equal(median_bounds(x16), c(3, 3))
  expect_equal(median_bounds(c(x16, NA)), c(3, 3))
  expect_equal(median_bounds(c(x16, rep(NA, 2))), c(2.5, 4))
  expect_equal(median_bounds(c(x16, rep(NA, 3))), c(2, 5))
  expect_equal(median_bounds(c(x16, rep(NA, 4))), c(1.5, 7))
  expect_equal(median_bounds(c(x16, rep(NA, 5))), c(1, 9))
  expect_equal(median_bounds(c(x16, rep(NA, 6))), c(-Inf, Inf))
  expect_equal(median_bounds(c(x16, rep(NA, 7))), c(-Inf, Inf))
})

test_that("`median_bounds()` works correctly with `x17`", {
  expect_equal(median_bounds(x17), c(-Inf, Inf))
  expect_equal(median_bounds(c(x17, NA)), c(-Inf, Inf))
})

test_that("the return type is double for non-`NA` vectors", {
  expect_equal(typeof(median_bounds(x1)), "double")
  expect_equal(typeof(median_bounds(x2)), "double")
  expect_equal(typeof(median_bounds(x4)), "double")
  expect_equal(typeof(median_bounds(x5)), "double")
  expect_equal(typeof(median_bounds(x6)), "double")
  expect_equal(typeof(median_bounds(x8)), "double")

  expect_equal(typeof(median_bounds(as.integer(x1))), "double")
  expect_equal(typeof(median_bounds(as.integer(x2))), "double")
  expect_equal(typeof(median_bounds(as.integer(x4))), "double")
})

test_that("non-numeric input is an error by default", {
  expect_error(median_bounds(letters))
  expect_error(median_bounds(factor(1:5)))
  expect_error(median_bounds(as.Date("2010-01-01")))
})

test_that("non-numeric input is not an error with non-default `even`", {
  expect_no_error(median_bounds(letters, even = "low"))
  expect_no_error(median_bounds(factor(1:5), even = "high"))
  expect_no_error(median_bounds(as.Date("2010-01-01"), even = "low"))
})

