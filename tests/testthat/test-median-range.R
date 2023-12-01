
# Example vectors ---------------------------------------------------------

x12 <- c(7, 7, 7, 8, 9, 9, NA, NA)
x13 <- c(7, 7, 7, 8, NA, NA)
x14 <- c(7, 7, 8, 9, NA)
x15 <- c(6, 7, 9, 9, NA)
x16 <- c(1, 2, 3, 3, 5, 9)
x17 <- c(1, 1, 2, NA, NA, NA)


# Testing -----------------------------------------------------------------

test_that("`median_range()` works correctly with `x12`", {
  expect_equal(median_range(x12), c(7, 8.5))
  expect_equal(median_range(c(x12, NA)), c(7, 9))
  expect_equal(median_range(c(x12, rep(NA, 2))), c(7, 9))
  expect_equal(median_range(c(x12, rep(NA, 3))), c(7, 9))
  expect_equal(median_range(c(x12, rep(NA, 4))), c(NA_real_, NA_real_))
  expect_equal(median_range(c(x12, rep(NA, 5))), c(NA_real_, NA_real_))
})

test_that("`median_range()` works correctly with `x13`", {
  expect_equal(median_range(x13), c(7, 7.5))
  expect_equal(median_range(c(x13, NA)), c(7, 8))
  expect_equal(median_range(c(x13, rep(NA, 2))), c(NA_real_, NA_real_))
  expect_equal(median_range(c(x13, rep(NA, 3))), c(NA_real_, NA_real_))
})

test_that("`median_range()` works correctly with `x14`", {
  expect_equal(median_range(x14), c(7, 8))
  expect_equal(median_range(c(x14, NA)), c(7, 8.5))
  expect_equal(median_range(c(x14, rep(NA, 2))), c(7, 9))
  expect_equal(median_range(c(x14, rep(NA, 3))), c(NA_real_, NA_real_))
})

test_that("`median_range()` works correctly with `x15`", {
  expect_equal(median_range(x15), c(7, 9))
  expect_equal(median_range(c(x15, NA)), c(6.5, 9))
  expect_equal(median_range(c(x15, rep(NA, 2))), c(6, 9))
  expect_equal(median_range(c(x15, rep(NA, 3))), c(NA_real_, NA_real_))
  expect_equal(median_range(c(x15, rep(NA, 4))), c(NA_real_, NA_real_))
})

test_that("`median_range()` works correctly with `x16`", {
  expect_equal(median_range(x16), c(3, 3))
  expect_equal(median_range(c(x16, NA)), c(3, 3))
  expect_equal(median_range(c(x16, rep(NA, 2))), c(2.5, 4))
  expect_equal(median_range(c(x16, rep(NA, 3))), c(2, 5))
  expect_equal(median_range(c(x16, rep(NA, 4))), c(1.5, 7))
  expect_equal(median_range(c(x16, rep(NA, 5))), c(1, 9))
  expect_equal(median_range(c(x16, rep(NA, 6))), c(NA_real_, NA_real_))
  expect_equal(median_range(c(x16, rep(NA, 7))), c(NA_real_, NA_real_))
})

test_that("`median_range()` works correctly with `x17`", {
  expect_equal(median_range(x17), c(NA_real_, NA_real_))
  expect_equal(median_range(c(x17, NA)), c(NA_real_, NA_real_))
})
