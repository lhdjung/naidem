
# Test vectors:
x12 <- c(7, 7, 7, 8, 9, 9, NA, NA)
x13 <- c(7, 7, 7, 8, NA, NA)
x14 <- c(7, 7, 8, 9, NA)
x15 <- c(6, 7, 9, 9, NA)
x16 <- c(1, 2, 3, 3, 5, 9)
x17 <- c(1, 1, 2, NA, NA, NA)


test_that("`median_possible_values()` works correctly with even lengths", {
  expect_equal(median_possible_values(x12), c(7, 7.5, 8.5, 9))
  expect_equal(median_possible_values(x13), c(7, 7.5))
})

test_that("`median_possible_values()` works correctly with odd lengths", {
  expect_equal(median_possible_values(x14), c(7, 8))
  expect_equal(median_possible_values(x15), c(7, 9))
})

test_that("`median_range()` works correctly with even lengths", {
  expect_equal(median_range(x12), c(7, 9))
  expect_equal(median_range(x13), c(7, 7.5))
})

test_that("`median_range()` works correctly with odd lengths", {
  expect_equal(median_range(x14), c(7, 8))
  expect_equal(median_range(x15), c(7, 9))
})

test_that("early returns are correct in both possible-medians functions", {
  expect_equal(median_possible_values(x16), 3)
  expect_equal(median_possible_values(x17), NA_real_)
  expect_equal(median_range(x16), c(3, 3))
  expect_equal(median_range(x17), c(NA_real_, NA_real_))
})

