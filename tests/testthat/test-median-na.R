
# Test vectors:
x1  <- c(0, 1, 1, 1, NA)
x2  <- c(1, 1, NA)
x3  <- c(1, 2, NA)
x4  <- c(0, 0, NA, 0, 0)
x5  <- c(1, 1, 1, 1, NA, NA)
x6  <- c(1, 1, 1, 1, NA, NA, NA)
x7  <- c(1, 1, 1, 1, NA, NA, NA, NA)
x8  <- iris$Sepal.Length
x9  <- c(5.6, 5.7, 5.9, 6, 6.1, 6.3, 6.4, 6.6, 6.7, NA)
x10 <- c(6.1, 6.3, 5.9, 6, 6.1, 6.3, 6.4, 6.6, 6.7, NA, NA, NA, NA)
x11 <- c(7, 7, 7, 8, NA, NA)
x12 <- c(7, 7, 7, 8, 9, 9, NA, NA)
x13 <- c(7, 7, 7, 8, NA, NA)
x14 <- c(7, 7, 8, 9, NA)
x15 <- c(6, 7, 9, 9, NA)
x16 <- c(1, 2, 3, 3, 5, 9)
x17 <- c(1, 1, 2, NA, NA, NA)


# Actual medians (i.e., the main function) --------------------------------

test_that("the default method returns non-`NA` values when it should", {
  expect_equal(median_na(x1), 1)
  expect_equal(median_na(x2), 1)
  expect_equal(median_na(x4), 0)
  expect_equal(median_na(x5), 1)
  expect_equal(median_na(x6), 1)
  expect_equal(median_na(x8), 5.8)
})

test_that("the default method returns `NA` when it should", {
  expect_equal(median_na(x3 ), NA_real_)
  expect_equal(median_na(x7 ), NA_real_)
  expect_equal(median_na(x9 ), NA_real_)
  expect_equal(median_na(x10), NA_real_)
  expect_equal(median_na(x11), NA_real_)
})


# Possible medians --------------------------------------------------------

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

