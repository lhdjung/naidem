
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

