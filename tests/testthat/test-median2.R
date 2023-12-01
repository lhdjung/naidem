
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
  expect_equal(median2(x1), 1)
  expect_equal(median2(x2), 1)
  expect_equal(median2(x4), 0)
  expect_equal(median2(x5), 1)
  expect_equal(median2(x6), 1)
  expect_equal(median2(x8), 5.8)
})

test_that("the default method returns `NA` when it should", {
  expect_equal(median2(x3 ), NA_real_)
  expect_equal(median2(x7 ), NA_real_)
  expect_equal(median2(x9 ), NA_real_)
  expect_equal(median2(x10), NA_real_)
  expect_equal(median2(x11), NA_real_)
})

test_that("`na.rm.amount` can make the default method return non-`NA` values", {
  expect_equal(median2(x3 , na.rm.amount = 1), 1.5)
  expect_equal(median2(x7 , na.rm.amount = 1), 1)
  expect_equal(median2(x9 , na.rm.amount = 1), 6.1)
  expect_equal(median2(x10, na.rm.amount = 4), 6.3)
  expect_equal(median2(x11, na.rm.amount = 1), 7)
})

test_that("`na.rm.amount` doesn't necessarily make the default method
          return non-`NA` values", {
  expect_equal(median2(x10, na.rm.amount = 3), NA_real_)
  expect_equal(median2(x10, na.rm.amount = 2), NA_real_)
  expect_equal(median2(x10, na.rm.amount = 1), NA_real_)
  expect_equal(median2(x10, na.rm.amount = 0), NA_real_)
})

test_that("specifying both `na.rm = TRUE` and `na.rm.amount` is an error
          (but `na.rm = FALSE` with `na.rm.amount` specified is not)", {
  expect_error(median2(x1, na.rm = TRUE, na.rm.amount = 5))
  expect_no_error(median2(x1, na.rm = FALSE, na.rm.amount = 5))
})

