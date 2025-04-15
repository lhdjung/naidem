
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

test_that("the return type is double for non-`NA` vectors", {
  expect_equal(typeof(median2(x1)), "double")
  expect_equal(typeof(median2(x2)), "double")
  expect_equal(typeof(median2(x4)), "double")
  expect_equal(typeof(median2(x5)), "double")
  expect_equal(typeof(median2(x6)), "double")
  expect_equal(typeof(median2(x8)), "double")

  expect_equal(typeof(median2(as.integer(x1))), "double")
  expect_equal(typeof(median2(as.integer(x2))), "double")
  expect_equal(typeof(median2(as.integer(x4))), "double")
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

test_that("non-numeric input is an error by default", {
  expect_error(median2(letters))
  expect_error(median2(factor(1:5)))
  expect_error(median2(as.Date("2010-01-01")))
})

test_that("non-numeric input is not an error with non-default `even`", {
  expect_no_error(median2(letters, even = "low"))
  expect_no_error(median2(factor(1:5), even = "high"))
  expect_no_error(median2(as.Date("2010-01-01"), even = "low"))
})

