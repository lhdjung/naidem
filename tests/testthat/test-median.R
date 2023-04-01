
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


# Actual medians (i.e., the main function) --------------------------------

test_that("the default method returns non-`NA` values when it should", {
  x1 |> median_na() |> expect_equal(1)
  x2 |> median_na() |> expect_equal(1)
  x4 |> median_na() |> expect_equal(0)
  x5 |> median_na() |> expect_equal(1)
  x6 |> median_na() |> expect_equal(1)
  x8 |> median_na() |> expect_equal(5.8)
})

test_that("the default method returns `NA` when it should", {
  x3  |> median_na() |> expect_equal(NA_real_)
  x7  |> median_na() |> expect_equal(NA_real_)
  x9  |> median_na() |> expect_equal(NA_real_)
  x10 |> median_na() |> expect_equal(NA_real_)
  x11 |> median_na() |> expect_equal(NA_real_)
})


# Possible medians --------------------------------------------------------

test_that("`median_possible_values()` works correctly with even lengths", {
  x12 |> median_possible_values() |> expect_equal(c(7, 7.5, 8.5, 9))
  x13 |> median_possible_values() |> expect_equal(c(7, 7.5))
})

test_that("`median_possible_values()` works correctly with odd lengths", {
  x14 |> median_possible_values() |> expect_equal(c(7, 8))
  x15 |> median_possible_values() |> expect_equal(c(7, 9))
})

test_that("`median_range()` works correctly with even lengths", {
  x12 |> median_range() |> expect_equal(c(7, 9))
  x13 |> median_range() |> expect_equal(c(7, 7.5))
})

test_that("`median_range()` works correctly with odd lengths", {
  x14 |> median_range() |> expect_equal(c(7, 8))
  x15 |> median_range() |> expect_equal(c(7, 9))
})


