
# Test vectors:
x1 <- c(0, 1, 1, 1, NA)
x2 <- c(1, 1, NA)
x3 <- c(1, 2, NA)
x4 <- c(0, 0, NA, 0, 0)
x5 <- c(1, 1, 1, 1, NA, NA)


test_that("the default method returns non-`NA` values when it should", {
  x1 |> median_na() |> expect_equal(1)
  x2 |> median_na() |> expect_equal(1)
  x4 |> median_na() |> expect_equal(0)
  x5 |> median_na() |> expect_equal(1)
})

test_that("the default method returns `NA` when it should", {
  x3 |> median_na() |> expect_equal(NA_real_)
})
