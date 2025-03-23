
# Example vectors ---------------------------------------------------------

vec1 <- list(
  a = 1:15,
  b = c(1, 1, NA),
  c = c(4, 4, NA, NA, NA, NA),
  d = c(96, 24, 3, NA)
)

vec2 <- iris[1:4]

vec1_exp <- tibble::tibble(
  term = c("a", "b", "c", "d"),
  estimate = c(8, 1, 4, 24),
  certainty = rep(c(TRUE, FALSE), each = 2L),
  min = c(8, 1, NA, 13.5),
  max = c(8, 1, NA, 60),
  na_ignored = c(0L, 0L, 3L, 1L),
  na_total = c(0L, 1L, 4L, 1L),
  rate_ignored_na = c(NaN, 0, 0.75, 1),
  sum_total = c(15L, 3L, 6L, 4L),
  rate_ignored_sum = c(0, 0, 0.5, 0.25),
)

vec2_exp <- tibble::tibble(
  term = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  estimate = c(5.8, 3, 4.35, 1.3),
  certainty = TRUE,
  min = c(5.8, 3, 4.35, 1.3),
  max = c(5.8, 3, 4.35, 1.3),
  na_ignored = 0L,
  na_total = 0L,
  rate_ignored_na = NaN,
  sum_total = 150L,
  rate_ignored_sum = 0,
)


# Testing -----------------------------------------------------------------

test_that("`median_table()` works correctly", {
  expect_equal(median_table(vec1), vec1_exp)
  expect_equal(median_table(vec2), vec2_exp)
})
