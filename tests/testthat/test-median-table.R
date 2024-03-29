
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
  certainty = c(TRUE, FALSE, FALSE, FALSE),
  na_ignored = c(0L, 1L, 3L, 1L),
  na_total = c(0L, 1L, 4L, 1L),
  rate_ignored_na = c(NaN, 1, 0.75, 1),
  sum_total = c(15L, 3L, 6L, 4L),
  rate_ignored_sum = c(0, 0.3333333333333333, 0.5, 0.25),
)

vec2_exp <- tibble::tibble(
  term = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
  estimate = c(5.8, 3, 4.35, 1.3),
  certainty = rep(TRUE, 4L),
  na_ignored = integer(4),
  na_total = integer(4),
  rate_ignored_na = rep(NaN, 4L),
  sum_total = rep(150L, 4L),
  rate_ignored_sum = numeric(4),
)


# Testing -----------------------------------------------------------------

test_that("`median_df()` works correctly", {
  expect_equal(median_df(vec1), vec1_exp)
  expect_equal(median_df(vec2), vec2_exp)
})
