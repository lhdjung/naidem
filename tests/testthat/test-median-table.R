
# Example vectors ---------------------------------------------------------

vec1 <- list(
  a = 1:15,
  b = c(1, 1, NA),
  c = c(4, 4, NA, NA, NA, NA),
  d = c(96, 24, 3, NA)
)

vec2 <- iris[1:4]

vec3 <- rep(NA_real_, 10)


# Expected results --------------------------------------------------------

vec1_exp <- structure(
  tibble::tibble(
    term = c("a", "b", "c", "d"),
    estimate = c(8, 1, 4, 24),
    certainty = rep(c(TRUE, FALSE), each = 2L),
    min = c(8, 1, -Inf, 13.5),
    max = c(8, 1, Inf, 60),
    na_ignored = c(0L, 0L, 3L, 1L),
    na_total = c(0L, 1L, 4L, 1L),
    rate_ignored_na = c(0, 0, 0.75, 1),
    sum_total = c(15L, 3L, 6L, 4L),
    rate_ignored_sum = c(0, 0, 0.5, 0.25),
  ),
  class = c("median_table", "tbl_df", "tbl", "data.frame")
)

vec2_exp <- structure(
  tibble::tibble(
    term = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    estimate = c(5.8, 3, 4.35, 1.3),
    certainty = TRUE,
    min = c(5.8, 3, 4.35, 1.3),
    max = c(5.8, 3, 4.35, 1.3),
    na_ignored = 0L,
    na_total = 0L,
    rate_ignored_na = numeric(4),
    sum_total = 150L,
    rate_ignored_sum = 0,
  ),
  class = c("median_table", "tbl_df", "tbl", "data.frame")
)

vec3_exp <- structure(
  tibble::tibble(
    term = "",
    estimate = NA_real_,
    certainty = FALSE,
    min = -Inf,
    max = Inf,
    na_ignored = 10L,
    na_total = 10L,
    rate_ignored_na = 1,
    sum_total = 10L,
    rate_ignored_sum = 1,
  ),
  class = c("median_table", "tbl_df", "tbl", "data.frame")
)


# Testing -----------------------------------------------------------------

test_that("`median_table()` works correctly", {
  expect_equal(median_table(vec1), vec1_exp)
  expect_equal(median_table(vec2), vec2_exp)
  expect_equal(median_table(vec3), vec3_exp)
})
