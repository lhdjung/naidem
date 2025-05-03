
test_that("`median_count_tolerable()` returns correct values for odd length", {
  expect_equal(median_count_tolerable(1), 0L)
  expect_equal(median_count_tolerable(c(1, 1, NA)), 1L)
  expect_equal(median_count_tolerable(c(1, 2, NA)), 0L)
  expect_equal(median_count_tolerable(c(1, 2, 7, NA, NA)), 0L)
  expect_equal(median_count_tolerable(c(8, 8, NA, NA, NA)), 1L)
})

test_that("`median_count_tolerable()` returns correct values for even length", {
  expect_equal(median_count_tolerable(c(1, 1)), 1L)
  expect_equal(median_count_tolerable(c(1, NA)), 0L)
  expect_equal(median_count_tolerable(c(1, 1, 1, 2)), 1L)
  expect_equal(median_count_tolerable(c(1, 1, 1, 1)), 3L)
  expect_equal(median_count_tolerable(c(1, 2, 2, 3)), 1L)
  expect_equal(median_count_tolerable(c(1, 1, 1, 1, 2, 2)), 1L)
  expect_equal(median_count_tolerable(c(1, 1, 2, 2, 3, 3)), 1L)
  expect_equal(median_count_tolerable(c(1, 7, 7, 7, 7, NA)), 2L)
  expect_equal(median_count_tolerable(c(1, 1, NA, NA)), 1L)
  expect_equal(median_count_tolerable(c(1, 2, NA, NA)), 0L)
  expect_equal(median_count_tolerable(c(1, 1, 2, NA)), 0L)
  expect_equal(median_count_tolerable(c(1, 2, 2, NA)), 0L)
  expect_equal(median_count_tolerable(c(8, 8, 9, 9)), 0L)
})

test_that("`median_count_tolerable()` handles non-numeric data correctly", {
  expect_equal(median_count_tolerable(c("a", "b", "b", "b")), 1L)
  expect_equal(median_count_tolerable(c("a", "b", "b")), 0L)
})
