
test_that("`median_count_na_ignore()` returns correct values for odd length", {
  expect_equal(median_count_na_ignore(1), 0L)
  # expect_equal(median_count_na_ignore(c(1, 1, NA)), 0L)
  # expect_equal(median_count_na_ignore(c(1, 2, NA)), 1L)
  expect_equal(median_count_na_ignore(c(1, 2, 7, NA, NA)), 2L)
  # expect_equal(median_count_na_ignore(c(8, 8, NA, NA, NA)), 2L)
})

# test_that("`median_count_na_ignore()` returns correct values for even length", {
#   expect_equal(median_count_na_ignore(c(1, 1)), 0L)
  expect_equal(median_count_na_ignore(c(1, NA)), 1L)
  # expect_equal(median_count_na_ignore(c(1, 1, NA, NA)), 1L)
  # expect_equal(median_count_na_ignore(c(1, 2, NA, NA)), 2L)
  expect_equal(median_count_na_ignore(c(1, 1, 2, NA)), 1L)
  expect_equal(median_count_na_ignore(c(1, 2, 2, NA)), 1L)
# })

