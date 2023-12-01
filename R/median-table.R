#' Tabulate median estimates with the certainty about them
#'
#' @description `median_df()` takes a data frame (or another list) of numeric
#'   vectors and computes the median of each element. Where the true median is
#'   unknown due to missing values, more and more missings are ignored until an
#'   estimate for the median is found.
#'
#'   Estimates are presented along with information about whether they are known
#'   to be the true median, how many missing had to be ignored during
#'   estimation, the rate of ignored values, etc.
#'
#' @param x List of vectors. Each vector needs to be numeric or similar. Note
#'   that data frames are lists, so `x` can be a data frame.
#' @param even Passed on to [`median2()`].
#' @param ... Optional further arguments for [`median2()`] methods. Not used in
#'   its default method.
#'
#' @details The function deals with missing values (`NA`s) by first checking
#'   whether they make the true median unknown. If they do, it removes one `NA`,
#'   then checks again; and so on until an estimate is found.
#'
#'   This strategy is based on [`median2()`] and its `na.rm.amount` argument,
#'   which represents a middle way between simply ignoring all `NA`s and not
#'   even trying to compute an estimate. Instead, it only removes the minimum
#'   number of `NA`s necessary, because some distributions have a known median
#'   even if some of their values are missing. By keeping track of the removed
#'   `NA`s, `median_df()` quantifies the uncertainty about its estimates.
#'
#' @return Data frame with these columns:
#'   - `term`: the names of `x` elements. Only present if any are named.
#'   - `estimate`: the medians of `x` elements, ignoring as many `NA`s as
#'   necessary.
#'   - `certainty`: `TRUE` if the corresponding estimate is certain to be the
#'   true median, and `FALSE` if this is unclear due to missing values.
#'   - `na_ignored`: the number of missing values that had to be ignored to
#'   arrive at the estimate.
#'   - `na_total`: the total number of missing values.
#'   - `rate_ignored_na`: the proportion of missing values that had to be
#'   ignored from among all missing values.
#'   - `sum_total`: the total number of values, missing or not.
#'   - `rate_ignored_sum`: the proportion of missing values that had to be
#'   ignored from among all values, missing or not.
#'
#' @export
#'
#' @include median2.R
#'
#' @examples
#' # Use a list of numeric vectors:
#' my_list <- list(
#'   a = 1:15,
#'   b = c(1, 1, NA),
#'   c = c(4, 4, NA, NA, NA, NA),
#'   d = c(96, 24, 3, NA)
#' )
#'
#' median_df(my_list)
#'
#' # Data frames are allowed:
#' median_df(iris[1:4])

median_df <- function(x, even = c("mean", "low", "high"), ...) {
  # Check that `x` is a list because the point of this function is to find
  # estimates for the median of each element of `x`:
  if (!is.list(x)) {
    stop("`x` must be a list.")
  }
  # Initialize the two most important output vectors. They will be columns of
  # the output data frame. `estimate` is integer for the corner case that all
  # values that will be assigned to its elements are integers (i.e., none is a
  # double; this would coerce any integer elements):
  length_x <- length(x)
  estimate <- integer(length_x)
  na_ignored <- integer(length_x)
  # Loop through the `x` elements, attempting to find a median estimate for each
  # by removing one `NA` at a time:
  for (i in seq_len(length_x)) {
    # Vectors where all elements are missing have an unknown median, so the
    # estimate should be `NA`, as well. This is implemented via a shortcut:
    if (all(is.na(x[[i]]))) {
      estimate[[i]] <- x[[i]][NA_integer_]
      next
    }
    # Initialize the number of `NA`s that need to be ignored at zero because the
    # true median of a given element of `x` may yet be known at this point. This
    # value will be incremented in the while loop once for every failed attempt
    # to get a non-`NA` estimate. If this was successful, `looking_for_estimate`
    # will cause the loop to terminate:
    na_ignored_current <- 0L
    repeat {
      estimate_current <- median2(
        x[[i]], na.rm = FALSE, na.rm.amount = na_ignored_current, even = even
      )
      # -- If the estimate is `NA`, there will be a next iteration of the while
      # loop, and it will ignore one more `NA`.
      # -- If a non-`NA` estimate has been found, enter it into the `estimate`
      # vector, record the number of `NA`s that needed to be ignored for this
      # result, and set the flag to `FALSE` to terminate the while loop.
      if (is.na(estimate_current)) {
        na_ignored_current <- na_ignored_current + 1L
      } else {
        na_ignored[i] <- na_ignored_current
        estimate[[i]] <- estimate_current
        break
      }
    }
  }
  # As a purely mechanical consequence of the `na_ignored` integer vector,
  # `certainty` marks those cases where no `NA`s needed to be ignored to compute
  # an estimate; and thus, where there is a known and determinate median:
  certainty <- logical(length_x)
  certainty[na_ignored == 0L] <- TRUE
  # Count the missing values in each element of `x`, then compute the rate of
  # missing values that had to be ignored (to find a median estimate) as a share
  # of the total number of missing values:
  na_total <- vapply(
    x, function(y) length(y[[1L]][is.na(y)]), integer(1L), USE.NAMES = FALSE
  )
  # Compute the rates of ignored `NA`s from all `NA`s and from all values:
  sum_total <- vapply(x, length, integer(1L), USE.NAMES = FALSE)
  rate_ignored_na <- na_ignored / na_total
  rate_ignored_sum <- na_ignored / sum_total
  # Collect the length-`x` vectors in a data frame, adding a `term` column that
  # stores the names of `x` if there are any:
  tibble::tibble(
    term = names(x), estimate, certainty, na_ignored, na_total, rate_ignored_na,
    sum_total, rate_ignored_sum, stringsAsFactors = FALSE
  )
}
