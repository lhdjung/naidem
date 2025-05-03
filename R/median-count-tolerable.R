#' How many `NA`s can be tolerated for a median estimate?
#'
#' @description `median_count_tolerable()` returns the number of missing values
#'   that can be preserved while determining the median. The point is to retain
#'   as many data points as possible, instead of simply ignoring all `NA`s.
#'
#'   This is only based on the number of known values, not any `NA`s there might
#'   be.
#'
#'   It is used within [`median_table()`] to determine how many missing values
#'   need to be ignored.
#'
#' @param x A numeric vector or similar.
#' @param needs_prep Logical. Ignore unless the function is used as a helper.
#'   See details.
#'
#' @details With the default `needs_prep = TRUE`, missing values will be removed
#'   from `x`, and `x` will be sorted. If this was already done elsewhere,
#'   setting `needs_prep` to `FALSE` is more efficient. Proceed with caution as
#'   this is not checked.
#'
#' @return Integer (length 1). Never `NA`, never negative.
#'
#' @export
#'
#' @examples
#' # With two or fewer `NA`s, the median can only be `8`,
#' # so these `NA`s are tolerated:
#' median_count_tolerable(c(8, 8, 8, NA, NA))
#'
#' # When adding a third `NA`, the median will be unknown.
#' # Compare using naidem's correct median function:
#' median2(c(8, 8, 8, NA, NA))
#' median2(c(8, 8, 8, NA, NA, NA))
#'
#' # No `NA`s are tolerable here because
#' # a single one could change the median:
#' median_count_tolerable(c(8, 9, 9, NA, NA, NA))
#'
#' # Here too, the median depends on the value behind `NA`,
#' # so the `NA` cannot be tolerated:
#' median_count_tolerable(c(8, 9, NA))


# x <- c(1, 7, 7, 7, 7, NA)
# x <- c(6, 7, 7, 7, 7, NA)
# x <- c(8, 8, 9, 9, NA, NA, NA)


median_count_tolerable <- function(x, needs_prep = TRUE) {

  # When `median_count_tolerable()` is used as a helper, the calling function
  # will likely remove `NA`s from `x` and then sort `x`. Redundantly doing so
  # again here would be inefficient. The optional `needs_prep` argument allows
  # the caller to skip this step. Note: this requires `x` to no longer have any
  # `NA`s, and to be sorted!
  if (needs_prep) {
    tryCatch(
      x <- sort(x[!is.na(x)]),
      error = stop_data_invalid
    )
  }

  # In case all values are missing, none of them are tolerable. This is
  # recursive: remove one, and the median is still unknown.
  if (length(x) == 0L) {
    return(0L)
  }

  # From here on, the function will only work with the known (non-`NA`) values.
  n_known <- length(x)
  n_known_is_even <- n_known %% 2L == 0L

  if (n_known_is_even) {
    # Just using integer division here for integer typing; `n_known / 2L` would
    # be a double value but otherwise equal.
    half_lower <- n_known %/% 2L
    half_upper <- half_lower + 1L
    # In case of two unequal central values, even just a single `NA` can shift
    # the median, so none of them are tolerable. This can only occur with an
    # even number of known values. `near_or_equal()` is like `==` except it
    # ignores spurious differences that arise due to floating point errors.
    # E.g., `0.1 + 0.2 == 0.3` is `FALSE` but `near_or_equal(0.1 + 0.2, 0.3)` is
    # `TRUE`.
    if (!near_or_equal(x[half_lower], x[half_upper])) {
      return(0L)
    }
    # With an odd number of known values, there is only one central index for
    # them. Even so, however, this single value is assigned to two variables
    # here because these are needed in case of an even number. This avoids
    # having to split the remainder of the code into one odd-length arm and one
    # even-length arm.
  } else {
    half_lower <- (n_known + 1L) %/% 2L
    half_upper <- half_lower
  }

  # Special rules apply if there are not enough known values to meaningfully
  # count the steps within their center:
  # -- With two known values, they are known to be equal due to the check above.
  # They tolerate exactly one `NA`. Subtraction can't return a negative number
  # here because the zero-`NA` case was already handled before.
  # -- A single known value can't tolerate any missing values. At this point,
  # `n_known` can only be one, not zero, because of the checks above.
  if (n_known < 3L) {
    if (n_known == 2L) {
      return(1L)
    }
    return(0L)
  }

  # Using a helper function, count the steps from the central value outward in
  # each direction where the value is still the same as in the center. Maybe
  # read the `steps_right` part first; it is certainly more intuitive.

  # -- For the left arm, `seq_len()` creates a vector from 1 to the first value
  # to the left of the lower central value, and `rev()` then reverses it, so
  # that `x[half_lower]` will first be compared to the first value to its left,
  # then the next value on the left, and so on.
  steps_left <- count_central_steps(
    center = x[half_lower],
    path = x[rev(seq_len(half_lower - 1L))]
  )

  # -- For the right arm, start at the first value to the right of the upper
  # central value and go from there to `n_known`; the total number of known
  # values. In this way, the upper central value will be compared successively
  # to the values to its right, until a different value is found. See the docs
  # for `count_central_steps()` below this function.
  steps_right <- count_central_steps(
    center = x[half_upper],
    path = x[(half_upper + 1L):n_known]
  )

  # Calculate the maximal number of `NA`s that can be tolerated -- i.e., that
  # don't need to be ignored -- when attempting to determine the median:

  # NEW CONJECTURE: For even values of `n_known`, the correct solution is
  # `min(steps_left, steps_right) * 2L + 1L` -- or without `+ 1L` if the
  # `half_*` indices themselves count as central steps? Or just `min(...) + 1L`?
  # Or perhaps it is `n_stable - 1L`, where  `n_stable` is the length of the
  # stable region (i.e., the subarray of contiguous values that include the
  # central value and are equal to it)?

  # - 1, 1, 1, 2
  # - 1, 1, 1, 1
  # - 1, 1, 1, 1, 2, 2
  # - 1, 2, 2, 3
  # - 1, 1, 2, 2, 3, 3

  # Note that (1, 2) and (1, 2, 3, 4) do NOT count -- the two central indices
  # are unequal, so they would have been filtered out earlier!

  if (n_known_is_even) {
    min(steps_left, steps_right) * 2L + 1L
  } else {
    min(steps_left, steps_right) * 2L
  }

}


#' Count steps from the central value where the same value is still found
#'
#' @description The internal helper `count_central_steps()` is used within
#'   `median_count_tolerable()`.
#'
#'   The function starts from one of the two central values (upper or lower) of
#'   the sorted distribution after all `NA`s were removed. It counts the steps
#'   outward (i.e., left or right) where the value is still the same as the
#'   central value. Finally, it returns the number of these steps.
#'
#' @details `FALSE` is matched against the logical test result from `center` out
#'   towards `path`. It will find the index of the first value that is not equal
#'   to the central value, unless all are equal to it. `1L` is subtracted from
#'   this index to get the number of steps where the value found is still equal.
#'
#'   `n_steps` is `NA_integer_` if and only if the `near_or_equal()` comparison
#'   does not yield any `FALSE` elements (see `?match`). The only possible
#'   reason is that the comparison only contains `TRUE` values: the case where
#'   it contains no values at all is ruled out by the check for `n_known < 3L`,
#'   and any `NA`s were removed beforehand. In any case without `NA`s, the
#'   length of `path` would be correct:
#'
#'   - Zero values, zero matching steps.
#'   - All values are `TRUE`, so all of them are matching steps.
#'
#' @param center Lower or upper central value of the sorted vector.
#' @param path The lower or upper half of the sorted vector (apart from the
#'   center, of course).
#'
#' @return Integer (length 1). Never `NA`.
#'
#' @noRd
count_central_steps <- function(center, path) {
  n_steps <- match(FALSE, near_or_equal(center, path)) - 1L
  if (is.na(n_steps)) {
    length(path)
  } else {
    n_steps
  }
}


# For interactive use -- shows what the algorithm must be able to get behind:
print_sort_both <- function(x) {
  n <- length(x)
  x <- sort(x[!is.na(x)])
  nna <- n - length(x)
  na_all <- rep(NA_real_, nna)
  print(c(na_all, x))
  print(c(x, na_all))
}
