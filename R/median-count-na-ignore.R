#' How many `NA`s need to be ignored for a median estimate?
#'
#' @description `median_count_na_ignore()` returns the number of missing values
#'   that need to be ignored to safely determine the median of the remaining
#'   values.
#'
#'   The point is to retain as many `NA`s as possible when looking for a median
#'   estimate, instead of simply ignoring all of them.
#'
#' @param x A numeric vector or similar.
#' @param format String. If `"total"` (the default), the function returns the
#'   absolute number of `NA`s that must be ignored. If `"proportion"`, it
#'   returns that number divided by the count of all `NA`s.
#' @param nna Integer. Ignore unless the function is used as a helper. If
#'   missing values in `x` were counted elsewhere and then removed, specify
#'   `nna` as that number to avoid redundantly dealing with `NA`s again. In this
#'   case, `x` must already be sorted and no longer have any `NA`s! Default is
#'   to deal with `NA`s within this function.
#'
#' @details If you specify `nna`, make sure that all `NA`s were already removed
#'   from `x`, and that `x` is sorted. The function won't check for either, and
#'   if you don't get this right, it may silently return wrong results.
#'
#'   The main purpose is to speed up [`median_table()`], which uses the present
#'   function as a helper. Although both are exported, [`median_table()`] is
#'   generally more useful.
#'
#' @return Numeric (length 1), never `NA`. The value depends on `format`:
#'   - With the default `format = "total"`, it is an integer that ranges from
#'   `0` to `length(which(is.na(x)))`, i.e., the total number of missing values
#'   in `x`.
#'   - With `format = "proportion"`, it is a double that ranges from `0` to `1`.
#'
#' @export
#'
#' @examples
#' # The median can only be `8`, so
#' # there is no need to ignore the `NA`:
#' median_count_na_ignore(c(8, 8, 8, NA))
#'
#' # Here, the median depends on the value
#' # behind `NA`, so the `NA` really must be ignored:
#' median_count_na_ignore(c(8, 9, 9, NA))
#'
#' # Ignoring two `NA`s leads to the same
#' # state as in the first example:
#' median_count_na_ignore(c(8, 8, 8, NA, NA, NA))
#'
#' # All need to be ignored here because
#' # a single one could change the median:
#' median_count_na_ignore(c(8, 9, NA, NA, NA))


# x <- c(1, 7, 7, 7, 7, NA)
# x <- c(6, 7, 7, 7, 7, NA)
# x <- c(8, 8, 9, 9, NA, NA, NA)


median_count_na_ignore <- function(x,
                                   format = c("total", "proportion"),
                                   nna = NULL) {
  format <- match.arg(format)
  n <- length(x)

  # When `median_count_na_ignore()` is used as a helper, the calling function
  # will likely count `NA`s itself, as well as remove them from `x` and then
  # sort `x`. Doing so again here would be redundant and inefficient. The `nna`
  # argument allows the caller to pass this number to the present function
  # instead. This requires `x` to no longer have any `NA`s, and to be sorted!
  if (is.null(nna)) {
    # This removes all `NA`s:
    x <- sort(x, na.last = NA)
    n_known <- length(x)
    nna <- n - n_known
  } else {
    nna <- as.integer(nna)
    n_known <- n
    n <- n + nna
  }

  # Special rules apply with extreme (low or high) numbers of missing values:
  # -- No missing values, nothing to ignore.
  # -- All values are missing, all must be ignored. This is recursive: remove
  # one, and the median is still unknown.
  if (nna == 0L) {
    return(0L)
  } else if (nna == n) {
    return(nna)
  }

  n_known_is_even <- n_known %% 2L == 0L

  if (n_known_is_even) {
    # Just using integer division here for integer typing; `n_known / 2L` would
    # be a double value but otherwise equal.
    half_lower <- n_known %/% 2L
    half_upper <- half_lower + 1L
    # In case of two unequal central values, even just a single `NA` can shift
    # the median, so all `NA`s must be ignored. This can only occur with an even
    # number of known values.
    if (x[half_lower] != x[half_upper]) {
      return(nna)
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
  # They tolerate exactly one `NA`, so all but one need to be ignored.
  # Subtraction can't return a negative number here because the zero-`NA` case
  # was already handled before.
  # -- A single known value can't tolerate any missing values, so all of them
  # must be ignored. At this point, `n_known` can only be one, not zero, because
  # of the checks above.
  if (n_known < 3L) {
    if (n_known == 2L) {
      return(nna - 1L)
    }
    return(nna)
  }

  # Using a helper function, count the steps from the central value outward in
  # each direction where the value is still the same as in the center:
  steps_left  <- count_central_steps(x[half_lower] == x[rev(seq_len(half_upper - 1L))])
  steps_right <- count_central_steps(x[half_upper] == x[(half_lower + 1L):n_known])

  # Calculate the maximal number of `NA`s that can be tolerated -- i.e., that
  # don't need to be ignored -- when attempting to determine the median:
  nna_tolerated <- if (n_known_is_even) {
    min(steps_left, steps_right)
  } else {
    min(steps_left, steps_right) * 2L
  }

  # Subtract this quantity from the total number of missing values to get the
  # number of `NA`s that are *not* tolerated, and therefore need to be ignored.
  # If this number is greater than `nna` -- i.e., if the distribution of known
  # values could tolerate more `NA`s than there actually are -- the resulting
  # negative number needs to be replaced by zero because there can't be a
  # negative amount of values.
  nna_ignored <- max(0L, nna - nna_tolerated)

  # Return the result, either as an absolute number or as a share of all `NA`s:
  switch(
    format,
    "total"      = nna_ignored,
    "proportion" = nna_ignored / nna
  )

}


#' Count steps from the central value where the same value is still found
#'
#' @description The internal helper `count_central_steps()` is used within
#'   `median_count_na_ignore()`.
#'
#'   The function starts from one of the two central values (upper or lower) of
#'   the sorted distribution after all `NA`s were removed. It counts the steps
#'   outward (i.e., up or down) where the value is still the same as the central
#'   value. Finally, it returns the number of these steps.
#'
#' @param test_arm Logical vector resulting from the `==` comparison between one
#'   of the two (lower and upper) central values and the rest of the (lower or
#'   upper) half of the sorted distribution.
#'
#' @return Integer (length 1).
#'
#' @noRd
count_central_steps <- function(test_arm) {
  n_steps <- match(FALSE, test_arm) - 1L

  # # Maybe the rest of the function can be replaced by just the next block? This
  # # would require that the two special cases that are currently checked cover
  # # all possible cases. In other words, if `test_arm` is `NA`, either
  # # `length(test_arm) == 0L` or `all(test_arm)` is `TRUE`. So the challenge is
  # # to figure out whether that is correct.
  # if (is.na(n_steps)) {
  #   length(test_arm)
  # } else {
  #   n_steps
  # }

  if (!is.na(n_steps)) {
    n_steps
  } else if (length(test_arm) == 0L || all(test_arm)) {
    # message("Length of test arm returned!")
    length(test_arm)
  } else {
    stop(paste(
      "Violation of internal assumption in `median_count_na_ignore()` -->",
      "`count_central_steps()`. Please report to the maintainer of naidem."
    ))
  }

}


# For interactive use -- shows what the algorithm must be able to get behind:
print_sort_both <- function(x) {
  n <- length(x)
  x <- sort(x[!is.na(x)])
  nna <- n - length(x)
  na_all <- rep(x[NA_integer_], nna)
  print(c(na_all, x))
  print(c(x, na_all))
}
