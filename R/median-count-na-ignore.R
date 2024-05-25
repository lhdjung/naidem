#' How many `NA`s need to be ignored for a median estimate?
#'
#' @description `median_count_na_ignore()` returns the number of missing values
#'   that need to be ignored to safely determine the median of the remaining
#'   values. The point is to retain as many `NA`s as possible when looking for a
#'   median estimate, instead of simply ignoring all of them.
#'
#'   This function is currently experimental. It only works if the number of
#'   known values is odd (see examples).
#'
#' @param x A numeric vector or similar.
#'
#' @return Integer (length 1). The value ranges from zero to
#'   `length(which(is.na(x)))`, i.e., the total number of missing values in `x`.
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
#' # state as in the first case:
#' median_count_na_ignore(c(8, 8, 8, NA, NA, NA))
#'
#' # However, the function wrongly treats
#' # this in the same way:
#' median_count_na_ignore(c(8, 9, NA, NA, NA))


# x <- c(1, 7, 7, 7, 7, NA)
# x <- c(6, 7, 7, 7, 7, NA)
# x <- c(8, 8, 9, 9, NA, NA, NA)


median_count_na_ignore <- function(x) {
  n <- length(x)
  x <- sort(x[!is.na(x)])
  n_known <- length(x)
  nna <- n - n_known

  # # New attempt -- is it enough to count points of overlap? No, this is too
  # # simple: it won't detect the difference between, e.g., `c(8, 8, NA, NA, NA)`
  # # and `c(8, 9, NA, NA, NA)`.
  # na_all <- rep(x[NA_integer_], times = nna)
  # nna_tolerated <- length(which(c(na_all, x) == c(x, na_all)))
  # return(max(0L, nna - nna_tolerated))

  # With an odd number of known values, there is only one central index for
  # them. Even so, however, this single value is assigned to two variables
  # because these are needed in case of an even number. This avoids having to
  # split the remainder of the code into one odd-length and one even-length arm.
  if (n_known %% 2L == 1L) {
    half_lower <- (n_known + 1L) %/% 2L
    half_upper <- half_lower
    one_if_even <- 0L
  } else {
    # stop("Even length of known values not yet supported!")
    half_lower <- n_known %/% 2L
    half_upper <- half_lower + 1L
    one_if_even <- 1L
  }

  # # Starting at the central index (or indices), go out "left" and "right", i.e.,
  # # toward the start and the end of the distribution. Record the numbers of
  # # steps between the value(s) at the central index / indices and those on the
  # # left and right. If no match is found, the number of steps that can be taken
  # # in the respective direction is zero, so `0L` is returned as a safe fallback:
  # steps_left  <- match(x[half_lower], rev(x[seq_len(half_lower - 1L)]), 0L)
  # steps_right <- match(x[half_upper], x[(half_upper + add_upper + 1L):n_known], 0L)

  # indices_x_arm <- seq_len(n_known %/% 2L + one_if_even)
  # left_test_arm_against_half  <- x[half_lower] == rev(x[indices_x_arm])
  # right_test_arm_against_half <- x[half_upper] == x[indices_x_arm + half_lower]

  # steps_left  <- match(FALSE, left_test_arm_against_half)  - 1L
  # steps_right <- match(FALSE, right_test_arm_against_half) - 1L

  # TODO: Fix this; it seems correct if `n_known` is odd, but not if it's even.
  steps_left <- count_steps_within_center(
    test_arm_against_half = x[half_lower] == rev(x[seq_len(half_upper - 1L)]),
    one_if_even_else_zero = one_if_even
  )

  steps_right <- count_steps_within_center(
    test_arm_against_half = x[half_upper] == x[(half_lower + 1L):n_known],
    one_if_even_else_zero = one_if_even
  )

  # Calculate the maximal number of `NA`s that can be tolerated -- i.e., that
  # don't need to be ignored -- when attempting to determine the median:
  nna_tolerated <- 2L * min(steps_left, steps_right)

  # Subtract this quantity from the total number of missing values to get the
  # number of `NA`s that are *not* tolerated, and therefore need to be ignored.
  # If this number is greater than `nna` -- i.e., if the distribution of known
  # values could tolerate more `NA`s than there actually are -- the resulting
  # negative number needs to be replaced by zero because it is not possible to
  # ignore a negative amount of values.
  max(0L, nna - nna_tolerated)
}


# Helper function used within `median_count_na_ignore()`. If the output of
# `match()` is `NA`, provide an appropriate replacement based on the logic of
# the two "arms" of the distribution:
count_steps_within_center <- function(test_arm_against_half,
                                      one_if_even_else_zero) {
  steps <- match(FALSE, test_arm_against_half) - 1L
  if (!is.na(steps)) {
    return(steps)
  }
  message("Steps derived, not directly counted")
  if (length(test_arm_against_half) == 0L) {
    0L
  } else if (all(test_arm_against_half)) {
    length(test_arm_against_half) + one_if_even_else_zero
  } else {
    stop(paste(
      "Unknown error in `median_count_na_ignore()` -->",
      "`count_steps_within_center()`"
    ))
  }
}


# Or: vector subtraction of the values at the central indices; or of the entire
# vectors? Binary search could come in when looking for the first `NA` instead
# of going from the middle out to the left and right.



# median_count_na_ignore_old <- function(x) {
#   n <- length(x)
#   x <- sort(x[!is.na(x)])
#   nna <- n - length(x)
#   half <- if (n %% 2L == 1L) {
#     (n + 1L) %/% 2L
#   } else {
#     (n + 1L:2L) %/% 2L
#   }
#   if (n == nna) {
#     return(nna)
#   } else if (length(x) > nna && isTRUE(all(x[half - nna] == x[half]))) {
#     return(0L)
#   }
#   steps_through_na <- if (all(x == x[1L])) {
#     nna - length(x) + 1L
#   } else {
#     match(x[1L], rev(x))
#   }
#   min(nna, steps_through_na)
# }

# n_side <- n %/% 2L
# (nna + 1L) - n_side
# na_all <- rep(x[NA_integer_], nna)
# nna_reduced <- nna - length(which(c(na_all, x) == c(x, na_all)))
# max(0L, nna_reduced)
# half <- if (n %% 2L == 1L) {
#   (n + 1L) %/% 2L
# } else {
#   (n + 1L:2L) %/% 2L
# }
# x[half - nna] == x[half]
# na_all <- rep(x[NA_integer_], nna)
# nna_reduced <- nna - length(which(c(na_all, x) == c(x, na_all)))
# max(0L, nna_reduced)
# n_side <- n %/% 2L
# min(nna, match(x[1L], rev(x)))
# half_upper <- if (n %% 2L == 1L) {
#   n_side + 2L
# } else {
#   n_side + 1L
# }
# # Starting from the inside, going out along the non-`NA` paths:
# side_left <- x[n_side:1L]
# side_right <- x[nna + (half_upper:length(x))]
# first_match <- match(side_right[1L], side_left)
# if (is.na(first_match)) {
#   return(nna)
# }
# max(0L, nna - first_match)
# if (nna >= n_side) {
#   return(nna)
# }
# min(nna, )
# half <- x[n_side]
# half_offset <- x[n_side - nna]
# frequency_first <- length(x[x == x[1L]])
# max(0L, nna - frequency_first + 1L)


# half <- (n + 1L) %/% 2L
# offset <- n %% 2L
# steps_back <- match(x[1L], rev(x))

# first_from_back <- length(x) - match(x[1L], rev(x)) + 1L
# steps_back <- max(0L, first_from_back + nna)
# return(steps_back)
# min(first_from_back, nna)
# max(0L, steps_back)
# max(0L, nna - length(x) + steps_back)
# x[half - nna]

# # I thought this would be needed above, but it doesn't seem to be:
# if (length(x) %% 2L == 1L) {
#   steps_back <- 1L + match(x[1L], rev(x))
#   min(steps_back, nna)
# } else {
#   half_lower <- n / 2L
#   half_lower_all_na_less <- half_lower - nna
#   # ...
# }


# For interactive use -- shows what the algorithm must be able to get behind:
print_sort_both <- function(x) {
  n <- length(x)
  x <- sort(x[!is.na(x)])
  nna <- n - length(x)
  na_all <- rep(x[NA_integer_], nna)
  print(c(na_all, x))
  print(c(x, na_all))
}
