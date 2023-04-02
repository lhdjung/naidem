
# TO DO: THINK ABOUT NEW FUNCTION THAT COMPUTES THE RANGE OF THE MEDIAN. THIS
# MAY BE USEFUL IN CASES LIKE `median_na(c(7, 7, 7, 8, NA, NA))` WHERE THE
# MEDIAN IS EITHER `7` OR `7.5`. IT WOULD FIT WITHIN naidem BECAUSE IT ALSO
# WRINGS KNOWLEDGE ABOUT THE MEDIAN FROM DATA WITH MISSING VALUES.


#' Possible medians
#'
#' @description These functions are helpful if `median_na()` returns `NA`:
#' - `median_possible_values()` returns all values that might possibly be the
#'   median, given the number of missing values.
#' - `median_range()` returns the minimal and maximal possible median values.
#'
#' @param x Numeric or similar. Vector to search for its possible medians.
#'
#' @details Like `median_na()`, these functions are generic, so methods can be
#'   defined for other classes. This documentation describes the default
#'   methods.
#'
#' @return Vector of the same type as `x`. Its values are unique and sorted by
#'   size. It always has length 2 when calling `median_range()`.
#'
#'   If the median can be determined (i.e., `median_na()` would return a
#'   non-`NA` value), `median_possible_values()` returns just one value, and
#'   `median_range()` returns two identical values.
#'
#'   If there are missing values at the median position, the set of possible
#'   medians cannot be determined. The functions will then return `NA` or `c(NA,
#'   NA)`, respectively.
#'
#' @name median-possible
#'
#' @export
#'
#' @examples
#' # All possible values,
#' # plus the range:
#' x1 <- c(7, 7, 7, 8, 9, 9, NA, NA)
#' median_possible_values(x1)
#' median_range(x1)
#'
#' # The output of both functions
#' # is often the same:
#' x2 <- c(7, 7, 7, 8, NA, NA)
#' median_possible_values(x2)
#' median_range(x2)
#'
#' # If there are missing values at
#' # the median, even these functions
#' # return `NA`:
#' x3 <- c(1, 1, 2, NA, NA, NA)
#' median_possible_values(x3)
#' median_range(x3)

median_possible_values <- function(x) {
  UseMethod("median_possible_values")
}

#' @name median-possible
#' @export

median_range <- function(x) {
  UseMethod("median_range")
}

#' @name median-possible
#' @export

median_possible_values.default <- function(x) {
  half <- if (length(x) %% 2L == 1L) {
    (length(x) + 1L) %/% 2L
  } else {
    (length(x) + 1L:2L) %/% 2L
  }
  nna <- length(x[is.na(x)])
  if (nna == 0L) {
    return(median_na(x))
    # This part might be correct, but I'm genuinely unsure:
  } else if (any(nna >= half)) {
    return(x[NA_integer_])
  }
  x <- sort(x[!is.na(x)])
  half_span <- c(half - nna)[1L]:half[length(half)]
  if (length(half) == 2L) {
    out <- numeric(length(half_span))
    for (i in seq_along(half_span)) {
      out[i] <- sum(x[half_span[i:(i + 1L)]]) / 2
    }
    half_span_last <- half_span[length(half_span)]
    out[length(out)] <- sum(x[half_span_last], x[half_span_last + 1L]) / 2
    unique(out[!is.na(out)])
  } else {
    unique(x[c(half - nna)[1L]:half[length(half)]])
  }
}

#' @name median-possible
#' @export

median_range.default <- function(x) {
  x <- median_possible_values(x)
  c(x[[1L]], x[[length(x)]])
}

