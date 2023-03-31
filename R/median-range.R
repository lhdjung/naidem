
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
#' @name median-possible
#'
#' @export
#'
#' @examples

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
    return(sort(unique(x[!is.na(x)])))
  }
  x <- sort(x[!is.na(x)])
  # Could there be a "missing middle" here?:
  sort(unique(c(x[half], x[half - nna])))
}

#' @name median-possible
#' @export

median_range.default <- function(x) {
  x <- median_possible_values(x)
  c(x[[1L]], x[[length(x)]])
}


