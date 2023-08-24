#' Possible medians
#'
#' @description These functions are helpful if `median2()` returns `NA`:
#' - `median_possible_values()` returns all values that might possibly be the
#'   median, given the number of missing values.
#' - `median_range()` returns the minimal and maximal possible median values.
#'
#' @param x Numeric or similar. Vector to search for its possible medians.
#'
#' @details Like `median2()`, these functions are generic, so methods can be
#'   defined for other classes. This documentation describes the default
#'   methods.
#'
#' @return Vector of the same type as `x`. Its values are unique and sorted. It
#'   always has length 2 when calling `median_range()`.
#'
#'   If the median can be determined (i.e., `median2()` would return a non-`NA`
#'   value), `median_possible_values()` returns just one value, and
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
#' @author Lukas Jung, R Core Team
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
  # As in `median2.default()`:
  n <- length(x)
  half <- if (n %% 2L == 1L) {
    (n + 1L) %/% 2L
  } else {
    (n + 1L:2L) %/% 2L
  }
  nna <- length(x[is.na(x)])
  x <- sort(x[!is.na(x)])
  # Some special rules:
  # -- If all values are known, the only possible median is the actual one as
  # determined by `median2()`.
  # -- If any central value is missing, there is no way to determine the
  # possible median values.
  if (nna == 0L) {
    return(median2(x))
  } else if (any(nna >= half)) {
    return(x[NA_integer_])
  }
  # Order `x` values, then compute the range of possible median locations:
  x <- sort(x)
  half_span <- c(half - nna)[1L]:half[length(half)]
  # For odd-length vectors only:
  if (length(half) == 1L) {
    return(unique(x[half_span]))
  }
  # The rest is for even-length vectors only. Each possible value (except for
  # the last) is computed by taking the mean of two consecutive values at the
  # possible median positions (`half_span`):
  out <- vector(typeof(x), length(half_span))
  for (i in seq_along(out)) {
    out[i] <- mean(x[half_span[i:(i + 1L)]])
  }
  # Compute the last possible value "manually":
  half_span_last <- half_span[length(half_span)]
  out[length(out)] <- mean(c(x[half_span_last], x[half_span_last + 1L]))
  unique(out[!is.na(out)])
}

#' @name median-possible
#' @export

median_range.default <- function(x) {
  x <- median_possible_values(x)
  c(x[[1L]], x[[length(x)]])
}

