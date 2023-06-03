#' Median with missing values analysis
#'
#' @description Compute the sample median.
#'
#'   `median2()` works like the standard [`median()`] unless one or more values
#'   are missing: `median()` always returns `NA` in this case, but `median2()`
#'   checks if the median can be determined nevertheless.
#'
#' @param x Numeric or similar. Vector to search for its median.
#' @param na.rm Boolean. If set to `TRUE`, missing values are removed before
#'   computation proceeds. Default is `FALSE`.
#' @param ... Optional further arguments for methods. Not used in the default
#'   method.
#'
#' @details `median2()` is a generic function, so new methods can be defined for
#'   it. As with [`stats::median()`], the default method should work for most
#'   classes for which a median is a reasonable concept (e.g., "[`Date`]").
#'
#'   If a new method is necessary, please make sure it deals with missing values
#'   like `median2.default()` does.

# # DON'T REFLOW THIS SECTION:

#' @return Length-1 vector of the same type as `x`. The only exception:
#'   If `x` is logical or integer and has an even length, the result will be a
#'   double.
#'
#'   Returns `NA` (of the same type as `x`) if and only if the median can't be
#'   determined because of missing values, or if there are no values.

#' @export
#'
#' @author Lukas Jung, R Core Team
#'
#' @examples
#' # If no values are missing,
#' # it works like `median()`:
#' median(1:4)
#' median2(1:4)
#' median(c(1:3, 100, 1000))
#' median2(c(1:3, 100, 1000))
#'
#' # With some `NA`s, the median can
#' # sometimes still be determined...
#' median2(c(0, 1, 1, 1, NA))
#' median2(c(0, 0, NA, 0, 0, NA, NA))
#'
#' # ...unless there are too many `NA`s...
#' median2(c(0, 1, 1, 1, NA, NA))
#'
#' # ...or too many unique values:
#' median2(c(0, 1, 2, 3, NA))

median2 <- function(x, na.rm = FALSE, ...) {
  UseMethod("median2")
}


# Some of this code is copied from `stats::median.default()`. The default method
# for `median2()` is only different from that for `stats::median()` if one or
# more values are missing from `x`.

#' @name median2
#' @export

median2.default <- function(x, na.rm = FALSE, ...) {
  if (is.factor(x) || is.data.frame(x))
    stop("need numeric data")
  if (length(names(x)))
    names(x) <- NULL
  if (na.rm)
    x <- x[!is.na(x)]
  ### START of new code
  else if (anyNA(x)) {
    n <- length(x)
    # Central index or indices in `x`; length 1 if the length of `x` is odd,
    # length 2 if it is even:
    half <- if (n %% 2L == 1L) {
      (n + 1L) %/% 2L
    } else {
      (n + 1L:2L) %/% 2L
    }
    nna <- length(x[is.na(x)])
    # Check for non-positive indices:
    if (any(nna + 1L > half)) {
      return(x[NA_integer_])
    }
    x <- sort(x[!is.na(x)])
    # Check for equality with offset value(s); see `vignette("algorithm")` for
    # details:
    if (!isTRUE(all(x[half] == x[half - nna]))) {
      return(x[NA_integer_])
    } else if (length(half) == 2L) {
      return(mean(x[half]))
    } else {
      return(x[half])
    }
  }
  ### END of new code
  n <- length(x)
  if (n == 0L)
    return(x[NA_integer_])
  half <- (n + 1L)%/%2L
  if (n%%2L == 1L)
    sort(x, partial = half)[half]
  else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}

