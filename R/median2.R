#' Median with missing values analysis
#'
#' @description Compute the sample median.
#'
#'   By default, `median2()` works like the standard [`median()`] unless one or
#'   more values are missing: `median()` always returns `NA` in this case, but
#'   `median2()` checks if the median can be determined nevertheless.
#'
#' @param x Numeric or similar. Vector to search for its median.
#' @param na.rm Logical. If set to `TRUE`, missing values are removed before
#'   computation proceeds. Default is `FALSE`.
#' @param na.rm.amount Numeric. Alternative to `na.rm` that only removes a
#'   specified number of missing values. Default is `0`.
#' @param na.rm.from String. If `na.rm.amount` is used, from which position in
#'   `x` should missing values be removed? Options are `"first"`, `"last"`, and
#'   `"random"`. Default is `"first"`.
#' @param even String. What to do if `x` has an even length and contains no
#'   missing values (or they were removed). The default, `"mean"`, averages the
#'   two central values, `"low"` returns the lower central value, and `"high"`
#'   returns the higher one.
#' @param ... Optional further arguments for methods. Not used in the default
#'   method.
#'
#' @details `median2()` is a generic function, so new methods can be defined for
#'   it. As with [`stats::median()`] from base R, the default method described
#'   here should work for most classes for which a median is a reasonable
#'   concept (e.g., "[`Date`]").
#'
#'   If a new method is necessary, please make sure it deals with missing values
#'   like the default method does. See
#'   \href{https://lhdjung.github.io/naidem/articles/algorithm.html}{*Implementing
#'   the algorithm*} for further details.

#' @return Length-1 vector of the same type as `x`. The only exception occurs if
#'   `x` is logical or integer and its length is even, in which case the return
#'   value is double.
#'
#'   The output is `NA` (of the same type as `x`) if and only if the median
#'   can't be determined because of missing values, or if there are no values.

#' @export
#'
#' @author Lukas Jung, R Core Team
#'
#' @examples
#' # If no values are missing,
#' # it works like `median()`:
#' median(1:4)
#' median2(1:4)
#'
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

median2 <- function(x, na.rm = FALSE, na.rm.amount = 0,
                    na.rm.from = c("first", "last", "random"),
                    even = c("mean", "low", "high"), ...) {
  UseMethod("median2")
}


# Some of this code is copied from `stats::median.default()`. The default method
# for `median2()` is only different from that for `stats::median()` if one or
# more values are missing from `x`.

#' @name median2
#' @export

median2.default <- function(x, na.rm = FALSE, na.rm.amount = 0,
                            na.rm.from = c("first", "last", "random"),
                            even = c("mean", "low", "high"), ...) {
  na.rm.from <- match.arg(na.rm.from)
  even <- match.arg(even)
  if (is.factor(x) || is.data.frame(x))
    stop("need numeric data")
  # The user may choose to ignore any number of missing values (see the utils.R
  # file for the `decrease_na_amount()` helper function):
  if (!missing(na.rm.amount)) {
    x <- decrease_na_amount(x, na.rm, na.rm.amount, na.rm.from)
  }
  if (length(names(x)))
    names(x) <- NULL
  if (na.rm)
    x <- x[!is.na(x)]
  ### START of key part
  else if (anyNA(x)) {
    # Using an `n` variable for consistency with the older code at the bottom:
    n <- length(x)
    x <- sort(x[!is.na(x)])
    nna <- n - length(x)
    # Central index or indices in `x`; length 1 if the length of `x` is odd,
    # length 2 if it is even:
    half <- if (n %% 2L == 1L) {
      (n + 1L) %/% 2L
    } else {
      (n + 1L:2L) %/% 2L
    }
    # Check for non-positive indices:
    if (any(nna >= half)) {
      return(x[NA_integer_])
    }
    # Check for equality with offset value(s); see
    # https://lhdjung.github.io/naidem/articles/algorithm.html for details:
    if (isTRUE(all(x[half - nna] == x[half]))) {
      return(x[half[1L]])
    }
    return(x[NA_integer_])
  }
  ### END of key part
  n <- length(x)
  if (n == 0L)
    return(x[NA_integer_])
  half <- (n + 1L)%/%2L
  if (n%%2L == 1L) {
    sort(x, partial = half)[half]
  } else {
    # In keeping with the original base R code, `x` is reduced
    # to its values at the two central indices here:
    x <- sort(x, partial = half + 0L:1L)[half + 0L:1L]
    switch(
      even,
      "mean" = mean(x),
      "low"  = x[1L],
      "high" = x[2L]
    )
  }
}

