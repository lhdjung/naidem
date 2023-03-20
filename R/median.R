#' Median with missing values analysis
#'
#' @description Compute the sample median.
#'
#'   `median_na()` works like the standard [`median()`] unless one or more
#'   values are missing: `median()` always returns `NA` in this case, but
#'   `median_na()` checks if the median can be determined nevertheless.
#'
#' @param x Numeric or similar. Vector to search for its median.
#' @param na.rm Boolean. If set to `TRUE`, missing values are removed before
#'   computation proceeds. Default is `FALSE`.
#' @param ... Optional further arguments for methods. Not used in the default
#'   method.
#'
#' @details `median_na()` is a generic function, so new methods can be defined
#'   for it. As with [`stats::median()`], the default method should work for
#'   most classes for which a median is a reasonable concept (e.g., "[`Date`]").
#'
#'   If a new method is necessary, please make sure it takes care with missing
#'   values like `median_na.default()` does.
#'
#' @return Length-1 vector of the same type as `x`. (The only exception: If `x`
#'   is logical or integer and has an even length, the result will be a double.)
#'
#'   Returns `NA` of the same type as `x` if and only if the median can't be
#'   determined because of missing values, or if there are no values.
#'
#' @export
#'
#' @examples
#' # If no values are missing,
#' # it works like `median()`:
#' median_na(1:4)                # = 2.5 [even number]
#' median_na(c(1:3, 100, 1000))  # = 3 [odd, robust]
#'
#' # With some `NA`s, the median can
#' # sometimes still be determined:
#' median_na(c(0, 1, 1, 1, NA))          # = 1
#' median_na(c(0, 0, NA, 0, 0, NA, NA))  # = 0

median_na <- function(x, na.rm = FALSE, ...) {
  UseMethod("median_na")
}


# Most of this code is copied from `stats::median.default()`. The only
# difference is in the line that says `return(decide_median_or_na(x))` instead
# of `return(x[NA_integer_])`. This is the only way in which the default method
# for `naidem::median_na()` should differ from `stats::median.default()`:
# handling input vectors with one or more missing values.

#' @name median_na
#' @export

median_na.default <- function(x, na.rm = FALSE, ...) {
  if (is.factor(x) || is.data.frame(x))
    stop("need numeric data")
  if (length(names(x)))
    names(x) <- NULL
  if (na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
  ### START of new code
    return(decide_median_or_na(x)) # used to be: return(x[NA_integer_])
  ### END of new code
  n <- length(x)
  if (n == 0L)
    return(x[NA_integer_])
  half <- (n + 1L)%/%2L
  if (n%%2L == 1L)
    sort(x, partial = half)[half]
  else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}


# Internal helper function:
decide_median_or_na <- function(x) {
  half <- if (length(x) %% 2L == 1L) {
    (length(x) + 1L) %/% 2L
  } else {
    (length(x) + 1L:2L) %/% 2L
  }
  # xna <- x[is.na(x)]
  nna <- length(x[is.na(x)])
  x <- sort(x[!is.na(x)])
  # if (!isTRUE(all(c(x, xna)[half] == c(xna, x)[half]))) {
  # # Experimental replacement for the condition right above (using `nna`,
  # # the number of missings; instead of `xna`, the missings themselves):
  if (!isTRUE(all(x[half] == x[half - nna]))) {
    return(x[NA_integer_])
  } else if (length(half) == 2L) {
    return(mean(x[half]))
  } else {
    return(x[half])
  }
}

