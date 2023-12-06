#' Bounds on an unknown median
#'
#' @description `median_range()` computes the minimal and maximal possible
#'   median values. This is helpful if [`median2()`] returns `NA`: the median
#'   can't be determined, but at least its bounds might be known.
#'
#' @param x Numeric or similar. Vector to search for its possible medians.
#' @param na.rm.amount,even Passed on to [`median2()`].
#'
#' @details Like [`median2()`], this function is generic, so methods can be
#'   defined for other classes. This documentation describes the default method.
#'
#' @return Vector of length 2 and the same type as `x`.
#'
#'   If the median can be determined (i.e., [`median2()`] would return a
#'   non-`NA` value), `median_range()` returns two identical values.
#'
#'   If there are missing values at the median position, the median range cannot
#'   be determined. The function will then return `c(NA, NA)`.
#'
#' @name median-range
#'
#' @export
#'
#' @author Lukas Jung, R Core Team
#'
#' @examples
#' median_range(c(7, 7, 8, 9, NA))
#' median_range(c(7, 7, 7, 8, 9, 9, NA, NA))


median_range <- function(x, na.rm.amount = 0, even = c("mean", "low", "high")) {
  UseMethod("median_range")
}


#' @name median-range
#' @export

median_range.default <- function(x, na.rm.amount = 0,
                                 even = c("mean", "low", "high")) {
  # As in `median2.default()`:
  even <- match.arg(even)
  n <- length(x)
  half <- if (n %% 2L == 1L) {
    (n + 1L) %/% 2L
  } else {
    (n + 1L:2L) %/% 2L
  }
  x <- sort(x[!is.na(x)])
  nna <- n - length(x)
  # Some special rules apply if the number of missing values is either zero or
  # so high that the `NA`s extend into the median position (if all `NA`s are
  # either at the start or the end):
  # -- If all values are known, there is only one possible median, and it can be
  # determined by `median2()`.
  # -- If any central value is missing, there is no way to determine the bounds.
  if (nna == 0L) {
    return(rep(median2(x, even = even), length = 2L))
  } else if (any(nna >= half)) {
    return(rep(x[NA_integer_], length = 2L))
  }
  # Compute the bounds by checking what the median would be if all `NA`s were
  # positioned at the start or the end of `x`:
  bound_lower <- median2(
    c(rep(x[[1L]] - 1, times = nna), x),
    na.rm = FALSE, na.rm.amount = na.rm.amount, even = even
  )
  bound_upper <- median2(
    c(x, rep(x[[length(x)]] + 1, times = nna)),
    na.rm = FALSE, na.rm.amount = na.rm.amount, even = even
  )
  # Return the bounds:
  c(bound_lower, bound_upper)
}
