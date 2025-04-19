#' Bounds on an unknown median
#'
#' @description `median_bounds()` computes the minimal and maximal possible
#'   median values. This is helpful if [`median2()`] returns `NA`: the median
#'   can't be determined, but at least it might have lower and upper bounds.
#'
#'   It is used within [`median_table()`] to compute the `lower` and `upper`
#'   columns.
#'
#' @param x Numeric or similar. Vector to search for its possible medians.
#' @param x Vector that can be ordered using [`sort()`]. It will be searched for
#'   its possible medians.
#' @param na.rm.amount,even Passed on to [`median2()`].
#' @param nna Integer. Ignore unless the function is used as a helper. It is
#'   like `needs_prep` in [`median_count_tolerable()`] except it can submit the
#'   number of missing values.
#'
#' @details Two edge cases may occur:
#'
#'   - If the median can be precisely determined, `median_bounds()` returns two
#'   identical values. This is the same case in which [`median2()`] would return
#'   a non-`NA` value, and [`median_plot_errorbar()`] would show a "ring of
#'   certainty".
#'   - If the number of missing values is so high that a continuous array of
#'   them could extend from the start or end of `x` into the median position,
#'   the data do not constrict the median to fall in between any finite bounds.
#'   The function will then return `c(NA, NA)` (of the appropriate type) because
#'   such an array of `NA`s would act as a tunnel to negative or positive
#'   infinity, enabling the median to assume indefinitely low or high values.
#'
#'   In the second case, `c(NA, NA)` means there are no bounds, not that
#'   existing bounds are unknown. This is unfortunate but necessary: although
#'   `c(-Inf, Inf)` would be more appropriate conceptually, it is always of type
#'   double, so it would lead to coercion bugs when combined with non-numeric
#'   data, which is possible in [`median_table()`]. To illustrate, try `c("abc",
#'   Inf)`.
#'
#'   Like [`median2()`], this function is generic, so methods can be defined for
#'   other classes. This documentation describes the default method.
#'
#' @return Vector of length 2. Its type is double if `x` is numeric (double or
#'   integer). Otherwise, it has the same type as `x`.
#'
#' @name median-bounds
#'
#' @export
#'
#' @author Lukas Jung, R Core Team
#'
#' @examples
#' # Lower and upper bounds can be found,
#' # even though the precise median is unknown:
#' median_bounds(c(7, 7, 8, 9, NA))
#' median_bounds(c(7, 7, 7, 8, 9, 9, NA, NA))
#'
#' # Too many missing values, so there is no finite range:
#' median_bounds(c(7, 7, 8, 9, NA, NA, NA, NA))


median_bounds <- function(
    x,
    na.rm.amount = 0,
    even = c("mean", "low", "high"),
    nna = NULL
  ) {
  UseMethod("median_bounds")
}


#' @name median-bounds
#' @export

median_bounds.default <- function(
    x,
    na.rm.amount = 0,
    even = c("mean", "low", "high"),
    nna = NULL
  ) {
  # As in `median2.default()`:
  even <- match.arg(even)
  x_is_numeric <- is.numeric(x)
  n <- length(x)
  # This is also checked in `median2()`. It is only needed because one early
  # return below would otherwise make it possible that a call with wrong
  # arguments would silently pass through without reaching a `median2()` call.
  if (!x_is_numeric && even == "mean") {
    error_non_numeric_mean(x)
  }
  # The `nna` argument here follows the same basic idea as `needs_prep` in
  # `median_count_tolerable()`. However, it is integer instead of logical; and
  # if specified, it needs to be added to `n` which, in this case, previously
  # was just the number of known values. Note: this requires `x` to no longer
  # have any `NA`s, and to be sorted!
  if (is.null(nna)) {
    x <- sort(x[!is.na(x)])
    nna <- n - length(x)
  } else {
    n <- n + nna
  }
  # As in `median2()`:
  half <- if (n %% 2L == 1L) {
    (n + 1L) %/% 2L
  } else {
    (n + 1L:2L) %/% 2L
  }
  # Special rules apply to extremely low or high numbers of missing values:
  # -- If no values are missing, there is only one possible median, and it can
  # be determined by `median2()`.
  # -- If the number of missing values is so high that the `NA`s would extend
  # into the median position if all of them were either at the start or the end,
  # the data do not constrict the bounds to any finite values. For type safety,
  # however, `NA`s need to be returned instead of `Inf`s. See details.
  if (nna == 0L) {
    return(rep(median2(x, even = even), times = 2L))
  } else if (any(nna >= half)) {
    if (x_is_numeric) {
      return(c(NA_real_, NA_real_))
    }
    return(rep(x[NA_integer_], 2))
  }
  # Compute the bounds by checking what the median would be if all `NA`s were
  # positioned at the start or the end of `x`. This implementation creates such
  # hypothetical extreme values that stand in for the `NA`s by subtracting `1`
  # from the lowest known value and adding it to the greatest known value,
  # although any other positive number would do.
  bound_lower <- median2(
    x = c(rep(x[[1L]] - 1, times = nna), x),
    na.rm = FALSE,
    na.rm.amount = na.rm.amount,
    even = even
  )
  bound_upper <- median2(
    x = c(x, rep(x[[length(x)]] + 1, times = nna)),
    na.rm = FALSE,
    na.rm.amount = na.rm.amount,
    even = even
  )
  # Return the bounds:
  c(bound_lower, bound_upper)
}
