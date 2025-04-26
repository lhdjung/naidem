#' Drop-in replacement for `median()`
#'
#' @description `median2()` computes the sample median. By default, it works
#'   like [`median()`] from base R, with these exceptions:
#'   - If one or more values are missing, `median2()` checks if the median can
#'   be determined nevertheless. `median()` always returns `NA` in this case,
#'   but `median2()` only returns `NA` if the median is genuinely unknown.
#'   - You can opt to only ignore a certain number of missing values using the
#'   `na.rm.amount` and `na.rm.from` arguments.
#'   - Factors and all other data that can be ordered by [`sort()`] are allowed.
#'   However, non-numeric data, including dates and factors, require one of
#'   `even = "low"` and `even = "high"`. This avoids "computing the mean" of the
#'   two central values of sorted vectors with an even length when no such
#'   operation is possible, e.g., with strings.
#'   - The return type is always double if the input vector is numeric (i.e.,
#'   double or integer). This is consistent and predictable, regardless of the
#'   length being even or odd.
#'
#' @param x Vector that can be ordered using [`sort()`]. It will be searched for
#'   its median.
#' @param na.rm Logical. If set to `TRUE`, missing values are removed before
#'   computation proceeds. Default is `FALSE`.
#' @param na.rm.amount Numeric. Alternative to `na.rm` that only removes a
#'   specified number of missing values. Default is `0`.
#' @param na.rm.from String. If `na.rm.amount` is used, from which position in
#'   `x` should missing values be removed? Options are `"first"`, `"last"`, and
#'   `"random"`. Default is `"first"`.
#' @param even String. What to return if `x` has an even length and contains no
#'   missing values (or they were removed). The default, `"mean"`, averages the
#'   two central values of the sorted vector, `"low"` returns the lower central
#'   value, and `"high"` returns the higher one. Note that `"mean"` is only
#'   allowed if `x` is numeric.
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

#' @return Length-1 vector of type double if the input is numeric, and the same
#'   type as `x` otherwise. This is tested by [`is.numeric()`], so factors and
#'   dates do not count as numeric.
#'
#' @export
#'
#' @author Lukas Jung, R Core Team
#'
#' @examples
#' # If no values are missing,
#' # it works mostly like `median()`:
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

median2 <- function(
    x,
    na.rm = FALSE,
    na.rm.amount = 0,
    na.rm.from = c("first", "last", "random"),
    even = c("mean", "low", "high"),
    ...
  ) {
  UseMethod("median2")
}


# Some of this code is copied from `stats::median.default()`.

#' @name median2
#' @export

median2.default <- function(
    x,
    na.rm = FALSE,
    na.rm.amount = 0,
    na.rm.from = c("first", "last", "random"),
    even = c("mean", "low", "high"),
    ...
  ) {

  # Validate arguments
  if (is.list(x)) {
    cli::cli_abort("Need data that can be ordered using `sort()`.")
  }
  na.rm.from <- rlang::arg_match(na.rm.from)
  even <- rlang::arg_match(even)

  x_is_numeric <- is.numeric(x)

  # Prevent even-length problems
  if (!x_is_numeric && even == "mean") {
    error_non_numeric_mean(x)
  }

  # The user may choose to ignore any number of missing values (see the utils.R
  # file for the `remove_some_na()` helper function):
  if (na.rm.amount != 0) {
    x <- remove_some_na(x, na.rm, na.rm.amount, na.rm.from)
  }

  # Remove any names
  if (length(names(x))) {
    names(x) <- NULL
  }

  # Handle missing values
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (anyNA(x)) {
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
    # Check for non-positive indices. If `nna` is too high, return a missing
    # value. It has type double if the input is numeric, and the type of `x`
    # otherwise. This logic is repeated at all points in the function where a
    # value may be returned.
    if (any(nna >= half)) {
      if (x_is_numeric) {
        return(NA_real_)
      }
      return(x[NA_integer_])
    }
    # Check for equality with offset value(s); see
    # https://lhdjung.github.io/naidem/articles/algorithm.html for details.
    # Also, note that `near_or_equal()` is a variant of `dplyr::near()` that
    # compares non-numeric data using `==`.
    out <- if (isTRUE(all(near_or_equal(x[half - nna], x[half])))) {
      x[half[1L]]
    } else {
      x[NA_integer_]
    }
    if (x_is_numeric) {
      return(as.numeric(out))
    }
    return(out)
  }

  n <- length(x)

  if (n == 0L) {
    if (is.null(x)) {
      return(NULL)
    }
    if (x_is_numeric) {
      return(NA_real_)
    }
    return(x[NA_integer_])
  }

  half <- (n + 1L)%/%2L

  out <- if (n%%2L == 1L) {
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

  if (x_is_numeric) {
    as.numeric(out)
  } else {
    out
  }

}

