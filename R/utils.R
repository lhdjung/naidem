
# Adapted with modifications from the examples of the `?integer` documentation.
# For each element of `x`, this vectorized helper checks whether it is
# *conceptually* an integer (but not necessarily by object type). If `x` is not
# numeric, a vector of `FALSE` values with the same length as `x` is returned
# because every single element is not a (whole) number.
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  if (is.numeric(x)) {
    abs(x - round(x)) < tolerance
  } else {
    logical(length(x))
  }
}


# This helper handles the `na.rm.amount` argument in the proper mode functions:
# `mode_first()`, `mode_all()`, and `mode_single()`. It removes a number of
# missing values from `x` equal to `na.rm.amount`, then returns `x`. Notes:
# -- The specification of `na.rm.from` should be checked by the calling
# function, like `na.rm.from <- match.arg(na.rm.from)`.
# -- For efficiency, `decrease_na_amount()` should only be called under very
# specific conditions, as in `mode_first()` etc.
decrease_na_amount <- function(x, na.rm, na.rm.amount, na.rm.from = "first") {
  # Check for misspecifications of the calling function's arguments:
  if (na.rm) {
    stop(paste(
      "Conflicting instructions: `na.rm` removes all missing values,",
      "`na.rm.amount` only removes some number of them."
    ))
  }
  amount_is_wrong <- length(na.rm.amount) != 1L ||
    !is_whole_number(na.rm.amount) ||
    na.rm.amount < 0
  if (amount_is_wrong) {
    stop("`na.rm.amount` must be a single whole, non-negative number.")
  }
  # Determine the indices of missing values in `x`:
  na_indices <- which(is.na(x))
  # Special rules apply in edge cases:
  # -- Vectors without any `NA`s have nothing to remove, so they should be
  # returned as they are.
  # -- In case no `NA`s remain after subtracting `na.rm.amount` from the number
  # of missing values in `x`, all of them need to be removed from `x`. Taking
  # `na.rm.from` into account, as below, is not necessary here because this
  # argument is only relevant if some `NA`s will be left over.
  if (length(na_indices) == 0L) {
    return(x)
  } else if (length(na_indices) <= na.rm.amount) {
    return(x[-na_indices])
  }
  # Target the indices of those missings that should be ignored:
  na_indices_ignored <- switch(
    na.rm.from,
    "first"  = utils::head(na_indices, na.rm.amount),
    "last"   = utils::tail(na_indices, na.rm.amount),
    "random" = sample(na_indices, na.rm.amount)
  )
  # If no `NA`s are to be ignored, `x` should be returned as it is:
  if (length(na_indices_ignored) == 0L) {
    return(x)
  }
  # Return `x`, excluding the values in question:
  x[-na_indices_ignored]
}

