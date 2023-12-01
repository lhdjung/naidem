
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


# This helper handles the `na.rm.amount` argument in `median2()`. It removes a
# number of missing values from `x` equal to `na.rm.amount`, then returns `x`.
decrease_na_amount <- function(x, na.rm, na.rm.amount, na.rm.from) {
  # Check for misspecifications of the calling function's arguments:
  if (na.rm) {
    msg_error <- paste(
      "Conflicting instructions: `na.rm` removes all missing values,",
      "`na.rm.amount` only removes some number of them."
    )
    stop(msg_error)
  }
  amount_is_wrong <- length(na.rm.amount) != 1L ||
    !is_whole_number(na.rm.amount) ||
    na.rm.amount < 0
  if (amount_is_wrong) {
    stop("`na.rm.amount` must be a single whole, non-negative number.")
  }
  # In vectors without any missing values, there is nothing to remove, so they
  # should be returned as they are:
  if (!anyNA(x)) {
    return(x)
  }
  # Record the indices of missing values in `x`:
  x_missing <- which(is.na(x))
  # Subtract `na.rm.amount` from the number of missing values:
  nna <- length(x_missing) - na.rm.amount
  # In case no `NA`s remain after this subtraction, all of them need to be
  # removed from `x`:
  if (nna <= 0L) {
    return(x[-x_missing])
  }
  # Target the indices of those missings that should be ignored:
  x_missing_but_ignored <- switch(
    na.rm.from,
    "first"  = utils::head(x_missing, length(x_missing) - nna),
    "last"   = utils::tail(x_missing, length(x_missing) - nna),
    "random" = sample(x_missing, size = length(x_missing) - nna)
  )
  # Return `x`, excluding those values:
  x[-x_missing_but_ignored]
}
