
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
# Notes:
# -- The specification of `na.rm.from` should be checked by the calling
# function, like `na.rm.from <- match.arg(na.rm.from)`.
# -- For efficiency, `remove_some_na()` should only be called under very
# specific conditions, as in in `median2()`.
remove_some_na <- function(x, na.rm, na.rm.amount, na.rm.from = "first") {
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

  tryCatch(
    na_indices <- which(is.na(x)),
    error = stop_checking_na
  )

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


# Like `dplyr::near()` but able to handle non-numeric data, as well
near_or_equal <- function(x, y) {
  if (is.numeric(x)) {
    abs(x - y) < .Machine$double.eps^0.5
  } else {
    x == y
  }
}


# # Test function below with:
# geom <- geom_uncertainty_bars
# field <- "aes_params"
# aes_name <- "linewidth"
# aes_value <- 0.35

# Copied from an MIT-licensed repo:
# https://github.com/lhdjung/moder/blob/06195ed218b875a797b7175287cf3ff4c4b19350/R/utils.R
aes_add <- function(geom, field, aes_name, aes_value) {
  names(aes_value) <- aes_name
  geom[[field]] <- c(geom[[field]], aes_value)
  invisible(NULL)
}


# Keep record of corner case classes that are built on top of numeric types but
# don't put their values on the number line
get_type_classes <- function() {
  c("Date", "factor")
}


# Throw an error if `median2()` or a function that called it took a non-numeric
# `x` argument but failed to override the `even = "mean"` default. In the
# even-length scenario, such a function would compute the mean of the two
# central values, which is only meaningful for numeric data. Therefore, the user
# needs to explicitly choose the lower or the higher central value when taking
# the median of non-numeric vectors, including logicals.
stop_non_numeric_mean <- function(x) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  # Date and factor are classes, not types, so they need special treatment when
  # identifying why `x` is not the right kind of object here. In the future,
  # this could possibly be extended to other classes.
  data_label <- NULL
  type_classes <- get_type_classes()
  for (type in type_classes) {
    if (inherits(x, type)) {
      data_label <- tolower(paste0(type, "s"))
      break
    }
  }
  # Otherwise, the type is the problem
  if (is.null(data_label)) {
    data_label <- typeof(x)
    if (rlang::is_vector(x)) {
      data_label <- paste(data_label, "vectors")
    } else {
      data_label <- paste0(data_label, "s")
    }
  }
  # Give a begrudging nod to traditional bool conversion
  hint_for_logicals <- if (is.logical(x)) {
    paste(
      "Alternatively, you may use `as.numeric()` to explicitly coerce the",
      "logical vector beforehand."
    )
  } else {
    NULL
  }
  # Throw a bespoke error that names the directly calling function -- such as
  # `median2.default()` -- as its source.
  cli::cli_abort(
    message = c(
      "Median of {data_label} requires `even = \"low\"` or \
      `even = \"high\"`.",
      "x" = "You left the default `even = \"mean\"` in place.",
      "x" = "In case `x` has an even length, the mean of the two \
      central values of the sorted vector is computed by default, \
      but this is not possible with {data_label}.",
      "i" = "Choose `even = \"low\"` for the lower central value or \
      `even = \"high\"` for the higher one.",
      "i" = hint_for_logicals
    ),
    call = rlang::caller_call()
  )
}


# This takes `data$term` as an argument, where `data` is a data frame returned
# by `median_table()`. It fills any empty strings by an index number and returns
# `term` as a factor, which is useful for plots to keep the data in order.
as_factor_sequence <- function(term) {
  if (any(term == "")) {
    index_rows <- as.character(seq_along(term))
    empty <- term == ""
    term[empty] <- index_rows[empty]
  }
  factor(term, levels = term)
}


# Versions of ggplot2 before 3.4.0 have the `size` aesthetic instead of
# `linewidth`. This helper checks which name is needed on the user's system.
# Copied from an MIT-licensed repo:
# https://github.com/lhdjung/moder/blob/8bc72738f2a4ab7eb38f9a5c617dfee40f2eee83/R/utils.R
get_linewidth_name <- function() {
  if (utils::packageVersion("ggplot2") < "3.4.0") {
    "size"
  } else {
    "linewidth"
  }
}


# Error if `x` contains both numeric and non-numeric vectors. If not caught by
# such a check, this could lead to all kinds of automatic, silent coercion bugs.
# Copied from an MIT-licensed repo:
# https://github.com/lhdjung/moder/blob/59490e3eb585dff83584810b4ca5c759a55adb45/R/utils.R
check_types_consistent <- function(x) {

  x_is_numeric <- vapply(x, is.numeric, logical(1))

  if (all(x_is_numeric)) {
    return(invisible(NULL))
  }

  types <- vapply(x, typeof, character(1))

  # Given that not all elements are numeric, no element can be numeric or there
  # will be coercion bugs and incompatible scales. Similarly, all elements must
  # have the same (non-numeric) type.
  some_are_numeric <- any(x_is_numeric)
  some_are_unequal <- !all(types == types[1])

  if (!any(some_are_numeric, some_are_unequal)) {
    return(invisible(NULL))
  }

  # Demonstrate my knowledge that factor and date are not, in fact, types
  info_type <- NULL
  type_classes <- get_type_classes()

  # Record the presence of any type-like class (date, factor) and replace the
  # corresponding type by that class. With a factor, for example, the misleading
  # "integer" is replaced by "factor".
  for (tc in type_classes) {
    for (i in seq_along(x)) {
      if (inherits(x[[i]], tc)) {
        info_type <- c(info_type, tc)
        types[i] <- tc
      }
    }
  }

  # First type that is different from the very first type
  index_type_diff1 <- which(types != types[1])[1]
  type_diff1 <- types[index_type_diff1]

  # Tell the user that dates and factors are effectively types of their own
  msg_type <- if (is.null(info_type)) {
    NULL
  } else {
    type_types <- if (length(info_type) == 1) "a type" else "types"
    info_type <- paste(info_type, collapse = " and ")
    paste("Pragmatically counting", info_type, "as", type_types, "here.")
  }

  # Some numeric, though not all: mixing types not allowed
  if (some_are_numeric) {
    numeric1 <- which(x_is_numeric)[1]
    non_numeric_1 <- which(!x_is_numeric)[1]
    numeric1_type <- types[numeric1]
    non_numeric1_type <- types[non_numeric_1]
    cli::cli_abort(
      message = c(
        "Mixing numeric and non-numeric data is not allowed.",
        "x" = "Numeric type: {numeric1_type} (index {numeric1})",
        "x" = "Non-numeric type: {non_numeric1_type} (index {non_numeric_1})",
        "i" = msg_type
      ),
      call = rlang::caller_call()
    )
  }

  # No numeric, but different non-numeric types: mixing types not allowed.
  # Currently, the condition is necessarily true at this point, but that may
  # change with further conditions; hence the explicit check.
  if (some_are_unequal) {
    cli::cli_abort(
      message = c(
        "Mixing different types of non-numeric data is not allowed.",
        "x" = "Contains type {types[1]} (index 1).",
        "x" = "But also type {type_diff1} (index {index_type_diff1}).",
        "i" = msg_type
      ),
      call = rlang::caller_call()
    )
  }

}


# Error within `tryCatch()` if sorting a vector or removing missing values from
# it failed, also displaying the original error.
stop_data_invalid <- function(
    cnd,
    action = c("sort", "is.na", "subsetting")
  ) {
  action <- rlang::arg_match(action)
  fn_name <- switch(
    action,
    "sort" = "`sort()`",
    "is.na" = "`is.na()`",
    "subsetting" = "`[`"
  )
  desciption <- switch(
    action,
    "sort" = "Sorting the input",
    "is.na" = "Checking for `NA`s",
    "subsetting" = "Subsetting with `[`"
  )
  cli::cli_abort(
    message = c(
      "{desciption} failed.",
      "i" = "If this action makes sense for your object type, \
      you may implement a new {fn_name} method for it.",
      "i" = "Original error:",
      "x" = as.character(cnd)
    ),
    call = rlang::caller_call(5)
  )
}


stop_sorting     <- function(cnd) stop_data_invalid(cnd, "sort")
stop_checking_na <- function(cnd) stop_data_invalid(cnd, "is.na")
stop_subsetting  <- function(cnd) stop_data_invalid(cnd, "subsetting")


