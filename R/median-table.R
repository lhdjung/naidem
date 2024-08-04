#' Tabulate median estimates with the certainty about them
#'
#' @description `median_table()` takes a data frame (or another list) of numeric
#'   vectors and computes the median of each element. Where the true median is
#'   unknown due to missing values, it only ignores as many of them as
#'   necessary.
#'
#'   Estimates are presented along with information about whether they are known
#'   to be the true median, how many missing had to be ignored during
#'   estimation, the rate of ignored values, etc.
#'
#' @param x List of vectors. Each vector needs to be numeric or similar. Note
#'   that data frames are lists, so `x` can be a data frame.
#' @param even Passed on to [`median2()`].
#' @param ... Optional further arguments for [`median2()`] methods. Not used in
#'   its default method.
#'
#' @details The function deals with missing values (`NA`s) by first checking
#'   whether they make the true median unknown. If they do, it removes one `NA`,
#'   then checks again; and so on until an estimate is found.
#'
#'   This strategy is based on [`median2()`] and its `na.rm.amount` argument,
#'   which represents a middle way between simply ignoring all `NA`s and not
#'   even trying to compute an estimate. Instead, it only removes the minimum
#'   number of `NA`s necessary, because some distributions have a known median
#'   even if some of their values are missing. By keeping track of the removed
#'   `NA`s, `median_table()` quantifies the uncertainty about its estimates.
#'
#' @return Data frame with these columns:
#'   - `term`: the names of `x` elements. Only present if any are named.
#'   - `estimate`: the medians of `x` elements, ignoring as many `NA`s as
#'   necessary.
#'   - `certainty`: `TRUE` if the corresponding estimate is certain to be the
#'   true median, and `FALSE` if this is unclear due to missing values.
#'   - `na_ignored`: the number of missing values that had to be ignored to
#'   arrive at the estimate.
#'   - `na_total`: the total number of missing values.
#'   - `rate_ignored_na`: the proportion of missing values that had to be
#'   ignored from among all missing values.
#'   - `sum_total`: the total number of values, missing or not.
#'   - `rate_ignored_sum`: the proportion of missing values that had to be
#'   ignored from among all values, missing or not.
#'
#' @export
#'
#' @include median2.R
#'
#' @examples
#' # Use a list of numeric vectors:
#' my_list <- list(
#'   a = 1:15,
#'   b = c(1, 1, NA),
#'   c = c(4, 4, NA, NA, NA, NA),
#'   d = c(96, 24, 3, NA)
#' )
#'
#' median_table(my_list)
#'
#' # Data frames are allowed:
#' median_table(iris[1:4])


median_table <- function(x, even = c("mean", "low", "high")) {

  # Check that `x` is a list, because the point of this function is to find
  # estimates for the median of each element of `x`:
  if (!is.list(x)) {
    x <- as.list(x)
  }

  # Initialize the two most important output vectors. They will be columns of
  # the output data frame. Note that `estimate` is integer in case all values
  # that will be assigned to its elements are integers (i.e., none is a double,
  # which would coerce any integer elements):
  nx <- length(x)
  estimate <- integer(nx)

  # These others will also be output columns. At present, they should be integer
  # vectors just like `estimate`, so they can be initialized like this:
  na_ignored <- estimate
  na_total <- estimate
  sum_total <- estimate

  # Loop through the `x` elements, estimating the median of each and checking
  # how many `NA`s need to be ignored to do so:
  for (i in seq_len(nx)) {

    n_current <- length(x[[i]])
    x_known_current <- sort(x[[i]])  # equivalent to indexing `[!is.na(x[[i]])]`
    nna_current <- n_current - length(x_known_current)
    sum_total[[i]] <- n_current

    # Vectors where all elements are missing have an unknown median, so the
    # estimate should be `NA`, as well. This is implemented via a shortcut:
    if (nna_current == n_current) {
      estimate[[i]] <- x[[i]][NA_integer_]
      na_ignored[[i]] <- nna_current
      na_total[[i]] <- nna_current
      next
    }

    estimate[[i]] <- median2(x_known_current, even = even)
    na_ignored[[i]] <- median_count_na_ignore(
      x = x_known_current,
      nna = nna_current
    )
    na_total[[i]] <- nna_current
  }

  # As a purely mechanical consequence of the `na_ignored` integer vector,
  # `certainty` marks those cases where no `NA`s needed to be ignored to compute
  # an estimate; and thus, where there is a known and determinate median:
  certainty <- logical(nx)
  certainty[na_ignored == 0L] <- TRUE

  # Compute the rates of ignored `NA`s from all `NA`s and from all values:
  rate_ignored_na <- na_ignored / na_total
  rate_ignored_sum <- na_ignored / sum_total

  # Collect the length-`x` vectors in a data frame, adding a `term` column that
  # stores the names of `x` if there are any:
  tibble::tibble(
    term = names(x),
    estimate,
    certainty,
    na_ignored,
    na_total,
    rate_ignored_na,
    sum_total,
    rate_ignored_sum
  )
}
