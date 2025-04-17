#' Tabulate median estimates with the certainty about them
#'
#' @description `median_table()` computes the sample median. If the median is
#'   unknown due to missing values, it only ignores as many of them as
#'   necessary. In this way, a true median estimate of the remaining known and
#'   unknown values can be found, preserving as much data as possible.
#'
#'   Estimates are presented along with lower and upper bounds, the number of
#'   missing values that had to be ignored, etc.
#'
#'   The function can also take a data frame (or another list) of numeric
#'   vectors. It will then compute the median of each element.
#'
#' @param x Vector or list of vectors. Each vector needs to be numeric or
#'   similar. Note that data frames are lists, so `x` can be a data frame.
#' @param even Passed on to [`median2()`].
#' @param ... Optional further arguments for [`median2()`] methods. Not used in
#'   its default method.
#'
#' @seealso
#'   - [`median_count_tolerable()`] for the logic of preserving as many `NA`s
#' as possible.
#'   - [`median_range()`] for the `lower` and `upper` columns; the bounds of an
#' uncertain median.
#'   - [`median_plot_errorbar()`] and [`median_plot_col()`] for follow-up
#' visualizations.
#'
#' @return Data frame with these columns:
#'   - `term`: the names of `x` elements.
#'   - `estimate`: the medians of `x` elements, ignoring as many `NA`s as
#'   necessary.
#'   - `certainty`: `TRUE` if the corresponding estimate is certain to be the
#'   true median, and `FALSE` if this is unclear due to missing values.
#'   - `lower`, `upper`: Bounds of the median. Equal if `certainty` is `TRUE`
#'   because in that case, the precise value is known.
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
#' @include median2.R median-range.R
#'
#' @examples
#' median_table(c(5, 23, 5, NA, 5, NA))
#'
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


median_table <- function(x, even = c("mean", "low", "high"), ...) {

  # Check that `x` is a list, because the point of this function is to find
  # estimates for the median of each element of `x`:
  if (!is.list(x)) {
    x <- list(x)
  }

  # Initialize the two most important output vectors. They will be columns of
  # the output data frame. Note that `estimate` is integer in case all values
  # that will be assigned to its elements are integers (i.e., none is a double,
  # which would coerce any integer elements):
  nx <- length(x)
  estimate <- integer(nx)

  # These others will also be output columns. At present, they should be integer
  # vectors just like `estimate`, so they can be initialized like this:
  lower <- estimate
  upper <- estimate
  na_ignored <- estimate
  na_total <- estimate
  sum_total <- estimate

  # Loop through the `x` elements, estimating the median of each and checking
  # how many `NA`s need to be ignored to do so:
  for (i in seq_len(nx)) {

    n_current <- length(x[[i]])
    x_known_current <- sort(x[[i]], na.last = NA)  # equivalent to indexing `[!is.na(x[[i]])]`
    nna_current <- n_current - length(x_known_current)
    sum_total[[i]] <- n_current

    estimate[[i]] <- median2(x = x_known_current, even = even, ...)

    nna_tolerable <- median_count_tolerable(
      x = x_known_current,
      needs_prep = FALSE
    )

    na_ignored[[i]] <- max(0L, nna_current - nna_tolerable)
    na_total[[i]] <- nna_current

    range_current <- median_range(
      x = x_known_current,
      even = even,
      nna = nna_current
    )

    lower[[i]] <- range_current[1L]
    upper[[i]] <- range_current[2L]
  }

  # As a purely mechanical consequence of the `na_ignored` integer vector,
  # `certainty` marks those cases where no `NA`s needed to be ignored to compute
  # an estimate; and thus, where there is a known and determinate median:
  certainty <- na_ignored == 0L

  # Compute the rates of ignored `NA`s from all `NA`s and from all values:
  rate_ignored_na  <- na_ignored / na_total
  rate_ignored_sum <- na_ignored / sum_total

  # This can turn up as `NaN` because of division by zero, but that still means
  # no `NA`s had to be ignored.
  rate_ignored_na[is.nan(rate_ignored_na)] <- 0

  # Record any names of `x` elements. If there are none, just empty strings.
  term <- names(x)

  if (is.null(term)) {
    term <- rep("", times = nx)
  }

  # Collect the length-`x` vectors in a data frame. This uses a low-level tibble
  # constructor for performance and for adding an S3 class.
  tibble::new_tibble(
    x = list(
      term = term,
      estimate = estimate,
      certainty = certainty,
      lower = lower,
      upper = upper,
      na_ignored = na_ignored,
      na_total = na_total,
      rate_ignored_na = rate_ignored_na,
      sum_total = sum_total,
      rate_ignored_sum = rate_ignored_sum
    ),
    nrow = nx,
    class = "median_table"
  )

}

