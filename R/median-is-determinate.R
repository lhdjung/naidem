#' Can the median be determined?
#'
#' `median_is_determinate()` tests whether a vector's median can be determined
#' despite missing values. It is based on `median2()`.
#'
#' @inheritParams median2
#'
#' @return Logical (length 1).
#'
#' @export
#'
#' @examples
#' # The median is 1, no matter which number
#' # `NA` represents:
#' median_is_determinate(c(1, 1, NA))
#'
#' # Here, the median may be 1 or 2, depending
#' # on the value behind `NA`; so the median
#' # cannot be determined:
#' median_is_determinate(c(1, 2, NA))

median_is_determinate <- function(x) {
  !is.na(median2(x, na.rm = FALSE))
}
