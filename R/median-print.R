
median_print <- function(x) {

  n <- length(x)
  x <- sort_known_values(x)
  nna <- n - length(x)

  na_all <- rep(NA_real_, nna)
  xstr <- as.character(x)

  # The maximum number of characters can't be less than 2 because there are 2
  # characters in "NA".
  nchar_max <- max(
    vapply(xstr, nchar, integer(1), USE.NAMES = FALSE),
    2L
  )

  if (n %% 2L == 1L) {
    half <- (n + 1L) %/% 2L
    # n_arm <- half - 1L
  } else {
    half <- (n + 1L:2L) %/% 2L
    # n_arm <- half[1L] - 1L
  }

  n_arm <- half[1L] - 1L

  central_highlight <- paste0(rep(" ", nchar_max), collapse = "")
  line_outer <- paste0(rep("--", times = n_arm), collapse = "")
  line_whole <- paste0(line_outer, central_highlight, line_outer, collapse = "")

  # Now, the printing:
  cat(c(line_whole, "\n"))
  print(c(na_all, x, "\n"))
  print(c(x, na_all, "\n"))
  cat(line_whole)
}
