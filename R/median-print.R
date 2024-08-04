
median_print <- function(x) {
  n <- length(x)
  x <- sort(x[!is.na(x)])
  nna <- n - length(x)
  na_all <- rep(x[NA_integer_], nna)
  if (n %% 2L == 1L) {
    half <- (n + 1L) %/% 2L
    # n_arm <- half - 1L
  } else {
    half <- (n + 1L:2L) %/% 2L
    # n_arm <- half[1L] - 1L
  }
  n_arm <- half[1L] - 1L
  central_highlight <- paste0(rep(" ", 4), collapse = "")
  line_outer <- paste0(rep("--", times = n_arm), collapse = "")
  line_whole <- paste0(line_outer, central_highlight, line_outer, collapse = "")
  # Now, the printing:
  print(line_whole, quote = FALSE)
  print(c(na_all, x))
  print(c(x, na_all))
  print(line_whole, quote = FALSE)
}
