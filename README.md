
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Median with missing values analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of naidem is to compute the median in a way that handles
missing values properly: return `NA` if and only if the median can’t be
determined from the data. Its source code has no dependencies.

Use `median_na()` as a drop-in replacement for `median()`. Its default
method is the same as in `median()` except for handling missing values.

## Installation

You can install the development version of naidem like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

# Get started

``` r
library(naidem)
```

Why and how to use naidem:

## The problem

The standard `median()` function returns `NA` whenever the input vector
contains one or more `NA`s. In many cases, missing values do make it
impossible to compute the median. Yet some distributions have a clear
median even so:

``` r
median(c(6, 7, 7, 7, NA))
#> [1] NA
```

The actual median is 7, irrespective of the true value behind `NA`. Yet
`median()` returns `NA`.

## The solution

Use naidem’s `median_na()` instead. This function will return the median
whenever it can be determined:

``` r
median_na(c(6, 7, 7, 7, NA))
#> [1] 7
```

If the median really is unclear, both functions return `NA`:

``` r
median(c(5, 3, 4, NA, NA))
#> [1] NA
median_na(c(5, 3, 4, NA, NA))
#> [1] NA
```

Compare this to `NA ^ 0`, which returns `1` even though `NA ^ 2` returns
`NA`. If the exponent is 0, the result is the same for all possible
bases. The same is sometimes true for the median. Therefore,
implementations should check for this case and return the median if and
only if it can be determined. This also makes `NA` more meaningful when
it is returned: users can be sure that the median really is unknown.

See `vignette("algorithm")` for more information on naidem’s solution.

# About this package

“naidem” is “median” backwards, but it also expresses the goal of
treating `NA` in the same (*idem*) conceptual way as R language
primitives like `^` do. Note that naidem is not about imputation at all,
but simply about determining the median.

The [moder](https://github.com/lhdjung/moder) package serves a similar
purpose, but for the mode instead of the median. Working on moder
provided the impetus behind naidem.
