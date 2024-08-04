
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Median with missing values analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of naidem is to compute the median in a way that handles
missing values properly: returning `NA` if and only if the median can’t
be determined from the data. Its source code has no dependencies.

Use `median2()` as a drop-in replacement for `median()`. Except for
handling missing values, it works like `median()` by default.

## Installation

You can install the development version of naidem like so:

``` r
remotes::install_github("lhdjung/naidem")
```

## Get started

``` r
library(naidem)
```

Why and how to use naidem:

### The problem

Base R’s `median()` function returns `NA` whenever the input vector
contains one or more `NA`s. In many cases, missing values do make it
impossible to compute the median. Yet some distributions have a clear
median even so:

``` r
x1 <- c(6, 7, 7, 7, NA)
median(x1)
#> [1] NA
```

The actual median is 7, irrespective of the true value behind `NA`.

### The solution

Use naidem’s `median2()` instead. This function will return the median
whenever it can be determined:

``` r
median2(x1)
#> [1] 7
```

If the median really is unclear, both functions return `NA`:

``` r
x2 <- c(3, 4, 4, 5, NA, NA)
median(x2)
#> [1] NA
median2(x2)
#> [1] NA
```

Compare this to `NA^0`, which returns `1` even though `NA^2` returns
`NA`. If the exponent is 0, the result is the same for all possible
bases. The same is sometimes true for the median. Therefore,
implementations should check for this case and return the median if and
only if it can be determined. This also makes `NA` more meaningful when
it is returned: users can be sure that the median really is unknown.

See [*Implementing the
algorithm*](https://lhdjung.github.io/naidem/articles/algorithm.html)
for more information on naidem’s solution.

### Possible medians

What to do if the median really is unknown, like above? `median_range()`
will attempt to narrow it down to its minimal and maximal possible
medians:

``` r
median_range(x2)
#> [1] 3.5 4.5
```

However, `median_table()` is even more informative. It estimates the
median while only ignoring as many missing values as absolutely
necessary. This balances the need for knowledge about the central
tendency with its degree of uncertainty.

``` r
x1
#> [1]  6  7  7  7 NA
x2
#> [1]  3  4  4  5 NA NA

median_table(list(x1, x2))
#> # A tibble: 2 × 7
#>   estimate certainty na_ignored na_total rate_ignored_na sum_total
#>      <dbl> <lgl>          <int>    <int>           <dbl>     <int>
#> 1        7 TRUE               0        1             0           5
#> 2        4 FALSE              1        2             0.5         6
#> # ℹ 1 more variable: rate_ignored_sum <dbl>
```

## About this package

“naidem” is “median” backwards, but it also expresses the goal of
treating `NA` in the same (*idem*) conceptual way as R language
primitives like `^` do. Note that naidem is not about imputation at all,
but simply about determining the median.

The [moder](https://github.com/lhdjung/moder) package serves a similar
purpose, but for the mode instead of the median. Working on moder
provided the impetus behind naidem.
