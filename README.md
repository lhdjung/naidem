
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Median with missing values analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of naidem is to compute the median in a way that handles
missing values properly: returning `NA` if and only if the median can’t
be determined from the data. Its source code has no dependencies.

Use `median2()` as a drop-in replacement for `median()`. Its default
method is the same as in `median()` except for handling missing values.

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

What to do if the median really is unknown, like above? These two
functions will attempt to narrow it down to its possible values, or the
range of such values:

``` r
median_possible_values(x2)
#> [1] 3.5 4.0 4.5
median_range(x2)
#> [1] 3.5 4.5
```

## About this package

“naidem” is “median” backwards, but it also expresses the goal of
treating `NA` in the same (*idem*) conceptual way as R language
primitives like `^` do. Note that naidem is not about imputation at all,
but simply about determining the median.

The [moder](https://github.com/lhdjung/moder) package serves a similar
purpose, but for the mode instead of the median. Working on moder
provided the impetus behind naidem.
