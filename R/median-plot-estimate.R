#' Visualize median estimate uncertainty
#'
#' @description `median_plot_errorbar()` visualizes the results of
#'   [`median_table()`]. It shows the `min` and `max` median bounds using error
#'   bars. Median estimates are displayed as points.
#'
#'   `median_plot_pointrange()` is a variant that uses simple lines instead of
#'   error bars.
#'
#' @section Visual guide:
#' - Points are medians of the non-`NA` values.
#' - Points that are known to be true medians have a "ring of certainty"
#'   around them.
#' - Error bars (or just vertical lines) display lower and upper bounds of the
#'   true median, reflecting any uncertainty created by missing values. The
#'   median is known to fall in this range, even if its exact value is unknown.
#' - If no bounds can be found for a sample because of too many missing
#'   values, the error bars span the height of the plot, and the point is a
#'   hexagram. The median is particularly uncertain in this case because it
#'   cannot even be confined to a range. See [`median_range()`].
#'
#' @param data Data frame returned by [`median_table()`].
#' @param point_size Numeric. Size of the median estimate points. Default is
#'   `2`.
#' @param point_color String (length 1). Color of the estimate points, including
#'   any "ring of certainty". By default, the same as `bar_color`.
#' @param line_width Numeric (length 1). Width of the error bar lines. Default
#'   is `0.5`.
#' @param bar_width Numeric (length 1). Extension of the horizontal bars.
#'   Default is `0.9`.
#' @param bar_color String (length 1). Color of the error bars. Default is
#'   `"black"`.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#' @export
#'
#' @rdname median-plot-estimate
#' @name median-plot-estimate
#'
#' @examples
#' # Example data:
#' data <- median_table(
#'   list(
#'     c(0, 1, 1, 1, NA),
#'     c(1, 1, NA),
#'     c(1, 2, NA),
#'     c(0, 0, NA, 0, 0),
#'     c(1, 1, 1, 1, NA, NA),
#'     c(1, 1, 1, 1, NA, NA, NA),
#'     c(1, 1, 1, 1, NA, NA, NA, NA),
#'     iris$Sepal.Length,
#'     c(5.6, 5.7, 5.9, 6, 6.1, 6.3, 6.4, 6.6, 6.7, NA),
#'     c(6.1, 6.3, 5.9, 6, 6.1, 6.3, 6.4, 6.6, 6.7, NA, NA, NA, NA),
#'     c(7, 7, 7, 8, NA, NA)
#'   )
#' )
#'
#' data
#'
#' # -- Some medians are known: "ring of certainty" and no
#' # error bar expansion.
#' # -- Some medians are unknown but confined to a range.
#' # -- One median doesn't even have a range, its error bars
#' # extend into infinity, and its point is a hexagram.
#' median_plot_errorbar(data)
#'
#' # Very similar but simpler plot:
#' median_plot_pointrange(data)


# # Test using:
# data <- median_table(list(x1, x2, x3, x4, a = x5, x6, x7, x8, x9, x10, x11))
# point_size <- 2
# line_width <- 0.35


median_plot_errorbar <- function(data,
                                 point_color = bar_color,
                                 point_size = 2,
                                 line_width = 0.5,
                                 bar_color = "black",
                                 bar_width = 0.9) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  # Replace any empty "term" sample names by index numbers, and convert the
  # column to factor for consistent order
  data$term <- as_factor_sequence(data$term)

  # To mark median estimates that are not confined to a range
  range_is_inf <- is.infinite(data$min)

  # Versions of ggplot2 before 3.4.0 have the `size` aesthetic instead of the
  # more modern `linewidth`...
  linewidth_name <- if (utils::packageVersion("ggplot2") < "3.4.0") {
    "size"
  } else {
    "linewidth"
  }

  # ...so the geom where `size` / `linewidth` will be used is pre-assigned...
  geom_uncertainty_bars <- ggplot2::geom_errorbar(
    mapping = ggplot2::aes(ymin = .data$min, ymax = .data$max),
    width = bar_width,
    color = bar_color
  )

  # ...and the aesthetic with the correct name is added to the geom. Unlike most
  # R functions, this helper modifies in place, so no assignment is needed.
  aes_add(
    field = geom_uncertainty_bars[["aes_params"]],
    aes_name = linewidth_name,
    aes_value = line_width
  )


  # Build the plot
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$term,
      y = .data$estimate
    )
  ) +

    # Min and max "errorbars" -- drawing them first to make the points go on top
    geom_uncertainty_bars +

    # Point estimate
    ggplot2::geom_point(
      shape = ifelse(range_is_inf, 11, 19),
      size  = ifelse(range_is_inf, point_size + 1, point_size),
      color = point_color
    ) +

    # "Ring of certainty"
    ggplot2::geom_point(
      shape = 1,
      size  = point_size + 3,
      color = point_color,
      data  = data[data$certainty, ]
    ) +

    # All the rest
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Median estimate"
    )

}


#' @rdname median-plot-estimate
#' @export

median_plot_pointrange <- function(data,
                                   point_color = bar_color,
                                   point_size = 2,
                                   line_width = 0.5,
                                   bar_color = "black",
                                   bar_width = 0.9) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  # Replace any empty "term" sample names by index numbers, and convert the
  # column to factor for consistent order
  data$term <- as_factor_sequence(data$term)

  # To mark median estimates that are not confined to a range
  range_is_inf <- is.infinite(data$min)

  # Versions of ggplot2 before 3.4.0 have the `size` aesthetic instead of the
  # more modern `linewidth`...
  linewidth_name <- if (utils::packageVersion("ggplot2") < "3.4.0") {
    "size"
  } else {
    "linewidth"
  }

  # ...so the geom where `size` / `linewidth` will be used is pre-assigned...
  geom_uncertainty_range <- ggplot2::geom_pointrange(
    mapping = ggplot2::aes(ymin = .data$min, ymax = .data$max),
    shape = ifelse(range_is_inf, 11, 19),
    size  = ifelse(range_is_inf, point_size + 2, point_size),
    # size  = point_size,
    color = bar_color,
    fatten = 0.75
  )

  # ...and the aesthetic with the correct name is added to the geom. Unlike most
  # R functions, this helper modifies in place, so no assignment is needed.
  aes_add(
    field = geom_uncertainty_range[["aes_params"]],
    aes_name = linewidth_name,
    aes_value = line_width
  )


  # Build the plot
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$term,
      y = .data$estimate
    )
  ) +

    # Min and max "errorbars" -- drawing them first to make the points go on top
    geom_uncertainty_range +

    # "Ring of certainty"
    ggplot2::geom_point(
      shape = 1,
      size  = point_size + 3.5,
      color = point_color,
      data  = data[data$certainty, ]
    ) +

    # All the rest
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Median estimate"
    )

}

