#' Visualize median estimate uncertainty
#'
#' @description `median_plot()` visualizes the results of [`median_table()`]. It
#'   shows the `min` and `max` median bounds using error bars. Median estimates
#'   are displayed as points.
#'
#'   If no bounds can be found for a sample because of too many missing values,
#'   the error bars span the height of the plot, and the point is empty. The
#'   median is particularly uncertain because it cannot be confined to a range.
#'   See [`median_range()`].
#'
#' @param data Data frame returned by [`median_table()`].
#' @param point_size Numeric. Size of the median estimate points. Default is
#'   `2`.
#' @param line_size Numeric. Thickness of the error bars, including the vertical
#'   lines. Default is `0.5`.
#' @param line_width Numeric (length 1). Width of the horizontal lines. Default
#'   is `0.75`.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#' @export
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
#' # Some medians are known (no error bar expansion).
#' # Some medians are unknown but confined to a range.
#' # One median doesn't even have a range, its error bars
#' # are expanded into infinity, and its point is empty.
#' median_plot(data)


# # Test using:
# data <- median_table(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
# point_size <- 2
# line_size <- 0.5
# line_width <- 0.35


median_plot <- function(data,
                        point_size = 2,
                        line_size = 0.5,
                        line_width = 0.75) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  index_rows <- seq_len(nrow(data))
  data$index <- index_rows

  if (all(data$term == "")) {
    data$term <- index_rows
  }

  range_is_inf <- is.infinite(data$min)

  # Exclude cases where `estimate` would occlude `min` / `max`
  data_min <- data[!near(data$estimate, data$min), ]
  data_max <- data[!near(data$estimate, data$max), ]


  # Build the plot
  ggplot2::ggplot(data, ggplot2::aes(x = .data$term, y = .data$estimate)) +

    # Min and max "errorbars" -- drawing them first to make the points go on top
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(ymin = .data$min, ymax = .data$max),
      # linetype = ifelse(range_is_inf, 2, 1),
      width = line_width,
      size = line_size
    ) +

    # Point estimate
    ggplot2::geom_point(
      shape = ifelse(range_is_inf, 1, 19),
      size = point_size
    ) +

    # All the rest
    ggplot2::scale_x_continuous(
      breaks = index_rows,
      labels = index_rows
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Median estimate"
    )

}

