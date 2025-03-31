#' Visualize median estimate certainty
#'
#' @description Use `median_plot()` to visualize the results of
#'   [`median_table()`].
#'
#' @param data Data frame returned by [`median_table()`].
#' @param line_width Numeric (length 1). Width of the horizontal lines. Default
#'   is `0.35`.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples


# # Test using:
# data <- median_table(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
# line_width <- 0.35


median_plot <- function(data, line_width = 0.35) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  index_rows <- seq_len(nrow(data))
  data$index <- index_rows

  if (!any(colnames(data) == "term")) {
    data$term <- index_rows
  }

  range_is_inf <- is.infinite(data$min)

  # Exclude cases where `estimate` would occlude `min` / `max`
  data_min <- data[!near(data$estimate, data$min), ]
  data_max <- data[!near(data$estimate, data$max), ]


  # Build the plot
  ggplot2::ggplot(data, ggplot2::aes(x = .data$term, y = .data$estimate)) +

    # Vertical connection lines
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$index,
        xend = .data$index,
        y = min,
        yend = max
      ),
      # Dotted vertical lines run from top to bottom if the range is infinite.
      # Else, the same lines go between `min` and `max`.
      linetype = 3, # ifelse(range_is_inf, 2, 3),
      alpha = 0.6
    ) +

    # Minimum
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$index - line_width,
        xend = .data$index + line_width,
        y = min,
        yend = min
      ),
      color = "royalblue1",
      linetype = ifelse(is.infinite(data_min$min), 2, 1),
      data = data_min
    ) +

    # Maximum
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$index - line_width,
        xend = .data$index + line_width,
        y = max,
        yend = max
      ),
      color = "red",
      linetype = ifelse(is.infinite(data_max$max), 2, 1),
      data = data_max
    ) +

    # Median
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data$index - line_width,
        xend = .data$index + line_width,
        y = .data$estimate,
        yend = .data$estimate
      ),
      color = "black"
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

