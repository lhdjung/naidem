#' Visualize rates of ignored `NA`s
#'
#' @description `median_plot_col()` visualizes the results of
#'   [`median_table()`]. It shows the rates of missing values that had to be
#'   ignored to estimate the median of the remaining values.
#'
#' @inheritParams median_plot_errorbar
#'
#' @param point_size Numeric. Size of the "ring of certainty" half circle.
#'   Default is `5`.
#' @param bar_alpha Numeric. Opacity of the bars. Default is `0.4`.
#' @param bar_color_na,bar_color_all Strings. Colors of the bars representing
#'   the number of missing values that had to be ignored as a share of all
#'   missing values (`_na`) or of the entire sample (`_all`).
#'
#' @section Visual guide (default colors):
#' - Orange bars show the share of missing values that had to be ignored as a
#'   share of all missing values.
#' - Purple bars show the same but as a share of *all* values, missing or not.
#' - The y-axis is fixed between 0 and 1 for a consistent display of
#'   proportions.
#' - Samples without any bar do not require ignoring any `NA`s, so the median is
#'   known. They are also marked by a "ring of certainty", which is just a half
#'   circle here.
#'
#' @returns A ggplot object.
#'
#' @importFrom rlang .data
#' @export
#'
#' @seealso [`median_plot_errorbar()`]
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
#' # See visual guide above
#' median_plot_col(data)


median_plot_col <- function(data,
                            bar_alpha = 0.4,
                            bar_color_na = "red",
                            bar_color_all = "blue",
                            point_size = 5) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  # Replace any empty "term" sample names by index numbers, and convert the
  # column to factor for consistent order
  data$term <- as_factor_sequence(data$term)

  data$rate_ignored_na[is.nan(data$rate_ignored_na)] <- 0


  # Build the plot
  ggplot2::ggplot(data, ggplot2::aes(x = .data$term)) +

    # Draw the bars. Add black bars in between -- it has the same extension as
    # the smaller ones (proportion of all values) but adds contrast.
    ggplot2::geom_col(ggplot2::aes(y = .data$rate_ignored_sum), fill = bar_color_all, alpha = bar_alpha) +
    ggplot2::geom_col(ggplot2::aes(y = .data$rate_ignored_sum), fill = "black",       alpha = bar_alpha) +
    ggplot2::geom_col(ggplot2::aes(y = .data$rate_ignored_na),  fill = bar_color_na,  alpha = bar_alpha) +

    # "Ring of certainty" -- just a half circle here
    ggplot2::geom_point(
      ggplot2::aes(y = .data$rate_ignored_na),
      shape = 1,
      size  = point_size,
      data  = data[data$certainty, ]
    ) +

    # All the rest
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Share of ignored missing values"
    )

}
