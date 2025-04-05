#' Visualize median estimate uncertainty
#'
#' @description `median_plot()` visualizes the results of [`median_table()`]. It
#'   shows the `min` and `max` median bounds using error bars. Median estimates
#'   are displayed as points.
#'
#' @section Visuals legend:
#' - Point estimates that are known to be the true median have a "ring of
#'   certainty" around them.
#' - Error bars display the uncertainty about the true median created by missing
#'   values. The median is known to fall between the two error bars, even if its
#'   exact value in that range is unknown.
#' - If no bounds can be found for a sample because of too many missing
#'   values, the error bars span the height of the plot, and the point is a
#'   hexagram. The median is particularly uncertain in this case because it
#'   cannot be confined to a range. See [`median_range()`].
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
#' # -- Some medians are known: "ring of certainty" and no
#' # error bar expansion.
#' # -- Some medians are unknown but confined to a range.
#' # -- One median doesn't even have a range, its error bars
#' # extend into infinity, and its point is a hexagram.
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

  index_rows <- as.character(seq_len(nrow(data)))
  index_rows <- factor(index_rows, levels = index_rows)

  data$index <- index_rows

  if (all(data$term == "")) {
    data$term <- index_rows
  }

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
    width = line_width
    # linetype = ifelse(range_is_inf, 2, 1)
  )

  # ...and the aesthetic with the correct name is added to the geom. Unlike most
  # R functions, this helper modifies in place, so no assignment is needed.
  aes_add(
    geom = geom_uncertainty_bars,
    field = "aes_params",
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

    # Alternative to true error bars for infinitely wide ranges
    ggplot2::geom_vline(
      xintercept = index_rows[range_is_inf]  # ,
      # data = data[range_is_inf, ]
    ) +

    # Point estimate
    ggplot2::geom_point(
      shape = ifelse(range_is_inf, 11, 19),
      size  = ifelse(range_is_inf, point_size + 1, point_size)
    ) +

    # "Ring of certainty"
    ggplot2::geom_point(
      shape = 1,
      size  = point_size + 3,
      data  = data[data$certainty, ]
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

