#' Visualize rates of ignored `NA`s
#'
#' @description `median_plot_col()` visualizes the results of
#'   [`median_table()`]. It shows the rates of missing values that had to be
#'   ignored to estimate the median of the remaining values.
#'
#' @inheritParams median-plot-estimate
#'
#' @param bar_alpha Numeric. Opacity of the bars. Default is `0.4`.
#' @param bar_color_na,bar_color_all Strings. Colors of the bars representing
#'   the number of missing values that had to be ignored as a share of all
#'   missing values (`_na`) or of the entire sample (`_all`).
#' @param ring_color String. Color of any "ring of certainty" half circle.
#'   Default is `"black"`.
#' @param ring_size Numeric. Size of any "ring of certainty" half circle.
#'   Default is `8`.
#' @param show_ring Logical. Should samples with a known median be marked by a
#'   "ring of certainty" half circle? Default is `TRUE`.
#' @param show_legend Logical. Should a legend be displayed? Default is `TRUE`.
#'   Note: there is no legend if there are no bars.
#'
#' @section Visual guide (default):
#' - Red bars show the share of missing values that had to be ignored as a share
#'   of all missing values.
#' - Blue bars show the same but as a share of *all* values, missing or not.
#'   They cover part of the blue bars; both types of bars start at zero.
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
#' @seealso
#' - [`median_plot_errorbar()`] for an alternative visualization.
#' - [`median_table()`] for the basis of these plots.
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


median_plot_col <- function(
    data,
    bar_alpha = 0.8,
    bar_color_na = "#F77774",
    bar_color_all = "#747DF7",
    ring_color = "black",
    ring_size = 8,
    show_ring = TRUE,
    show_legend = TRUE
  ) {

  if (!inherits(data, "median_table")) {
    stop("needs output of `median_table()`.")
  }

  # Replace any empty "term" sample names by index numbers, and convert the
  # column to factor for consistent order
  data$term <- as_factor_sequence(data$term)

  nrow_data <- nrow(data)

  # Similarly to `tidyr::pivot_longer()`, create more combinations of row values
  # but without depending on tidyr
  data_stacked <- tibble::new_tibble(
    x = list(
      term = rep(data$term, 2),
      certainty = rep(data$certainty, 2),
      value = c(
        data$rate_ignored_sum,
        data$rate_ignored_na - data$rate_ignored_sum
      ),
      category = c(
        rep("sum", nrow_data),
        rep("na",  nrow_data)
      )
    ),
    nrow = 2L * nrow_data,
    class = NULL
  )

  # Prepare to remove the legend if it is not desired by the user or if the
  # medians of all samples are known, and therefore, there are no bars and no
  # need for a legend to explain them.
  legend_position <- if (!show_legend || all(data$certainty)) {
    "none"
  } else {
    "right"
  }


  # Build the stacked bar chart
  ggplot2::ggplot(
    data = data_stacked,
    mapping = ggplot2::aes(
      x    = .data$term,
      y    = .data$value,
      fill = .data$category
    )
  ) +

    # Bars
    ggplot2::geom_col(alpha = bar_alpha) +

    # "Ring of certainty" -- just a half circle here
    ggplot2::geom_point(
      ggplot2::aes(y = .data$value),
      shape = 1,
      color = ring_color,
      size  = ring_size,
      show.legend = FALSE,
      data = if (show_ring) {
        data_stacked[data_stacked$certainty, ]
      } else {
        data_stacked[FALSE, ]
      }
    ) +

    # Scale with custom colors for the bars
    ggplot2::scale_fill_manual(
      "Ignored NAs as\na proportion of:",
      labels = c("NAs", "All values"),
      values = c(
        sum = bar_color_all,
        na  = bar_color_na
      )
    ) +

    # All the rest
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = legend_position
    ) +
    ggplot2::labs(
      x = "Sample",
      y = "Share of ignored missing values"
    )
}

