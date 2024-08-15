#' Visualize TIDES results using an umbrella plot
#'
#' TODO: Explanation to be added
#'
#' @inheritParams tides_single
#'
#' @param show_labels String. One of these:
#'   - `"consistency"` (the default) declares whether the data are
#'   mathematically consistent.
#'   - `"values"` displays the data in "mean (SD)" format.
#'   - `"both"` combines the two labels above.
#'   - `"none"` shows no labels.
#' @param max_range Integer. TODO: Explain `max_range`
#' @param color_cons String. Fill color of consistent data points. Default is
#'   `"royalblue1"`.
#' @param color_incons String. Fill color of inconsistent data points. Default
#'   is `"red"`.
#' @param point_alpha Numeric. Opacity of the data points, ranging from 0 to 1.
#'   Default is `1`. Choose lower values for more transparency.
#' @param point_shape Numeric or string. Shape of the data points; see
#'   https://ggplot2.tidyverse.org/articles/ggplot2-specs.html#sec:shape-spec.
#'   Default is `19`.
#' @param point_size Numeric. Size of the data points. Default is `2`.
#' @param label_force Numeric. Force of repulsion between overlapping text
#'   labels. Default is `175`.
#' @param label_force_pull Numeric. Force of attraction between a text label and
#'   its corresponding data point. Default is `0.75`.
#'
#' @returns A ggplot object: the TIDES umbrella plot.
#'
#' @export
#'
#' @examples
#' umbrella_plot(mean = 1.1, sd = 0.5, n = 12, min = 1, max = 7)
#'
#' # Assuming a discrete scale:
#' umbrella_plot(mean = 1.1, sd = 0.5, n = 12, min = 1, max = 7, calculate_min_sd = TRUE)

# # Example data for testing
# mean <- 1.1
# sd <- 0.5
# n <- 12
# min <- 1
# max <- 7
# calculate_min_sd <- FALSE
# show_labels <- "consistency"
# max_range <- 10
# color_cons <- "royalblue1"
# color_incons <- "red"
# point_alpha <- 1
# point_shape <- 19
# point_size <- 2
# label_force <- 175
# label_force_pull <- 0.75

umbrella_plot <- function(mean,
                          sd,
                          n,
                          min,
                          max,
                          calculate_min_sd = FALSE,
                          show_labels = c("consistency", "values", "both", "none"),
                          max_range = 10,
                          color_cons = "royalblue1",
                          color_incons = "red",
                          point_alpha = 1,
                          point_shape = 19,
                          point_size = 2,
                          label_force = 175,
                          label_force_pull = 0.75) {

  show_labels <- rlang::arg_match(show_labels)

  data_reported <- tides_single(
    mean = mean,
    sd = sd,
    n = n,
    min = min,
    max = max,
    calculate_min_sd = calculate_min_sd,
    verbose = TRUE
  )

  # Prepare to rename columns. One of them, `min_sd`, might not be present.
  if (calculate_min_sd) {
    colnames_old <- c("min_sd", "max_sd", "plotting_mean")
    colnames_new <- c("y_min",  "y_max",  "x")
  } else {
    colnames_old <- c("max_sd", "plotting_mean")
    colnames_new <- c("y_max",  "x")
  }

  data_plot <- expand_grid(
    min,
    max,
    plotting_mean = seq(from = min, to = max, by = 0.01),
    n
  ) |>
    mutate(results = pmap(
      list(plotting_mean, sd, n, min, max, calculate_min_sd, verbose = FALSE),
      tides_single
    )) |>
    unnest(results) |>
    rename_with(
      .fn   = function(x) colnames_new,
      .cols = all_of(colnames_old)
    )

  data_polygon_above <- tibble(
    x = c(data_plot$x, rev(data_plot$x)),
    y = c(data_plot$y_max, rep(Inf, nrow(data_plot)))
  )


  # The bottom polygon is conditional, unlike the other three:
  if (calculate_min_sd) {
    data_polygon_below <- tibble(
      x = c(data_plot$x, rev(data_plot$x)),
      y = c(data_plot$y_min, rep(-Inf, nrow(data_plot)))
    )
    geom_polygon_below <- geom_polygon(
      data = data_polygon_below,
      aes(x = x, y = y),
      fill  = "grey10",
      alpha = 0.4
    )
  } else {
    geom_polygon_below <- NULL
  }

  data_polygon_left <- tibble(
    x = c(-Inf,  min, min, -Inf),
    y = c(-Inf, -Inf, Inf,  Inf)
  )

  data_polygon_right <- tibble(
    x = c( max,  Inf, Inf, max),
    y = c(-Inf, -Inf, Inf, Inf)
  )

  color_consistency <- if_else(data_reported$consistency, color_cons, color_incons)

  if (show_labels == "none") {
    geom_labels <- NULL
  } else {
    label_consistency <- if_else(data_reported$consistency, "Consistent", "Inconsistent")
    label_complete <- switch(
      show_labels,
      "consistency" = label_consistency,
      "values"      = paste0(data_reported$mean, " (", data_reported$sd, ")"),
      "both"        = paste0(data_reported$mean, " (", data_reported$sd, "): ", label_consistency)
    )
    geom_labels <- ggrepel::geom_text_repel(
      data = data_reported,
      aes(mean, sd, label = label_complete),
      vjust = -1,
      size  = 5
    )
  }

  scale_x <- if (max - min > max_range) {
    scale_x_continuous(breaks = scales::breaks_extended(n = max_range))
  } else {
    scale_x_continuous(
      expand = expansion(mult = c(0, 0)),
      labels = min:max,
      breaks = min:max
    )
  }

  # Build and return the plot:
  ggplot() +
    geom_polygon(data = data_polygon_above, aes(x, y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x, y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x, y), fill = "grey10", alpha = 0.4) +
    geom_polygon_below +
    geom_point(
      data = data_reported,
      aes(mean, sd),
      color = color_consistency,
      alpha = point_alpha,
      shape = point_shape,
      size  = point_size
    ) +
    geom_labels +
    scale_x +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Mean", y = "SD") +
    theme_minimal(base_size = 20) +
    theme(legend.position = "none")
}

