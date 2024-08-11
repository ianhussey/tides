#' Calculate and plot a TIDES test for a single set of values
#'
#' Explanation to be added
#'
#' @inheritParams tides_single
#'
#' @param show_labels String. One of these: `"consistency"` says whether the
#'   data are numerically consistent; `"values"` displays them in "mean (SD)"
#'   format; `"both"` combines the two labels, and `"none"` shows neither.
#' @param point_alpha Numeric. Opacity of the data points. Default is `1`.
#' @param label_force Numeric. Force of repulsion between overlapping text
#'   labels. Default is `175`.
#' @param label_force_pull Numeric. Force of attraction between a text label and
#'   its corresponding data point. Default is `0.75`.
#'
#' @returns A ggplot object: the TIDES umbrella plot and results.
#'
#' @export
#'
#' @examples
#' umbrella_plot(mean = 1.1, sd = 0.5, n = 12, min = 1, max = 7)

# mean <- 1.1
# sd <- 0.5
# n <- 12
# min <- 1
# max <- 7
# calculate_min_sd <- FALSE
# show_labels <- "consistency"
# label_force <- 175
# label_force_pull <- 0.75

umbrella_plot <- function(mean,
                          sd,
                          n,
                          min,
                          max,
                          calculate_min_sd = FALSE,
                          show_labels = c("consistency", "values", "both", "none"),
                          color_cons = "royalblue1",
                          color_incons = "red",
                          point_alpha = 1,
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
    min = min,
    max = max,
    plotting_mean = seq(from = min, to = max, by = 0.01),
    n = n
  ) |>
    mutate(results = pmap(
      list(plotting_mean, sd, n, min, max, calculate_min_sd, verbose = FALSE),
      tides_single
    )) |>
    unnest(results) |>
    rename_with(
      .fn = function(x) colnames_new,
      .cols = all_of(colnames_old)
    )

  data_polygon_above <- bind_rows(
    tibble(x = data_plot$x, y = data_plot$y_max),
    tibble(x = rev(data_plot$x), y = Inf)
  )

  # The bottom polygon is conditional, unlike the other three:
  if (calculate_min_sd) {
    data_polygon_below <- bind_rows(
      tibble(x = data_plot$x, y = data_plot$y_min),
      tibble(x = rev(data_plot$x), y = -Inf)
    )
    geom_polygon_below <- geom_polygon(
      data = data_polygon_below,
      aes(x = x, y = y),
      fill = "grey10",
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

  label_consistency <- if_else(data_reported$consistency, "Consistent", "Inconsistent")
  color_consistency <- if_else(data_reported$consistency, color_cons, color_incons)

  if (show_labels == "none") {
    geom_labels <- NULL
  } else {
    labels <- switch(
      show_labels,
      "consistency" = label_consistency,
      "values"      = paste0(data_reported$mean, " (", data_reported$sd, ")"),
      "both"        = paste0(data_reported$mean, " (", data_reported$sd, ")", ": ", label_consistency)
    )
    geom_labels <- ggrepel::geom_text_repel(
      data = data_reported,
      aes(mean, sd, label = labels),
      vjust = -1,
      size = 5
    )
  }

  scale_x <- if (max - min > 10) {
    scale_x_continuous(breaks = scales::breaks_extended(n = 10))
  } else {
    scale_x_continuous(
      expand = expansion(mult = c(0, 0)),
      labels = seq(from = min, to = max, by = 1),
      breaks = seq(from = min, to = max, by = 1)
    )
  }

  # Build and return the plot:
  ggplot() +
    geom_polygon(data = data_polygon_above, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon_below +
    geom_point(
      data = data_reported,
      aes(mean, sd),
      color = color_consistency,
      alpha = point_alpha,
      size = 2
    ) +
    geom_labels +
    scale_x +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Mean", y = "SD") +
    theme_minimal(base_size = 20) +
    theme(legend.position = "none")
}

