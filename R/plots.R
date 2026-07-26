# ggplot2 visualisations of the SD bounds, POMP-normalised bounds, and umbrella.

.bounds_point_layer <- function(pts, xvar, yvar) {
  if (is.null(pts[["consistent"]])) pts$consistent <- NA
  pts$.consistent <- as.character(pts$consistent)
  list(
    ggplot2::geom_point(data = pts,
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data$.consistent),
      shape = 21, colour = "black", size = 2.4, na.rm = TRUE),
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#43BF71", "FALSE" = "#D7191C"),
      na.value = "grey50", name = "Consistent",
      labels = c("TRUE" = "consistent", "FALSE" = "inconsistent")))
}

#' Plot SD bounds on the native scale
#'
#' The feasible SD band (floor to ceiling) against the mean, optionally with
#' reported points coloured by consistency (green/red outlined dots).
#'
#' @param curve Output of [sd_bounds_curve()].
#' @param points Optional data.frame with `mean`, `sd`, and (optionally)
#'   `consistent`; e.g. the output of [brimmer_multiple()].
#' @param title Optional plot title.
#' @param fill,line_colour Band fill and outline colours.
#' @return A ggplot object.
#' @examples
#' curve <- sd_bounds_curve(l = 1, u = 7, n = 30, by = 0.1)
#' plot_sd_bounds(curve, title = "Feasible SDs, 1-7 scale, n = 30")
#'
#' # overlay reported values, coloured by consistency (the second is
#' # above the ceiling, so it plots as inconsistent)
#' reports <- data.frame(mean = c(2.97, 3.51), sd = c(2.83, 3.50))
#' checked <- brimmer_multiple(reports, l = 1, u = 7, n = 30,
#'                             mean_digits = 2, sd_digits = 2)
#' plot_sd_bounds(curve, points = checked)
#' @export
plot_sd_bounds <- function(curve, points = NULL, title = NULL,
                           fill = "grey85", line_colour = "grey30") {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  cur <- curve[curve$feasible & is.finite(curve$max_sd), ]
  p <- ggplot2::ggplot(cur, ggplot2::aes(x = .data$mean)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$min_sd, ymax = .data$max_sd),
                         fill = fill) +
    ggplot2::geom_line(ggplot2::aes(y = .data$max_sd), colour = line_colour) +
    ggplot2::geom_line(ggplot2::aes(y = .data$min_sd), colour = line_colour) +
    ggplot2::labs(x = "Mean", y = "SD", title = title) +
    ggplot2::theme_minimal()
  if (!is.null(points)) p <- p + .bounds_point_layer(points, "mean", "sd")
  p
}

#' Plot SD bounds on a percent-of-maximum-possible (POMP) scale
#'
#' `reference = "parity"` normalises every SD by the mean-agnostic parity
#' (Popoviciu) ceiling: a linear rescaling, so the Structure S ceiling appears
#' as a dome under 1 and the umbrella geometry is undistorted. `reference =
#' "sharp"` normalises each SD by its own sharp mean-conditional band, so the
#' feasible region is exactly the unit square and a point's height is its
#' position within the band (`pomp_sd_sharp`); regions outside `[0, 1]^2` are
#' shaded infeasible.
#'
#' @param curve Output of [sd_bounds_curve()] (used for the parity band).
#' @param points Optional data.frame with `pomp_mean` and `pomp_sd_parity` /
#'   `pomp_sd_sharp` and `consistent` (e.g. from [brimmer_multiple()]).
#' @param reference `"sharp"` (default) or `"parity"`.
#' @param title Optional plot title.
#' @return A ggplot object.
#' @examples
#' curve <- sd_bounds_curve(l = 1, u = 7, n = 30, by = 0.1)
#'
#' # "sharp" makes the feasible region exactly the unit square
#' plot_sd_bounds_pomp(curve)
#'
#' # "parity" is a linear rescaling, so the umbrella keeps its shape
#' plot_sd_bounds_pomp(curve, reference = "parity")
#' @export
plot_sd_bounds_pomp <- function(curve, points = NULL,
                                reference = c("sharp", "parity"),
                                title = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  reference <- match.arg(reference)
  if (reference == "parity") {
    cur <- curve[curve$feasible & is.finite(curve$max_sd), ]
    p <- ggplot2::ggplot(cur, ggplot2::aes(x = .data$pomp_mean)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$floor_parity,
                                        ymax = .data$ceil_parity), fill = "grey85") +
      ggplot2::geom_line(ggplot2::aes(y = .data$ceil_parity), colour = "grey30") +
      ggplot2::geom_line(ggplot2::aes(y = .data$floor_parity), colour = "grey30") +
      ggplot2::labs(x = "Relative location (POMP mean)",
                    y = "Relative dispersion (parity-normalised SD)", title = title)
    if (!is.null(points)) p <- p + .bounds_point_layer(points, "pomp_mean", "pomp_sd_parity")
  } else {
    shade <- data.frame(
      xmin = c(-Inf, 1, 0, 0), xmax = c(0, Inf, 1, 1),
      ymin = c(-Inf, -Inf, 1, -Inf), ymax = c(Inf, Inf, Inf, 0))
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = shade,
        ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                     ymin = .data$ymin, ymax = .data$ymax),
        fill = "grey10", alpha = 0.12) +
      ggplot2::geom_rect(data = data.frame(x = 0),
        ggplot2::aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        fill = NA, colour = "black", linewidth = 0.3) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 1.1)) +
      ggplot2::labs(x = "Relative location (POMP mean)",
                    y = "Position in sharp SD band", title = title)
    if (!is.null(points)) p <- p + .bounds_point_layer(points, "pomp_mean", "pomp_sd_sharp")
  }
  p + ggplot2::theme_minimal()
}

#' Plot the umbrella grid
#'
#' Renders the `(mean, sd)` grid from [umbrella_data()], each cell coloured by
#' verdict: consistent, GRIMMER-inconsistent (passes bounds, fails GRIMMER), or
#' out of bounds. Optionally overlays the bound curves from [sd_bounds_curve()].
#'
#' @param umbrella Output of [umbrella_data()].
#' @param curve Optional [sd_bounds_curve()] output to overlay as bound lines.
#' @param title Optional plot title.
#' @return A ggplot object.
#' @examples
#' grid <- umbrella_data(n = 12, l = 1, u = 3, digits = 1)
#' plot_umbrella(grid, title = "n = 12, 1-3 scale")
#' @export
plot_umbrella <- function(umbrella, curve = NULL, title = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  umbrella$category <- ifelse(umbrella$consistent, "consistent",
    ifelse(umbrella$in_bounds & !is.na(umbrella$grimmer) & !umbrella$grimmer,
           "GRIMMER-inconsistent", "out of bounds"))
  p <- ggplot2::ggplot(umbrella,
      ggplot2::aes(x = .data$mean, y = .data$sd, fill = .data$category)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_manual(values = c(
      "consistent" = "#43BF71", "GRIMMER-inconsistent" = "#FDAE61",
      "out of bounds" = "grey80"), name = NULL) +
    ggplot2::labs(x = "Mean", y = "SD", title = title) +
    ggplot2::theme_minimal()
  if (!is.null(curve)) {
    cur <- curve[curve$feasible & is.finite(curve$max_sd), ]
    p <- p +
      ggplot2::geom_line(data = cur,
        ggplot2::aes(x = .data$mean, y = .data$max_sd), inherit.aes = FALSE,
        colour = "grey20") +
      ggplot2::geom_line(data = cur,
        ggplot2::aes(x = .data$mean, y = .data$min_sd), inherit.aes = FALSE,
        colour = "grey20")
  }
  p
}
