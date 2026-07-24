# One plotting entry point for the feasible-region panels: each `rule` is a
# constraint set from the nested framework, drawn either as a continuous band or
# (for strictly integer data) as the lattice of attainable reported tuples.

# Internal: the alpha-free quasi-integer sharp band, in mean-score units for a
# `k`-item composite (k = 1 is the single-item case).
.quasi_band <- function(mean, n, l, u, k) {
  data.frame(
    mean = mean,
    lo = sd_min_quasi_integer(k * (mean - l), n) / k,
    hi = sd_max_structure_s(k * (mean - l), n, 0, k * (u - l)) / k
  )
}

#' Feasible-region data for one constraint set
#'
#' The `(mean, lo, hi)` band, or the lattice of attainable reported `(mean, sd)`
#' tuples, for a chosen constraint set. See [plot_sd_region()] for the `rule`
#' vocabulary; this is the data behind that plot.
#'
#' @inheritParams plot_sd_region
#' @return For band rules, a data frame with `mean`, `lo`, `hi`. For the lattice
#'   rules (`"integer"`, `"integer_alpha"`), a data frame with `mean`, `sd` — the
#'   tuples passing every applicable test. The `type` attribute is `"band"` or
#'   `"points"`.
#' @export
sd_region_data <- function(l, u, n,
                           rule = c("quasi", "range", "range_n", "mean",
                                    "mean_naive_floor", "mestdagh",
                                    "pesant_regin", "alpha", "integer",
                                    "integer_alpha"),
                           n_items = 1, alpha = NULL, digits = 2, by = NULL) {
  rule <- match.arg(rule)
  if (rule %in% c("alpha", "integer_alpha") && is.null(alpha))
    stop(sprintf("rule = '%s' requires alpha", rule))
  k <- n_items

  # lattice rules: only GRIM/GRIMMER-attainable reported tuples exist, so the
  # region is a set of points, not a band (no curve is defined at the means and
  # SDs that strictly integer data cannot produce).
  if (rule %in% c("integer", "integer_alpha")) {
    um <- umbrella_data(n = n, l = l, u = u, digits = digits, Z = "integer",
                        scoring = if (k > 1) "meanscored" else "singleitem",
                        n_items = k,
                        alpha = if (rule == "integer_alpha") alpha else NULL)
    out <- um[which(um$consistent), c("mean", "sd"), drop = FALSE]
    rownames(out) <- NULL
    attr(out, "type") <- "points"
    return(out)
  }

  if (is.null(by)) by <- (u - l) / 1000
  m <- seq(l, u, by = by)
  q <- .quasi_band(m, n, l, u, k)
  naive <- sd_min_integer(k * m, n) / k        # strict Bernoulli floor, off-grid too

  d <- switch(rule,
    range        = data.frame(mean = m, lo = 0,      hi = sd_max_span(l, u)),
    range_n      = data.frame(mean = m, lo = 0,      hi = sd_max_span_n(l, u, n)),
    mean         = data.frame(mean = m, lo = 0,      hi = sd_max_muilwijk(m, n, l, u)),
    mean_naive_floor = data.frame(mean = m, lo = naive,
                                  hi = sd_max_muilwijk(m, n, l, u)),
    mestdagh     = data.frame(mean = m, lo = 0,      hi = q$hi),
    pesant_regin = data.frame(mean = m, lo = q$lo,   hi = sd_max_span_n(l, u, n)),
    quasi        = data.frame(mean = m, lo = q$lo,   hi = q$hi),
    alpha        = {
      cc <- (k - 1) / k
      D  <- 1 - cc * alpha
      if (k < 2) stop("rule = 'alpha' needs n_items >= 2 (alpha is inert at one item)")
      if (D <= 1e-12) stop("alpha too high for this n_items")
      ceil_a <- sqrt((n / (n - 1)) * v_max_alpha(k * m, k, n, l, u) / D) / k
      data.frame(mean = m,
                 lo = q$lo / sqrt(D),          # alpha-amplified quasi-integer floor
                 hi = pmin(ceil_a, q$hi))      # alpha can only tighten
    }
  )
  # an empty band (floor above ceiling) is infeasible, not a negative region
  bad <- d$lo > d$hi + 1e-12
  d$lo[bad] <- NA_real_; d$hi[bad] <- NA_real_
  attr(d, "type") <- "band"
  d
}

#' Plot the feasible SD region for a chosen constraint set
#'
#' Draws the region of sample SDs a constraint set asserts to be possible, as a
#' function of the mean, with the sharp quasi-integer band optionally repeated as
#' a dashed reference. This reproduces the panels of the nested-constraints
#' figure in the TIDES article from a single entry point.
#'
#' Rules, in the order they enter the framework (`n_items > 1` puts every rule in
#' mean-score units for a composite of that many integer items):
#'
#' \describe{
#'   \item{`"range"`}{`(u - l)/sqrt(2)`, no floor; ignores `n` and the mean
#'     (Popoviciu 1935, range only).}
#'   \item{`"range_n"`}{the parity ceiling, no floor (Popoviciu, as restored by
#'     Petocz 2005).}
#'   \item{`"mean"`}{the smooth mean-conditional arch, no floor (Muilwijk 1966;
#'     Bhatia-Davis 2000).}
#'   \item{`"mean_naive_floor"`}{that arch plus the naive Bernoulli floor
#'     (Fuenderich et al. 2025).}
#'   \item{`"pesant_regin"`}{integer minimum with a loose (parity) ceiling
#'     (Pesant and Regin 2005).}
#'   \item{`"mestdagh"`}{the sharp integer maximum, no floor (Mestdagh et al.
#'     2018).}
#'   \item{`"quasi"`}{both bounds sharp and GRIM-free (this package's default).}
#'   \item{`"alpha"`}{additionally conditioning on a reported Cronbach's `alpha`;
#'     needs `n_items >= 2`.}
#'   \item{`"integer"`}{strictly integer data: the lattice of reported
#'     `(mean, sd)` tuples passing GRIM, GRIMMER and the bounds. Drawn as points,
#'     because no band is defined at means and SDs integer data cannot produce.}
#'   \item{`"integer_alpha"`}{that lattice, additionally inside the
#'     alpha-conditional bounds.}
#' }
#'
#' @param l,u Numeric scalars, the scale limits (mean-score units when
#'   `n_items > 1`).
#' @param n Integer scalar, sample size.
#' @param rule Which constraint set to draw; see Details.
#' @param n_items Integer, number of items in the composite (default 1).
#' @param alpha Reported Cronbach's alpha; required by the alpha rules.
#' @param digits Reported decimal places, used by the lattice rules (default 2).
#' @param reference Draw the alpha-free sharp quasi-integer band as a dashed
#'   reference (default `TRUE`; skipped for `rule = "quasi"`, which is that band).
#' @param title Optional plot title.
#' @param by Optional mean-grid spacing (default `(u - l) / 1000`); ignored by
#'   the lattice rules, which step by `10^-digits`.
#' @param fill,line_colour,reference_colour,point_colour,point_size Appearance.
#' @return A ggplot object.
#' @examples
#' plot_sd_region(l = 1, u = 5, n = 7, rule = "mean")            # Bhatia-Davis
#' plot_sd_region(l = 1, u = 5, n = 7, rule = "quasi")           # sharp, GRIM-free
#' \donttest{
#' plot_sd_region(l = 1, u = 5, n = 7, rule = "integer")         # attainable tuples
#' plot_sd_region(l = 1, u = 5, n = 7, rule = "alpha",
#'                n_items = 2, alpha = 0.7)
#' }
#' @export
plot_sd_region <- function(l, u, n,
                           rule = c("quasi", "range", "range_n", "mean",
                                    "mean_naive_floor", "mestdagh",
                                    "pesant_regin", "alpha", "integer",
                                    "integer_alpha"),
                           n_items = 1, alpha = NULL, digits = 2,
                           reference = TRUE, title = NULL, by = NULL,
                           fill = "grey85", line_colour = "black",
                           reference_colour = "grey45",
                           point_colour = "#1d4ed8", point_size = 0.5) {
  rule <- match.arg(rule)
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  d <- sd_region_data(l = l, u = u, n = n, rule = rule, n_items = n_items,
                      alpha = alpha, digits = digits, by = by)

  p <- ggplot2::ggplot()
  # the region itself first, so the dashed reference stays visible on top of it
  if (identical(attr(d, "type"), "band"))
    p <- p + ggplot2::geom_ribbon(data = d,
                                  ggplot2::aes(x = .data$mean, ymin = .data$lo,
                                               ymax = .data$hi),
                                  fill = fill, na.rm = TRUE)

  # dashed reference: the sharp alpha-free band this rule should be judged against
  if (isTRUE(reference) && rule != "quasi") {
    step <- if (is.null(by)) (u - l) / 1000 else by
    ref <- .quasi_band(seq(l, u, by = step), n, l, u, n_items)
    p <- p +
      ggplot2::geom_line(data = ref, ggplot2::aes(.data$mean, .data$hi),
                         colour = reference_colour, linetype = "dashed",
                         linewidth = 0.25, na.rm = TRUE) +
      ggplot2::geom_line(data = ref, ggplot2::aes(.data$mean, .data$lo),
                         colour = reference_colour, linetype = "dashed",
                         linewidth = 0.25, na.rm = TRUE)
  }

  if (identical(attr(d, "type"), "points")) {
    p <- p + ggplot2::geom_point(data = d, ggplot2::aes(.data$mean, .data$sd),
                                 colour = point_colour, size = point_size,
                                 na.rm = TRUE)
  } else {
    p <- p +
      ggplot2::geom_line(data = d, ggplot2::aes(.data$mean, .data$hi),
                         colour = line_colour, linewidth = 0.4, na.rm = TRUE) +
      ggplot2::geom_line(data = d, ggplot2::aes(.data$mean, .data$lo),
                         colour = line_colour, linewidth = 0.4, na.rm = TRUE)
  }

  p +
    ggplot2::labs(x = "Mean", y = "Sample standard deviation", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
