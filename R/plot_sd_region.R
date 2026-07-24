# One plotting entry point for the feasible-region panels: each `rule` is a
# constraint set from the nested framework, drawn either as a continuous band or
# (for strictly integer data) as the lattice of attainable reported tuples.

# Internal: the alpha-free quasi-integer sharp band. `mg` is the granularity
# multiplier: the affine map w = mg * (x - l) sends the reported grid to the
# integers, so mg = n_items for mean-scored composites and 1 otherwise.
.quasi_band <- function(mean, n, l, u, mg) {
  data.frame(
    mean = mean,
    lo = sd_min_quasi_integer(mg * (mean - l), n) / mg,
    hi = sd_max_structure_s(mg * (mean - l), n, 0, mg * (u - l)) / mg
  )
}

# Internal: resolve a `scoring` choice into the three quantities the region
# formulas need. `mg` is the granularity multiplier (above); `to_sum` maps a
# reported mean to the sum-score mean that the alpha bounds are stated in;
# `sd_div` converts a sum-score SD back to the reported SD's units.
.scoring_geometry <- function(scoring, k, l, u) {
  switch(scoring,
    singleitem = {
      if (k != 1L) stop("scoring = 'singleitem' requires n_items = 1")
      list(mg = 1, item_l = l, item_u = u, to_sum = function(m) m, sd_div = 1)
    },
    meanscored = list(mg = k, item_l = l, item_u = u,
                      to_sum = function(m) k * m, sd_div = k),
    sumscored  = {
      if (abs(l / k - round(l / k)) > 1e-9 || abs(u / k - round(u / k)) > 1e-9)
        stop("scoring = 'sumscored' needs l and u divisible by n_items ",
             "(they are the composite's limits, k times the per-item limits)")
      list(mg = 1, item_l = l / k, item_u = u / k,
           to_sum = function(m) m, sd_div = 1)
    }
  )
}

# Internal: the EXACT attainable (mean, SD) pairs for strictly integer data,
# with no reporting grid. A DP over the n observations accumulates the set of
# reachable (sum, sum-of-squares) pairs on the shifted integer grid
# y = mg * (x - l) in 0..W; both statistics are recovered from (S, Q) at the
# end. Unlike the rounded `"integer"` lattice this shows the true interior
# holes, which rounding smears shut.
.attainable_lattice <- function(l, u, n, mg = 1, max_cells = 2e7) {
  W <- mg * (u - l)
  if (abs(W - round(W)) > 1e-9)
    stop("l, u and n_items must put the scale limits on the integer grid")
  W <- as.integer(round(W))
  Smax <- n * W; Qmax <- n * W * W
  if ((Smax + 1) * (Qmax + 1) > max_cells)
    stop("the exact lattice is too large to enumerate here (",
         format(Smax + 1), " x ", format(Qmax + 1), " cells); use ",
         "rule = 'integer' with a reporting precision instead")
  cur <- matrix(FALSE, Smax + 1L, Qmax + 1L)
  cur[1L, 1L] <- TRUE
  for (i in seq_len(n)) {
    nxt <- matrix(FALSE, Smax + 1L, Qmax + 1L)
    for (v in 0:W) {
      dq <- v * v
      nxt[(1L + v):(Smax + 1L), (1L + dq):(Qmax + 1L)] <-
        nxt[(1L + v):(Smax + 1L), (1L + dq):(Qmax + 1L)] |
        cur[1L:(Smax + 1L - v), 1L:(Qmax + 1L - dq), drop = FALSE]
    }
    cur <- nxt
  }
  w <- which(cur, arr.ind = TRUE)
  S <- w[, 1] - 1L; Q <- w[, 2] - 1L
  ss <- Q - S^2 / n                      # sum of squares is translation-free
  out <- data.frame(mean = l + S / (n * mg),
                    sd   = sqrt(pmax(0, ss) / (n - 1)) / mg)
  out[order(out$mean, out$sd), , drop = FALSE]
}

#' Feasible-region data for one constraint set
#'
#' The `(mean, lo, hi)` band, or the lattice of attainable reported `(mean, sd)`
#' tuples, for a chosen constraint set. See [plot_sd_region()] for the `rule`
#' vocabulary; this is the data behind that plot.
#'
#' @inheritParams plot_sd_region
#' @return For band rules, a data frame with `mean`, `lo`, `hi`. For the lattice
#'   rules (`"integer"`, `"integer_alpha"`, `"attainable"`,
#'   `"attainable_alpha"`), a data frame with `mean`, `sd`: the tuples passing
#'   every applicable test. The `type` attribute is `"band"` or `"points"`.
#' @export
sd_region_data <- function(l, u, n,
                           rule = c("quasi", "range", "range_n", "mean",
                                    "mean_naive_floor", "mestdagh",
                                    "pesant_regin", "alpha", "integer",
                                    "integer_alpha", "attainable",
                                    "attainable_alpha"),
                           scoring = NULL,
                           n_items = 1, alpha = NULL, digits = 2, by = NULL) {
  rule <- match.arg(rule)
  if (rule %in% c("alpha", "integer_alpha", "attainable_alpha") && is.null(alpha))
    stop(sprintf("rule = '%s' requires alpha", rule))
  k <- n_items
  # back-compatible default: a composite is in mean-score units unless told
  # otherwise, and one item is a single item.
  if (is.null(scoring)) scoring <- if (k > 1) "meanscored" else "singleitem"
  scoring <- match.arg(scoring, c("singleitem", "sumscored", "meanscored"))
  g <- .scoring_geometry(scoring, k, l, u)

  # lattice rules: only GRIM/GRIMMER-attainable reported tuples exist, so the
  # region is a set of points, not a band (no curve is defined at the means and
  # SDs that strictly integer data cannot produce).
  if (rule %in% c("integer", "integer_alpha")) {
    um <- umbrella_data(n = n, l = l, u = u, digits = digits, Z = "integer",
                        scoring = scoring, n_items = k,
                        alpha = if (rule == "integer_alpha") alpha else NULL)
    out <- um[which(um$consistent), c("mean", "sd"), drop = FALSE]
    rownames(out) <- NULL
    attr(out, "type") <- "points"
    return(out)
  }

  # exact lattice rules: the true attainable tuples, with no reporting grid.
  if (rule %in% c("attainable", "attainable_alpha")) {
    out <- .attainable_lattice(l, u, n, g$mg)
    if (rule == "attainable_alpha") {
      mus <- unique(out$mean)
      bd <- do.call(rbind, lapply(mus, function(mu) {
        d <- sd_bounds(l = l, u = u, n = n, mean = mu, Z = "integer",
                       scoring = scoring, n_items = k, alpha = alpha)
        data.frame(mean = mu, lo = d$min_sd, hi = d$max_sd,
                   ok = isTRUE(d$feasible) && !is.na(d$min_sd))
      }))
      out <- merge(out, bd, by = "mean")
      out <- out[out$ok & out$sd >= out$lo - 1e-9 & out$sd <= out$hi + 1e-9,
                 c("mean", "sd"), drop = FALSE]
      out <- out[order(out$mean, out$sd), , drop = FALSE]
    }
    rownames(out) <- NULL
    attr(out, "type") <- "points"
    return(out)
  }

  if (is.null(by)) by <- (u - l) / 1000
  m <- seq(l, u, by = by)
  q <- .quasi_band(m, n, l, u, g$mg)
  naive <- sd_min_integer(g$mg * m, n) / g$mg  # strict Bernoulli floor, off-grid too

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
      ceil_a <- sqrt((n / (n - 1)) *
                     v_max_alpha(g$to_sum(m), k, n, g$item_l, g$item_u) / D) / g$sd_div
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
#'   \item{`"attainable"`}{the EXACT attainable `(mean, sd)` tuples of strictly
#'     integer data, with no reporting grid and so no `digits`. Unlike
#'     `"integer"` this shows the true interior holes, which rounding to a
#'     reporting grid smears shut; it is enumerated by dynamic programming and
#'     errors if the lattice is too large.}
#'   \item{`"attainable_alpha"`}{those exact tuples, additionally inside the
#'     alpha-conditional bounds.}
#' }
#'
#' @param l,u Numeric scalars, the scale limits (mean-score units when
#'   `n_items > 1` and `scoring` is left at its default).
#' @param n Integer scalar, sample size.
#' @param rule Which constraint set to draw; see Details.
#' @param scoring One of `"singleitem"`, `"sumscored"`, `"meanscored"`, as in
#'   [sd_bounds()]. Defaults to `"meanscored"` when `n_items > 1` and
#'   `"singleitem"` otherwise. Under `"sumscored"`, `l` and `u` are the
#'   composite's limits and the reported values live on the unit grid, so
#'   `n_items` enters only as Cronbach's alpha's `k`.
#' @param n_items Integer, number of items in the composite (default 1).
#' @param alpha Reported Cronbach's alpha; required by the alpha rules.
#' @param digits Reported decimal places, used by the rounded lattice rules
#'   (default 2); ignored by the exact `"attainable"` rules.
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
                                    "integer_alpha", "attainable",
                                    "attainable_alpha"),
                           scoring = NULL,
                           n_items = 1, alpha = NULL, digits = 2,
                           reference = TRUE, title = NULL, by = NULL,
                           fill = "grey85", line_colour = "black",
                           reference_colour = "grey45",
                           point_colour = "#1d4ed8", point_size = 0.5) {
  rule <- match.arg(rule)
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  d <- sd_region_data(l = l, u = u, n = n, rule = rule, scoring = scoring,
                      n_items = n_items, alpha = alpha, digits = digits,
                      by = by)
  gg <- .scoring_geometry(
    if (is.null(scoring)) (if (n_items > 1) "meanscored" else "singleitem")
    else match.arg(scoring, c("singleitem", "sumscored", "meanscored")),
    n_items, l, u)

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
    ref <- .quasi_band(seq(l, u, by = step), n, l, u, gg$mg)
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
