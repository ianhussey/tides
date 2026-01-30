#' Plot TIDES consistency on a standardized relative scale
#'
#' Given a data frame of TIDES outcomes (with percent-of-maximum-possible
#' transformed mean and SD), `plot_tides_relative()` draws each point’s
#' relative location (proportion of maximum possible mean) on the x-axis
#' against its relative dispersion (proportion of maximum possible SD)
#' on the y-axis, transformed using an *asymmetric signed log₁₀ scale*.
#'
#' Regions outside the feasible 0–1 square are shaded to indicate
#' infeasible value combinations. Points are colored by whether they passed
#' the TIDES consistency check.
#'
#' ## Y-Axis Transformation
#' The y-axis applies a **custom signed log₁₀ transformation** that stretches
#' negative values by a factor of 10 to enhance interpretability and visual
#' separation of implausibly low dispersions. Specifically:
#'
#' \deqn{
#'   y =
#'   \begin{cases}
#'     \log_{10}(x + 1) & \text{if } x \geq 0 \\
#'     -10 \cdot \log_{10}(|x| + 1) & \text{if } x < 0
#'   \end{cases}
#' }
#'
#' The inverse transformation is:
#'
#' \deqn{
#'   x =
#'   \begin{cases}
#'     10^y - 1 & \text{if } y \geq 0 \\
#'     -(10^{-y / 10} - 1) & \text{if } y < 0
#'   \end{cases}
#' }
#'
#' This scaling preserves 0 as a fixed point and stretches the range of
#' negative values, helping to distinguish near-zero and implausibly low
#' dispersions that would otherwise be visually compressed.
#'
#' @param res A \code{data.frame} or \code{tibble} containing at minimum:
#'   \describe{
#'     \item{\code{relative_location}}{Numeric in [0,1]: POMP mean.}
#'     \item{\code{relative_dispersion}}{Numeric: POMP SD as a proportion of max SD.}
#'     \item{\code{tides_consistent}}{Logical: consistency flag.}
#'     \item{\code{method}}{Character: must be \code{"approximate"} for all rows.}
#'   }
#' @param color_true Colour for points with \code{tides_consistent == TRUE}.
#'   Defaults to \code{"#43BF71FF"}.
#' @param color_false Colour for points with \code{tides_consistent == FALSE}.
#'   Defaults to \code{"#35608DFF"}.
#' @param color_region Colour used to outline the feasible 0–1 square and to
#'   shade the four “infeasible” quadrants. Defaults to \code{"turquoise4"}.
#' @param alpha Transparency level for plotted points. Defaults to \code{0.7}.
#' @param shade_improbable Logical. If \code{TRUE}, highlights regions with
#'   implausibly low or high relative dispersion or extreme means.
#'
#' @return A \code{ggplot} object showing:
#' \itemize{
#'   \item Shaded infeasible regions beyond the unit square.
#'   \item A black-bordered central feasible region (0–1 for both axes).
#'   \item Points colored by TIDES consistency.
#'   \item A y-axis transformed using a signed log₁₀ scale that stretches
#'         negative values 10× more than positive ones for visual clarity.
#' }
#'
#' @import ggplot2
#' @importFrom scales trans_new breaks_pretty label_percent
#'
#' @examples
#' \dontrun{
#' # multiple M-SD pairs which have identical N, min, and mean too
#' set.seed(12)
#'
#' res <-
#'   tibble(mean    = round_half_up(runif(n = 20, min = 1, max = 7), 2),
#'          sd      = round_half_up(runif(n = 20, min = 0, max = 4), 2),
#'          n       = 11,
#'          min     = 1,
#'          max     = 7,
#'          n_items = 1,
#'          digits  = 2,
#'          calculate_min_sd = TRUE,
#'          verbose = FALSE,
#'          method = "approximate") |>
#'   mutate(results = pmap_dfr(list(mean = mean,
#'                                  sd = sd,
#'                                  n = n,
#'                                  min = min,
#'                                  max = max,
#'                                  n_items = n_items,
#'                                  digits = digits,
#'                                  calculate_min_sd = calculate_min_sd,
#'                                  verbose = verbose,
#'                                  method = method),
#'                             tides)) |>
#'   unnest(results)
#'
#' plot_tides_relative(res)
#'
#' # More likely in real life: different M-SD pairs that also differ in their N, min and max
#' set.seed(12)
#'
#' res <-
#'   bind_rows(
#'     tibble(mean    = round_half_up(runif(n = 20, min = 1, max = 5), 2),
#'            sd      = round_half_up(runif(n = 20, min = 0, max = 3), 2),
#'            n       = 11,
#'            min     = 1,
#'            max     = 5,
#'            n_items = 1,
#'            digits  = 2,
#'            calculate_min_sd = TRUE,
#'            verbose = FALSE,
#'            method = "approximate"),
#'     tibble(mean    = round_half_up(runif(n = 20, min = 1, max = 7), 2),
#'            sd      = round_half_up(runif(n = 20, min = 0, max = 4), 2),
#'            n       = 29,
#'            min     = 1,
#'            max     = 7,
#'            n_items = 1,
#'            digits  = 2,
#'            calculate_min_sd = TRUE,
#'            verbose = FALSE,
#'            method = "approximate"),
#'     tibble(mean    = round_half_up(runif(n = 20, min = 1, max = 10), 2),
#'            sd      = round_half_up(runif(n = 20, min = 0, max = 5), 2),
#'            n       = 41,
#'            min     = 1,
#'            max     = 10,
#'            n_items = 1,
#'            digits  = 2,
#'            calculate_min_sd = TRUE,
#'            verbose = FALSE,
#'            method = "approximate")
#'   ) |>
#'   mutate(results = pmap_dfr(list(mean = mean,
#'                                  sd = sd,
#'                                  n = n,
#'                                  min = min,
#'                                  max = max,
#'                                  n_items = n_items,
#'                                  digits = digits,
#'                                  calculate_min_sd = calculate_min_sd,
#'                                  verbose = verbose,
#'                                  method = method),
#'                             tides)) |>
#'   unnest(results)
#'
#' # # note that the standard tides plot won't run, as N/min/max differ
#' # plot_tides(res) # throws an error
#'
#' # relative plot allows combining of these different results
#' plot_tides_relative(res)
#' }
#'
#' @import ggplot2
#' @importFrom scales trans_new breaks_pretty label_percent
#' @importFrom forcats fct_relevel
#'
#' @export
plot_tides_relative <- function(
  res,
  color_true = "#43BF71FF",
  color_false = "#35608DFF",
  color_region = "turquoise4",
  alpha = 0.7,
  shade_improbable = FALSE
) {
  # check for "method" column and its values
  if (!"method" %in% names(res)) {
    stop("Relative TIDES plot requires a `method` column in the input data.")
  }
  if (!all(res$method == "approximate", na.rm = TRUE)) {
    stop("Relative TIDES plot requires `method = 'approximate'` for all rows.")
  }

  # signed_log10_trans <- scales::trans_new(
  #   name = "signed_log10",
  #   transform = function(x) sign(x) * log10(abs(x) + 1),
  #   inverse = function(x) sign(x) * (10^abs(x) - 1)
  # )

  signed_log10_trans <- scales::trans_new(
    name = "signed_log10_trans",
    transform = function(x) {
      ifelse(x < 0, -10 * log10(abs(x) + 1), log10(x + 1))
    },
    inverse = function(x) {
      ifelse(x < 0, -(10^(-x / 10) - 1), 10^x - 1)
    }
  )

  # Define helper functions

  geom_rect_shade <- function(..., .fill = "grey10") {
    geom_rect(
      data = tibble::tibble(...),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = .fill,
      alpha = 0.3
    )
  }

  geom_segment_separator <- function(...) {
    geom_segment(
      data = tibble(...),
      aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE,
      linewidth = 0.25,
      color = "black"
    )
  }

  # Initial plotting
  p <- res |>
    mutate(
      tides_consistent = fct_relevel(
        as.character(tides_consistent),
        "TRUE",
        "FALSE"
      )
    ) |>
    ggplot(aes(
      relative_location,
      relative_dispersion,
      color = tides_consistent
    )) +

    # Shade the impossible areas in grey
    geom_rect_shade(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf) +
    geom_rect_shade(xmin = 1, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_rect_shade(xmin = 0, xmax = 1, ymin = 1, ymax = Inf) +
    geom_rect_shade(xmin = 0, xmax = 1, ymin = -Inf, ymax = 0) +

    # Black line separating shaded and unshaded areas
    geom_segment_separator(x = 0, y = 0, xend = 1, yend = 0) + # bottom
    geom_segment_separator(x = 1, y = 0, xend = 1, yend = 1) + # right
    geom_segment_separator(x = 1, y = 1, xend = 0, yend = 1) + # top
    geom_segment_separator(x = 0, y = 1, xend = 0, yend = 0) + # left

    # Data points
    geom_point(alpha = alpha) + # shape = 15,  size = 2,

    # Axes and theme
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 10),
      labels = scales::label_percent(),
      #name = "Percent-Of-Maximum-Possible Mean") +
      name = "Relative location"
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 10),
      labels = scales::label_percent(),
      #name = "Percent-Of-Maximum-Possible SD",
      name = "Relative dispersion",
      trans = signed_log10_trans
    ) +
    scale_color_manual(
      values = c("TRUE" = color_true, "FALSE" = color_false),
      labels = c("TRUE" = "TIDES consistent", "FALSE" = "TIDES inconsistent")
    ) +
    theme_linedraw() +
    theme(legend.position = "top") +
    guides(
      color = guide_legend(
        reverse = FALSE,
        override.aes = list(size = 4, ncol = 1),
        title = NULL
      )
    )

  if (!shade_improbable) {
    return(p)
  }

  # Shade the improbable areas in red, not in grey
  formals(geom_rect_shade)$.fill <- "darkred"

  p +
    geom_rect_shade(xmin = 0, xmax = 1, ymin = 0, ymax = 0.05) +
    geom_rect_shade(xmin = 0, xmax = 1, ymin = 0.70, ymax = 1) +
    geom_rect_shade(xmin = 0, xmax = 0.05, ymin = 0.05, ymax = 0.70) +
    geom_rect_shade(xmin = 0.95, xmax = 1, ymin = 0.05, ymax = 0.70)
}
