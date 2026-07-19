#' Create an umbrella plot of feasible means and SDs
#'
#' Given a tibble of (mean, SD) pairs that pass GRIM, GRIMMER, and TIDES
#' consistency tests (as returned by \code{\link{umbrella}()}), produce
#' a square-point scatterplot of SD vs. mean.
#'
#' @param dat   A tibble containing at least the columns
#'              \describe{
#'                \item{mean}{Numeric: a feasible reported mean.}
#'                \item{sd}{Numeric: a feasible reported standard deviation.}
#'              }
#'              (typically the output of \code{\link{umbrella}()}).
#' @param size  Numeric. Point size (in mm) passed to \code{\link[ggplot2]{geom_point}()};
#'              defaults to 0.5.
#' @param digits Integer. Decimal precision of the reported values, used only to
#'              set the axis expansion. If \code{NULL} (default), taken from a
#'              \code{digits} column in \code{dat} when present, otherwise 2.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object: a scatterplot of \code{sd} vs.\ \code{mean}
#'         with square markers, appropriately scaled axes, and a linedraw theme.
#'
#' @examples
#' \donttest{
#' df <- umbrella(n = 14, min = 1, max = 7, n_items = 1, digits = 2)
#' plot_umbrella(df)
#' plot_umbrella(df, size = 1)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous scale_x_continuous theme_linedraw
#' @importFrom scales breaks_pretty
#' @export
plot_umbrella <- function(dat, size = 0.5, digits = NULL) {
  if (is.null(digits)) {
    digits <- if ("digits" %in% names(dat)) min(dat$digits) else 2
  }
  expand_val <- 10^-digits * 5

  ggplot(dat, aes(x = mean, y = sd)) +
    geom_point(shape = 15, size = size, alpha = 0.8) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 8),
      name = "Standard Deviation",
      limits = c(min(dat$sd), max(dat$sd)),
      expand = c(expand_val, expand_val)
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 7),
      name = "Mean",
      limits = c(min(dat$mean), max(dat$mean)),
      expand = c(expand_val, expand_val)
    ) +
    theme_linedraw()
}
