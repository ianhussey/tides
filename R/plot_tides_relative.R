#' Plot TIDES consistency on a standardized relative scale
#'
#' Given a data frame of TIDES outcomes (with percent-of-maximum-possible 
#' transformed mean and SD), \code{plot_tides_relative()} draws each 
#' point’s relative location (\% of maximum possible mean) on the x-axis 
#' against its relative dispersion (\% of maximum possible SD) on the 
#' y-axis (signed log₁₀ scale).  Regions outside the feasible 0–1 square 
#' are shaded, and points are coloured by whether they passed the TIDES 
#' consistency check.
#'
#' @param res A \code{data.frame} or \code{tibble} containing at minimum:
#'   \describe{
#'     \item{\code{relative_location}}{Numeric in [0,1]: POMP mean.}
#'     \item{\code{relative_dispersion}}{Numeric: POMP SD on a signed log₁₀ scale.}
#'     \item{\code{tides_consistent}}{Logical: consistency flag.}
#'   }
#' @param color_true  Colour for points with \code{tides_consistent == TRUE}.
#'   Defaults to \code{"\#43BF71FF"}.
#' @param color_false Colour for points with \code{tides_consistent == FALSE}.
#'   Defaults to \code{"\#35608DFF"}.
#' @param color_region Colour used to outline the feasible 0–1 square and to
#'   shade the four “infeasible” quadrants. Defaults to \code{"turquoise4"}.
#'
#' @return A \code{ggplot} object showing:
#'   \itemize{
#'     \item Shaded regions outside the unit square.
#'     \item A black border and separator lines demarcating the 0–1 square.
#'     \item Points coloured by consistency, with a signed log₁₀ transform on the
#'           y-axis for dispersion.
#'   }
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
#' 
#' @export
plot_tides_relative <- function(res, color_true = "#43BF71FF", color_false = "#35608DFF", color_region = "turquoise4"){
  
  # check for "method" column and its values
  if (!"method" %in% names(res)) {
    stop("Relative TIDES plot requires a `method` column in the input data.")
  }
  if (any(res$method != "approximate", na.rm = TRUE)) {
    stop("Relative TIDES plot requires `method = 'approximate'` for all rows.")
  }
  
  signed_log10_trans <- scales::trans_new(
    name = "signed_log10",
    transform = function(x) sign(x) * log10(abs(x) + 1),
    inverse = function(x) sign(x) * (10^abs(x) - 1)
  )
  
  ggplot(res, aes(relative_location, relative_dispersion, color = tides_consistent)) +
    # shaded areas
    geom_rect(data = tibble(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey10",
              alpha = 0.3) +
    geom_rect(data = tibble(xmin = 1, xmax = Inf, ymin = -Inf, ymax = Inf),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey10",
              alpha = 0.3) +
    geom_rect(data = tibble(xmin = 0, xmax = 1, ymin = 1, ymax = Inf),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey10",
              alpha = 0.3) +
    geom_rect(data = tibble(xmin = 0, xmax = 1, ymin = -Inf, ymax = 0),
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey10",
              alpha = 0.3) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
              fill = NA,
              color = "black") +
    # black line separating shaded and unshaded areas
    geom_segment(data = tibble(x = 0, y = 0, xend = 1, yend = 0),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, 
                 linewidth = 0.2, 
                 color = "black") +   # bottom
    geom_segment(data = tibble(x = 1, y = 0, xend = 1, yend = 1),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, 
                 linewidth = 0.2, 
                 color = "black") +   # right
    geom_segment(data = tibble(x = 1, y = 1, xend = 0, yend = 1),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, 
                 linewidth = 0.2, 
                 color = "black") +   # top
    geom_segment(data = tibble(x = 0, y = 1, xend = 0, yend = 0),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE, 
                 linewidth = 0.2, 
                 color = "black") +   # left
    # data points
    geom_point(alpha = 0.7) + # shape = 15,  size = 2, 
    # axes and theme
    scale_x_continuous(breaks = scales::breaks_pretty(n = 10),
                       labels = scales::label_percent(),
                       #name = "Percent-Of-Maximum-Possible Mean") +
                       name = "Relative location") +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                       labels = scales::label_percent(),
                       #name = "Percent-Of-Maximum-Possible SD",
                       name = "Relative dispersion",
                       trans = signed_log10_trans) +
    scale_color_manual(values = c("TRUE" = color_true, "FALSE" = color_false),
                       labels = c("TRUE" = "TIDES consistent", "FALSE" = "TIDES inconsistent")) +
    theme_linedraw() +
    theme(legend.position = "top") +
    guides(color = guide_legend(reverse = TRUE,
                                override.aes = list(size = 4, ncol = 1), 
                                title = NULL))
}



