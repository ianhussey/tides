#' Plot TIDES consistency check
#'
#' Depending on the input, this function either plots a single reported mean/SD value with the TIDES envelope or multiple reported values sharing the same parameters. It shows:
#' \itemize{
#'   \item The reported mean and SD values.
#'   \item The TIDES-consistent region as an envelope of SD bounds across the range of means.
#'   \item Shaded regions where reported values would be inconsistent.
#' }
#'
#' @details This plot helps visualize whether reported mean and standard deviation values are internally consistent with bounded measurement scales, using the TIDES (Test for Internal Data Error Sensitivity) method.
#'
#' @param res data.frame. Output of tides(), containing at minimum the columns mean, sd, n, min, max, n_items, digits, calculate_min_sd, method, and tides_consistent.
#' @param method Character. Method to use for boundary calculation (optional).
#' @param text_size Numeric. Scaling factor for text elements (default: 0.6).
#' @param color_true Character. Color for consistent points (default: green).
#' @param color_false Character. Color for inconsistent points (default: blue).
#'
#' @return A ggplot object showing the TIDES boundary envelope, shaded inconsistency zones, and reported mean–SD points with legend reversed.
#'
#' @examples
#' \donttest{
#' # A single reported mean-SD pair
#' tides(mean = 3.10, sd = 0.80, n = 1100, min = 1, max = 7) |>
#'   plot_tides()
#'
#' # An SD near the upper bound
#' tides(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7) |>
#'   plot_tides()
#'
#' # The more liberal "approximate" envelope
#' tides(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7, method = "approximate") |>
#'   plot_tides()
#'
#' # Multiple reported values that share the same n, scale and precision
#' dat <- data.frame(
#'   mean    = c(3.1, 4.0, 5.5, 6.2),
#'   sd      = c(0.8, 1.4, 2.9, 0.6),
#'   n       = 30,
#'   min     = 1,
#'   max     = 7,
#'   n_items = 1,
#'   digits  = 2,
#'   calculate_min_sd = TRUE,
#'   method  = "approximate"
#' )
#' plot_tides(tides_df(dat))
#' }
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_line geom_point scale_color_manual scale_y_continuous scale_x_continuous theme_minimal theme guides guide_legend
#' @importFrom dplyr distinct filter mutate slice bind_rows
#' @importFrom tidyr unnest expand_grid drop_na
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @importFrom scales breaks_pretty
#'
#' @export
plot_tides <- function(res, method = NULL, text_size = 0.6, color_true = "#43BF71FF", color_false = "#35608DFF") {
  if (nrow(res) == 1) {
    data_params <- res
  } else {
    if (res |> distinct(min, max, n_items, digits, method) |> nrow() != 1) {
      stop("The parameters min, max, n_items, digits, and method must be identical on all rows.")
    }
    data_params <- res |> slice(1)
  }
  
  if (is.null(method)) {
    method <- data_params$method
  }
  
  boundary_data <- expand_grid(
    mean = seq(data_params$min, data_params$max, by = 10^-data_params$digits),
    n = data_params$n,
    n_items = data_params$n_items,
    digits = data_params$digits,
    min = data_params$min,
    max = data_params$max,
    calculate_min_sd = data_params$calculate_min_sd
  ) |> 
    mutate(sd_bounds = purrr::pmap(list(mean, n, min, max, n_items, digits, calculate_min_sd), sd_bounds)) |> 
    unnest(sd_bounds)
  
  if (method == "approximate") {
    boundary_data <- boundary_data |> 
      filter(mean >= min, mean <= max) |> 
      approximate_sd_bounds()
    
    true_label <- "TIDES consistent"
    false_label <- "TIDES inconsistent"
  } else if (method == "exact"){
    boundary_data <- boundary_data |> 
      drop_na(min_sd, max_sd)
    
    if (!data_params$calculate_min_sd) {
      boundary_data <- boundary_data |> 
        mutate(min_sd = 0)
    }
    true_label <- "GRIMMER-TIDES consistent"
    false_label <- "GRIMMER-TIDES inconsistent"
  } else {
    stop("`method` must be one of c(NULL, 'exact', 'approximate')")
  }
  
  poly_above <- bind_rows(
    tibble(x = boundary_data$mean, y = boundary_data$max_sd),
    tibble(x = rev(boundary_data$mean), y = rep(Inf, nrow(boundary_data)))
  )
  poly_below <- bind_rows(
    tibble(x = boundary_data$mean, y = boundary_data$min_sd),
    tibble(x = rev(boundary_data$mean), y = rep(-Inf, nrow(boundary_data)))
  )
  poly_left <- tibble(x = c(-Inf, data_params$min, data_params$min, -Inf), y = c(-Inf, -Inf, Inf, Inf))
  poly_right <- tibble(x = c(data_params$max, Inf, Inf, data_params$max), y = c(-Inf, -Inf, Inf, Inf))
  
  p <- ggplot() +
    geom_polygon(data = poly_above, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_below, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_left, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_right, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_line(data = boundary_data, aes(x = mean, y = max_sd)) +
    geom_line(data = boundary_data, aes(x = mean, y = min_sd)) +
    geom_point(data = res, aes(mean, sd, color = tides_consistent)) +
    scale_color_manual(values = c("TRUE" = color_true, "FALSE" = color_false),
                       labels = c("TRUE" = true_label, "FALSE" = false_label)) +
    scale_y_continuous(name = "Standard Deviation", 
                       limits = c(0, NA), 
                       breaks = scales::breaks_pretty(n = 8),
                       expand = c(10^-min(data_params$digits)*5, 10^-min(data_params$digits)*5)) +
    scale_x_continuous(name = "Mean",
                       breaks = scales::breaks_pretty(n = 10),
                       expand = c(10^-min(data_params$digits)*5, 10^-min(data_params$digits)*5)) + 
    theme_minimal(base_size = text_size * 20) +
    theme(legend.position = "top") +
    guides(color = guide_legend(reverse = TRUE, 
                                override.aes = list(size = 4, ncol = 1), 
                                title = NULL)) 

  return(p)
}



