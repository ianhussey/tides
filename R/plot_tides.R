#' Plot a TIDES consistency test for a single summary
#'
#' \code{plot_tides} takes the output of \code{tides()} (a one‐row data frame)
#' and produces a ggplot2 graphic showing:
#' \itemize{
#'   \item The observed mean and standard deviation (SD) as a single point.
#'   \item An envelope of allowable SD values as a function of the mean,
#'     computed by \code{sd_bounds()}.
#'   \item Shaded regions where the reported SD falls outside those limits.
#' }
#'
#' @details
#' The TIDES (Test for Internal Data Error Sensitivity) plot helps you
#' visualize whether a reported mean and SD for bounded data are
#' internally consistent.  The upper and lower SD bounds are
#' determined by sampling all possible response patterns on your
#' measurement scale.  If the point lies outside the shaded area,
#' it is flagged as inconsistent.
#'
#' @param res A one‐row data frame, the output of \code{tides()}, containing
#'   at minimum the columns \code{mean}, \code{sd}, \code{n}, \code{min},
#'   \code{max}, \code{n_items}, \code{digits}, and \code{tides_consistent}.
#' @param text_size A numeric scalar; scaling factor for all text elements
#'   in the plot (default: 0.6).
#'
#' @return
#' A \code{ggplot} object showing the TIDES consistency envelope,
#' the observed point, and shaded regions where the SD is impossible
#' given the mean.
#'
#' @examples
#' \dontrun{
#' tides(mean = 3.10, sd = 0.80, n = 1100, min = 1, max = 7, n_items = 1, digits = 2) |>
#'   plot_tides()
#' 
#' tides(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7, n_items = 1, digits = 2) |>
#'   plot_tides()
#' }
#' 
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purrr
#' @import scales
#' 
#' @export 
plot_tides <- function(res, text_size = 0.6) {
  # 1. check input
  if (nrow(res) != 1) {
    stop("The input data frame must have one row, i.e., the output of tides().")
  }
  
  # 2. pull out metadata and label
  data_reported <- res |>
    mutate(
      label = if_else(tides_consistent,
                      "Consistent",
                      "Inconsistent")
    )
  
  # 3. build the grid and compute min_sd / max_sd via sd_bounds (as in umbrella_plot_boundary)
  boundary_data <- expand_grid(
    mean      = seq(res$min, res$max, by = 10^(-res$digits)),
    n_obs     = res$n,
    n_items   = res$n_items,
    precision = res$digits,
    min_val   = res$min,
    max_val   = res$max,
    calculate_min_sd = res$calculate_min_sd
  ) |>
    mutate(sd_bounds = purrr::pmap(list(mean = mean, 
                                        n = n_obs, 
                                        min = min_val, 
                                        max = max_val, 
                                        n_items = n_items, 
                                        digits = precision,
                                        calculate_min_sd = calculate_min_sd),
                                   sd_bounds)) |>
    unnest(sd_bounds) |>
    drop_na(min_sd, max_sd)
  
  # override lower bound to zero if not requested
  if (!res$calculate_min_sd) {
    boundary_data <- boundary_data |>
      mutate(min_sd = 0)
  }
  
  # 4. construct polygons to shade outside [min_sd, max_sd]
  poly_above <- bind_rows(
    tibble(x = boundary_data$mean,      y = boundary_data$max_sd),
    tibble(x = rev(boundary_data$mean), y = rep(Inf, nrow(boundary_data)))
  )
  poly_below <- bind_rows(
    tibble(x = boundary_data$mean,      y = boundary_data$min_sd),
    tibble(x = rev(boundary_data$mean), y = rep(-Inf, nrow(boundary_data)))
  )
  poly_left  <- tibble(
    x = c(-Inf, res$min, res$min, -Inf),
    y = c(-Inf, -Inf,     Inf,     Inf)
  )
  poly_right <- tibble(
    x = c(res$max, Inf, Inf, res$max),
    y = c(-Inf,   -Inf, Inf, Inf)
  )
  
  # 5. assemble plot
  p <- ggplot() +
    # shading
    geom_polygon(data = poly_above, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_left,  aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_right, aes(x, y), fill = "grey10", alpha = 0.3) +
    # always shade below the lower bound (which is 0 if calculate_min_sd == FALSE)
    geom_polygon(data = poly_below, aes(x, y), fill = "grey10", alpha = 0.3)
  
  # # only shade below if requested
  # if (res$calculate_min_sd) {
  #   p <- p +
  #     geom_polygon(data = poly_below, aes(x, y), fill = "grey10", alpha = 0.4)
  # }
  
  # boundary lines
  p <- p +
    geom_line(
      data = boundary_data,
      aes(x = mean, y = max_sd),
      size = 0.5
    ) +
    geom_line(
      data = boundary_data,
      aes(x = mean, y = min_sd),
      size = 0.5
    ) +
    
    # reported point + label
    geom_point(
      data = data_reported,
      aes(mean, sd, color = tides_consistent),
      size = 3
    ) +
    geom_text(
      data = data_reported,
      aes(mean, sd, label = label),
      vjust = -1,
      size = text_size * 7
    ) +
    
    # scales & theme
    scale_x_continuous(
      name   = "Mean",
      expand = c(0.05, 0.05),
      breaks = 
        if(res$max - res$min > 10){
          scales::breaks_pretty(10)
        } else {
          seq(res$min, res$max, by = 1)
        }
    ) +
    scale_y_continuous(
      name   = "Standard Deviation",
      limits = c(0, NA), # ensure y ≥ 0
      breaks = scales::breaks_pretty((res$max - res$min)),
      expand = c(0.05, 0.05)
    ) +
    scale_color_manual(
      values = c("TRUE"  = "#43BF71FF",
                 "FALSE" = "#35608DFF")
    ) +
    theme_minimal(base_size = text_size * 20) +
    theme(legend.position = "none")
  
  return(p)
}
