#' Calculate and plot a TIDES test for a single set of values
#'
#' Explanation to be added
#' 
#' @param res a data frame with one tides result on each row (e.g., created by tides_single() or tides_multiple()). Note that The parameters min, max, n_items, and digits must be idential on all rows for plot_tides_multiple() to run."
#' @returns A ggplot object: the TIDES plot 
#' @examples
#' \dontrun{
#' res <- bind_rows(tides_single(mean = 3.10, sd = 0.80, n = 65, min = 1, max = 7, n_items = 1, digits = 2),
#'                  tides_single(mean = 4.10, sd = 0.80, n = 65, min = 1, max = 7, n_items = 1, digits = 2),
#'                  tides_single(mean = 5.06, sd = 0.83, n = 65, min = 1, max = 7, n_items = 1, digits = 2))
#' 
#' plot_tides_multiple(res)
#' }
#' 
#' @export 
plot_tides_multiple <- function(res){
  
  if(res |> distinct(min, max, n_items, digits) |> nrow() != 1){
    stop("The parameters min, max, n_items, and digits must be idential on all rows for plot_tides_multiple() to run.")
  }
    
  data_reported <- res 
  
  data_params <- res |>
    slice(1)
  
  data_plot <- 
    expand_grid(min = data_params$min,
                max = data_params$max,
                plotting_mean = seq(from = data_params$min, to = data_params$max, by = 0.01),
                n = data_params$n) |>
    mutate(results = pmap(list(plotting_mean, data_params$sd, data_params$n, data_params$min, data_params$max, data_params$n_items, data_params$digits, data_params$calculate_min_sd, verbose = FALSE), 
                          tides_single)) |>
    unnest(results) |>
    rename(x = plotting_mean,
           y_max = max_sd,
           y_min = min_sd) |>
    drop_na()
  
  data_polygon_above <- bind_rows(
    tibble(x = data_plot$x, y = data_plot$y_max),
    tibble(x = rev(data_plot$x), y = rep(Inf, length(data_plot$x)))
  )
  
  data_polygon_below <- bind_rows(
    tibble(x = data_plot$x, y = data_plot$y_min),
    tibble(x = rev(data_plot$x), y = rep(-Inf, length(data_plot$x)))
  )
  
  data_polygon_left <- tibble(
    x = c(-Inf,  data_params$min, data_params$min, -Inf),
    y = c(-Inf, -Inf, Inf,  Inf)
  )
  
  data_polygon_right <- tibble(
    x = c( data_params$max,  Inf, Inf, data_params$max),
    y = c(-Inf, -Inf, Inf, Inf)
  )
  
  p <- 
    ggplot() +
    geom_polygon(data = data_polygon_above, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_point(data = data_reported, aes(mean, sd, color = tides_consistent)) +
    # geom_text(data = data_reported, aes(mean, sd, label = tides_consistent), 
    #           vjust = -1, size = 7) +
    #scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
    scale_y_continuous(expand = c(0.01, 0.1)) +
    # scale_x_continuous(expand = expansion(mult = c(0, 0)),
    #                    labels = seq(from = min, to = max, by = 1),
    #                    breaks = seq(from = min, to = max, by = 1)) +
    scale_x_continuous(expand = c(0.01, 0.01),
                       labels = seq(from = data_params$min, to = data_params$max, by = 1),
                       breaks = seq(from = data_params$min, to = data_params$max, by = 1)) +
    # theme_minimal(base_size = 20) +
    theme_minimal() +
    theme(legend.position = "top",
          legend.direction = "vertical") +
    scale_color_manual(values = c("TRUE" = "#43BF71FF", 
                                  "FALSE" = "#35608DFF"),
                       labels = c("TRUE" = "TIDES-consistent value", 
                                  "FALSE" = "TIDES-inconsistent value")) +
    labs(x = "Mean",
         y = "SD",
         color = "") +
    guides(color = guide_legend(override.aes = list(size = 4, ncol = 1), title = NULL))
  
  if(data_params$calculate_min_sd){
    p <- p + geom_polygon(data = data_polygon_below, aes(x = x, y = y), fill = "grey10", alpha = 0.4)
  }
  
  if(data_params$max - data_params$min > 10){
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }
  
  return(p)
}


