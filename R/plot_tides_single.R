#' Calculate and plot a TIDES test for a single set of values
#'
#' Explanation to be added
#' 
#' @param res a data frame, the output of tides_single().
#' @param base_size scaling for the text in the plot. 
#' @returns A ggplot object: the TIDES plot 
#' @examples
#' \dontrun{
#' tides_single(mean = 3.10, sd = 0.80, n = 1100, min = 1, max = 7, n_items = 1, digits = 2) |>
#'   plot_tides_single()
#' 
#' tides_single(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7, n_items = 1, digits = 2) |>
#'   plot_tides_single()
#' }
#' 
#' @export 
plot_tides_single <- function(res, text_size = 0.6){
  
  if(res |> nrow() != 1){
    stop("The input data frame must have one row, i.e., the output of tides_single().")
  }
  
  data_reported <- res |>
    mutate(label = case_when(tides_consistent == TRUE ~ "TIDES-consistent value", 
                             tides_consistent == FALSE ~ "TIDES-inconsistent value"))
  
  data_plot <- 
    expand_grid(min = data_reported$min,
                max = data_reported$max,
                plotting_mean = seq(from = data_reported$min, to = data_reported$max, by = 0.01),
                n = data_reported$n) |>
    mutate(results = pmap(list(plotting_mean, data_reported$sd, data_reported$n, data_reported$min, data_reported$max, data_reported$n_items, data_reported$digits, data_reported$calculate_min_sd, verbose = FALSE), 
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
    x = c(-Inf,  data_reported$min, data_reported$min, -Inf),
    y = c(-Inf, -Inf, Inf,  Inf)
  )
  
  data_polygon_right <- tibble(
    x = c( data_reported$max,  Inf, Inf, data_reported$max),
    y = c(-Inf, -Inf, Inf, Inf)
  )
  
  p <- 
    ggplot() +
    geom_polygon(data = data_polygon_above, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_point(data = data_reported, aes(mean, sd, color = tides_consistent)) +
    geom_text(data = data_reported, aes(mean, sd, label = label), 
              vjust = -1, size = text_size*7) +
    #scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
    scale_y_continuous(expand = c(0.01, 0.1)) +
    # scale_x_continuous(expand = expansion(mult = c(0, 0)),
    #                    labels = seq(from = min, to = max, by = 1),
    #                    breaks = seq(from = min, to = max, by = 1)) +
    scale_x_continuous(expand = c(0.01, 0.01),
                       labels = seq(from = data_reported$min, to = data_reported$max, by = 1),
                       breaks = seq(from = data_reported$min, to = data_reported$max, by = 1)) +
    theme_minimal(base_size = text_size*20) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("TRUE" = "#43BF71FF", 
                                  "FALSE" = "#35608DFF")) +
    labs(x = "Mean",
         y = "SD") 
  
  if(data_reported$calculate_min_sd){
    p <- p + geom_polygon(data = data_polygon_below, aes(x = x, y = y), fill = "grey10", alpha = 0.4)
  }
  
  if(data_reported$max - data_reported$min > 10){
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }
  
  return(p)
}
