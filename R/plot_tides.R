
plot_tides <- function(mean, sd, n, min, max){
  dat <- tibble(mean = mean, 
                sd = sd,
                n = n,
                min = min,
                max = max) |>
    mutate(results = pmap(list(mean, sd, n, min, max, verbose = FALSE), tides)) |>
    unnest(results) 
  
  data_plot <- 
    expand_grid(min = min,
                max = max,
                plotting_mean = seq(from = min, to = max, by = 0.01),
                n = n) |>
    mutate(results = pmap(list(plotting_mean, sd, n, min, max, verbose = FALSE), tides)) |>
    unnest(results) |>
    mutate(x = plotting_mean,
           y = max_sd) 
  
  data_polygon_above <- bind_rows(
    data.frame(x = data_plot$x, y = data_plot$y),
    data.frame(x = rev(data_plot$x), y = rep(Inf, length(data_plot$x)))
  )
  
  data_polygon_left <- data.frame(
    x = c(-Inf, min, min, -Inf),
    y = c(-Inf, -Inf, Inf, Inf)
  )
  
  data_polygon_right <- data.frame(
    x = c(max, Inf, Inf, max),
    y = c(-Inf, -Inf, Inf, Inf)
  )
  
  ggplot() +
    geom_polygon(data = data_polygon_above, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_point(data = dat, aes(mean, sd)) +
    # geom_text(data = dat, aes(mean, sd, label = paste0("M = ", mean, ", SD = ", sd, ": ", result)), 
    #           vjust = -1, size = 7) +
    geom_text(data = dat, aes(mean, sd, label = result), 
              vjust = -1, size = 7) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       labels = seq(from = min, to = max, by = 1),
                       breaks = seq(from = min, to = max, by = 1)) +
    labs(x = "Mean",
         y = "SD") +
    theme_minimal(base_size = 20) +
    theme(legend.position = "none")
}
