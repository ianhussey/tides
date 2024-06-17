
# dependencies
library(shiny)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

min_sd <- function(n, mean) {
  # determine the closest integer values around the mean
  lower_value <- floor(mean)
  upper_value <- ceiling(mean)
  
  # calculate frequencies required to achieve the mean
  total_sum <- n * mean
  lower_count <- n * (upper_value - mean)
  upper_count <- n * (mean - lower_value)
  
  # if the calculated counts are not integers, round them appropriately
  lower_count <- round(lower_count)
  upper_count <- round(upper_count)
  
  # ensure the total count equals n
  if (lower_count + upper_count != n) {
    lower_count <- n - upper_count
  }
  
  # form the combination of values
  values <- c(rep(lower_value, lower_count), rep(upper_value, upper_count))
  
  # calculate the mean and standard deviation
  actual_mean <- mean(values)
  min_sd <- sd(values)
  
  return(list(actual_mean = actual_mean, min_sd = min_sd))
}


# Function to check the consistency of the reported SD
tides <- function(mean, sd, n, min, max, verbose = TRUE){
  
  require(tibble)
  require(dplyr)
  
  # Handle edge cases where the mean is at the minimum or maximum
  if (mean == min || mean == max) {
    max_sd <- 0
    min_sd <- 0
  } else {
    
    # Mirror values for special cases to ensure min < max
    if (abs(min) > abs(max)) {
      temp <- min
      min <- max
      max <- temp
    }
    
    # Calculate the number of observations at the maximum value
    num_max <- floor((n * mean - n * min) / (max - min))
    
    # Calculate the number of observations at the minimum value
    num_min <- n - 1 - num_max
    
    # Adjust max if num_max is 0
    if (num_max == 0) {
      max <- 0
    }
    
    # Compute the sum of means adjusted by the number of min and max values
    adjusted_mean_sum <- n * mean - num_min * min - num_max * max
    
    # Compute maximum variability
    max_variability <- (num_min * (min - mean)^2 + num_max * (max - mean)^2 + (mean - adjusted_mean_sum)^2) / (n - 1)
    
    # Calculate the maximum standard deviation
    max_sd <- sqrt(max_variability)
    
    # Calculate the minimum standard deviation using the provided function
    min_sd <- min_sd(n, mean)$min_sd
  }
  
  # Standardize the mean and SD based on the max possible range
  standardized_mean <- (mean - min) / (max - min)
  max_standardized_sd <- ( sqrt( (standardized_mean) * (1 - (standardized_mean)) ) ) * ( sqrt(n / (n - 1)) )
  standardized_sd <- sd / max_sd
  
  # Create the result tibble based on verbosity
  if (verbose) {
    res <- tibble(mean = mean,
                  sd = sd,
                  n = n,
                  min = min,
                  max = max,
                  standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | sd < min_sd ~ "Inconsistent",
                                     sd <= max_sd & sd >= min_sd ~ "Consistent"))
  } else if (!verbose){
    res <- tibble(standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | sd < min_sd ~ "Inconsistent",
                                     sd <= max_sd & sd >= min_sd ~ "Consistent"))
  }
  
  return(res)
}



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


plot_tides_with_min_sd <- function(mean, sd, n, min, max){
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
           y_max = max_sd,
           y_min = min_sd)
  
  data_polygon_above <- bind_rows(
    data.frame(x = data_plot$x, y = data_plot$y_max),
    data.frame(x = rev(data_plot$x), y = rep(Inf, length(data_plot$x)))
  )
  
  data_polygon_below <- bind_rows(
    data.frame(x = data_plot$x, y = data_plot$y_min),
    data.frame(x = rev(data_plot$x), y = rep(-Inf, length(data_plot$x)))
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
    geom_polygon(data = data_polygon_below, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_left,  aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_polygon(data = data_polygon_right, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
    geom_point(data = dat, aes(mean, sd)) +
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


# ui
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('white_wave.png');
        background-repeat: repeat;
      }
      # h4 {
      #   color: #123095;
      # }
      # h3 {
      #   color: #123095;
      # }
      h2 {
        color: #123095;
      }
      # h1 {
      #   color: #123095;
      # }
    "))
  ),
  #titlePanel("TIDES: Truncation-Induced Dependency in Summary Statistics"),
  titlePanel(title = span(img(src = "logo.png", height = 90), "Truncation-Induced Dependency in Summary Statistics")),
  tags$h3("A method for checking the compatibility of reported means, SDs, and Ns given the min and max of the scale"),
  tags$h4("Hussey, Norwood, Cummins, Arslan, & Elson (2024)"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "mean", label = "Mean:", value = 2.1, step = 0.1),
      numericInput(inputId = "sd", label = "Standard Deviation:", value = 2.4, step = 0.1, min = 0),
      numericInput(inputId = "n", label = "Sample Size:", value = 30, step = 1, min = 2),
      numericInput(inputId = "min", label = "Minimum Value:", value = 1, step = 1),
      numericInput(inputId = "max", label = "Maximum Value:", value = 7, step = 1),
      tags$b("Scale is interval:"),
      checkboxInput(inputId = "scale_is_interval", label = "Yes, also calculate minimum SD", value = TRUE),
      actionButton(inputId = "run_test", label = "Run Test")
    ),
    mainPanel(
      plotOutput("result_plot")
    )
  )
)

# server
server <- function(input, output) {

  output$result_plot <- renderPlot({
    if (input$scale_is_interval) {
      plot_tides_with_min_sd(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
    } else {
      plot_tides(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
    }
  })
  
}

# app
shinyApp(ui = ui, server = server)

