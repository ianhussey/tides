library(shiny)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

maximumVAR <- function(M, MIN, MAX, n){
  if (M == MIN || M == MAX){
    mv = 0
  } else {
    if(abs(MIN) > abs(MAX)){
      MINt <- -MAX
      MAX <- MIN
      MIN <- MINt
      M <- M
    }
    
    nMax <- floor((n*M-n*MIN)/(MAX-MIN))
    nMin <- n-1-nMax
    
    if(nMax == 0){
      MAX <- 0
    }
    
    m <- n*M-nMin*MIN-nMax*MAX
    mv <- (nMin*(MIN-M)^2+nMax*(MAX-M)^2+(M-m)^2)/(n-1)
  }
  
  return(mv)
}

maximumSD <- function(M, MIN, MAX, n){
  return(sqrt(maximumVAR(M, MIN, MAX, n)))
}

# truncation_induced_dependency_test <- function(mean, sd, n, min, max){
#   tibble(mean = mean,
#          sd = sd,
#          n = n,
#          min = min,
#          max = max,
#          max_sd = maximumSD(M = mean, MIN = min, MAX = max, n = n),
#          result = case_when(sd > max_sd ~ "Inconsistent",
#                             sd <= max_sd ~ "Consistent"))
# }

plot_dependency <- function(mean, sd, n, min, max){
  dat <- tibble(mean = mean, 
                sd = sd,
                n = n,
                min = min,
                max = max) %>%
    mutate(max_sd = pmap(list(mean, min, max, n), maximumSD),
           max_sd = as.numeric(max_sd),
           result = ifelse(sd > max_sd | mean < min | mean > max, "Inconsistent", "Consistent"))
  
  data_standardized <- 
    expand_grid(min = min,
                max = max,
                plotting_mean = seq(from = min, to = max, by = 0.001),
                n = n) %>%
    mutate(max_sd = pmap(list(plotting_mean, min, max, n), maximumSD),
           max_sd = as.numeric(max_sd))
  
  data_standardized_reshaped <- data_standardized %>%
    mutate(x = plotting_mean,
           y = max_sd) 
  
  data_polygon_above <- bind_rows(
    data.frame(x = data_standardized_reshaped$x, y = data_standardized_reshaped$y),
    data.frame(x = rev(data_standardized_reshaped$x), y = rep(Inf, length(data_standardized_reshaped$x)))
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
    geom_polygon(data = data_polygon_left, aes(x = x, y = y), fill = "grey10", alpha = 0.4) +
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

ui <- fluidPage(
  titlePanel("TIDES: Truncation-Induced Dependency of Summary Statistics"),
  tags$h3("A method for checking the compatibility of reported means, SDs, and Ns given the min and max of the scale"),
  tags$h4("Hussey, Norwood, Cummins, Arslan, & Elson (2024)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mean", "Mean:", value = 2.1, step = 0.1),
      numericInput("sd", "Standard Deviation:", value = 2.4, step = 0.1, min = 0),
      numericInput("n", "Sample Size:", value = 30, step = 1, min = 2),
      numericInput("min", "Minimum Value:", value = 1, step = 1),
      numericInput("max", "Maximum Value:", value = 7, step = 1),
      actionButton("run_test", "Run Test")
    ),
    mainPanel(
      plotOutput("result_plot")
      #tableOutput("result_table")
    )
  )
)

server <- function(input, output) {
  # result <- eventReactive(input$run_test, {
  #   truncation_induced_dependency_test(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
  # })
  # 
  # output$result_table <- renderTable({
  #   result()
  # })
  
  output$result_plot <- renderPlot({
    plot_dependency(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
  })
}

shinyApp(ui = ui, server = server)
