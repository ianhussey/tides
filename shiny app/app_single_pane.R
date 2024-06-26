
# dependencies
library(shiny)
library(TIDES)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

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
  title = "TIDES",
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
    plot_tides(mean = input$mean, 
               sd = input$sd, 
               n = input$n, 
               min = input$min, 
               max = input$max,
               calculate_min_sd = input$scale_is_interval)
    
  })
  
}

# app
shinyApp(ui = ui, server = server)

