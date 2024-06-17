
# dependencies
library(shiny)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

setwd("~/git/TIDES/") # while it isn't a deployed package, use setwd()
source("R/tides.R")
#source("R/tides_multiple.R")
source("R/plot_tides.R")

# ui
ui <- fluidPage(
  titlePanel("TIDES: Truncation-Induced Dependency in Summary Statistics"),
  tags$h3("A method for checking the compatibility of reported means, SDs, and Ns given the min and max of the scale"),
  tags$h4("Hussey, Norwood, Cummins, Arslan, & Elson (2024)"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "mean", label = "Mean:", value = 2.1, step = 0.1),
      numericInput(inputId = "sd", label = "Standard Deviation:", value = 2.4, step = 0.1, min = 0),
      numericInput(inputId = "n", label = "Sample Size:", value = 30, step = 1, min = 2),
      numericInput(inputId = "min", label = "Minimum Value:", value = 1, step = 1),
      numericInput(inputId = "max", label = "Maximum Value:", value = 7, step = 1),
      actionButton(inputId = "run_test", label = "Run Test")
    ),
    mainPanel(
      plotOutput("result_plot")
      #tableOutput("result_table")
    )
  )
)

# server
server <- function(input, output) {
  # result <- eventReactive(input$run_test, {
  #   truncation_induced_dependency_test(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
  # })
  # 
  # output$result_table <- renderTable({
  #   result()
  # })
  
  output$result_plot <- renderPlot({
    plot_tides(mean = input$mean, sd = input$sd, n = input$n, min = input$min, max = input$max)
  })
}

# app
shinyApp(ui = ui, server = server)

