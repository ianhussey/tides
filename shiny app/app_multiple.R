
# dependencies
library(shiny)
library(TIDES)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(janitor)

# Round down and up functions
round_down <- function(x, digits = 3) {
  factor <- 10 ^ digits
  floor(x * factor) / factor
}

round_up <- function(x, digits = 3) {
  factor <- 10 ^ digits
  ceiling(x * factor) / factor
}


data_example <- 
  tibble(
    id   = c(1,2,3,4,5,6,7),
    mean = c(1.1, 1.2, 1.4, 5.6, 3.4, 6.6, 3.8),
    sd   = c(0.5, 0.5, 0.6, 0.1, 2.4, 2.0, 0.8),
    n    = c(12, 30, 350, 13, 62, 55, 48),
    min  = 1,
    max  = c(7, 5, 7, 63, 7, 8, 5)
  )

# ui
ui <- navbarPage(
  title = "TIDES",
  tabPanel("Mean scored", 
           fluidPage(
             tags$head(
               tags$style(HTML("
                  body {
                    background-image: url('white_wave.png');
                    background-repeat: repeat;
                  }
                  h2 {
                    color: #123095;
                  }
                "))
             ),
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
  ),
  tabPanel("Second Panel", # This is the new second tab with simple content
           fluidPage(
             tags$h2("Welcome to the Second Tab"),
             tags$p("This is a placeholder for additional content that may be included in the app.")
           )
  ),
  tabPanel("Upload and Analyze",
           fluidPage(
             titlePanel("Analyse multiple sets of summary statistics"),
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 downloadButton("downloadResults", "Download TIDES Results"),
                 tags$hr(),
                 downloadButton("downloadData", "Download Example Correctly Formatted CSV")
               ),
               mainPanel(
                 tableOutput("resultsTable")
               )
             )
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
  
  # Reactive expression for reading uploaded CSV
  data_input <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  # Reactive expression for TIDES analysis results
  tides_results <- reactive({
    req(data_input())  # ensure the data is available
    df <- data_input() |>
      janitor::clean_names()
    
    # Check if the necessary columns are present in the dataframe
    if (!all(c("mean", "sd", "n", "min", "max") %in% names(df))) {
      stop("CSV must contain the columns 'mean', 'sd', 'n', 'min', and 'max'")
    }
    
    TIDES::tides_multiple(mean = df$mean,
                          sd = df$sd,
                          n = df$n,
                          min = df$min,
                          max = df$max,
                          calculate_min_sd = TRUE) %>%
      select(mean, sd, n, min, max, min_sd, max_sd, result) %>%
      mutate(min_sd = round_down(min_sd),
             max_sd = round_up(max_sd))
  })
  
  # Render the results table
  output$resultsTable <- renderTable({
    req(tides_results())
    tides_results()
  })
  
  # Download handlers
  output$downloadData <- downloadHandler(
    filename = function() {
      "example_data.csv"
    },
    content = function(file) {
      write.csv(data_example, file, row.names = FALSE)
    }
  )
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("tides_results_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(tides_results())
      write.csv(tides_results(), file, row.names = FALSE)
    }
  )
}

# app
shinyApp(ui = ui, server = server)

