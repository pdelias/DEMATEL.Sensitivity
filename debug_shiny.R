# DEMATEL Sensitivity Analysis Shiny App - Basic Working Version
# File: app.R

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Try to source R files with error handling
source_files <- c(
  "R/dematel_spectral.R",
  "R/sensitivity-core.R",
  "R/sensitivity-methods.R",
  "R/sensitivity-visualization.R",
  "R/ui_components.R"
)

# Source files with error checking
for (file in source_files) {
  if (file.exists(file)) {
    tryCatch({
      source(file, local = TRUE)
      cat("✅ Successfully sourced:", file, "\n")
    }, error = function(e) {
      cat("❌ Error sourcing", file, ":", e$message, "\n")
      cat("Continuing without this file...\n")
    })
  } else {
    cat("⚠️ File not found:", file, "\n")
  }
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DEMATEL Sensitivity Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "input", icon = icon("upload")),
      menuItem("Spectral Analysis", tabName = "spectral", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            title = "Upload Matrix", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            radioButtons(
              "input_method",
              "Select input method:",
              choices = list(
                "Use example dataset" = "example",
                "Upload CSV file" = "upload"
              ),
              selected = "example"
            ),
            
            conditionalPanel(
              condition = "input.input_method == 'upload'",
              fileInput("file_A", "Choose CSV File:", accept = c(".csv"))
            ),
            
            actionButton("process_matrix", "Process Matrix", class = "btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "Matrix Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("matrix_preview")
          )
        )
      ),
      
      tabItem(
        tabName = "spectral",
        fluidRow(
          box(
            title = "Spectral Analysis Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            conditionalPanel(
              condition = "output.matrix_processed == false",
              div(
                style = "text-align: center; padding: 50px;",
                h4("Please process a matrix first in the Data Input tab.")
              )
            ),
            
            conditionalPanel(
              condition = "output.matrix_processed == true",
              verbatimTextOutput("spectral_results")
            )
          )
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    matrix_A = NULL,
    spectral_results = NULL,
    matrix_processed = FALSE
  )
  
  # Matrix processing
  observeEvent(input$process_matrix, {
    tryCatch({
      if (input$input_method == "example") {
        # Create example matrix
        A <- matrix(c(
          0, 3, 3, 1, 3,
          0, 0, 2, 0, 1,
          0, 0, 0, 2, 0,
          3, 1, 2, 0, 3,
          4, 1, 2, 1, 0
        ), nrow = 5, byrow = TRUE)
        
        factor_names <- c("Leadership", "Communication", "Innovation", "Risk_Management", "Quality")
        rownames(A) <- colnames(A) <- factor_names
        
        values$matrix_A <- A
        
      } else if (input$input_method == "upload") {
        req(input$file_A)
        A <- as.matrix(read.csv(input$file_A$datapath, header = FALSE))
        mode(A) <- "numeric"
        values$matrix_A <- A
      }
      
      # Try spectral analysis if function exists
      if (exists("dematel_spectral_analysis", mode = "function") && 
          exists("compute_dematel_matrices", mode = "function")) {
        
        dematel_matrices <- compute_dematel_matrices(values$matrix_A)
        spectral_results <- dematel_spectral_analysis(
          dematel_matrices$D, 
          dematel_matrices$T, 
          return_eigenvalues = TRUE,
          verbose = FALSE
        )
        values$spectral_results <- spectral_results
      } else {
        # Fallback: basic eigenvalue calculation
        eigenvals <- eigen(values$matrix_A)$values
        values$spectral_results <- list(
          lambda_max = max(Re(eigenvals)),
          spectral_radius = max(abs(eigenvals))
        )
      }
      
      values$matrix_processed <- TRUE
      showNotification("Matrix processed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Outputs
  output$matrix_processed <- reactive({
    values$matrix_processed
  })
  outputOptions(output, "matrix_processed", suspendWhenHidden = FALSE)
  
  output$matrix_preview <- DT::renderDataTable({
    req(values$matrix_A)
    DT::datatable(values$matrix_A, options = list(scrollX = TRUE))
  })
  
  output$spectral_results <- renderText({
    req(values$spectral_results)
    
    result_text <- ""
    
    if (!is.null(values$spectral_results$lambda_max)) {
      result_text <- paste(result_text, "Dominant Eigenvalue:", 
                           round(values$spectral_results$lambda_max, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$lambda_2)) {
      result_text <- paste(result_text, "Second Eigenvalue:", 
                           round(values$spectral_results$lambda_2, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$spectral_radius)) {
      result_text <- paste(result_text, "Spectral Radius:", 
                           round(values$spectral_results$spectral_radius, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$condition_number)) {
      result_text <- paste(result_text, "Condition Number:", 
                           round(values$spectral_results$condition_number, 2), "\n")
    }
    
    return(result_text)
  })
}

# Run the application
cat("Starting Shiny app...\n")
shinyApp(ui = ui, server = server)