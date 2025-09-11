# DEMATEL Sensitivity Analysis Shiny App - DEBUG VERSION
# File: app.R (with debugging enhancements)

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(viridis)
library(reshape2)

# Source all R functions
source("R/dematel_spectral.R", local = TRUE)
source("R/sensitivity-core.R", local = TRUE)
source("R/sensitivity-methods.R", local = TRUE)
source("R/sensitivity-visualization.R", local = TRUE)
source("R/ui_components.R", local = TRUE)

# Define UI (Add debug tab)
ui <- dashboardPage(
  
  # Header
  dashboardHeader(
    title = "DEMATEL Sensitivity Analysis",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("📊 Data Input", tabName = "input", icon = icon("upload")),
      menuItem("📈 Spectral Analysis", tabName = "spectral", icon = icon("chart-line")),
      menuItem("🔍 Sensitivity Analysis", tabName = "sensitivity", icon = icon("search")),
      menuItem("🐛 DEBUG DATA", tabName = "debug", icon = icon("bug")),  # NEW DEBUG TAB
      menuItem("🎯 Critical Relationships", tabName = "critical", icon = icon("bullseye")),
      menuItem("💡 Intervention Analysis", tabName = "intervention", icon = icon("lightbulb")),
      menuItem("📋 Comprehensive Report", tabName = "report", icon = icon("file-alt")),
      menuItem("❓ Help & Examples", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # Body (Add debug tab content)
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      # Data Input Tab (unchanged)
      tabItem(
        tabName = "input",
        fluidRow(
          box(
            title = "📁 Upload Your DEMATEL Matrix", 
            status = "primary", 
            solidHeader = TRUE,
            width = 4,
            
            h4("Step 1: Choose Your Input Method"),
            
            radioButtons(
              "input_method",
              "Select input method:",
              choices = list(
                "Upload CSV file with original direct influence matrix (A)" = "upload_A",
                "Use example dataset" = "example"
              ),
              selected = "example"
            ),
            
            conditionalPanel(
              condition = "input.input_method == 'upload_A'",
              h4("Upload Original Matrix A"),
              fileInput(
                "file_A",
                "Choose CSV File (Original Direct Influence Matrix A):",
                accept = c(".csv")
              ),
              
              checkboxInput("header_A", "File has header", value = FALSE),
              
              helpText("The original direct influence matrix should contain the raw expert judgments (typically 0-4 scale). 
                       Diagonal elements should be zero (no self-influence).")
            ),
            
            conditionalPanel(
              condition = "input.input_method == 'example'",
              h4("📋 Using Example Dataset"),
              p("A 5×5 example matrix will be loaded automatically.", 
                style = "color: #666; font-style: italic;")
            ),
            
            br(),
            
            conditionalPanel(
              condition = "input.input_method == 'upload_A'",
              h4("Step 2: Optional Settings"),
              textAreaInput(
                "factor_names_input",
                "Factor Names (optional, comma-separated):",
                placeholder = "e.g., Leadership, Communication, Risk Management, Innovation, Quality",
                height = "60px"
              ),
              
              helpText("Leave empty to use default names (F1, F2, F3, ...)")
            ),
            
            h4("Step 3: Process Matrix"),
            actionButton(
              "process_matrix",
              "Process Matrix & Start Analysis",
              class = "btn-primary btn-lg",
              style = "margin-top: 10px;"
            )
          ),
          
          box(
            title = "📊 Matrix Preview", 
            status = "info", 
            solidHeader = TRUE,
            width = 8,
            
            conditionalPanel(
              condition = "output.matrix_processed",
              h5("Original Matrix A:"),
              DT::dataTableOutput("matrix_preview"),
              br(),
              h5("Matrix Information:"),
              verbatimTextOutput("matrix_info")
            ),
            
            conditionalPanel(
              condition = "!output.matrix_processed",
              div(
                style = "text-align: center; padding: 50px;",
                icon("upload", style = "font-size: 48px; color: #ccc;"),
                h4("No matrix loaded", style = "color: #999; margin-top: 20px;"),
                p("Upload a matrix or use example data to begin analysis.")
              )
            )
          )
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "output.matrix_processed",
            box(
              title = "✅ Processing Status", 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              
              div(id = "processing_status")
            )
          )
        )
      ),
      
      # NEW DEBUG TAB
      tabItem(
        tabName = "debug",
        conditionalPanel(
          condition = "!output.matrix_processed",
          no_data_message("Please upload and process a matrix first in the Data Input tab.")
        ),
        
        conditionalPanel(
          condition = "output.matrix_processed",
          fluidRow(
            box(
              title = "🐛 DEBUG: Input Matrices", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              
              h4("Original Matrix A:"),
              DT::dataTableOutput("debug_matrix_A"),
              
              br(),
              h4("Normalized Matrix D:"),
              DT::dataTableOutput("debug_matrix_D"),
              
              br(),
              h4("Total Relations Matrix T:"),
              DT::dataTableOutput("debug_matrix_T")
            ),
            
            box(
              title = "🐛 DEBUG: Scaling and Eigenvalues", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              
              h4("Scaling Information:"),
              verbatimTextOutput("debug_scaling_info"),
              
              br(),
              h4("Eigenvalue Information:"),
              verbatimTextOutput("debug_eigenvalue_info"),
              
              br(),
              h4("Assumptions Check:"),
              verbatimTextOutput("debug_assumptions")
            )
          ),
          
          conditionalPanel(
            condition = "output.sensitivity_computed",
            fluidRow(
              box(
                title = "🐛 DEBUG: Sensitivity Matrix Raw Data", 
                status = "danger", 
                solidHeader = TRUE,
                width = 12,
                
                h4("Raw Sensitivity Matrix:"),
                DT::dataTableOutput("debug_sensitivity_matrix"),
                
                br(),
                h4("Sensitivity Matrix Statistics:"),
                verbatimTextOutput("debug_sensitivity_stats"),
                
                br(),
                h4("Sensitivity Object Structure:"),
                verbatimTextOutput("debug_sensitivity_object")
              )
            )
          )
        )
      ),
      
      # Spectral Analysis Tab (unchanged)
      tabItem(
        tabName = "spectral",
        conditionalPanel(
          condition = "!output.matrix_processed",
          no_data_message("Please upload and process a matrix first in the Data Input tab.")
        ),
        
        conditionalPanel(
          condition = "output.matrix_processed",
          fluidRow(
            box(
              title = "📈 Spectral Analysis Results", 
              status = "primary", 
              solidHeader = TRUE,
              width = 8,
              
              h4("System Eigenvalue Analysis"),
              verbatimTextOutput("spectral_results"),
              
              br(),
              h4("Interpretation Guide:"),
              tags$ul(
                tags$li(strong("Dominant Eigenvalue (λmax):"), " Controls system amplification"),
                tags$li(strong("Spectral Radius:"), " Maximum absolute eigenvalue"),
                tags$li(strong("Condition Number:"), " System sensitivity to perturbations"),
                tags$li(strong("Convergence Rate:"), " How quickly the system stabilizes")
              )
            ),
            
            box(
              title = "📊 Key Metrics Summary", 
              status = "info", 
              solidHeader = TRUE,
              width = 4,
              
              h4("Quick Overview:"),
              tableOutput("spectral_summary_table"),
              
              br(),
              downloadButton(
                "download_spectral",
                "📥 Download Spectral Results",
                class = "btn-info"
              )
            )
          )
        )
      ),
      
      # Sensitivity Analysis Tab (unchanged)
      tabItem(
        tabName = "sensitivity",
        conditionalPanel(
          condition = "!output.matrix_processed",
          no_data_message("Please upload and process a matrix first in the Data Input tab.")
        ),
        
        conditionalPanel(
          condition = "output.matrix_processed",
          fluidRow(
            box(
              title = "⚙️ Sensitivity Analysis Settings", 
              status = "primary", 
              solidHeader = TRUE,
              width = 4,
              
              h4("Analysis Parameters:"),
              h4("Computation Method: Analytical"),
              p("Using eigenvalue perturbation theory for precise sensitivity calculation.", 
                style = "color: #666; font-style: italic;"),
              
              br(),
              actionButton(
                "run_sensitivity",
                "🔍 Run Sensitivity Analysis",
                class = "btn-success btn-lg"
              ),
              
              br(), br(),
              
              conditionalPanel(
                condition = "output.sensitivity_computed",
                h4("Visualization Options:"),
                
                checkboxInput("show_heatmap_values", "Show values on heatmaps", value = TRUE),
                
                sliderInput(
                  "critical_threshold",
                  "Critical relationships threshold (percentile):",
                  min = 0,
                  max = 100,
                  value = 90,
                  step = 5
                )
              )
            ),
            
            box(
              title = "📊 Sensitivity Statistics", 
              status = "info", 
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = "!output.sensitivity_computed",
                div(
                  style = "text-align: center; padding: 50px;",
                  icon("cog", style = "font-size: 48px; color: #ccc;"),
                  h4("Sensitivity analysis not computed", style = "color: #999; margin-top: 20px;"),
                  p("Click 'Run Sensitivity Analysis' to begin computation.")
                )
              ),
              
              conditionalPanel(
                condition = "output.sensitivity_computed",
                h4("Statistical Summary:"),
                verbatimTextOutput("sensitivity_stats"),
                
                br(),
                h4("Relationship Classification:"),
                plotOutput("sensitivity_classification_plot", height = "200px")
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.sensitivity_computed",
            fluidRow(
              box(
                title = "🌡️ Sensitivity Heatmap", 
                status = "warning", 
                solidHeader = TRUE,
                width = 6,
                
                h4("Sensitivity Values Heatmap:"),
                plotOutput("sensitivity_heatmap", height = "400px"),
                
                br(),
                h4("Sensitivity Value Distribution:"),
                plotOutput("sensitivity_distribution", height = "300px")
              ),
              
              box(
                title = "📊 DEMATEL Classical Analysis", 
                status = "warning", 
                solidHeader = TRUE,
                width = 6,
                
                h4("Classical Interrelationship Map:"),
                plotOutput("interrelationship_map", height = "400px"),
                
                br(),
                h4("Top Critical Relationships:"),
                plotOutput("top_relationships_plot", height = "300px")
              )
            )
          )
        )
      ),
      
      # Critical Relationships Tab (unchanged)
      tabItem(
        tabName = "critical",
        conditionalPanel(
          condition = "!output.sensitivity_computed",
          no_data_message("Please complete sensitivity analysis first.")
        ),
        
        conditionalPanel(
          condition = "output.sensitivity_computed",
          fluidRow(
            box(
              title = "🎯 Critical Relationships Analysis", 
              status = "danger", 
              solidHeader = TRUE,
              width = 12,
              
              h4("Interactive Critical Relationships Table:"),
              p("These relationships have the highest impact on the system's dominant eigenvalue."),
              
              DT::dataTableOutput("critical_relationships_table"),
              
              br(),
              
              fluidRow(
                column(
                  4,
                  downloadButton(
                    "download_critical",
                    "📥 Download Critical Relationships",
                    class = "btn-danger"
                  )
                ),
                column(
                  8,
                  div(
                    style = "text-align: right;",
                    h5("Legend:"),
                    span("🔴 Amplifying: ", style = "color: #E31A1C;"),
                    span("Increases λmax | "),
                    span("🔵 Dampening: ", style = "color: #1F78B4;"),
                    span("Decreases λmax")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Other tabs remain unchanged...
      tabItem(tabName = "intervention", p("Intervention tab content...")),
      tabItem(tabName = "report", p("Report tab content...")),
      tabItem(tabName = "help", p("Help tab content..."))
    )
  )
)

# Define Server Logic (Enhanced with debugging)
server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    matrix_A = NULL,
    spectral_results = NULL,
    sensitivity_results = NULL,
    intervention_results = NULL,
    matrix_processed = FALSE,
    sensitivity_computed = FALSE,
    intervention_computed = FALSE,
    factor_names = NULL,
    # NEW DEBUG VALUES
    debug_D = NULL,
    debug_T = NULL,
    debug_scaling_factor = NULL,
    debug_eigenvalues = NULL,
    debug_assumptions = NULL
  )
  
  # Matrix processing (Enhanced with debug data storage)
  observeEvent(input$process_matrix, {
    req(input$input_method)
    
    tryCatch({
      if (input$input_method == "example") {
        # Parse the actual example data from CSV format
        example_text <- "0,3,3,1,3\n0,0,2,0,1\n0,0,0,2,0\n3,1,2,0,3\n4,1,2,1,0"
        A <- as.matrix(read.csv(textConnection(example_text), header = FALSE))
        
        factor_names <- c("Leadership", "Communication", "Innovation", "Risk_Management", "Quality")
        rownames(A) <- colnames(A) <- factor_names
        
        values$matrix_A <- A
        values$factor_names <- factor_names
        
        
      } else if (input$input_method == "upload_A") {
        req(input$file_A)
        
        # Read uploaded file
        A_raw <- read.csv(input$file_A$datapath, header = input$header_A, stringsAsFactors = FALSE)
        
        # Convert to matrix
        A <- as.matrix(A_raw)
        mode(A) <- "numeric"
        
        # Validate matrix
        if (nrow(A) != ncol(A)) {
          stop("Matrix must be square")
        }
        
        if (any(is.na(A)) || any(!is.finite(A))) {
          stop("Matrix must contain only finite numeric values")
        }
        
        # Handle factor names
        if (input$factor_names_input != "") {
          factor_names <- trimws(strsplit(input$factor_names_input, ",")[[1]])
          if (length(factor_names) != nrow(A)) {
            stop(paste("Number of factor names (", length(factor_names), ") must equal matrix size (", nrow(A), ")"))
          }
        } else {
          factor_names <- paste0("F", 1:nrow(A))
        }
        
        rownames(A) <- colnames(A) <- factor_names
        
        values$matrix_A <- A
        values$factor_names <- factor_names
      }
      
      # Compute DEMATEL matrices with DEBUG information
      cat("DEBUG: Computing DEMATEL matrices...\n")
      dematel_matrices <- compute_dematel_matrices(values$matrix_A)
      
      # DEBUG: Store intermediate results
      values$debug_D <- dematel_matrices$D
      values$debug_T <- dematel_matrices$T
      
      # Calculate scaling factor
      row_sums <- rowSums(values$matrix_A)
      col_sums <- colSums(values$matrix_A)
      s <- max(max(row_sums), max(col_sums))
      values$debug_scaling_factor <- s
      
      cat("DEBUG: Scaling factor s =", s, "\n")
      cat("DEBUG: D matrix computed:\n")
      print(dematel_matrices$D)
      cat("DEBUG: T matrix computed:\n")
      print(dematel_matrices$T)
      cat("DEBUG: Lambda max =", dematel_matrices$lambda_max, "\n")
      
      # Store eigenvalue information
      eigenvals <- eigen(dematel_matrices$T, only.values = TRUE)$values
      values$debug_eigenvalues <- eigenvals
      
      cat("DEBUG: All eigenvalues:\n")
      print(eigenvals)
      
      spectral_results <- list(
        lambda_max = dematel_matrices$lambda_max,
        D_matrix = dematel_matrices$D,
        T_matrix = dematel_matrices$T,
        A_matrix = values$matrix_A,
        factor_names = values$factor_names,
        n = nrow(values$matrix_A)
      )
      
      # Add these debug lines:
      cat("DEBUG: dematel_matrices$lambda_max:", dematel_matrices$lambda_max, "\n")
      cat("DEBUG: spectral_results$lambda_max:", spectral_results$lambda_max, "\n")
      cat("DEBUG: T matrix dimensions:", dim(dematel_matrices$T), "\n")
      cat("DEBUG: T matrix class:", class(dematel_matrices$T), "\n")
      
      # Add more spectral analysis if available
      tryCatch({
      if (exists("dematel_spectral_analysis", mode = "function")) {
        spectral_detailed <- dematel_spectral_analysis(
          dematel_matrices$D, 
          dematel_matrices$T, 
          verbose = FALSE
        )
        spectral_results <- c(spectral_results, spectral_detailed)
        cat("DEBUG: spectral_detailed computed successfully\n")
      } else {
        cat("DEBUG: dematel_spectral_analysis function not found\n")
      }
      }, error = function(e) {
        cat("DEBUG: ERROR in spectral_detailed computation:", e$message, "\n")
      })
      
      values$spectral_results <- spectral_results
      values$matrix_processed <- TRUE
      
      showNotification("✅ Matrix processed successfully!", type = "message")
      
    }, error = function(e) {
      cat("ERROR in matrix processing:", e$message, "\n")
      showNotification(paste("❌ Error processing matrix:", e$message), type = "error")
    })
  })
  
  # Sensitivity analysis (Enhanced with DEBUG information)
  observeEvent(input$run_sensitivity, {
    req(values$matrix_A)
    
    withProgress(message = "Computing sensitivity analysis...", {
      tryCatch({
        cat("DEBUG: Starting sensitivity analysis...\n")
        cat("DEBUG: Input matrix A:\n")
        print(values$matrix_A)
        
        # Create sensitivity object
        sens_obj <- DEMATEL_Sensitivity(values$matrix_A, values$factor_names)
        
        cat("DEBUG: DEMATEL_Sensitivity object created\n")
        cat("DEBUG: sens_obj$A:\n")
        print(sens_obj$A)
        cat("DEBUG: sens_obj$D:\n")
        print(sens_obj$D)
        cat("DEBUG: sens_obj$T:\n")
        print(sens_obj$T)
        cat("DEBUG: sens_obj$lambda_max:", sens_obj$lambda_max, "\n")
        
        # Compute sensitivity
        cat("DEBUG: Calling compute_sensitivity_analytical...\n")
        sens_obj <- compute_sensitivity_analytical(sens_obj)
        
        cat("DEBUG: Sensitivity computation completed\n")
        cat("DEBUG: Computation method:", sens_obj$computation_method, "\n")
        
        if (!is.null(sens_obj$sensitivity_matrix)) {
          cat("DEBUG: Sensitivity matrix dimensions:", dim(sens_obj$sensitivity_matrix), "\n")
          cat("DEBUG: Sensitivity matrix:\n")
          print(sens_obj$sensitivity_matrix)
          cat("DEBUG: Sensitivity matrix range:", range(sens_obj$sensitivity_matrix, na.rm = TRUE), "\n")
          cat("DEBUG: Number of NA values:", sum(is.na(sens_obj$sensitivity_matrix)), "\n")
        } else {
          cat("DEBUG: ERROR - Sensitivity matrix is NULL!\n")
        }
        
        # Store assumptions check if available
        if (!is.null(sens_obj$assumptions_check)) {
          values$debug_assumptions <- sens_obj$assumptions_check
          cat("DEBUG: Assumptions check:\n")
          print(sens_obj$assumptions_check)
        }
        
        values$sensitivity_results <- sens_obj
        values$sensitivity_computed <- TRUE
        
        showNotification("✅ Sensitivity analysis completed!", type = "message")
        
      }, error = function(e) {
        cat("ERROR in sensitivity analysis:", e$message, "\n")
        cat("ERROR traceback:\n")
        traceback()
        showNotification(paste("❌ Error in sensitivity analysis:", e$message), type = "error")
      })
    })
  })
  
  # DEBUG OUTPUTS
  
  # Output: Matrix processed flag
  output$matrix_processed <- reactive({
    values$matrix_processed
  })
  outputOptions(output, "matrix_processed", suspendWhenHidden = FALSE)
  
  # Output: Sensitivity computed flag
  output$sensitivity_computed <- reactive({
    values$sensitivity_computed
  })
  outputOptions(output, "sensitivity_computed", suspendWhenHidden = FALSE)
  
  # DEBUG: Original matrix A
  output$debug_matrix_A <- DT::renderDataTable({
    req(values$matrix_A)
    DT::datatable(
      values$matrix_A,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      )
    ) %>% 
      DT::formatRound(columns = 1:ncol(values$matrix_A), digits = 3)
  })
  
  # DEBUG: Normalized matrix D
  output$debug_matrix_D <- DT::renderDataTable({
    req(values$debug_D)
    DT::datatable(
      values$debug_D,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      )
    ) %>% 
      DT::formatRound(columns = 1:ncol(values$debug_D), digits = 6)
  })
  
  # DEBUG: Total relations matrix T
  output$debug_matrix_T <- DT::renderDataTable({
    req(values$debug_T)
    DT::datatable(
      values$debug_T,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      )
    ) %>% 
      DT::formatRound(columns = 1:ncol(values$debug_T), digits = 6)
  })
  
  # DEBUG: Scaling information
  output$debug_scaling_info <- renderText({
    req(values$matrix_A, values$debug_scaling_factor)
    
    row_sums <- rowSums(values$matrix_A)
    col_sums <- colSums(values$matrix_A)
    
    paste(
      "Original Matrix A Properties:\n",
      "Row sums:", paste(round(row_sums, 3), collapse = ", "), "\n",
      "Column sums:", paste(round(col_sums, 3), collapse = ", "), "\n",
      "Max row sum:", round(max(row_sums), 3), "\n",
      "Max column sum:", round(max(col_sums), 3), "\n",
      "Scaling factor (s):", round(values$debug_scaling_factor, 6), "\n",
      "\nNormalized Matrix D Properties:\n",
      "D = A / s\n",
      "Max row sum in D:", ifelse(!is.null(values$debug_D), round(max(rowSums(values$debug_D)), 6), "N/A"), "\n",
      "Max column sum in D:", ifelse(!is.null(values$debug_D), round(max(colSums(values$debug_D)), 6), "N/A")
    )
  })
  
  # DEBUG: Eigenvalue information
  output$debug_eigenvalue_info <- renderText({
    req(values$debug_eigenvalues)
    
    eigenvals_real <- Re(values$debug_eigenvalues)
    eigenvals_sorted <- sort(eigenvals_real, decreasing = TRUE)
    
    paste(
      "All Eigenvalues (real parts):\n",
      paste(round(eigenvals_real, 6), collapse = ", "), "\n",
      "\nSorted Eigenvalues:\n",
      paste(round(eigenvals_sorted, 6), collapse = ", "), "\n",
      "\nDominant eigenvalue (λmax):", round(eigenvals_sorted[1], 6), "\n",
      "Second largest eigenvalue:", ifelse(length(eigenvals_sorted) > 1, round(eigenvals_sorted[2], 6), "N/A"), "\n",
      "Spectral radius:", round(max(abs(values$debug_eigenvalues)), 6), "\n",
      "Number of eigenvalues:", length(values$debug_eigenvalues)
    )
  })
  
  # DEBUG: Assumptions check
  output$debug_assumptions <- renderText({
    if (!is.null(values$debug_assumptions)) {
      paste(
        "Theorem 1 Assumptions Check:\n",
        "Valid:", values$debug_assumptions$valid, "\n",
        "Message:", values$debug_assumptions$message, "\n",
        "Dominant is simple:", values$debug_assumptions$dominant_is_simple, "\n",
        "Matrix is irreducible:", values$debug_assumptions$matrix_is_irreducible, "\n",
        "Well conditioned:", values$debug_assumptions$well_conditioned, "\n",
        "Eigenvalue gaps:", ifelse(!is.null(values$debug_assumptions$eigenvalue_gaps), 
                                   round(values$debug_assumptions$eigenvalue_gaps, 6), "N/A"), "\n",
        "Condition number:", ifelse(!is.null(values$debug_assumptions$condition_number), 
                                    round(values$debug_assumptions$condition_number, 2), "N/A")
      )
    } else {
      "Assumptions check not available"
    }
  })
  
  # DEBUG: Sensitivity matrix
  output$debug_sensitivity_matrix <- DT::renderDataTable({
    req(values$sensitivity_results, values$sensitivity_results$sensitivity_matrix)
    
    sens_matrix <- values$sensitivity_results$sensitivity_matrix
    
    DT::datatable(
      sens_matrix,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      )
    ) %>% 
      DT::formatRound(columns = 1:ncol(sens_matrix), digits = 8)
  })
  
  # DEBUG: Sensitivity statistics
  output$debug_sensitivity_stats <- renderText({
    req(values$sensitivity_results, values$sensitivity_results$sensitivity_matrix)
    
    sens_matrix <- values$sensitivity_results$sensitivity_matrix
    sens_values <- as.vector(sens_matrix)
    sens_values_clean <- sens_values[!is.na(sens_values)]
    
    paste(
      "Sensitivity Matrix Debug Info:\n",
      "Dimensions:", paste(dim(sens_matrix), collapse = " x "), "\n",
      "Total elements:", length(sens_values), "\n",
      "NA elements:", sum(is.na(sens_values)), "\n",
      "Finite elements:", sum(is.finite(sens_values), na.rm = TRUE), "\n",
      "Range (all values):", paste(round(range(sens_values, na.rm = TRUE), 8), collapse = " to "), "\n",
      "Range (finite only):", paste(round(range(sens_values_clean[is.finite(sens_values_clean)]), 8), collapse = " to "), "\n",
      "Mean:", round(mean(sens_values_clean), 8), "\n",
      "Std Dev:", round(sd(sens_values_clean), 8), "\n",
      "Positive values:", sum(sens_values_clean > 0), "\n",
      "Negative values:", sum(sens_values_clean < 0), "\n",
      "Zero values:", sum(abs(sens_values_clean) < 1e-10), "\n",
      "Max absolute:", round(max(abs(sens_values_clean)), 8)
    )
  })
  
  # DEBUG: Sensitivity object structure
  output$debug_sensitivity_object <- renderText({
    req(values$sensitivity_results)
    
    obj <- values$sensitivity_results
    
    paste(
      "Sensitivity Object Structure:\n",
      "Class:", paste(class(obj), collapse = ", "), "\n",
      "Names:", paste(names(obj), collapse = ", "), "\n",
      "n:", obj$n, "\n",
      "Factor names:", paste(obj$factor_names, collapse = ", "), "\n",
      "Lambda max:", round(obj$lambda_max, 8), "\n",
      "Computation method:", obj$computation_method %||% "NULL", "\n",
      "Sensitivity matrix class:", class(obj$sensitivity_matrix), "\n",
      "Sensitivity matrix is null:", is.null(obj$sensitivity_matrix), "\n",
      "A matrix dimensions:", paste(dim(obj$A), collapse = " x "), "\n",
      "D matrix dimensions:", paste(dim(obj$D), collapse = " x "), "\n",
      "T matrix dimensions:", paste(dim(obj$T), collapse = " x ")
    )
  })
  
  # Regular outputs (unchanged)
  output$matrix_preview <- DT::renderDataTable({
    req(values$matrix_A)
    DT::datatable(
      values$matrix_A,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      )
    ) %>% 
      DT::formatRound(columns = 1:ncol(values$matrix_A), digits = 2)
  })
  
  # Output: Matrix info
  output$matrix_info <- renderText({
    req(values$matrix_A)
    paste(
      "Matrix size:", nrow(values$matrix_A), "×", ncol(values$matrix_A), "\n",
      "Factors:", paste(values$factor_names, collapse = ", "), "\n",
      "Value range: [", round(min(values$matrix_A), 2), ",", round(max(values$matrix_A), 2), "]", "\n",
      "Diagonal sum:", sum(diag(values$matrix_A)), "(should be 0)"
    )
  })
  
  # Output: Spectral results
  output$spectral_results <- renderText({
    req(values$spectral_results)
    
    result_text <- paste(
      "Dominant Eigenvalue (λmax):", round(values$spectral_results$lambda_max, 6), "\n"
    )
    
    if (!is.null(values$spectral_results$spectral_radius)) {
      result_text <- paste(result_text, 
                           "Spectral Radius:", round(values$spectral_results$spectral_radius, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$condition_number)) {
      result_text <- paste(result_text,
                           "Condition Number:", round(values$spectral_results$condition_number, 2), "\n")
    }
    
    # Add interpretation
    lambda_max <- values$spectral_results$lambda_max
    if (lambda_max > 1) {
      result_text <- paste(result_text, "\n⚠️  System shows potential for influence amplification (λmax > 1)")
    } else {
      result_text <- paste(result_text, "\n✅ System is stable with bounded influence (λmax ≤ 1)")
    }
    
    return(result_text)
  })
  
  # Output: Spectral summary table
  output$spectral_summary_table <- renderTable({
    req(values$spectral_results)
    
    summary_data <- data.frame(
      Metric = c("Dominant Eigenvalue", "System Size", "Non-zero Elements"),
      Value = c(
        round(values$spectral_results$lambda_max, 4),
        paste0(values$spectral_results$n, " × ", values$spectral_results$n),
        sum(values$matrix_A != 0)
      )
    )
    
    return(summary_data)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Output: Sensitivity statistics
  output$sensitivity_stats <- renderText({
    req(values$sensitivity_results)
    
    # DEBUG: Add debug output here too
    cat("DEBUG in sensitivity_stats: sensitivity_results exists\n")
    cat("DEBUG: sensitivity_matrix is null?", is.null(values$sensitivity_results$sensitivity_matrix), "\n")
    
    if (is.null(values$sensitivity_results$sensitivity_matrix)) {
      return("ERROR: Sensitivity matrix is NULL")
    }
    
    tryCatch({
      stats <- get_sensitivity_stats(values$sensitivity_results)
      
      paste(
        "Computation Method:", values$sensitivity_results$computation_method, "\n",
        "Mean Sensitivity:", round(stats$mean, 6), "\n",
        "Standard Deviation:", round(stats$sd, 6), "\n",
        "Range: [", round(stats$min, 6), ",", round(stats$max, 6), "]\n",
        "Mean Absolute Sensitivity:", round(stats$mean_abs, 6), "\n",
        "Total Relationships:", stats$total_elements, "\n",
        "Amplifying (positive):", stats$n_positive, "(", round(100*stats$n_positive/stats$total_elements, 1), "%)\n",
        "Dampening (negative):", stats$n_negative, "(", round(100*stats$n_negative/stats$total_elements, 1), "%)\n",
        "Near-zero:", stats$n_zero, "(", round(100*stats$n_zero/stats$total_elements, 1), "%)"
      )
    }, error = function(e) {
      cat("ERROR in sensitivity_stats:", e$message, "\n")
      paste("ERROR computing sensitivity statistics:", e$message)
    })
  })
  
  # Output: Sensitivity classification plot
  output$sensitivity_classification_plot <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      stats <- get_sensitivity_stats(values$sensitivity_results)
      
      classification_data <- data.frame(
        Type = c("Amplifying", "Dampening", "Near-zero"),
        Count = c(stats$n_positive, stats$n_negative, stats$n_zero),
        Percentage = c(
          round(100*stats$n_positive/stats$total_elements, 1),
          round(100*stats$n_negative/stats$total_elements, 1),
          round(100*stats$n_zero/stats$total_elements, 1)
        )
      )
      
      ggplot(classification_data, aes(x = Type, y = Count, fill = Type)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = paste0(Count, "\n(", Percentage, "%)")), 
                  vjust = -0.5, fontface = "bold") +
        scale_fill_manual(values = c("Amplifying" = "#E31A1C", "Dampening" = "#1F78B4", "Near-zero" = "#33A02C")) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              plot.title = element_text(size = 14, face = "bold")) +
        labs(title = "Relationship Type Distribution",
             y = "Number of Relationships") +
        ylim(0, max(classification_data$Count) * 1.2)
    }, error = function(e) {
      cat("ERROR in sensitivity_classification_plot:", e$message, "\n")
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:", e$message), size = 6) +
        theme_void()
    })
  })
  
  # Output: Sensitivity heatmap
  output$sensitivity_heatmap <- renderPlot({
    req(values$sensitivity_results)
    
    # DEBUG: Add extensive debugging here
    cat("DEBUG in sensitivity_heatmap: Starting\n")
    cat("DEBUG: sensitivity_results exists:", !is.null(values$sensitivity_results), "\n")
    cat("DEBUG: sensitivity_matrix exists:", !is.null(values$sensitivity_results$sensitivity_matrix), "\n")
    
    if (is.null(values$sensitivity_results$sensitivity_matrix)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
               theme_void() +
               labs(title = "Sensitivity Heatmap - Matrix NULL"))
    }
    
    tryCatch({
      sens_matrix <- values$sensitivity_results$sensitivity_matrix
      cat("DEBUG: Sensitivity matrix dimensions:", dim(sens_matrix), "\n")
      cat("DEBUG: Sensitivity matrix class:", class(sens_matrix), "\n")
      cat("DEBUG: Has rownames:", !is.null(rownames(sens_matrix)), "\n")
      cat("DEBUG: Has colnames:", !is.null(colnames(sens_matrix)), "\n")
      
      # Print matrix values for debugging
      cat("DEBUG: Sensitivity matrix values:\n")
      print(sens_matrix)
      
      # Check for reshape2 package
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "reshape2 package not available", size = 6) +
                 theme_void())
      }
      
      sens_melted <- reshape2::melt(sens_matrix)
      cat("DEBUG: Melted data dimensions:", dim(sens_melted), "\n")
      cat("DEBUG: Melted data columns:", names(sens_melted), "\n")
      
      names(sens_melted) <- c("From_Factor", "To_Factor", "Sensitivity")
      sens_melted <- sens_melted[!is.na(sens_melted$Sensitivity), ]
      
      cat("DEBUG: After removing NA, dimensions:", dim(sens_melted), "\n")
      cat("DEBUG: Sensitivity range:", range(sens_melted$Sensitivity), "\n")
      
      if (nrow(sens_melted) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid sensitivity values", size = 6) +
                 theme_void())
      }
      
      p <- ggplot(sens_melted, aes(x = To_Factor, y = From_Factor, fill = Sensitivity)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                             midpoint = 0, name = "Sensitivity") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold"),
          legend.title = element_text(size = 12),
          panel.grid = element_blank()
        ) +
        labs(
          title = "Sensitivity Matrix: ∂λmax/∂aij",
          x = "To Factor (j)",
          y = "From Factor (i)"
        )
      
      if (input$show_heatmap_values && nrow(values$matrix_A) <= 10) {
        p <- p + geom_text(aes(label = round(Sensitivity, 3)), size = 3, color = "black")
      }
      
      cat("DEBUG: Heatmap created successfully\n")
      return(p)
      
    }, error = function(e) {
      cat("ERROR in sensitivity_heatmap:", e$message, "\n")
      traceback()
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Sensitivity heatmap\nerror:\n", e$message),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Sensitivity Heatmap - Error")
    })
  })
  
  # Output: DEMATEL Interrelationship Map
  output$interrelationship_map <- renderPlot({
    req(values$sensitivity_results)
    
    cat("DEBUG in interrelationship_map: Starting\n")
    
    tryCatch({
      create_dematel_interrelationship_map(values$sensitivity_results)
    }, error = function(e) {
      cat("ERROR in interrelationship_map:", e$message, "\n")
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Interrelationship map\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "DEMATEL Interrelationship Map - Error")
    })
  })
  
  # Output: Sensitivity distribution
  output$sensitivity_distribution <- renderPlot({
    req(values$sensitivity_results)
    
    cat("DEBUG in sensitivity_distribution: Starting\n")
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      sens_values <- as.vector(values$sensitivity_results$sensitivity_matrix)
      sens_values <- sens_values[!is.na(sens_values)]
      
      cat("DEBUG: Distribution - number of values:", length(sens_values), "\n")
      cat("DEBUG: Distribution - range:", range(sens_values), "\n")
      
      if (length(sens_values) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid sensitivity values", size = 6) +
                 theme_void())
      }
      
      ggplot(data.frame(Sensitivity = sens_values), aes(x = Sensitivity)) +
        geom_histogram(bins = 30, alpha = 0.7, fill = "steelblue", color = "white") +
        geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        labs(
          title = "Distribution of Sensitivity Values",
          subtitle = paste("Mean:", round(mean(sens_values), 4), "| SD:", round(sd(sens_values), 4)),
          x = "Sensitivity Value",
          y = "Frequency"
        )
    }, error = function(e) {
      cat("ERROR in sensitivity_distribution:", e$message, "\n")
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Distribution plot\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Sensitivity Distribution - Error")
    })
  })
  
  # Output: Top relationships plot
  output$top_relationships_plot <- renderPlot({
    req(values$sensitivity_results)
    
    cat("DEBUG in top_relationships_plot: Starting\n")
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                       threshold_percentile = input$critical_threshold)
      
      cat("DEBUG: Critical relationships found:", nrow(critical_rels), "\n")
      
      if (nrow(critical_rels) > 0) {
        top_10 <- head(critical_rels, 10)
        top_10$relationship <- paste0(top_10$from_factor, " → ", top_10$to_factor)
        top_10$relationship <- factor(top_10$relationship, levels = rev(top_10$relationship))
        
        ggplot(top_10, aes(x = relationship, y = sensitivity, fill = interpretation)) +
          geom_col(alpha = 0.8) +
          coord_flip() +
          scale_fill_manual(
            values = c("Amplifying" = "#E31A1C", "Dampening" = "#1F78B4"),
            name = "Effect Type"
          ) +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold")) +
          labs(
            title = paste("Top 10 Most Critical Relationships"),
            subtitle = paste(input$critical_threshold, "th percentile threshold"),
            x = "Relationship",
            y = "Sensitivity Value"
          ) +
          geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.3)
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No critical relationships\nfound at this threshold",
                   size = 6, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          labs(title = "Top Critical Relationships")
      }
    }, error = function(e) {
      cat("ERROR in top_relationships_plot:", e$message, "\n")
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Critical relationships plot\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Top Relationships - Error")
    })
  })
  
  # Output: Critical relationships table
  output$critical_relationships_table <- DT::renderDataTable({
    req(values$sensitivity_results)
    
    cat("DEBUG in critical_relationships_table: Starting\n")
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(DT::datatable(data.frame(Error = "Sensitivity matrix is NULL")))
      }
      
      critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                       threshold_percentile = input$critical_threshold)
      
      cat("DEBUG: Critical relationships table - found:", nrow(critical_rels), "relationships\n")
      
      if (nrow(critical_rels) > 0) {
        display_data <- critical_rels[, c("from_factor", "to_factor", "sensitivity", 
                                          "abs_sensitivity", "interpretation")]
        names(display_data) <- c("From Factor", "To Factor", "Sensitivity", 
                                 "Abs. Sensitivity", "Effect Type")
        
        DT::datatable(
          display_data,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            order = list(list(3, "desc"))  # Order by absolute sensitivity
          )
        ) %>%
          DT::formatRound(columns = c("Sensitivity", "Abs. Sensitivity"), digits = 6) %>%
          DT::formatStyle(
            "Effect Type",
            backgroundColor = DT::styleEqual(
              c("Amplifying", "Dampening"),
              c("#ffebee", "#e3f2fd")
            )
          )
      } else {
        DT::datatable(data.frame(Message = "No critical relationships found at this threshold"))
      }
    }, error = function(e) {
      cat("ERROR in critical_relationships_table:", e$message, "\n")
      DT::datatable(data.frame(Error = paste("Error generating table:", e$message)))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)