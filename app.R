# DEMATEL Sensitivity Analysis Shiny App - Complete Version
# File: app.R

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(ggrepel)
library(viridis)
library(reshape2)

# Source all R functions with error handling
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
      # Continue running - some functionality may be limited
    })
  } else {
    cat("⚠️ File not found:", file, "\n")
  }
}

# Define UI
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
      menuItem("🎯 Critical Relationships", tabName = "critical", icon = icon("bullseye")),
      menuItem("💡 Intervention Analysis", tabName = "intervention", icon = icon("lightbulb")),
      menuItem("📋 Comprehensive Report", tabName = "report", icon = icon("file-alt")),
      menuItem("❓ Help & Examples", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      # Data Input Tab
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
              
              div(id = "processing_status", 
                  p("Matrix processed successfully! You can now proceed to the Spectral Analysis tab."))
            )
          )
        )
      ),
      
      # ENHANCED Spectral Analysis Tab
      tabItem(
        tabName = "spectral",
        conditionalPanel(
          condition = "!output.matrix_processed",
          fluidRow(
            box(
              title = "⚠️ Data Required", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "text-align: center; padding: 50px;",
                icon("exclamation-triangle", style = "font-size: 48px; color: #f39c12;"),
                h4("Please upload and process a matrix first in the Data Input tab.", 
                   style = "color: #856404; margin-top: 20px;"),
                p("Complete the previous steps to access this analysis.", style = "color: #856404;")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.matrix_processed",
          fluidRow(
            box(
              title = "📈 Complete Spectral Analysis Results", 
              status = "primary", 
              solidHeader = TRUE,
              width = 8,
              
              h4("System Eigenvalue Analysis"),
              DT::dataTableOutput("spectral_metrics_table"),
              
              br(),
              h4("Matrix Properties"),
              verbatimTextOutput("matrix_properties")
            ),
            
            box(
              title = "📊 Key System Metrics", 
              status = "info", 
              solidHeader = TRUE,
              width = 4,
              
              h4("Primary Metrics:"),
              tableOutput("primary_metrics_table"),
              
              br(),
              h4("System Characteristics:"),
              tableOutput("system_characteristics_table"),
              
              br(),
              downloadButton(
                "download_spectral",
                "📥 Download Complete Spectral Results",
                class = "btn-info"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "📊 Eigenvalue Analysis", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              
              h4("Eigenvalue Details:"),
              verbatimTextOutput("eigenvalue_details")
            ),
            
            box(
              title = "🔍 System Dynamics", 
              status = "warning", 
              solidHeader = TRUE,
              width = 6,
              
              h4("Dynamic Properties:"),
              verbatimTextOutput("system_dynamics")
            )
          )
        )
      ),
      
      # Sensitivity Analysis Tab
      tabItem(
        tabName = "sensitivity",
        conditionalPanel(
          condition = "!output.matrix_processed",
          fluidRow(
            box(
              title = "⚠️ Data Required", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "text-align: center; padding: 50px;",
                icon("exclamation-triangle", style = "font-size: 48px; color: #f39c12;"),
                h4("Please upload and process a matrix first in the Data Input tab.", 
                   style = "color: #856404; margin-top: 20px;")
              )
            )
          )
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
      
      # Critical Relationships Tab
      tabItem(
        tabName = "critical",
        conditionalPanel(
          condition = "!output.sensitivity_computed",
          fluidRow(
            box(
              title = "⚠️ Data Required", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "text-align: center; padding: 50px;",
                h4("Please complete sensitivity analysis first.")
              )
            )
          )
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
                    class = "btn-primary"
                  )
                ),
                column(
                  8,
                  div(
                    style = "text-align: right;",
                    h5("Legend:"),
                    span("🔴 Amplifying: ", style = "color: #9EDEC5;"),
                    span("Increases λmax | "),
                    span("🔵 Stabilizing: ", style = "color: #295073;"),
                    span("Decreases λmax")
                  )
                )
              )
            )
          )
        )
      ),
      
      # Intervention Analysis Tab
      tabItem(
        tabName = "intervention",
        conditionalPanel(
          condition = "!output.sensitivity_computed",
          fluidRow(
            box(
              title = "⚠️ Data Required", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "text-align: center; padding: 50px;",
                h4("Please complete sensitivity analysis first.")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.sensitivity_computed",
          fluidRow(
            box(
              title = "⚙️ Intervention Settings", 
              status = "primary", 
              solidHeader = TRUE,
              width = 4,
              
              h4("Target Change in λmax:"),
              numericInput(
                "target_lambda_change",
                "Desired change in dominant eigenvalue:",
                value = -0.1,
                step = 0.01
              ),
              
              helpText("Negative values reduce system amplification, positive values increase it."),
              
              br(),
              h4("Intervention Type:"),
              radioButtons(
                "intervention_type",
                "Select intervention approach:",
                choices = list(
                  "Discrete changes (±1 on DEMATEL scale)" = "discrete",
                  "Continuous changes (any value)" = "continuous"
                ),
                selected = "discrete"
              ),
              
              br(),
              actionButton(
                "run_intervention",
                "💡 Analyze Interventions",
                class = "btn-warning btn-lg"
              )
            ),
            
            box(
              title = "📊 Intervention Results", 
              status = "warning", 
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = "!output.intervention_computed",
                div(
                  style = "text-align: center; padding: 50px;",
                  icon("lightbulb", style = "font-size: 48px; color: #ccc;"),
                  h4("Intervention analysis not computed", style = "color: #999; margin-top: 20px;"),
                  p("Set target change and click 'Analyze Interventions' to begin.")
                )
              ),
              
              conditionalPanel(
                condition = "output.intervention_computed",
                h4("Top Intervention Options:"),
                DT::dataTableOutput("intervention_table"),
                
                br(),
                downloadButton(
                  "download_interventions",
                  "📥 Download Intervention Analysis",
                  class = "btn-warning"
                )
              )
            )
          )
        )
      ),
      
      # Report Tab
      tabItem(
        tabName = "report",
        conditionalPanel(
          condition = "!output.sensitivity_computed",
          fluidRow(
            box(
              title = "⚠️ Data Required", 
              status = "warning", 
              solidHeader = TRUE,
              width = 12,
              
              div(
                style = "text-align: center; padding: 50px;",
                h4("Please complete sensitivity analysis first.")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.sensitivity_computed",
          fluidRow(
            box(
              title = "📋 Comprehensive Report", 
              status = "success", 
              solidHeader = TRUE,
              width = 12,
              
              h4("Executive Summary:"),
              verbatimTextOutput("executive_summary"),
              
              br(),
              h4("Download Options:"),
              
              fluidRow(
                column(3,
                       downloadButton(
                         "download_full_report",
                         "📄 Full Report (TXT)",
                         class = "btn-success"
                       )
                ),
                column(3,
                       downloadButton(
                         "download_summary_report",
                         "📝 Summary Report (CSV)",
                         class = "btn-info"
                       )
                ),
                column(3,
                       downloadButton(
                         "download_spectral_data",
                         "📦 Spectral Data (CSV)",
                         class = "btn-primary"
                       )
                ),
                column(3,
                       downloadButton(
                         "download_sensitivity_data",
                         "🔍 Sensitivity Data (CSV)",
                         class = "btn-warning"
                       )
                )
              )
            )
          )
        )
      ),
      
      # Help Tab
      tabItem(
        tabName = "help",
        fluidRow(
          box(
            title = "❓ Help & Documentation", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            
            h4("Getting Started:"),
            tags$ol(
              tags$li("Upload your DEMATEL direct influence matrix (A) or use the example dataset"),
              tags$li("Process the matrix to compute DEMATEL basics (D, T matrices)"),
              tags$li("Run spectral analysis to understand system dynamics"),
              tags$li("Compute sensitivity analysis to find critical relationships"),
              tags$li("Identify intervention opportunities"),
              tags$li("Generate comprehensive reports")
            ),
            
            br(),
            h4("File Format Requirements:"),
            tags$ul(
              tags$li("CSV format with numeric values only"),
              tags$li("Square matrix (n × n)"),
              tags$li("Diagonal elements should be zero"),
              tags$li("Values typically on 0-4 scale"),
              tags$li("No missing values (NA)")
            )
          ),
          
          box(
            title = "📊 Example Dataset Information", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            
            h4("Organizational Effectiveness Model:"),
            p("The example dataset represents a 5-factor organizational system:"),
            
            tags$ul(
              tags$li(strong("Leadership: "), "Strategic direction and decision-making"),
              tags$li(strong("Communication: "), "Information flow and transparency"),
              tags$li(strong("Innovation: "), "Creativity and adaptation to change"),
              tags$li(strong("Risk Management: "), "Threat identification and mitigation"),
              tags$li(strong("Quality: "), "Standards and continuous improvement")
            ),
            
            br(),
            actionButton(
              "load_example_now",
              "📋 Load Example Dataset",
              class = "btn-success"
            )
          )
        ),
        
        fluidRow(
          box(
            title = "💡 Tips & Best Practices", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            
            tags$ul(
              tags$li("Start with the example dataset to understand the workflow"),
              tags$li("Use analytical method for matrices up to 50×50"),
              tags$li("Focus on relationships above 90th percentile for interventions"),
              tags$li("Consider feasibility constraints when planning interventions"),
              tags$li("Validate results with domain experts")
            )
          )
        )
      )
    )
  )
)

# Define Server Logic
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
    factor_names = NULL
  )
  
  # Matrix processing
  observeEvent(input$process_matrix, {
    req(input$input_method)
    
    tryCatch({
      if (input$input_method == "example") {
        # Create example matrix
        set.seed(45)
        n <- 5
        A <- matrix(0, nrow = n, ncol = n)
        for (i in 1:n) {
          for (j in 1:n) {
            if (i != j) {
              A[i, j] <- sample(0:4, 1, prob = c(0.2, 0.3, 0.3, 0.15, 0.05))
            }
          }
        }
        # # Load example data
        # example_text <- "0,3,3,1,3\n0,0,2,0,1\n0,0,0,2,0\n3,1,2,0,3\n4,1,2,1,0"
        # A <- as.matrix(read.csv(textConnection(example_text), header = FALSE))
        
        factor_names <- c("Leadership", "Communication", "Innovation", "Risk_Management", "Quality")
        rownames(A) <- colnames(A) <- factor_names
        
        values$matrix_A <- A
        values$factor_names <- factor_names
        
      } else if (input$input_method == "upload_A") {
        req(input$file_A)
        
        # Read uploaded file
        A_raw <- read_csv_robust(input$file_A$datapath, header = input$header_A)
       # A_raw <- read.csv(input$file_A$datapath, header = input$header_A, stringsAsFactors = FALSE)
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
        if (nzchar(input$factor_names_input)) {
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
      
      # Compute DEMATEL matrices
      if (exists("compute_dematel_matrices", mode = "function")) {
        dematel_matrices <- compute_dematel_matrices(values$matrix_A)
      } else {
        # Fallback computation
        n <- nrow(values$matrix_A)
        s <- max(max(rowSums(values$matrix_A)), max(colSums(values$matrix_A)))
        D <- values$matrix_A / s
        I <- diag(n)
        T_matrix <- solve(I - D) - I
        eigenvals <- eigen(T_matrix, only.values = TRUE)$values
        lambda_max <- max(Re(eigenvals))
        
        dematel_matrices <- list(D = D, T = T_matrix, lambda_max = lambda_max)
      }
      
      # ENHANCED: Perform complete spectral analysis
      if (exists("dematel_spectral_analysis", mode = "function")) {
        spectral_results <- dematel_spectral_analysis(
          D = dematel_matrices$D, 
          T = dematel_matrices$T, 
          case_name = "Current Analysis",
          return_eigenvalues = TRUE,
          verbose = FALSE
        )
      } else {
        # Fallback if spectral analysis function not available
        eigenvals <- eigen(dematel_matrices$T, only.values = TRUE)$values
        spectral_results <- list(
          lambda_max = dematel_matrices$lambda_max,
          spectral_radius = max(abs(eigenvals)),
          case_name = "Current Analysis"
        )
      }
      
      # Add matrix data to spectral results
      spectral_results$D_matrix <- dematel_matrices$D
      spectral_results$T_matrix <- dematel_matrices$T
      spectral_results$A_matrix <- values$matrix_A
      spectral_results$factor_names <- values$factor_names
      spectral_results$n <- nrow(values$matrix_A)
      
      values$spectral_results <- spectral_results
      values$matrix_processed <- TRUE
      
      showNotification("✅ Matrix processed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Error processing matrix:", e$message), type = "error")
    })
  })
  
  # Load example from help tab
  observeEvent(input$load_example_now, {
    updateRadioButtons(session, "input_method", selected = "example")
    updateTabItems(session, "sidebar", "input")
    
    # Trigger processing
    Sys.sleep(0.1)  # Small delay to ensure UI updates
    shinyjs::click("process_matrix")
  })
  
  # Sensitivity analysis
  observeEvent(input$run_sensitivity, {
    req(values$matrix_A)
    
    withProgress(message = "Computing sensitivity analysis...", {
      tryCatch({
        # Create sensitivity object
        if (exists("DEMATEL_Sensitivity", mode = "function") && 
            exists("compute_sensitivity_analytical", mode = "function")) {
          
          sens_obj <- DEMATEL_Sensitivity(values$matrix_A, values$factor_names)
          sens_obj <- compute_sensitivity_analytical(sens_obj)
          values$sensitivity_results <- sens_obj
          values$sensitivity_computed <- TRUE
          
          showNotification("✅ Sensitivity analysis completed!", type = "message")
        } else {
          showNotification("❌ Sensitivity analysis functions not available", type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("❌ Error in sensitivity analysis:", e$message), type = "error")
      })
    })
  })
  
  # Intervention analysis
  observeEvent(input$run_intervention, {
    req(values$sensitivity_results, input$target_lambda_change)
    
    tryCatch({
      if (exists("intervention_analysis_enhanced", mode = "function") || 
          exists("intervention_analysis", mode = "function")) {
        
        if (input$intervention_type == "discrete" && exists("intervention_analysis_enhanced", mode = "function")) {
          interventions <- intervention_analysis_enhanced(
            values$sensitivity_results,
            target_lambda_change = input$target_lambda_change,
            intervention_type = "discrete"
          )
        } else if (exists("intervention_analysis", mode = "function")) {
          interventions <- intervention_analysis(
            values$sensitivity_results,
            target_lambda_change = input$target_lambda_change
          )
        } else {
          stop("Intervention analysis functions not available")
        }
        
        values$intervention_results <- interventions
        values$intervention_computed <- TRUE
        
        showNotification("✅ Intervention analysis completed!", type = "message")
      } else {
        showNotification("❌ Intervention analysis functions not available", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("❌ Error in intervention analysis:", e$message), type = "error")
    })
  })
  
  # Output: Reactive flags
  output$matrix_processed <- reactive({
    values$matrix_processed
  })
  outputOptions(output, "matrix_processed", suspendWhenHidden = FALSE)
  
  output$sensitivity_computed <- reactive({
    values$sensitivity_computed
  })
  outputOptions(output, "sensitivity_computed", suspendWhenHidden = FALSE)
  
  output$intervention_computed <- reactive({
    values$intervention_computed
  })
  outputOptions(output, "intervention_computed", suspendWhenHidden = FALSE)
  
  # Matrix preview and info
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
  
  output$matrix_info <- renderText({
    req(values$matrix_A)
    paste(
      "Matrix size:", nrow(values$matrix_A), "×", ncol(values$matrix_A), "\n",
      "Factors:", paste(values$factor_names, collapse = ", "), "\n",
      "Value range: [", round(min(values$matrix_A), 2), ",", round(max(values$matrix_A), 2), "]", "\n",
      "Diagonal sum:", sum(diag(values$matrix_A)), "(should be 0)"
    )
  })
  
  # ENHANCED Spectral analysis outputs
  output$spectral_metrics_table <- DT::renderDataTable({
    req(values$spectral_results)
    
    # Create comprehensive metrics table
    metrics_data <- data.frame(
      Metric = character(),
      Value = character(),
      Description = character(),
      stringsAsFactors = FALSE
    )
    
    # Add all available metrics from spectral analysis
    if (!is.null(values$spectral_results$lambda_max)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Dominant Eigenvalue (λmax)",
        Value = as.character(round(values$spectral_results$lambda_max, 6)),
        Description = "Largest eigenvalue of T matrix"
      ))
    }
    
    if (!is.null(values$spectral_results$lambda_2)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Second Eigenvalue (λ2)",
        Value = as.character(round(values$spectral_results$lambda_2, 6)),
        Description = "Second largest eigenvalue"
      ))
    }
    
    if (!is.null(values$spectral_results$lambda_min)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Minimum Eigenvalue (λmin)",
        Value = as.character(round(values$spectral_results$lambda_min, 6)),
        Description = "Smallest eigenvalue"
      ))
    }
    
    if (!is.null(values$spectral_results$spectral_radius)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Spectral Radius",
        Value = as.character(round(values$spectral_results$spectral_radius, 6)),
        Description = "Maximum absolute eigenvalue"
      ))
    }
    
    if (!is.null(values$spectral_results$condition_number)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Condition Number",
        Value = as.character(round(values$spectral_results$condition_number, 2)),
        Description = "Ratio λmax/λmin"
      ))
    }
    
    if (!is.null(values$spectral_results$amplification_factor)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Amplification Factor",
        Value = as.character(round(values$spectral_results$amplification_factor, 4)),
        Description = "System amplification potential"
      ))
    }
    
    if (!is.null(values$spectral_results$convergence_rate)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Convergence Rate",
        Value = as.character(round(values$spectral_results$convergence_rate, 4)),
        Description = "System convergence speed"
      ))
    }
    
    if (!is.null(values$spectral_results$concentration_ratio)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Concentration Ratio",
        Value = as.character(round(values$spectral_results$concentration_ratio, 4)),
        Description = "Eigenvalue concentration"
      ))
    }
    
    if (!is.null(values$spectral_results$eigenvector_sd)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Eigenvector Std Dev",
        Value = as.character(round(values$spectral_results$eigenvector_sd, 4)),
        Description = "Dominant eigenvector variability"
      ))
    }
    
    if (!is.null(values$spectral_results$eigenvector_range)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Eigenvector Range",
        Value = as.character(round(values$spectral_results$eigenvector_range, 4)),
        Description = "Dominant eigenvector range"
      ))
    }
    
    # Add diagonalizability information
    if (!is.null(values$spectral_results$is_diagonalizable)) {
      metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Is Diagonalizable",
        Value = ifelse(values$spectral_results$is_diagonalizable, "Yes", "No"),
        Description = "Matrix diagonalizability status"
      ))
    }
    
    DT::datatable(
      metrics_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'ft'
      ),
      rownames = FALSE
    )
  })
  
  output$primary_metrics_table <- renderTable({
    req(values$spectral_results)
    
    primary_data <- data.frame(
      Metric = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    if (!is.null(values$spectral_results$lambda_max)) {
      primary_data <- rbind(primary_data, data.frame(
        Metric = "λmax",
        Value = as.character(round(values$spectral_results$lambda_max, 6))
      ))
    }
    
    if (!is.null(values$spectral_results$spectral_radius)) {
      primary_data <- rbind(primary_data, data.frame(
        Metric = "Spectral Radius",
        Value = as.character(round(values$spectral_results$spectral_radius, 6))
      ))
    }
    
    if (!is.null(values$spectral_results$condition_number)) {
      primary_data <- rbind(primary_data, data.frame(
        Metric = "Condition Number",
        Value = as.character(round(values$spectral_results$condition_number, 2))
      ))
    }
    
    return(primary_data)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$system_characteristics_table <- renderTable({
    req(values$spectral_results)
    
    char_data <- data.frame(
      Property = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    char_data <- rbind(char_data, data.frame(
      Property = "Matrix Size",
      Value = paste0(values$spectral_results$n, " × ", values$spectral_results$n)
    ))
    
    if (!is.null(values$spectral_results$is_diagonalizable)) {
      char_data <- rbind(char_data, data.frame(
        Property = "Diagonalizable",
        Value = ifelse(values$spectral_results$is_diagonalizable, "Yes", "No")
      ))
    }
    
    if (!is.null(values$spectral_results$amplification_factor)) {
      char_data <- rbind(char_data, data.frame(
        Property = "Amplification",
        Value = as.character(round(values$spectral_results$amplification_factor, 4))
      ))
    }
    
    return(char_data)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$matrix_properties <- renderText({
    req(values$spectral_results)
    
    props_text <- ""
    
    if (!is.null(values$spectral_results$case_name)) {
      props_text <- paste(props_text, "Analysis Case:", values$spectral_results$case_name, "\n")
    }
    
    props_text <- paste(props_text, "Factor Names:", paste(values$factor_names, collapse = ", "), "\n")
    
    # Add matrix statistics
    if (!is.null(values$spectral_results$A_matrix)) {
      props_text <- paste(props_text, 
                          "Original Matrix (A) range: [", 
                          round(min(values$spectral_results$A_matrix), 2), ", ",
                          round(max(values$spectral_results$A_matrix), 2), "]\n")
    }
    
    if (!is.null(values$spectral_results$T_matrix)) {
      props_text <- paste(props_text, 
                          "Total Relations Matrix (T) range: [", 
                          round(min(values$spectral_results$T_matrix), 2), ", ",
                          round(max(values$spectral_results$T_matrix), 2), "]\n")
    }
    
    return(props_text)
  })
  
  output$eigenvalue_details <- renderText({
    req(values$spectral_results)
    
    details_text <- ""
    
    if (!is.null(values$spectral_results$lambda_max)) {
      details_text <- paste(details_text, "Dominant eigenvalue (λmax):", 
                            round(values$spectral_results$lambda_max, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$lambda_2)) {
      details_text <- paste(details_text, "Second largest eigenvalue (λ2):", 
                            round(values$spectral_results$lambda_2, 6), "\n")
      
      if (!is.null(values$spectral_results$lambda_max)) {
        gap <- values$spectral_results$lambda_max - values$spectral_results$lambda_2
        details_text <- paste(details_text, "Eigenvalue gap (λmax - λ2):", 
                              round(gap, 6), "\n")
      }
    }
    
    if (!is.null(values$spectral_results$lambda_min)) {
      details_text <- paste(details_text, "Smallest eigenvalue (λmin):", 
                            round(values$spectral_results$lambda_min, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$all_eigenvalues)) {
      details_text <- paste(details_text, "\nAll eigenvalues (real parts):\n")
      eigenvals <- round(values$spectral_results$all_eigenvalues, 4)
      details_text <- paste(details_text, paste(eigenvals, collapse = ", "), "\n")
    }
    
    return(details_text)
  })
  
  output$system_dynamics <- renderText({
    req(values$spectral_results)
    
    dynamics_text <- ""
    
    if (!is.null(values$spectral_results$convergence_rate)) {
      dynamics_text <- paste(dynamics_text, "Convergence rate:", 
                             round(values$spectral_results$convergence_rate, 6), "\n")
    }
    
    if (!is.null(values$spectral_results$concentration_ratio)) {
      dynamics_text <- paste(dynamics_text, "Concentration ratio:", 
                             round(values$spectral_results$concentration_ratio, 4), "\n")
    }
    
    if (!is.null(values$spectral_results$eigenvector_sd)) {
      dynamics_text <- paste(dynamics_text, "Dominant eigenvector std dev:", 
                             round(values$spectral_results$eigenvector_sd, 4), "\n")
    }
    
    if (!is.null(values$spectral_results$eigenvector_range)) {
      dynamics_text <- paste(dynamics_text, "Dominant eigenvector range:", 
                             round(values$spectral_results$eigenvector_range, 4), "\n")
    }
    
    if (!is.null(values$spectral_results$dominant_eigenvector)) {
      dynamics_text <- paste(dynamics_text, "\nDominant eigenvector components:\n")
      eigenvec <- round(values$spectral_results$dominant_eigenvector, 4)
      for (i in 1:length(eigenvec)) {
        dynamics_text <- paste(dynamics_text, values$factor_names[i], ":", eigenvec[i], "\n")
      }
    }
    
    return(dynamics_text)
  })
  
  # Sensitivity analysis outputs
  output$sensitivity_stats <- renderText({
    req(values$sensitivity_results)
    
    if (is.null(values$sensitivity_results$sensitivity_matrix)) {
      return("ERROR: Sensitivity matrix is NULL")
    }
    
    tryCatch({
      if (exists("get_sensitivity_stats", mode = "function")) {
        stats <- get_sensitivity_stats(values$sensitivity_results)
        
        paste(
          "Computation Method:", values$sensitivity_results$computation_method %||% "Unknown", "\n",
          "Mean Sensitivity:", round(stats$mean, 6), "\n",
          "Standard Deviation:", round(stats$sd, 6), "\n",
          "Range: [", round(stats$min, 6), ",", round(stats$max, 6), "]\n",
          "Mean Absolute Sensitivity:", round(stats$mean_abs, 6), "\n",
          "Total Relationships:", stats$total_elements, "\n",
          "Amplifying (positive):", stats$n_positive, "(", round(100*stats$n_positive/stats$total_elements, 1), "%)\n",
          "Stabilizing (negative):", stats$n_negative, "(", round(100*stats$n_negative/stats$total_elements, 1), "%)\n",
          "Near-zero:", stats$n_zero, "(", round(100*stats$n_zero/stats$total_elements, 1), "%)"
        )
      } else {
        return("Sensitivity statistics function not available")
      }
    }, error = function(e) {
      paste("ERROR computing sensitivity statistics:", e$message)
    })
  })
  
  output$sensitivity_classification_plot <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      if (exists("get_sensitivity_stats", mode = "function")) {
        stats <- get_sensitivity_stats(values$sensitivity_results)
        
        classification_data <- data.frame(
          Type = c("Amplifying", "Stabilizing", "Near-zero"),
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
          scale_fill_manual(values = c("Amplifying" = "#9EDEC5", "Stabilizing" = "#295073", "Near-zero" = "#F2F2F2")) +
          theme_minimal() +
          theme(legend.position = "none",
                axis.title.x = element_blank(),
                plot.title = element_text(size = 14, face = "bold")) +
          labs(title = "Relationship Type Distribution",
               y = "Number of Relationships") +
          ylim(0, max(classification_data$Count) * 1.2)
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "Sensitivity statistics function not available", size = 6) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:", e$message), size = 6) +
        theme_void()
    })
  })
  
  # Visualization outputs - with safe function checks
  output$sensitivity_heatmap <- renderPlot({
    req(values$sensitivity_results)
    
    if (is.null(values$sensitivity_results$sensitivity_matrix)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
               theme_void() +
               labs(title = "Sensitivity Heatmap - Matrix NULL"))
    }
    
    tryCatch({
      sens_matrix <- values$sensitivity_results$sensitivity_matrix
      
      if (!requireNamespace("reshape2", quietly = TRUE)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "reshape2 package not available", size = 6) +
                 theme_void())
      }
      
      sens_melted <- reshape2::melt(sens_matrix)
      names(sens_melted) <- c("From_Factor", "To_Factor", "Sensitivity")
      sens_melted <- sens_melted[!is.na(sens_melted$Sensitivity), ]
      
      if (nrow(sens_melted) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid sensitivity values", size = 6) +
                 theme_void())
      }
      
      p <- ggplot(sens_melted, aes(x = To_Factor, y = From_Factor, fill = Sensitivity)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(low = "#295073", mid = "white", high = "#9EDEC5",
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
      
      return(p)
      
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Sensitivity heatmap\nerror:\n", e$message),
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Sensitivity Heatmap - Error")
    })
  })
  
  output$interrelationship_map <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      if (exists("create_dematel_interrelationship_map", mode = "function")) {
        create_dematel_interrelationship_map(values$sensitivity_results)
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Interrelationship map function not available", size = 6) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Interrelationship map\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "DEMATEL Interrelationship Map - Error")
    })
  })
  
  output$sensitivity_distribution <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      sens_values <- as.vector(values$sensitivity_results$sensitivity_matrix)
      sens_values <- sens_values[!is.na(sens_values)]
      
      if (length(sens_values) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "No valid sensitivity values", size = 6) +
                 theme_void())
      }
      
      ggplot(data.frame(Sensitivity = sens_values), aes(x = Sensitivity)) +
        geom_histogram(bins = 30, alpha = 0.7, fill = "#295073", color = "white") +
        geom_vline(xintercept = 0, color = "#C81102", linetype = "dashed", size = 1) +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        labs(
          title = "Distribution of Sensitivity Values",
          subtitle = paste("Mean:", round(mean(sens_values), 4), "| SD:", round(sd(sens_values), 4)),
          x = "Sensitivity Value",
          y = "Frequency"
        )
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Distribution plot\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Sensitivity Distribution - Error")
    })
  })
  
  output$top_relationships_plot <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Sensitivity matrix is NULL", size = 6) +
                 theme_void())
      }
      
      if (exists("identify_critical_relationships", mode = "function")) {
        critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                         threshold_percentile = input$critical_threshold)
        
        if (nrow(critical_rels) > 0) {
          top_10 <- head(critical_rels, 10)
          top_10$relationship <- paste0(top_10$from_factor, " → ", top_10$to_factor)
          top_10$relationship <- factor(top_10$relationship, levels = rev(top_10$relationship))
          
          ##DEBUG
          cat("Unique interpretation values:", unique(top_10$interpretation), "\n")
          print(table(top_10$interpretation))
          
          ggplot(top_10, aes(x = relationship, y = sensitivity, fill = interpretation)) +
            geom_col(alpha = 0.8) +
            coord_flip() +
            scale_fill_manual(
              values = c("Amplifying" = "#9EDEC5", "Stabilizer links" = "#295073"),
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
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Critical relationships function not available", size = 6) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Critical relationships plot\nerror:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Top Relationships - Error")
    })
  })
  
  # Critical relationships table
  output$critical_relationships_table <- DT::renderDataTable({
    req(values$sensitivity_results)
    
    tryCatch({
      if (is.null(values$sensitivity_results$sensitivity_matrix)) {
        return(DT::datatable(data.frame(Error = "Sensitivity matrix is NULL")))
      }
      
      if (exists("identify_critical_relationships", mode = "function")) {
        critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                         threshold_percentile = input$critical_threshold)
        
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
              order = list(list(3, "desc"))
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
      } else {
        DT::datatable(data.frame(Error = "Critical relationships function not available"))
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error generating table:", e$message)))
    })
  })
  
  # Intervention analysis table
  output$intervention_table <- DT::renderDataTable({
    req(values$intervention_results)
    
    tryCatch({
      display_data <- values$intervention_results
      
      if (input$intervention_type == "discrete") {
        display_cols <- c("from_factor", "to_factor", "current_aij", "required_change", 
                          "new_aij", "actual_lambda_change", "target_achievement", "feasible")
        col_names <- c("From Factor", "To Factor", "Current Value", "Change", 
                       "New Value", "Lambda Change", "Target Achievement", "Feasible")
      } else {
        display_cols <- c("from_factor", "to_factor", "current_aij", "required_change", 
                          "new_aij", "efficiency", "feasible")
        col_names <- c("From Factor", "To Factor", "Current Value", "Required Change", 
                       "New Value", "Efficiency", "Feasible")
      }
      
      display_data <- display_data[, display_cols]
      names(display_data) <- col_names
      
      DT::datatable(
        head(display_data, 50),  # Limit to top 50 results
        options = list(
          pageLength = 15,
          scrollX = TRUE
        )
      ) %>%
        DT::formatRound(columns = which(sapply(display_data, is.numeric)), digits = 4) %>%
        DT::formatStyle(
          "Feasible",
          backgroundColor = DT::styleEqual(
            c(TRUE, FALSE),
            c("#d4edda", "#f8d7da")
          )
        )
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error generating intervention table:", e$message)))
    })
  })
  
  # Executive summary for report
  output$executive_summary <- renderText({
    req(values$sensitivity_results)
    
    tryCatch({
      if (exists("get_sensitivity_stats", mode = "function") && 
          exists("identify_critical_relationships", mode = "function")) {
        
        stats <- get_sensitivity_stats(values$sensitivity_results)
        critical_90 <- identify_critical_relationships(values$sensitivity_results, 90)
        critical_95 <- identify_critical_relationships(values$sensitivity_results, 95)
        
        summary_text <- paste(
          "EXECUTIVE SUMMARY\n",
          "================\n\n",
          "System Overview:\n",
          "- Matrix size:", values$spectral_results$n, "×", values$spectral_results$n, "\n",
          "- Dominant eigenvalue (λmax):", round(values$spectral_results$lambda_max, 6), "\n\n",
          
          "Sensitivity Analysis:\n",
          "- Total relationships analyzed:", stats$total_elements, "\n",
          "- Amplifying relationships:", stats$n_positive, 
          " (", round(100*stats$n_positive/stats$total_elements, 1), "%)\n",
          "- Dampening relationships:", stats$n_negative, 
          " (", round(100*stats$n_negative/stats$total_elements, 1), "%)\n",
          "- Mean absolute sensitivity:", round(stats$mean_abs, 6), "\n\n",
          
          "Critical Relationships:\n",
          "- 90th percentile threshold:", nrow(critical_90), "relationships\n",
          "- 95th percentile threshold:", nrow(critical_95), "relationships\n"
        )
        
        if (nrow(critical_95) > 0) {
          top_critical <- head(critical_95, 3)
          summary_text <- paste(summary_text, "\nTop 3 Most Critical:\n")
          for (i in 1:nrow(top_critical)) {
            summary_text <- paste(summary_text, 
                                  paste0(i, ". ", top_critical$from_factor[i], " → ", 
                                         top_critical$to_factor[i], ": ", 
                                         round(top_critical$sensitivity[i], 6), 
                                         " (", top_critical$interpretation[i], ")\n"))
          }
        }
        
        return(summary_text)
      } else {
        return("Executive summary functions not available")
      }
      
    }, error = function(e) {
      paste("Error generating executive summary:", e$message)
    })
  })
  
  # Download handlers
  output$download_spectral <- downloadHandler(
    filename = function() {
      paste0("complete_spectral_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$spectral_results)) {
        # Create comprehensive spectral analysis data frame
        spectral_df <- data.frame(
          Metric = character(),
          Value = numeric(),
          stringsAsFactors = FALSE
        )
        
        # Add all available metrics
        if (!is.null(values$spectral_results$lambda_max)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Dominant_Eigenvalue", Value = values$spectral_results$lambda_max))
        }
        if (!is.null(values$spectral_results$lambda_2)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Second_Eigenvalue", Value = values$spectral_results$lambda_2))
        }
        if (!is.null(values$spectral_results$lambda_min)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Minimum_Eigenvalue", Value = values$spectral_results$lambda_min))
        }
        if (!is.null(values$spectral_results$spectral_radius)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Spectral_Radius", Value = values$spectral_results$spectral_radius))
        }
        if (!is.null(values$spectral_results$condition_number)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Condition_Number", Value = values$spectral_results$condition_number))
        }
        if (!is.null(values$spectral_results$amplification_factor)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Amplification_Factor", Value = values$spectral_results$amplification_factor))
        }
        if (!is.null(values$spectral_results$convergence_rate)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Convergence_Rate", Value = values$spectral_results$convergence_rate))
        }
        if (!is.null(values$spectral_results$concentration_ratio)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Concentration_Ratio", Value = values$spectral_results$concentration_ratio))
        }
        if (!is.null(values$spectral_results$eigenvector_sd)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Eigenvector_Std_Dev", Value = values$spectral_results$eigenvector_sd))
        }
        if (!is.null(values$spectral_results$eigenvector_range)) {
          spectral_df <- rbind(spectral_df, data.frame(Metric = "Eigenvector_Range", Value = values$spectral_results$eigenvector_range))
        }
        
        write.csv(spectral_df, file, row.names = FALSE)
      }
    }
  )
  
  output$download_critical <- downloadHandler(
    filename = function() {
      paste0("critical_relationships_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$sensitivity_results) && exists("identify_critical_relationships", mode = "function")) {
        critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                         threshold_percentile = input$critical_threshold)
        write.csv(critical_rels, file, row.names = FALSE)
      }
    }
  )
  
  output$download_interventions <- downloadHandler(
    filename = function() {
      paste0("intervention_analysis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$intervention_results)) {
        write.csv(values$intervention_results, file, row.names = FALSE)
      }
    }
  )
  
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste0("dematel_full_report_", Sys.Date(), ".txt")
    },
    content = function(file) {
      if (!is.null(values$sensitivity_results)) {
        report_content <- ""
        
        # Add executive summary
        if (exists("get_sensitivity_stats", mode = "function") && 
            exists("identify_critical_relationships", mode = "function")) {
          
          stats <- get_sensitivity_stats(values$sensitivity_results)
          critical_90 <- identify_critical_relationships(values$sensitivity_results, 90)
          
          report_content <- paste(
            "DEMATEL COMPREHENSIVE ANALYSIS REPORT\n",
            "=====================================\n\n",
            "Generated on:", Sys.time(), "\n\n",
            "SYSTEM OVERVIEW:\n",
            "Matrix size:", values$spectral_results$n, "×", values$spectral_results$n, "\n",
            "Factors:", paste(values$factor_names, collapse = ", "), "\n",
            "Dominant eigenvalue (λmax):", round(values$spectral_results$lambda_max, 6), "\n\n",
            
            "SENSITIVITY ANALYSIS RESULTS:\n",
            "Method:", values$sensitivity_results$computation_method %||% "Unknown", "\n",
            "Total relationships:", stats$total_elements, "\n",
            "Mean absolute sensitivity:", round(stats$mean_abs, 6), "\n",
            "Amplifying relationships:", stats$n_positive, "\n",
            "Dampening relationships:", stats$n_negative, "\n",
            "Critical relationships (90th percentile):", nrow(critical_90), "\n\n"
          )
        }
        
        writeLines(report_content, file)
      }
    }
  )
  
  output$download_summary_report <- downloadHandler(
    filename = function() {
      paste0("dematel_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$sensitivity_results) && exists("get_sensitivity_stats", mode = "function")) {
        stats <- get_sensitivity_stats(values$sensitivity_results)
        
        summary_df <- data.frame(
          Analysis_Date = Sys.Date(),
          Matrix_Size = paste0(values$spectral_results$n, "x", values$spectral_results$n),
          Lambda_Max = values$spectral_results$lambda_max,
          Mean_Sensitivity = stats$mean,
          Mean_Abs_Sensitivity = stats$mean_abs,
          Amplifying_Count = stats$n_positive,
          Dampening_Count = stats$n_negative,
          Method = values$sensitivity_results$computation_method %||% "Unknown"
        )
        
        write.csv(summary_df, file, row.names = FALSE)
      }
    }
  )
  
  output$download_spectral_data <- downloadHandler(
    filename = function() {
      paste0("spectral_matrices_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$spectral_results$T_matrix)) {
        write.csv(values$spectral_results$T_matrix, file, row.names = TRUE)
      }
    }
  )
  
  output$download_sensitivity_data <- downloadHandler(
    filename = function() {
      paste0("sensitivity_matrix_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$sensitivity_results$sensitivity_matrix)) {
        write.csv(values$sensitivity_results$sensitivity_matrix, file, row.names = TRUE)
      }
    }
  )
  
  # Helper function for null coalescing
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
}
# Add this function before your server function in app.R

#' Robust CSV reader that detects separator automatically
#' @param filepath Path to the CSV file
#' @param header Whether file has header
#' @return Matrix with numeric data
read_csv_robust <- function(filepath, header = FALSE) {
  
  # Read first few lines to detect separator
  sample_lines <- readLines(filepath, n = 3)
  
  # Count occurrences of common separators
  comma_count <- sum(grepl(",", sample_lines))
  semicolon_count <- sum(grepl(";", sample_lines))
  tab_count <- sum(grepl("\t", sample_lines))
  
  # Determine most likely separator
  separator <- if (semicolon_count > comma_count && semicolon_count > tab_count) {
    ";"
  } else if (tab_count > comma_count && tab_count > semicolon_count) {
    "\t"
  } else {
    ","
  }
  
  cat("Detected separator:", ifelse(separator == "\t", "TAB", separator), "\n")
  
  # Read with detected separator
  data_raw <- read.csv(filepath, header = header, stringsAsFactors = FALSE, 
                       sep = separator, check.names = FALSE)
  
  # Convert to numeric matrix
  data_matrix <- as.matrix(data_raw)
  mode(data_matrix) <- "numeric"
  
  # Final validation
  if (any(is.na(data_matrix))) {
    stop("Matrix contains non-numeric values or missing data")
  }
  
  return(data_matrix)
}



# Run the application
cat("Starting complete DEMATEL Sensitivity Analysis app...\n")
shinyApp(ui = ui, server = server)