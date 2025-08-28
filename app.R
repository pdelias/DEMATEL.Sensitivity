# DEMATEL Sensitivity Analysis Shiny App
# File: app.R

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
source("R/ui_components.R", local = TRUE)

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
              
              div(id = "processing_status")
            )
          )
        )
      ),
      
      # Spectral Analysis Tab
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
      
      # Sensitivity Analysis Tab
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
              
              radioButtons(
                "sensitivity_method",
                "Computation Method:",
                choices = list(
                  "Numerical (Robust)" = "numerical",
                  "Analytical (Fast)" = "analytical"
                ),
                selected = "numerical"
              ),
              
              conditionalPanel(
                condition = "input.sensitivity_method == 'numerical'",
                numericInput(
                  "sensitivity_epsilon",
                  "Step Size (ε):",
                  value = 0.01,
                  min = 0.001,
                  max = 0.1,
                  step = 0.001
                ),
                helpText("Smaller values = more accurate but slower computation")
              ),
              
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
                  min = 50,
                  max = 99,
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
                title = "🌡️ Sensitivity Analysis", 
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
                title = "📊 DEMATEL Analysis", 
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
          ),
          
          fluidRow(
            box(
              title = "🌐 DEMATEL Network Visualization", 
              status = "info", 
              solidHeader = TRUE,
              width = 8,
              
              h4("Cause-Effect Network:"),
              p("Shows the strongest cause-effect relationships from the DEMATEL analysis."),
              plotOutput("dematel_network_plot", height = "500px"),
              
              br(),
              
              fluidRow(
                column(
                  6,
                  radioButtons(
                    "network_layout",
                    "Layout Style:",
                    choices = list(
                      "Spring Layout" = "spring",
                      "Circle Layout" = "circle", 
                      "Hierarchical" = "hierarchical"
                    ),
                    selected = "spring",
                    inline = TRUE
                  )
                ),
                column(
                  6,
                  sliderInput(
                    "network_threshold",
                    "Relationship threshold (percentile):",
                    min = 70,
                    max = 99,
                    value = 90,
                    step = 5
                  )
                )
              )
            ),
            
            box(
              title = "📈 Impact Analysis", 
              status = "success", 
              solidHeader = TRUE,
              width = 4,
              
              h4("System Impact Overview:"),
              
              verbatimTextOutput("impact_summary"),
              
              br(),
              
              h4("Key Insights:"),
              htmlOutput("key_insights")
            )
          )
            )
          ),
      
      # Intervention Analysis Tab
      tabItem(
        tabName = "intervention",
        conditionalPanel(
          condition = "!output.sensitivity_computed",
          no_data_message("Please complete sensitivity analysis first.")
        ),
        
        conditionalPanel(
          condition = "output.sensitivity_computed",
          fluidRow(
            box(
              title = "💡 Intervention Planning", 
              status = "warning", 
              solidHeader = TRUE,
              width = 4,
              
              h4("Target Change Settings:"),
              
              numericInput(
                "target_lambda_change",
                "Target change in λmax:",
                value = -0.1,
                step = 0.01
              ),
              
              helpText("Positive: Increase system amplification | Negative: Decrease system amplification"),
              
              checkboxInput(
                "feasibility_check",
                "Only show feasible interventions (non-negative values)",
                value = TRUE
              ),
              
              br(),
              actionButton(
                "run_intervention",
                "💡 Calculate Interventions",
                class = "btn-warning btn-lg"
              ),
              
              br(), br(),
              
              conditionalPanel(
                condition = "output.intervention_computed",
                h4("Filter Options:"),
                
                sliderInput(
                  "max_interventions",
                  "Show top n interventions:",
                  min = 0,
                  max = 100,
                  value = 15,
                  step = 5
                )
              )
            ),
            
            box(
              title = "📊 Intervention Results", 
              status = "info", 
              solidHeader = TRUE,
              width = 8,
              
              conditionalPanel(
                condition = "!output.intervention_computed",
                div(
                  style = "text-align: center; padding: 50px;",
                  icon("lightbulb", style = "font-size: 48px; color: #ccc;"),
                  h4("No intervention analysis computed", style = "color: #999; margin-top: 20px;"),
                  p("Set your target change and click 'Calculate Interventions'.")
                )
              ),
              
              conditionalPanel(
                condition = "output.intervention_computed",
                h4("Recommended Interventions (Ordered by Efficiency):"),
                DT::dataTableOutput("intervention_table"),
                
                br(),
                downloadButton(
                  "download_interventions",
                  "📥 Download Intervention Plan",
                  class = "btn-warning"
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.intervention_computed",
            fluidRow(
              box(
                title = "📈 Intervention Efficiency Analysis", 
                status = "success", 
                solidHeader = TRUE,
                width = 12,
                
                h4("Efficiency Distribution:"),
                p("Higher efficiency means smaller changes needed to achieve the target effect."),
                
                plotOutput("intervention_efficiency_plot", height = "400px")
              )
            )
          )
        )
      ),
      
      # Comprehensive Report Tab
      tabItem(
        tabName = "report",
        conditionalPanel(
          condition = "!output.matrix_processed",
          no_data_message("Please upload and process a matrix first.")
        ),
        
        conditionalPanel(
          condition = "output.matrix_processed",
          fluidRow(
            box(
              title = "📋 Comprehensive Analysis Report", 
              status = "primary", 
              solidHeader = TRUE,
              width = 12,
              
              h4("Generated Report:"),
              
              wellPanel(
                style = "background-color: #f8f9fa; max-height: 600px; overflow-y: auto;",
                verbatimTextOutput("comprehensive_report")
              ),
              
              br(),
              
              fluidRow(
                column(
                  6,
                  downloadButton(
                    "download_report",
                    "📥 Download Complete Report (TXT)",
                    class = "btn-primary"
                  )
                ),
                column(
                  6,
                  div(
                    style = "text-align: right;",
                    actionButton(
                      "refresh_report",
                      "🔄 Refresh Report",
                      class = "btn-info"
                    )
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
            title = "❓ How to Use This Application", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            
            h3("🚀 Quick Start Guide"),
            
            h4("Step 1: Data Input"),
            tags$ul(
              tags$li("Upload your original direct influence matrix (A) as a CSV file"),
              tags$li("Or use the provided example dataset to explore features"),
              tags$li("The matrix should contain expert judgment values (typically 0-4 scale)"),
              tags$li("Diagonal elements should be zero (no self-influence)")
            ),
            
            h4("Step 2: Spectral Analysis"),
            tags$ul(
              tags$li("Review the eigenvalue analysis results"),
              tags$li("Key metric: dominant eigenvalue (λmax) controls system behavior"),
              tags$li("Higher λmax = more system amplification and potential instability")
            ),
            
            h4("Step 3: Sensitivity Analysis"),
            tags$ul(
              tags$li("Choose numerical method (more accurate) or analytical (faster)"),
              tags$li("Results show how changes in each relationship affect λmax"),
              tags$li("Red = amplifying relationships, Blue = dampening relationships")
            ),
            
            h4("Step 4: Critical Relationships"),
            tags$ul(
              tags$li("Identify the most impactful relationships in your system"),
              tags$li("Focus management attention on these critical connections"),
              tags$li("Use network visualization to understand system structure")
            ),
            
            h4("Step 5: Intervention Analysis"),
            tags$ul(
              tags$li("Set a target change in system behavior (λmax)"),
              tags$li("Get specific recommendations for which relationships to modify"),
              tags$li("Higher efficiency = smaller changes needed for desired effect")
            ),
            
            br(),
            
            h3("📊 Understanding the Results"),
            
            h4("Sensitivity Values:"),
            tags$ul(
              tags$li(strong("Positive values:"), " Strengthening this relationship increases λmax"),
              tags$li(strong("Negative values:"), " Strengthening this relationship decreases λmax"),
              tags$li(strong("Large absolute values:"), " High impact relationships requiring careful management")
            ),
            
            h4("Practical Applications:"),
            tags$ul(
              tags$li(strong("Risk Management:"), " Identify relationships that amplify system risks"),
              tags$li(strong("Performance Improvement:"), " Find leverage points for positive change"),
              tags$li(strong("System Stability:"), " Understand which connections affect overall stability")
            ),
            
            br(),
            
            h3("📥 Example Dataset"),
            
            p("The example dataset represents a 5-factor organizational system with factors like leadership, communication, and innovation. 
              Use it to explore all application features before uploading your own data."),
            
            br(),
            
            div(
              style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px;",
              h4("💡 Tips for Best Results:"),
              tags$ul(
                tags$li("Ensure your input matrix has meaningful factor names"),
                tags$li("Start with example data to understand the workflow"),
                tags$li("Use numerical sensitivity method for final analysis"),
                tags$li("Focus on the top 10-20% most critical relationships"),
                tags$li("Consider feasibility constraints in intervention planning")
              )
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
        set.seed(42)
        n <- 5
        A <- matrix(0, nrow = n, ncol = n)
        for (i in 1:n) {
          for (j in 1:n) {
            if (i != j) {
              A[i, j] <- sample(0:4, 1, prob = c(0.2, 0.3, 0.3, 0.15, 0.05))
            }
          }
        }
        
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
      
      # Compute spectral analysis
      dematel_matrices <- compute_dematel_matrices(values$matrix_A)
      
      spectral_results <- list(
        lambda_max = dematel_matrices$lambda_max,
        D_matrix = dematel_matrices$D,
        T_matrix = dematel_matrices$T,
        A_matrix = values$matrix_A,
        factor_names = values$factor_names,
        n = nrow(values$matrix_A)
      )
      
      # Add more spectral analysis if available
      if (exists("dematel_spectral_analysis", mode = "function")) {
        spectral_detailed <- dematel_spectral_analysis(
          dematel_matrices$D, 
          dematel_matrices$T, 
          verbose = FALSE
        )
        spectral_results <- c(spectral_results, spectral_detailed)
      }
      
      values$spectral_results <- spectral_results
      values$matrix_processed <- TRUE
      
      showNotification("✅ Matrix processed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Error processing matrix:", e$message), type = "error")
    })
  })
  
  # Sensitivity analysis
  observeEvent(input$run_sensitivity, {
    req(values$matrix_A)
    
    withProgress(message = "Computing sensitivity analysis...", {
      tryCatch({
        # Create sensitivity object
        sens_obj <- DEMATEL_Sensitivity(values$matrix_A, values$factor_names)
        
        # Compute sensitivity
        if (input$sensitivity_method == "numerical") {
          sens_obj <- compute_sensitivity_numerical(sens_obj, epsilon = input$sensitivity_epsilon)
        } else {
          sens_obj <- compute_sensitivity_analytical(sens_obj)
        }
        
        values$sensitivity_results <- sens_obj
        values$sensitivity_computed <- TRUE
        
        showNotification("✅ Sensitivity analysis completed!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Error in sensitivity analysis:", e$message), type = "error")
      })
    })
  })
  
  # Intervention analysis
  observeEvent(input$run_intervention, {
    req(values$sensitivity_results)
    
    tryCatch({
      intervention_results <- intervention_analysis(
        values$sensitivity_results,
        target_lambda_change = input$target_lambda_change,
        feasibility_check = input$feasibility_check
      )
      
      values$intervention_results <- intervention_results
      values$intervention_computed <- TRUE
      
      showNotification("✅ Intervention analysis completed!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Error in intervention analysis:", e$message), type = "error")
    })
  })
  
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
  
  # Output: Intervention computed flag
  output$intervention_computed <- reactive({
    values$intervention_computed
  })
  outputOptions(output, "intervention_computed", suspendWhenHidden = FALSE)
  
  # Output: Matrix preview
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
  })
  
  # Output: Sensitivity classification plot
  output$sensitivity_classification_plot <- renderPlot({
    req(values$sensitivity_results)
    
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
  })
  
  # Output: Sensitivity heatmap
  output$sensitivity_heatmap <- renderPlot({
    req(values$sensitivity_results)
    
    sens_melted <- reshape2::melt(values$sensitivity_results$sensitivity_matrix)
    names(sens_melted) <- c("From_Factor", "To_Factor", "Sensitivity")
    sens_melted <- sens_melted[!is.na(sens_melted$Sensitivity), ]
    
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
    
    return(p)
  })
  
  # Output: DEMATEL Interrelationship Map
  output$interrelationship_map <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      create_dematel_interrelationship_map(values$sensitivity_results)
    }, error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Interrelationship map\nnot available:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "DEMATEL Interrelationship Map - Error")
    })
  })
  
  # Output: Sensitivity distribution
  output$sensitivity_distribution <- renderPlot({
    req(values$sensitivity_results)
    
    sens_values <- as.vector(values$sensitivity_results$sensitivity_matrix)
    sens_values <- sens_values[!is.na(sens_values)]
    
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
  })
  
  # Output: Top relationships plot
  output$top_relationships_plot <- renderPlot({
    req(values$sensitivity_results)
    
    critical_rels <- identify_critical_relationships(values$sensitivity_results, 
                                                     threshold_percentile = input$critical_threshold)
    
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
    }
  })
  
  # Output: Critical relationships table
  output$critical_relationships_table <- DT::renderDataTable({
    req(values$sensitivity_results)
    
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
    }
  })
  
  # Output: Network plot
  output$network_plot <- renderPlot({
    req(values$sensitivity_results)
    
    tryCatch({
      plot_sensitivity_network(values$sensitivity_results, 
                               threshold_percentile = input$critical_threshold,
                               layout = input$network_layout)
    }, error = function(e) {
      # Fallback network plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Network visualization\nnot available:\n", e$message),
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Network Plot - Error")
    })
  })
  
  # Output: Impact summary
  output$impact_summary <- renderText({
    req(values$sensitivity_results)
    
    stats <- get_sensitivity_stats(values$sensitivity_results)
    critical <- identify_critical_relationships(values$sensitivity_results, 
                                                threshold_percentile = input$critical_threshold)
    
    paste(
      "System Overview:\n",
      "Total relationships:", stats$total_elements, "\n",
      "Critical relationships (", input$critical_threshold, "th %):", nrow(critical), "\n",
      "Max sensitivity:", round(stats$max, 4), "\n",
      "Min sensitivity:", round(stats$min, 4), "\n\n",
      
      "Impact Distribution:\n",
      "High impact (>90th %):", nrow(identify_critical_relationships(values$sensitivity_results, 90)), "\n",
      "Medium impact (70-90th %):", 
      nrow(identify_critical_relationships(values$sensitivity_results, 70)) - 
        nrow(identify_critical_relationships(values$sensitivity_results, 90)), "\n"
    )
  })
  
  # Output: Key insights
  output$key_insights <- renderUI({
    req(values$sensitivity_results)
    
    stats <- get_sensitivity_stats(values$sensitivity_results)
    
    insights <- character()
    
    if (stats$mean > 0) {
      insights <- c(insights, "• System tends toward amplification (positive mean sensitivity)")
    } else {
      insights <- c(insights, "• System tends toward dampening (negative mean sensitivity)")
    }
    
    if (stats$n_positive > stats$n_negative) {
      insights <- c(insights, "• Majority of relationships are amplifying - monitor for cascading effects")
    }
    
    if (stats$max > 2 * stats$mean_abs) {
      insights <- c(insights, "• System contains highly sensitive relationships requiring careful management")
    }
    
    if (length(insights) == 0) {
      insights <- "• System shows balanced sensitivity characteristics"
    }
    
    HTML(paste(insights, collapse = "<br>"))
  })
  
  # Output: Intervention table
  output$intervention_table <- DT::renderDataTable({
    req(values$intervention_results)
    
    display_data <- head(values$intervention_results, input$max_interventions)
    display_data <- display_data[, c("from_factor", "to_factor", "current_aij", 
                                     "required_change", "new_aij", "efficiency", "feasible")]
    
    names(display_data) <- c("From Factor", "To Factor", "Current Value", 
                             "Required Change", "New Value", "Efficiency", "Feasible")
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(5, "desc"))  # Order by efficiency
      )
    ) %>%
      DT::formatRound(columns = c("Current Value", "Required Change", "New Value", "Efficiency"), 
                      digits = 4) %>%
      DT::formatStyle(
        "Feasible",
        backgroundColor = DT::styleEqual(
          c(TRUE, FALSE),
          c("#d4edda", "#f8d7da")
        )
      )
  })
  
  # Output: Intervention efficiency plot
  output$intervention_efficiency_plot <- renderPlot({
    req(values$intervention_results)
    
    top_interventions <- head(values$intervention_results, input$max_interventions)
    top_interventions$relationship <- paste0(top_interventions$from_factor, " → ", 
                                             top_interventions$to_factor)
    top_interventions$relationship <- factor(top_interventions$relationship, 
                                             levels = rev(top_interventions$relationship))
    
    ggplot(top_interventions, aes(x = relationship, y = efficiency, fill = feasible)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(
        values = c("TRUE" = "#28a745", "FALSE" = "#dc3545"),
        name = "Feasible",
        labels = c("FALSE" = "No", "TRUE" = "Yes")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 8)
      ) +
      labs(
        title = "Intervention Efficiency Ranking",
        subtitle = paste("Target λmax change:", input$target_lambda_change),
        x = "Relationship",
        y = "Efficiency (higher = better)"
      )
  })
  
  # Output: Comprehensive report
  output$comprehensive_report <- renderText({
    req(values$matrix_processed)
    
    report_lines <- c()
    
    # Header
    report_lines <- c(report_lines,
                      "COMPREHENSIVE DEMATEL ANALYSIS REPORT",
                      "=====================================",
                      "",
                      paste("Generated on:", Sys.time()),
                      paste("System size:", nrow(values$matrix_A), "×", ncol(values$matrix_A)),
                      ""
    )
    
    # Spectral Analysis
    if (!is.null(values$spectral_results)) {
      report_lines <- c(report_lines,
                        "SPECTRAL ANALYSIS RESULTS:",
                        "-------------------------",
                        paste("Dominant eigenvalue (λmax):", round(values$spectral_results$lambda_max, 6))
      )
      
      if (values$spectral_results$lambda_max > 1) {
        report_lines <- c(report_lines, "• System shows potential for influence amplification")
      } else {
        report_lines <- c(report_lines, "• System is stable with bounded influence")
      }
      
      report_lines <- c(report_lines, "")
    }
    
    # Sensitivity Analysis
    if (!is.null(values$sensitivity_results)) {
      stats <- get_sensitivity_stats(values$sensitivity_results)
      critical_90 <- identify_critical_relationships(values$sensitivity_results, 90)
      critical_95 <- identify_critical_relationships(values$sensitivity_results, 95)
      
      report_lines <- c(report_lines,
                        "SENSITIVITY ANALYSIS RESULTS:",
                        "-----------------------------",
                        paste("Computation method:", values$sensitivity_results$computation_method),
                        "",
                        "Statistical Summary:",
                        paste("  Mean sensitivity:", round(stats$mean, 6)),
                        paste("  Standard deviation:", round(stats$sd, 6)),
                        paste("  Range: [", round(stats$min, 6), ",", round(stats$max, 6), "]"),
                        paste("  Mean absolute sensitivity:", round(stats$mean_abs, 6)),
                        "",
                        "Relationship Classification:",
                        paste("  Amplifying relationships:", stats$n_positive),
                        paste("  Dampening relationships:", stats$n_negative),
                        paste("  Near-zero relationships:", stats$n_zero),
                        "",
                        "Critical Relationships:",
                        paste("  90th percentile threshold:", nrow(critical_90), "relationships"),
                        paste("  95th percentile threshold:", nrow(critical_95), "relationships")
      )
      
      if (nrow(critical_95) > 0) {
        report_lines <- c(report_lines,
                          "",
                          "Top 5 Most Critical Relationships (95th percentile):"
        )
        
        top_5 <- head(critical_95, 5)
        for (i in 1:nrow(top_5)) {
          report_lines <- c(report_lines,
                            paste("  ", i, ".", top_5$from_factor[i], "→", top_5$to_factor[i],
                                  ":", round(top_5$sensitivity[i], 6), "(", top_5$interpretation[i], ")")
          )
        }
      }
      
      report_lines <- c(report_lines, "")
    }
    
    # Intervention Analysis
    if (!is.null(values$intervention_results)) {
      report_lines <- c(report_lines,
                        "INTERVENTION ANALYSIS RESULTS:",
                        "------------------------------",
                        paste("Target λmax change:", input$target_lambda_change),
                        paste("Total interventions analyzed:", nrow(values$intervention_results)),
                        paste("Feasible interventions:", sum(values$intervention_results$feasible)),
                        ""
      )
      
      if (nrow(values$intervention_results) > 0) {
        top_3 <- head(values$intervention_results, 3)
        report_lines <- c(report_lines,
                          "Top 3 Most Efficient Interventions:"
        )
        
        for (i in 1:nrow(top_3)) {
          report_lines <- c(report_lines,
                            paste("  ", i, ".", top_3$from_factor[i], "→", top_3$to_factor[i],
                                  ": Change by", round(top_3$required_change[i], 4),
                                  "(Efficiency:", round(top_3$efficiency[i], 4), ")")
          )
        }
      }
      
      report_lines <- c(report_lines, "")
    }
    
    # System Insights
    if (!is.null(values$sensitivity_results)) {
      stats <- get_sensitivity_stats(values$sensitivity_results)
      
      report_lines <- c(report_lines,
                        "SYSTEM INSIGHTS AND RECOMMENDATIONS:",
                        "------------------------------------"
      )
      
      if (stats$mean > 0) {
        report_lines <- c(report_lines,
                          "• System shows overall amplifying tendency (positive mean sensitivity)"
        )
      } else {
        report_lines <- c(report_lines,
                          "• System shows overall dampening tendency (negative mean sensitivity)"
        )
      }
      
      if (stats$max > 2 * stats$mean_abs) {
        report_lines <- c(report_lines,
                          "• System contains highly sensitive relationships requiring careful management"
        )
      }
      
      if (stats$n_positive > stats$n_negative) {
        report_lines <- c(report_lines,
                          "• Majority of relationships are amplifying - monitor for cascading effects"
        )
      }
      
      report_lines <- c(report_lines, "")
    }
    
    # Footer
    report_lines <- c(report_lines,
                      "---",
                      "Report generated by DEMATEL Sensitivity Analysis Shiny App"
    )
    
    return(paste(report_lines, collapse = "\n"))
  })
  
  # Download handlers
  output$download_spectral <- downloadHandler(
    filename = function() {
      paste0("spectral_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$spectral_results)) {
        spectral_data <- data.frame(
          Metric = c("Dominant_Eigenvalue", "System_Size", "Matrix_Min", "Matrix_Max"),
          Value = c(
            values$spectral_results$lambda_max,
            nrow(values$matrix_A),
            min(values$matrix_A),
            max(values$matrix_A)
          )
        )
        write.csv(spectral_data, file, row.names = FALSE)
      }
    }
  )
  
  output$download_critical <- downloadHandler(
    filename = function() {
      paste0("critical_relationships_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$sensitivity_results)) {
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
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("dematel_comprehensive_report_", Sys.Date(), ".txt")
    },
    content = function(file) {
      report_text <- output$comprehensive_report()
      writeLines(report_text, file)
    }
  )
  
  # Refresh report
  observeEvent(input$refresh_report, {
    showNotification("📋 Report refreshed!", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)