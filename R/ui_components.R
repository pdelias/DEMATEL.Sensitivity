# UI Helper Functions for DEMATEL Sensitivity Analysis Shiny App
# File: R/ui_components.R

#' Create a "no data" message box
#'
#' @param message Character. The message to display
#' @return HTML div element with styled message
no_data_message <- function(message) {
  fluidRow(
    box(
      title = "⚠️ Data Required", 
      status = "warning", 
      solidHeader = TRUE,
      width = 12,
      
      div(
        style = "text-align: center; padding: 50px;",
        icon("exclamation-triangle", style = "font-size: 48px; color: #f39c12;"),
        h4(message, style = "color: #856404; margin-top: 20px;"),
        p("Complete the previous steps to access this analysis.", style = "color: #856404;")
      )
    )
  )
}

#' Create an info box with icon
#'
#' @param title Character. Title of the info box
#' @param value Character or numeric. Value to display
#' @param subtitle Character. Optional subtitle
#' @param icon Character. Icon name (FontAwesome)
#' @param color Character. Box color theme
#' @return infoBox element
create_info_box <- function(title, value, subtitle = NULL, icon = "info-circle", color = "blue") {
  infoBox(
    title = title,
    value = value,
    subtitle = subtitle,
    icon = icon(icon),
    color = color,
    width = NULL,
    href = NULL
  )
}

#' Create a metric summary card
#'
#' @param title Character. Card title
#' @param metrics Named list. Metrics to display
#' @param status Character. Box status color
#' @return box element
create_metric_card <- function(title, metrics, status = "primary") {
  metric_rows <- lapply(names(metrics), function(name) {
    div(
      class = "metric-row",
      style = "display: flex; justify-content: space-between; padding: 5px 0; border-bottom: 1px solid #eee;",
      span(name, style = "font-weight: 500;"),
      span(as.character(metrics[[name]]), style = "color: #666;")
    )
  })
  
  box(
    title = title,
    status = status,
    solidHeader = TRUE,
    width = NULL,
    
    div(
      class = "metrics-container",
      style = "padding: 10px;",
      metric_rows
    )
  )
}

#' Create a loading spinner
#'
#' @param message Character. Loading message
#' @return HTML div with spinner
loading_spinner <- function(message = "Processing...") {
  div(
    style = "text-align: center; padding: 50px;",
    tags$i(class = "fa fa-spinner fa-spin", style = "font-size: 48px; color: #3c8dbc;"),
    h4(message, style = "color: #3c8dbc; margin-top: 20px;")
  )
}

#' Create a status alert
#'
#' @param message Character. Alert message
#' @param type Character. Alert type: "success", "info", "warning", "danger"
#' @return HTML div with alert styling
status_alert <- function(message, type = "info") {
  type_colors <- list(
    "success" = list(bg = "#d4edda", border = "#c3e6cb", text = "#155724", icon = "check-circle"),
    "info" = list(bg = "#d1ecf1", border = "#bee5eb", text = "#0c5460", icon = "info-circle"),
    "warning" = list(bg = "#fff3cd", border = "#ffeaa7", text = "#856404", icon = "exclamation-triangle"),
    "danger" = list(bg = "#f8d7da", border = "#f5c6cb", text = "#721c24", icon = "exclamation-circle")
  )
  
  colors <- type_colors[[type]]
  
  div(
    class = paste0("alert alert-", type),
    style = paste0(
      "padding: 15px; margin-bottom: 20px; border: 1px solid ", colors$border, "; ",
      "border-radius: 4px; background-color: ", colors$bg, "; color: ", colors$text, ";"
    ),
    
    icon(colors$icon, style = paste0("margin-right: 10px; color: ", colors$text, ";")),
    span(message)
  )
}

#' Create a progress indicator
#'
#' @param current Integer. Current step
#' @param total Integer. Total steps
#' @param steps Character vector. Step names
#' @return HTML div with progress indicator
progress_indicator <- function(current, total, steps) {
  step_elements <- lapply(1:total, function(i) {
    is_complete <- i < current
    is_current <- i == current
    
    step_class <- if (is_complete) {
      "step-complete"
    } else if (is_current) {
      "step-current"
    } else {
      "step-pending"
    }
    
    step_style <- if (is_complete) {
      "background-color: #28a745; color: white;"
    } else if (is_current) {
      "background-color: #007bff; color: white;"
    } else {
      "background-color: #f8f9fa; color: #6c757d;"
    }
    
    div(
      class = paste("step", step_class),
      style = paste0(
        "display: inline-block; width: 30px; height: 30px; border-radius: 50%; ",
        "text-align: center; line-height: 30px; margin: 0 10px; ",
        step_style
      ),
      
      span(i),
      
      if (i <= length(steps)) {
        div(
          style = "font-size: 12px; margin-top: 5px; text-align: center;",
          steps[i]
        )
      }
    )
  })
  
  div(
    class = "progress-indicator",
    style = "text-align: center; padding: 20px;",
    step_elements
  )
}

#' Create a collapsible help section
#'
#' @param title Character. Section title
#' @param content Character or HTML. Help content
#' @param collapsed Logical. Whether to start collapsed
#' @return box element with collapsible content
help_section <- function(title, content, collapsed = TRUE) {
  box(
    title = paste("❓", title),
    status = "info",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = collapsed,
    width = NULL,
    
    div(
      style = "padding: 10px;",
      if (is.character(content)) {
        HTML(content)
      } else {
        content
      }
    )
  )
}

#' Create a download card with multiple format options
#'
#' @param title Character. Card title
#' @param downloads List. Named list of download buttons
#' @return box element with download options
download_card <- function(title, downloads) {
  download_buttons <- lapply(names(downloads), function(name) {
    div(
      style = "margin-bottom: 10px;",
      downloads[[name]]
    )
  })
  
  box(
    title = paste("📥", title),
    status = "success",
    solidHeader = TRUE,
    width = NULL,
    
    div(
      style = "padding: 15px;",
      download_buttons
    )
  )
}

#' Create a validation message
#'
#' @param checks List. Named list of validation results (TRUE/FALSE)
#' @param messages List. Named list of validation messages
#' @return HTML div with validation results
validation_summary <- function(checks, messages) {
  validation_items <- lapply(names(checks), function(name) {
    is_valid <- checks[[name]]
    message <- messages[[name]]
    
    div(
      class = "validation-item",
      style = "padding: 5px 0;",
      
      if (is_valid) {
        span(
          icon("check", style = "color: #28a745; margin-right: 10px;"),
          message,
          style = "color: #28a745;"
        )
      } else {
        span(
          icon("times", style = "color: #dc3545; margin-right: 10px;"),
          message,
          style = "color: #dc3545;"
        )
      }
    )
  })
  
  div(
    class = "validation-summary",
    style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;",
    
    h5("Validation Results:", style = "margin-top: 0;"),
    validation_items
  )
}

#' Create a feature comparison table
#'
#' @param features Data frame. Feature comparison data
#' @return HTML table with styled comparison
feature_table <- function(features) {
  if (!is.data.frame(features)) {
    stop("Features must be a data frame")
  }
  
  # Create table header
  header_row <- tags$tr(
    lapply(names(features), function(col_name) {
      tags$th(col_name, style = "padding: 12px; background-color: #f8f9fa; border-bottom: 2px solid #dee2e6;")
    })
  )
  
  # Create table rows
  data_rows <- lapply(1:nrow(features), function(i) {
    row_data <- features[i, ]
    
    tags$tr(
      lapply(1:ncol(features), function(j) {
        cell_value <- row_data[[j]]
        
        # Style cells based on content
        cell_style <- "padding: 10px; border-bottom: 1px solid #dee2e6;"
        
        if (is.logical(cell_value)) {
          if (cell_value) {
            cell_content <- icon("check", style = "color: #28a745;")
            cell_style <- paste0(cell_style, " text-align: center;")
          } else {
            cell_content <- icon("times", style = "color: #dc3545;")
            cell_style <- paste0(cell_style, " text-align: center;")
          }
        } else {
          cell_content <- as.character(cell_value)
        }
        
        tags$td(cell_content, style = cell_style)
      })
    )
  })
  
  tags$table(
    class = "table table-hover",
    style = "width: 100%; border-collapse: collapse; margin: 15px 0;",
    
    tags$thead(header_row),
    tags$tbody(data_rows)
  )
}

#' Create a methodology comparison card
#'
#' @param methods Data frame. Comparison of different methods
#' @return box element with method comparison
methodology_card <- function(methods = NULL) {
  if (is.null(methods)) {
    methods <- data.frame(
      Method = c("Numerical", "Analytical"),
      Accuracy = c("High", "Medium"),
      Speed = c("Slow", "Fast"),
      `Memory Usage` = c("Low", "Medium"),
      `Matrix Size Limit` = c("Large (1000+)", "Medium (100)"),
      Stability = c("Very Stable", "Depends on Eigenvalues"),
      check.names = FALSE
    )
  }
  
  box(
    title = "🔬 Methodology Comparison",
    status = "info",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = NULL,
    
    p("Compare different sensitivity analysis methods:", style = "margin-bottom: 15px;"),
    
    feature_table(methods),
    
    div(
      style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
      strong("Recommendation: "),
      "Use Numerical method for final analysis and Analytical for quick exploration."
    )
  )
}

#' Create a tips and tricks section
#'
#' @param tips Character vector. List of tips
#' @return box element with tips
tips_card <- function(tips) {
  tip_items <- lapply(tips, function(tip) {
    div(
      style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #007bff; border-radius: 3px;",
      icon("lightbulb", style = "color: #ffc107; margin-right: 10px;"),
      span(tip)
    )
  })
  
  box(
    title = "💡 Tips & Best Practices",
    status = "warning",
    solidHeader = TRUE,
    collapsible = TRUE,
    width = NULL,
    
    div(tip_items)
  )
}

#' Create a keyboard shortcuts reference
#'
#' @return box element with keyboard shortcuts
shortcuts_card <- function() {
  shortcuts <- data.frame(
    Action = c(
      "Navigate tabs",
      "Download current results", 
      "Refresh analysis",
      "Toggle sidebar",
      "Full screen plots"
    ),
    Shortcut = c(
      "Ctrl + ← / →",
      "Ctrl + D",
      "Ctrl + R", 
      "Ctrl + Shift + S",
      "Double-click plot"
    ),
    Description = c(
      "Move between analysis tabs",
      "Quick download of current view",
      "Re-run current analysis",
      "Show/hide navigation menu",
      "Expand plot to full screen"
    )
  )
  
  box(
    title = "⌨️ Keyboard Shortcuts",
    status = "info",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    
    feature_table(shortcuts)
  )
}

#' Create an FAQ section
#'
#' @return box element with frequently asked questions
faq_card <- function() {
  faqs <- list(
    list(
      question = "What file format should I use for uploading?",
      answer = "CSV files work best. Ensure your matrix is square and contains only numeric values. Diagonal elements should be zero."
    ),
    list(
      question = "How large can my matrix be?",
      answer = "For numerical method: up to 1000×1000. For analytical method: up to 100×100 recommended. Larger matrices may be slow."
    ),
    list(
      question = "What does a negative sensitivity mean?",
      answer = "Negative sensitivity means strengthening that relationship will decrease the dominant eigenvalue (dampening effect)."
    ),
    list(
      question = "Which analysis method should I choose?",
      answer = "Numerical method is more accurate but slower. Use it for final analysis. Analytical is faster for exploration."
    ),
    list(
      question = "How do I interpret intervention results?",
      answer = "Higher efficiency means smaller changes needed. Focus on feasible interventions (non-negative values)."
    )
  )
  
  faq_items <- lapply(faqs, function(faq) {
    div(
      style = "margin-bottom: 15px; border-bottom: 1px solid #eee; padding-bottom: 15px;",
      
      h5(
        icon("question-circle", style = "color: #007bff; margin-right: 10px;"),
        faq$question,
        style = "color: #495057; margin-bottom: 10px;"
      ),
      
      div(
        style = "margin-left: 30px; color: #6c757d;",
        faq$answer
      )
    )
  })
  
  box(
    title = "❓ Frequently Asked Questions",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    
    div(faq_items)
  )
}

#' Create a glossary of terms
#'
#' @return box element with term definitions
glossary_card <- function() {
  terms <- list(
    list(
      term = "Dominant Eigenvalue (λmax)",
      definition = "The largest eigenvalue of the total relations matrix T. Controls system amplification behavior."
    ),
    list(
      term = "Sensitivity ∂λmax/∂aij",
      definition = "How much the dominant eigenvalue changes when relationship aij changes by a small amount."
    ),
    list(
      term = "Amplifying Relationship",
      definition = "A relationship with positive sensitivity - strengthening it increases system amplification."
    ),
    list(
      term = "Dampening Relationship", 
      definition = "A relationship with negative sensitivity - strengthening it decreases system amplification."
    ),
    list(
      term = "Critical Relationship",
      definition = "A relationship in the top percentile of absolute sensitivity values."
    ),
    list(
      term = "Intervention Efficiency",
      definition = "Ratio of target effect to required change. Higher efficiency means smaller intervention needed."
    ),
    list(
      term = "Spectral Radius",
      definition = "Maximum absolute value of all eigenvalues. Indicates system stability boundary."
    ),
    list(
      term = "Condition Number",
      definition = "Ratio of largest to smallest eigenvalue. High values indicate sensitivity to perturbations."
    )
  )
  
  term_items <- lapply(terms, function(term_info) {
    div(
      style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 5px;",
      
      h6(
        strong(term_info$term),
        style = "color: #495057; margin-bottom: 8px;"
      ),
      
      div(
        style = "color: #6c757d; font-size: 14px;",
        term_info$definition
      )
    )
  })
  
  box(
    title = "📚 Glossary of Terms",
    status = "success",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,
    width = NULL,
    
    div(term_items)
  )
}

#' Create example data information card
#'
#' @return box element describing example data
example_data_card <- function() {
  box(
    title = "📊 About the Example Dataset",
    status = "info",
    solidHeader = TRUE,
    width = NULL,
    
    h4("Organizational System Model"),
    p("The example dataset represents a 5-factor organizational effectiveness system:"),
    
    tags$ul(
      tags$li(strong("Leadership: "), "Strategic direction and decision-making capability"),
      tags$li(strong("Communication: "), "Information flow and organizational transparency"),  
      tags$li(strong("Innovation: "), "Creativity and adaptation to change"),
      tags$li(strong("Risk Management: "), "Identification and mitigation of threats"),
      tags$li(strong("Quality: "), "Standards and continuous improvement processes")
    ),
    
    h4("Matrix Characteristics", style = "margin-top: 20px;"),
    tags$ul(
      tags$li("Values range from 0-4 (typical DEMATEL scale)"),
      tags$li("Zero diagonal (no self-influence)"),
      tags$li("Realistic organizational relationship patterns"),
      tags$li("Demonstrates both amplifying and dampening effects")
    ),
    
    div(
      style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-top: 15px;",
      icon("info-circle", style = "color: #31708f; margin-right: 10px;"),
      strong("Perfect for Learning: "),
      "This dataset demonstrates all application features and provides realistic results for practice."
    )
  )
}