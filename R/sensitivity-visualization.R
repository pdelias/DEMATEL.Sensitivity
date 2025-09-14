#' Visualize Sensitivity Analysis Results
#'
#' Creates comprehensive visualizations of DEMATEL sensitivity analysis,
#' including heatmaps, distributions, and critical relationships.
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param save_plots Logical. Whether to save plots to files (default: FALSE)
#' @param plot_dir Character. Directory to save plots (default: "plots")
#' @param show_values Logical. Whether to show values on heatmaps (default: TRUE for n <= 10)
#'
#' @return List of ggplot objects
#'
#' @details
#' Creates four main visualizations:
#' \itemize{
#'   \item Sensitivity heatmap (raw values with red/blue color scheme)
#'   \item Distribution histogram of sensitivity values
#'   \item Bar chart of top critical relationships
#'   \item Classical DEMATEL interrelationship map (prominence vs net effect)
#' }
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#' plots <- visualize_sensitivity(sens_obj)
#'
#' @export
visualize_sensitivity <- function(obj, save_plots = FALSE, plot_dir = "plots", show_values = NULL) {
  UseMethod("visualize_sensitivity")
}

#' @export
visualize_sensitivity.DEMATEL_Sensitivity <- function(obj, save_plots = FALSE, plot_dir = "plots", show_values = NULL) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }
  
  # Check if required packages are available
  required_packages <- c("ggplot2", "reshape2")
  missing_packages <- required_packages[!sapply(required_packages, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  })]
  
  if (length(missing_packages) > 0) {
    stop("Required packages not available: ", paste(missing_packages, collapse = ", "), 
         "\nPlease install with: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))")
  }
  
  if (save_plots && !dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
    cat("Created directory:", plot_dir, "\n")
  }
  
  # Auto-determine whether to show values based on matrix size
  if (is.null(show_values)) {
    show_values <- obj$n <= 10
  }
  
  # Prepare data for plotting
  sens_melted <- reshape2::melt(obj$sensitivity_matrix)
  names(sens_melted) <- c("From_Factor", "To_Factor", "Sensitivity")
  
  # Remove NA values for plotting
  sens_melted <- sens_melted[!is.na(sens_melted$Sensitivity), ]
  
  if (nrow(sens_melted) == 0) {
    stop("No valid sensitivity values to plot")
  }
  
  # 1. Sensitivity Heatmap (raw values)
  p1 <- ggplot2::ggplot(sens_melted, ggplot2::aes(x = To_Factor, y = From_Factor, fill = Sensitivity)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                                  midpoint = 0, name = "Sensitivity") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Sensitivity Matrix: ∂λmax/∂aij",
      subtitle = paste("Method:", obj$computation_method %||% "unknown"),
      x = "To Factor (j)",
      y = "From Factor (i)"
    )
  
  if (show_values) {
    p1 <- p1 + ggplot2::geom_text(ggplot2::aes(label = round(Sensitivity, 3)),
                                  size = 3, color = "black")
  }
  
  # 2. Distribution of sensitivity values
  p2 <- ggplot2::ggplot(sens_melted, ggplot2::aes(x = Sensitivity)) +
    ggplot2::geom_histogram(bins = max(20, min(50, nrow(sens_melted)/10)),
                            alpha = 0.7, fill = "steelblue", color = "white") +
    ggplot2::geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      title = "Distribution of Sensitivity Values",
      subtitle = paste("Mean:", round(mean(sens_melted$Sensitivity), 4),
                       "| SD:", round(sd(sens_melted$Sensitivity), 4)),
      x = "Sensitivity Value",
      y = "Frequency"
    )
  
  # 3. Top relationships
  p3 <- tryCatch({
    critical_rels <- identify_critical_relationships(obj, threshold_percentile = 80)
    
    if (nrow(critical_rels) > 0) {
      top_10 <- head(critical_rels, 10)
      top_10$relationship <- paste0(top_10$from_factor, " → ", top_10$to_factor)
      top_10$relationship <- factor(top_10$relationship, levels = rev(top_10$relationship))
      
      ggplot2::ggplot(top_10, ggplot2::aes(x = relationship, y = sensitivity, fill = interpretation)) +
        ggplot2::geom_col(alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c("Amplifier links" = "#9EDEC5", "Stabilizer links" = "#C81102"),
          name = "Effect Type"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.title = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 12)
        ) +
        ggplot2::labs(
          title = "Top 10 Most Critical Relationships",
          subtitle = "80th percentile threshold",
          x = "Relationship",
          y = "Sensitivity Value"
        ) +
        ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.3)
    } else {
      # Fallback plot if no critical relationships found
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No critical relationships\nfound at 80th percentile",
                          size = 6, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Critical Relationships")
    }
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error creating\ncritical relationships plot:\n", e$message),
                        size = 5, hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Critical Relationships - Error")
  })
  
  # 4. Classical DEMATEL Interrelationship Map
  p4 <- tryCatch({
    create_dematel_interrelationship_map(obj)
  }, error = function(e) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error creating\ninterrelationship map:\n", e$message),
                        size = 5, hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Interrelationship Map - Error")
  })
  
  # Combine plots
  combined_plot <- NULL
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    tryCatch({
      combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    }, error = function(e) {
      warning("Could not create combined plot: ", e$message)
      combined_plot <- NULL
    })
  }
  
  # Save plots if requested
  if (save_plots) {
    tryCatch({
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        ggplot2::ggsave(file.path(plot_dir, "sensitivity_heatmap.png"), p1,
                        width = 10, height = 8, dpi = 300)
        ggplot2::ggsave(file.path(plot_dir, "sensitivity_distribution.png"), p2,
                        width = 8, height = 6, dpi = 300)
        ggplot2::ggsave(file.path(plot_dir, "top_relationships.png"), p3,
                        width = 10, height = 8, dpi = 300)
        ggplot2::ggsave(file.path(plot_dir, "dematel_interrelationship_map.png"), p4,
                        width = 10, height = 8, dpi = 300)
        
        if (!is.null(combined_plot)) {
          ggplot2::ggsave(file.path(plot_dir, "combined_sensitivity_analysis.png"), combined_plot,
                          width = 16, height = 12, dpi = 300)
        }
        
        cat("Plots saved to:", plot_dir, "\n")
      }
    }, error = function(e) {
      warning("Error saving plots: ", e$message)
    })
  }
  
  plot_list <- list(
    sensitivity_heatmap = p1,
    distribution = p2,
    top_relationships = p3,
    interrelationship_map = p4
  )
  
  if (!is.null(combined_plot)) {
    plot_list$combined = combined_plot
  }
  
  return(plot_list)
}

#' Create Classical DEMATEL Interrelationship Map
#'
#' Creates the classical DEMATEL scatter plot showing factors positioned by
#' prominence (r+c) vs net effect (r-c).
#'
#' @param obj DEMATEL_Sensitivity object with computed matrices
#'
#' @return ggplot object showing the interrelationship map
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' map_plot <- create_dematel_interrelationship_map(sens_obj)
#'
#' @export
create_dematel_interrelationship_map <- function(obj) {
  UseMethod("create_dematel_interrelationship_map")
}

#' @export
create_dematel_interrelationship_map.DEMATEL_Sensitivity <- function(obj) {
  if (is.null(obj$T)) {
    stop("Total relations matrix T is required for interrelationship map")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for interrelationship map")
  }
  
  # Calculate r (row sums) and c (column sums) from T matrix
  r <- rowSums(obj$T)  # Total effects given
  c <- colSums(obj$T)  # Total effects received
  
  # Calculate prominence and net effect
  prominence <- r + c  # Total involvement in the system
  net_effect <- r - c  # Net influence (positive = more cause, negative = more effect)
  
  # Create data frame for plotting
  map_data <- data.frame(
    factor = obj$factor_names,
    prominence = prominence,
    net_effect = net_effect,
    quadrant = ifelse(net_effect > 0, 
                      ifelse(prominence > mean(prominence), "High Cause", "Low Cause"),
                      ifelse(prominence > mean(prominence), "High Effect", "Low Effect"))
  )
  
  # Create the plot
  p <- ggplot2::ggplot(map_data, ggplot2::aes(x = prominence, y = net_effect)) +
    # Add quadrant background
    ggplot2::geom_hline(yintercept = 0, color = "gray50", linetype = "dashed", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mean(prominence), color = "gray50", linetype = "dashed", alpha = 0.7) +
    
    # Add points
    ggplot2::geom_point(ggplot2::aes(color = quadrant), size = 4, alpha = 0.8) +
    
    # Add factor labels
    ggrepel::geom_text_repel(ggplot2::aes(label = factor), 
                             size = 3, fontface = "bold",
                             box.padding = 0.5, 
                             point.padding = 0.3,
                             max.overlaps = Inf,
                             min.segment.length = 0) +
    
    # Customize colors
    ggplot2::scale_color_manual(
      values = c(
        "High Cause" = "#9EDEC5",    # Red - high prominence, net cause
        "Low Cause" = "#F5DEB3",     # Orange - low prominence, net cause  
        "High Effect" = "#77BDD9",   # Blue - high prominence, net effect
        "Low Effect" = "#295073"     # Green - low prominence, net effect
      ),
      name = "Quadrant"
    ) +
    
    # Theme and labels
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 10),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "DEMATEL Interrelationship Map",
      subtitle = "Factor positioning by prominence and net influence",
      x = "Prominence (r + c): Total Involvement",
      y = "Net Effect (r - c): Cause ↑ / Effect ↓"
    )
  
  # Add quadrant labels with safe coordinate calculation
  tryCatch({
    x_range <- range(prominence)
    y_range <- range(net_effect)
    
    p <- p +
      ggplot2::annotate("text", x = x_range[2] * 0.9, y = y_range[2] * 0.9, 
                        label = "High Prominence\nNet Cause", 
                        size = 3, alpha = 0.6, fontface = "italic") +
      ggplot2::annotate("text", x = x_range[1] * 1.1, y = y_range[2] * 0.9, 
                        label = "Low Prominence\nNet Cause", 
                        size = 3, alpha = 0.6, fontface = "italic") +
      ggplot2::annotate("text", x = x_range[2] * 0.9, y = y_range[1] * 0.9, 
                        label = "High Prominence\nNet Effect", 
                        size = 3, alpha = 0.6, fontface = "italic") +
      ggplot2::annotate("text", x = x_range[1] * 1.1, y = y_range[1] * 0.9, 
                        label = "Low Prominence\nNet Effect", 
                        size = 3, alpha = 0.6, fontface = "italic")
  }, error = function(e) {
    # Skip quadrant labels if there's an error
    warning("Could not add quadrant labels: ", e$message)
  })
  
  return(p)
}

#' Create Network Visualization of DEMATEL Cause-Effect Relationships
#'
#' Creates a network diagram showing the most significant DEMATEL cause-effect relationships
#' based on the total relations matrix T, with proper directed edges from cause to effect.
#'
#' @param obj DEMATEL_Sensitivity object with computed matrices
#' @param threshold_percentile Numeric. Percentile threshold for significant relationships (default: 90)
#' @param layout Character. Network layout algorithm - "circle", "spring", or "hierarchical" (default: "spring")
#'
#' @return ggplot object showing network diagram with directed edges
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' network_plot <- plot_dematel_network(sens_obj)
#'
#' @export
plot_dematel_network <- function(obj, threshold_percentile = 90, layout = "spring") {
  UseMethod("plot_dematel_network")
}

#' @export
plot_dematel_network.DEMATEL_Sensitivity <- function(obj, threshold_percentile = 90, layout = "spring") {
  if (is.null(obj$T)) {
    stop("Total relations matrix T is required for network visualization")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for network visualization")
  }
  
  # Get significant relationships from T matrix (cause-effect relationships)
  T_values <- as.vector(obj$T)
  T_values <- T_values[T_values > 0]  # Only positive relationships
  
  if (length(T_values) == 0) {
    stop("No positive relationships found in T matrix")
  }
  
  threshold <- quantile(T_values, threshold_percentile / 100, na.rm = TRUE)
  
  # Find relationships above threshold
  significant_indices <- which(obj$T >= threshold, arr.ind = TRUE)
  
  if (nrow(significant_indices) == 0) {
    stop("No significant relationships found at the specified threshold")
  }
  
  # Create relationships data frame - CORRECTED CAUSAL DIRECTION
  # T[i,j] means factor i influences factor j, so i is cause, j is effect
  relationships <- data.frame(
    cause_factor = obj$factor_names[significant_indices[, 1]],  # Row index = cause
    effect_factor = obj$factor_names[significant_indices[, 2]], # Column index = effect
    cause_index = significant_indices[, 1],
    effect_index = significant_indices[, 2],
    strength = obj$T[significant_indices],
    stringsAsFactors = FALSE
  )
  
  # Create node positions based on layout
  n <- obj$n
  if (layout == "circle") {
    angles <- seq(0, 2*pi, length.out = n + 1)[1:n]
    node_positions <- data.frame(
      factor = obj$factor_names,
      x = cos(angles),
      y = sin(angles)
    )
  } else if (layout == "spring") {
    # Simple spring layout approximation
    set.seed(42)  # For reproducibility
    node_positions <- data.frame(
      factor = obj$factor_names,
      x = runif(n, -1, 1),
      y = runif(n, -1, 1)
    )
  } else if (layout == "hierarchical") {
    # Simple hierarchical layout
    cols <- ceiling(sqrt(n))
    rows <- ceiling(n / cols)
    node_positions <- data.frame(
      factor = obj$factor_names,
      x = rep(1:cols, length.out = n),
      y = rep(1:rows, each = cols, length.out = n)
    )
  } else {
    stop("Layout must be 'circle', 'spring', or 'hierarchical'")
  }
  
  # Prepare edge data with proper start/end coordinates - CORRECTED
  # Edges go from cause to effect
  edges <- relationships
  # Merge cause positions
  edges <- merge(edges, node_positions, by.x = "cause_factor", by.y = "factor")
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_start", "y_start")
  # Merge effect positions  
  edges <- merge(edges, node_positions, by.x = "effect_factor", by.y = "factor")
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_end", "y_end")
  
  # Adjust edge endpoints to stop before reaching the node centers
  # This prevents arrows from being hidden behind nodes
  edge_offset <- 0.08  # Distance to stop before node center
  
  # Calculate shortened edge endpoints
  edges$edge_length <- sqrt((edges$x_end - edges$x_start)^2 + (edges$y_end - edges$y_start)^2)
  edges$x_end_adj <- edges$x_end - edge_offset * (edges$x_end - edges$x_start) / edges$edge_length
  edges$y_end_adj <- edges$y_end - edge_offset * (edges$y_end - edges$y_start) / edges$edge_length
  edges$x_start_adj <- edges$x_start + edge_offset * (edges$x_end - edges$x_start) / edges$edge_length
  edges$y_start_adj <- edges$y_start + edge_offset * (edges$y_end - edges$y_start) / edges$edge_length
  
  # Handle any NaN values from zero-length edges
  edges$x_end_adj[!is.finite(edges$x_end_adj)] <- edges$x_end[!is.finite(edges$x_end_adj)]
  edges$y_end_adj[!is.finite(edges$y_end_adj)] <- edges$y_end[!is.finite(edges$y_end_adj)]
  edges$x_start_adj[!is.finite(edges$x_start_adj)] <- edges$x_start[!is.finite(edges$x_start_adj)]
  edges$y_start_adj[!is.finite(edges$y_start_adj)] <- edges$y_start[!is.finite(edges$y_start_adj)]
  
  # Create the plot
  p <- ggplot2::ggplot() +
    # Draw edges (cause → effect relationships) with better arrows
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x_start_adj, y = y_start_adj, 
                   xend = x_end_adj, yend = y_end_adj,
                   size = strength),  # Use 'size' instead of 'linewidth' for compatibility
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
      alpha = 0.8, color = "#2166ac"
    ) +
    # Draw nodes
    ggplot2::geom_point(
      data = node_positions,
      ggplot2::aes(x = x, y = y),
      size = 20, color = "white", fill = "#5e3c99",
      shape = 21, stroke = 2
    ) +
    # Add node labels
    ggplot2::geom_text(
      data = node_positions,
      ggplot2::aes(x = x, y = y, label = factor),
      size = 2.5, fontface = "bold", color = "white"
    ) +
    ggplot2::scale_size_continuous(
      name = "Relationship\nStrength",
      range = c(0.8, 2.5)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "DEMATEL Cause-Effect Network",
      subtitle = paste("Threshold:", threshold_percentile, "percentile |",
                       nrow(relationships), "relationships shown | Edges: Cause → Effect")
    ) +
    ggplot2::coord_fixed()
  
  return(p)
}

# Backward compatibility function
#' @export
plot_sensitivity_network <- function(obj, threshold_percentile = 90, layout = "spring") {
  warning("plot_sensitivity_network is deprecated. Use plot_dematel_network instead.")
  plot_dematel_network(obj, threshold_percentile, layout)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}