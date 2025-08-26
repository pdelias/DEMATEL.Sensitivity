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
#'   \item Absolute sensitivity heatmap (magnitude only)
#'   \item Distribution histogram of sensitivity values
#'   \item Bar chart of top critical relationships
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

  # Check if ggplot2 and related packages are available
  required_packages <- c("ggplot2", "reshape2", "viridis", "gridExtra")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop("Required packages not available: ", paste(missing_packages, collapse = ", "))
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

  # 2. Absolute Sensitivity Heatmap
  abs_sens_melted <- sens_melted
  abs_sens_melted$Sensitivity <- abs(abs_sens_melted$Sensitivity)

  p2 <- ggplot2::ggplot(abs_sens_melted, ggplot2::aes(x = To_Factor, y = From_Factor, fill = Sensitivity)) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    viridis::scale_fill_viridis(name = "Abs. Sensitivity", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Absolute Sensitivity: |∂λmax/∂aij|",
      subtitle = "Magnitude of sensitivity (impact strength)",
      x = "To Factor (j)",
      y = "From Factor (i)"
    )

  if (show_values) {
    p2 <- p2 + ggplot2::geom_text(ggplot2::aes(label = round(Sensitivity, 3)),
                                  size = 3, color = "white")
  }

  # 3. Distribution of sensitivity values
  p3 <- ggplot2::ggplot(sens_melted, ggplot2::aes(x = Sensitivity)) +
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

  # 4. Top relationships
  tryCatch({
    critical_rels <- identify_critical_relationships(obj, threshold_percentile = 80)

    if (nrow(critical_rels) > 0) {
      top_10 <- head(critical_rels, 10)
      top_10$relationship <- paste0(top_10$from_factor, " → ", top_10$to_factor)
      top_10$relationship <- factor(top_10$relationship, levels = rev(top_10$relationship))

      p4 <- ggplot2::ggplot(top_10, ggplot2::aes(x = relationship, y = sensitivity, fill = interpretation)) +
        ggplot2::geom_col(alpha = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c("Amplifying" = "#E31A1C", "Dampening" = "#1F78B4"),
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
      p4 <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No critical relationships\nfound at 80th percentile",
                          size = 6, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::labs(title = "Critical Relationships")
    }
  }, error = function(e) {
    p4 <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error creating\ncritical relationships plot:\n", e$message),
                        size = 5, hjust = 0.5, vjust = 0.5) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Critical Relationships - Error")
  })

  # Combine plots
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    tryCatch({
      combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
    }, error = function(e) {
      warning("Could not create combined plot: ", e$message)
      combined_plot <- NULL
    })
  } else {
    combined_plot <- NULL
  }

  # Save plots if requested
  if (save_plots) {
    tryCatch({
      ggplot2::ggsave(file.path(plot_dir, "sensitivity_heatmap.png"), p1,
                      width = 10, height = 8, dpi = 300)
      ggplot2::ggsave(file.path(plot_dir, "abs_sensitivity_heatmap.png"), p2,
                      width = 10, height = 8, dpi = 300)
      ggplot2::ggsave(file.path(plot_dir, "sensitivity_distribution.png"), p3,
                      width = 8, height = 6, dpi = 300)
      ggplot2::ggsave(file.path(plot_dir, "top_relationships.png"), p4,
                      width = 10, height = 8, dpi = 300)

      if (!is.null(combined_plot)) {
        ggplot2::ggsave(file.path(plot_dir, "combined_sensitivity_analysis.png"), combined_plot,
                        width = 16, height = 12, dpi = 300)
      }

      cat("Plots saved to:", plot_dir, "\n")
    }, error = function(e) {
      warning("Error saving plots: ", e$message)
    })
  }

  plot_list <- list(
    sensitivity_heatmap = p1,
    absolute_heatmap = p2,
    distribution = p3,
    top_relationships = p4
  )

  if (!is.null(combined_plot)) {
    plot_list$combined = combined_plot
  }

  return(plot_list)
}

#' Create Network Visualization of Critical Relationships
#'
#' Creates a network diagram showing the most critical sensitivity relationships.
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param threshold_percentile Numeric. Percentile threshold for critical relationships (default: 90)
#' @param layout Character. Network layout algorithm - "circle", "spring", or "hierarchical" (default: "spring")
#'
#' @return ggplot object showing network diagram
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#' network_plot <- plot_sensitivity_network(sens_obj)
#'
#' @export
plot_sensitivity_network <- function(obj, threshold_percentile = 90, layout = "spring") {
  UseMethod("plot_sensitivity_network")
}

#' @export
plot_sensitivity_network.DEMATEL_Sensitivity <- function(obj, threshold_percentile = 90, layout = "spring") {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for network visualization")
  }

  # Get critical relationships
  critical <- identify_critical_relationships(obj, threshold_percentile)

  if (nrow(critical) == 0) {
    stop("No critical relationships found at the specified threshold")
  }

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

  # Prepare edge data
  edges <- critical
  edges <- merge(edges, node_positions, by.x = "from_factor", by.y = "factor")
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_start", "y_start")
  edges <- merge(edges, node_positions, by.x = "to_factor", by.y = "factor")
  names(edges)[names(edges) %in% c("x", "y")] <- c("x_end", "y_end")

  # Create the plot
  p <- ggplot2::ggplot() +
    # Draw edges
    ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                   color = interpretation, linewidth = abs_sensitivity),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc"), type = "closed"),
      alpha = 0.7
    ) +
    # Draw nodes
    ggplot2::geom_point(
      data = node_positions,
      ggplot2::aes(x = x, y = y),
      size = 8, color = "white", fill = "lightblue",
      shape = 21, stroke = 2
    ) +
    # Add node labels
    ggplot2::geom_text(
      data = node_positions,
      ggplot2::aes(x = x, y = y, label = factor),
      size = 3, fontface = "bold"
    ) +
    ggplot2::scale_color_manual(
      values = c("Amplifying" = "#E31A1C", "Dampening" = "#1F78B4"),
      name = "Effect Type"
    ) +
    ggplot2::scale_size_continuous(
      name = "Sensitivity\nMagnitude",
      range = c(0.5, 3)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::labs(
      title = "Critical Sensitivity Relationships Network",
      subtitle = paste("Threshold:", threshold_percentile, "percentile |",
                       nrow(critical), "relationships shown")
    ) +
    ggplot2::coord_fixed()

  return(p)
}
