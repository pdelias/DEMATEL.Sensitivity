# The classical DEMATEL interrelationship map.
#
# This file held four S3 generics and only ever used one. visualize_sensitivity,
# plot_dematel_network and plot_sensitivity_network were never called from
# anywhere, and visualize_sensitivity could not have worked if they had been: it
# referenced app.R's `values` reactive from outside the server function, so it
# would have errored on its first plot. Roughly 390 lines, shipping in every
# WebAssembly bundle.
#
# Deleting it does not shrink that bundle. It was the only direct caller of
# gridExtra, but ggrepel and viridis both pull gridExtra in anyway, so the
# package still ships -- checked, rather than assumed from the dependency list
# getting shorter.
#
# What survives is the map app.R actually draws.

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
    # Full opacity: the palette above was validated at its stated values, and
    # alpha renders something 20% lighter than what was measured.
    ggplot2::geom_point(ggplot2::aes(color = quadrant), size = 4) +
    
    # Add factor labels
    ggrepel::geom_text_repel(ggplot2::aes(label = factor), 
                             size = 3, fontface = "bold",
                             box.padding = 0.5, 
                             point.padding = 0.3,
                             max.overlaps = Inf,
                             min.segment.length = 0) +
    
    # Quadrant colours.
    #
    # This is a categorical palette, not the diverging one the signed charts
    # use, and it deliberately borrows no hue from them: copper and navy mean
    # amplifying and stabilizing over there, and a quadrant is neither.
    #
    # The previous four were mint, wheat, pale blue and navy, and the comments
    # beside them said red, orange, blue and green -- left over from a palette
    # that had already been replaced twice. They separated by dE 4.2 for
    # protanopes and 10.9 for normal vision, so High Cause and Low Cause were
    # effectively one colour.
    #
    # These four measure, across all pairs: dE 18.7 normal vision, 7.6 at worst
    # for deuteranopes. That 7.6 sits in the band that is admissible only with a
    # second identity channel, which this plot has -- every point carries its
    # factor name in bold via geom_text_repel, with a leader line. Remove those
    # labels and this palette is no longer legal.
    ggplot2::scale_color_manual(
      values = c(
        "High Cause"  = "#E69F00",   # amber
        "Low Cause"   = "#CC79A7",   # mauve
        "High Effect" = "#0072B2",   # blue
        "Low Effect"  = "#009E73"    # green
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
