#' Enhanced DEMATEL Analysis with Spectral and Sensitivity Analysis
#'
#' Extended version of analyze_dematel_files that includes both spectral
#' analysis and optional sensitivity analysis.
#'
#' @param d_file Path to normalized direct influence matrix (D) CSV file
#' @param t_file Path to total relations matrix (T) CSV file
#' @param case_name Optional identifier for your analysis
#' @param header Whether CSV files contain headers (default: FALSE)
#' @param verbose Whether to print progress messages (default: TRUE)
#' @param include_sensitivity Whether to include sensitivity analysis (default: FALSE)
#' @param sensitivity_method Method for sensitivity computation: "numerical" or "analytical" (default: "numerical")
#' @param sensitivity_epsilon Step size for numerical sensitivity (default: 0.01)
#'
#' @return List containing both spectral and sensitivity analysis results
#'
#' @details
#' This enhanced function provides:
#' \itemize{
#'   \item All original spectral analysis features
#'   \item Optional sensitivity analysis of the dominant eigenvalue
#'   \item Critical relationship identification
#'   \item Intervention analysis capabilities
#'   \item Integrated metrics extraction
#' }
#'
#' @examples
#' # Basic spectral analysis (original functionality)
#' results <- analyze_dematel_files_enhanced("D_matrix.csv", "T_matrix.csv")
#'
#' # With sensitivity analysis
#' results <- analyze_dematel_files_enhanced("D_matrix.csv", "T_matrix.csv",
#'                                         include_sensitivity = TRUE)
#'
#' # Access sensitivity results
#' summary(results$sensitivity)
#' plots <- visualize_sensitivity(results$sensitivity)
#'
#' @export
analyze_dematel_files_enhanced <- function(d_file, t_file, case_name = NULL,
                                           header = FALSE, verbose = TRUE,
                                           include_sensitivity = FALSE,
                                           sensitivity_method = "numerical",
                                           sensitivity_epsilon = 0.01) {

  # Input validation
  if (!file.exists(d_file)) {
    stop("D matrix file not found: ", d_file)
  }
  if (!file.exists(t_file)) {
    stop("T matrix file not found: ", t_file)
  }

  if (!sensitivity_method %in% c("numerical", "analytical")) {
    stop("sensitivity_method must be 'numerical' or 'analytical'")
  }

  if (verbose) {
    cat("Enhanced DEMATEL Analysis\n")
    cat("========================\n")
    if (!is.null(case_name)) {
      cat("Case:", case_name, "\n")
    }
    cat("D matrix file:", d_file, "\n")
    cat("T matrix file:", t_file, "\n")
    cat("Sensitivity analysis:", ifelse(include_sensitivity, "Yes", "No"), "\n")
    if (include_sensitivity) {
      cat("Sensitivity method:", sensitivity_method, "\n")
    }
    cat("\n")
  }

  # Load matrices using existing function (if it exists)
  if (exists("load_dematel_matrices", mode = "function")) {
    matrices <- load_dematel_matrices(d_file, t_file, header)
    D_matrix <- matrices$D
    T_matrix <- matrices$T
  } else {
    # Fallback implementation
    if (verbose) cat("Loading matrices...\n")
    D_matrix <- as.matrix(read.csv(d_file, header = header))
    T_matrix <- as.matrix(read.csv(t_file, header = header))

    # Validation
    if (nrow(D_matrix) != ncol(D_matrix) || nrow(T_matrix) != ncol(T_matrix)) {
      stop("Matrices must be square")
    }
    if (nrow(D_matrix) != nrow(T_matrix)) {
      stop("D and T matrices must have the same dimensions")
    }
  }

  n <- nrow(D_matrix)

  # Perform original spectral analysis (if function exists)
  if (exists("dematel_spectral_analysis", mode = "function")) {
    if (verbose) cat("Performing spectral analysis...\n")
    spectral_results <- dematel_spectral_analysis(D_matrix, T_matrix, case_name, verbose)
  } else {
    # Fallback spectral analysis
    if (verbose) cat("Performing basic eigenvalue analysis...\n")
    eigenvals <- eigen(T_matrix, only.values = TRUE)$values
    lambda_max <- max(Re(eigenvals))
    spectral_radius <- max(abs(eigenvals))

    spectral_results <- list(
      lambda_max = lambda_max,
      spectral_radius = spectral_radius,
      case_name = case_name,
      D_matrix = D_matrix,
      T_matrix = T_matrix,
      n = n
    )
  }

  # Add sensitivity analysis if requested
  sensitivity_results <- NULL
  if (include_sensitivity) {
    if (verbose) cat("Performing sensitivity analysis...\n")

    # Reconstruct original A matrix from D matrix
    # A = D * s, where s is the normalization factor
    # We'll approximate s using the maximum row/column sum of D
    max_d_sum <- max(max(rowSums(D_matrix)), max(colSums(D_matrix)))
    if (max_d_sum > 0) {
      # If D is properly normalized, s should be 1/max_d_sum
      # But we need to be careful about the reconstruction
      s_approx <- 1 / max_d_sum
      A_reconstructed <- D_matrix / s_approx
    } else {
      stop("Cannot reconstruct original matrix A from normalized D matrix")
    }

    # Create factor names
    if (is.null(rownames(D_matrix))) {
      factor_names <- paste0("F", 1:n)
    } else {
      factor_names <- rownames(D_matrix)
    }

    # Create sensitivity analysis object
    sensitivity_obj <- DEMATEL_Sensitivity(A_reconstructed, factor_names)

    # Compute sensitivity matrix
    if (sensitivity_method == "numerical") {
      sensitivity_obj <- compute_sensitivity_numerical(sensitivity_obj, epsilon = sensitivity_epsilon)
    } else {
      sensitivity_obj <- compute_sensitivity_analytical(sensitivity_obj)
    }

    sensitivity_results <- sensitivity_obj

    if (verbose) {
      cat("Sensitivity analysis completed.\n")
      cat("Method used:", sensitivity_method, "\n")
    }
  }

  # Combine results
  enhanced_results <- list(
    spectral = spectral_results,
    sensitivity = sensitivity_results,
    case_name = case_name,
    include_sensitivity = include_sensitivity,
    matrices = list(D = D_matrix, T = T_matrix)
  )

  class(enhanced_results) <- "DEMATEL_Enhanced"

  if (verbose) {
    cat("\nAnalysis completed successfully!\n")
    if (include_sensitivity) {
      cat("Use summary(results$sensitivity) to view sensitivity analysis.\n")
      cat("Use visualize_sensitivity(results$sensitivity) to create plots.\n")
    }
  }

  return(enhanced_results)
}

#' Extract Enhanced Metrics
#'
#' Extracts metrics from both spectral and sensitivity analysis for research collection.
#'
#' @param enhanced_results DEMATEL_Enhanced object from analyze_dematel_files_enhanced
#'
#' @return Data frame with comprehensive metrics
#'
#' @examples
#' results <- analyze_dematel_files_enhanced("D.csv", "T.csv", include_sensitivity = TRUE)
#' metrics <- extract_enhanced_metrics(results)
#'
#' @export
extract_enhanced_metrics <- function(enhanced_results) {
  UseMethod("extract_enhanced_metrics")
}

#' @export
extract_enhanced_metrics.DEMATEL_Enhanced <- function(enhanced_results) {

  # Start with spectral metrics (use existing function if available)
  if (exists("extract_metrics", mode = "function") && !is.null(enhanced_results$spectral)) {
    spectral_metrics <- extract_metrics(enhanced_results$spectral)
  } else {
    # Fallback spectral metrics
    spectral_metrics <- data.frame(
      case_name = enhanced_results$case_name %||% "Unnamed",
      lambda_max = enhanced_results$spectral$lambda_max %||% NA,
      spectral_radius = enhanced_results$spectral$spectral_radius %||% NA
    )
  }

  # Add sensitivity metrics if available
  if (!is.null(enhanced_results$sensitivity)) {
    sens_stats <- get_sensitivity_stats(enhanced_results$sensitivity)

    # Get critical relationships summary
    critical_90 <- identify_critical_relationships(enhanced_results$sensitivity, 90)
    critical_95 <- identify_critical_relationships(enhanced_results$sensitivity, 95)

    sensitivity_metrics <- data.frame(
      sensitivity_method = enhanced_results$sensitivity$computation_method %||% "unknown",
      sensitivity_mean = sens_stats$mean,
      sensitivity_sd = sens_stats$sd,
      sensitivity_min = sens_stats$min,
      sensitivity_max = sens_stats$max,
      sensitivity_mean_abs = sens_stats$mean_abs,
      n_amplifying = sens_stats$n_positive,
      n_dampening = sens_stats$n_negative,
      n_critical_90th = nrow(critical_90),
      n_critical_95th = nrow(critical_95),
      max_abs_sensitivity = max(abs(enhanced_results$sensitivity$sensitivity_matrix), na.rm = TRUE),
      sensitivity_range = sens_stats$max - sens_stats$min
    )

    # Combine metrics
    enhanced_metrics <- cbind(spectral_metrics, sensitivity_metrics)
  } else {
    enhanced_metrics <- spectral_metrics
  }

  return(enhanced_metrics)
}
#' @export
extract_enhanced_metrics.DEMATEL_Complete <- function(enhanced_results) {
  # For DEMATEL_Complete objects, use the same logic as DEMATEL_Enhanced
  # Just need to adjust the structure slightly

  # Convert to DEMATEL_Enhanced-like structure for compatibility
  enhanced_format <- list(
    spectral = enhanced_results$spectral,
    sensitivity = enhanced_results$sensitivity,
    case_name = enhanced_results$case_name,
    include_sensitivity = enhanced_results$include_sensitivity
  )
  class(enhanced_format) <- "DEMATEL_Enhanced"

  # Call the existing method
  extract_enhanced_metrics.DEMATEL_Enhanced(enhanced_format)
}

#' Print Method for DEMATEL_Enhanced
#'
#' @param x DEMATEL_Enhanced object
#' @param ... Additional arguments (not used)
#'
#' @export
print.DEMATEL_Enhanced <- function(x, ...) {
  cat("Enhanced DEMATEL Analysis Results\n")
  cat("=================================\n")

  if (!is.null(x$case_name)) {
    cat("Case:", x$case_name, "\n")
  }

  # Spectral analysis summary
  if (!is.null(x$spectral)) {
    cat("\nSpectral Analysis:\n")
    if (!is.null(x$spectral$lambda_max)) {
      cat("  Dominant eigenvalue (λmax):", round(x$spectral$lambda_max, 6), "\n")
    }
    if (!is.null(x$spectral$spectral_radius)) {
      cat("  Spectral radius:", round(x$spectral$spectral_radius, 6), "\n")
    }
    if (!is.null(x$spectral$condition_number)) {
      cat("  Condition number:", round(x$spectral$condition_number, 6), "\n")
    }
  }

  # Sensitivity analysis summary
  if (!is.null(x$sensitivity)) {
    cat("\nSensitivity Analysis:\n")
    cat("  Method:", x$sensitivity$computation_method %||% "unknown", "\n")

    stats <- get_sensitivity_stats(x$sensitivity)
    cat("  Mean absolute sensitivity:", round(stats$mean_abs, 6), "\n")
    cat("  Range: [", round(stats$min, 6), ",", round(stats$max, 6), "]\n")
    cat("  Amplifying relationships:", stats$n_positive, "\n")
    cat("  Dampening relationships:", stats$n_negative, "\n")

    critical <- identify_critical_relationships(x$sensitivity, 90)
    cat("  Critical relationships (90th percentile):", nrow(critical), "\n")
  } else {
    cat("\nSensitivity Analysis: Not performed\n")
  }

  cat("\nUse summary() for detailed analysis.\n")
  invisible(x)
}

#' Summary Method for DEMATEL_Enhanced
#'
#' @param object DEMATEL_Enhanced object
#' @param ... Additional arguments (not used)
#'
#' @export
summary.DEMATEL_Enhanced <- function(object, ...) {
  cat("DEMATEL Enhanced Analysis Summary\n")
  cat("=================================\n\n")

  # Print spectral analysis summary
  if (!is.null(object$spectral)) {
    cat("SPECTRAL ANALYSIS:\n")
    if (inherits(object$spectral, "dematel_spectral")) {
      print(object$spectral)
    } else {
      cat("  Dominant eigenvalue:", round(object$spectral$lambda_max, 6), "\n")
      if (!is.null(object$spectral$spectral_radius)) {
        cat("  Spectral radius:", round(object$spectral$spectral_radius, 6), "\n")
      }
    }
    cat("\n")
  }

  # Print sensitivity analysis summary
  if (!is.null(object$sensitivity)) {
    cat("SENSITIVITY ANALYSIS:\n")
    summary(object$sensitivity)
  } else {
    cat("SENSITIVITY ANALYSIS: Not performed\n")
    cat("To include sensitivity analysis, use:\n")
    cat("analyze_dematel_files_enhanced(..., include_sensitivity = TRUE)\n")
  }

  invisible(object)
}

#' Create Comprehensive Report
#'
#' Generates a comprehensive analysis report combining spectral and sensitivity results.
#'
#' @param enhanced_results DEMATEL_Enhanced object
#' @param save_report Whether to save report to file (default: FALSE)
#' @param report_dir Directory to save report (default: "reports")
#'
#' @return Character vector with report content
#'
#' @examples
#' results <- analyze_dematel_files_enhanced("D.csv", "T.csv", include_sensitivity = TRUE)
#' report <- create_comprehensive_report(results)
#' cat(report, sep = "\n")
#'
#' @export
create_comprehensive_report <- function(enhanced_results, save_report = FALSE, report_dir = "reports") {
  UseMethod("create_comprehensive_report")
}

#' @export
create_comprehensive_report.DEMATEL_Enhanced <- function(enhanced_results, save_report = FALSE, report_dir = "reports") {

  report_lines <- c()

  # Header
  report_lines <- c(report_lines,
                    "COMPREHENSIVE DEMATEL ANALYSIS REPORT",
                    "=====================================",
                    "",
                    paste("Generated on:", Sys.time()),
                    ""
  )

  if (!is.null(enhanced_results$case_name)) {
    report_lines <- c(report_lines,
                      paste("Case Study:", enhanced_results$case_name),
                      ""
    )
  }

  # System Overview
  n <- nrow(enhanced_results$matrices$D)
  report_lines <- c(report_lines,
                    "SYSTEM OVERVIEW:",
                    paste("System size:", n, "×", n),
                    paste("Analysis date:", Sys.Date()),
                    ""
  )

  # Spectral Analysis Section
  if (!is.null(enhanced_results$spectral)) {
    report_lines <- c(report_lines,
                      "SPECTRAL ANALYSIS RESULTS:",
                      "-------------------------"
    )

    if (!is.null(enhanced_results$spectral$lambda_max)) {
      report_lines <- c(report_lines,
                        paste("Dominant eigenvalue (λmax):", round(enhanced_results$spectral$lambda_max, 6))
      )
    }

    if (!is.null(enhanced_results$spectral$spectral_radius)) {
      report_lines <- c(report_lines,
                        paste("Spectral radius:", round(enhanced_results$spectral$spectral_radius, 6))
      )
    }

    report_lines <- c(report_lines, "")
  }

  # Sensitivity Analysis Section
  if (!is.null(enhanced_results$sensitivity)) {
    report_lines <- c(report_lines,
                      "SENSITIVITY ANALYSIS RESULTS:",
                      "-----------------------------"
    )

    stats <- get_sensitivity_stats(enhanced_results$sensitivity)
    critical_90 <- identify_critical_relationships(enhanced_results$sensitivity, 90)
    critical_95 <- identify_critical_relationships(enhanced_results$sensitivity, 95)

    report_lines <- c(report_lines,
                      paste("Computation method:", enhanced_results$sensitivity$computation_method %||% "unknown"),
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

  # Recommendations section
  if (!is.null(enhanced_results$sensitivity)) {
    report_lines <- c(report_lines,
                      "SYSTEM INSIGHTS AND RECOMMENDATIONS:",
                      "------------------------------------"
    )

    stats <- get_sensitivity_stats(enhanced_results$sensitivity)

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
                        "• System contains highly sensitive relationships that require careful management"
      )
    }

    if (stats$n_positive > stats$n_negative) {
      report_lines <- c(report_lines,
                        "• Majority of relationships are amplifying - system may be prone to cascading effects"
      )
    }

    report_lines <- c(report_lines, "")
  }

  # Footer
  report_lines <- c(report_lines,
                    "---",
                    "Report generated by DEMATELSpectral package",
                    "For more information, visit: https://github.com/pdelias/DEMATELSpectral"
  )

  # Save report if requested
  if (save_report) {
    if (!dir.exists(report_dir)) {
      dir.create(report_dir, recursive = TRUE)
    }

    filename <- paste0("DEMATEL_report_", enhanced_results$case_name %||% "analysis", "_", Sys.Date(), ".txt")
    filepath <- file.path(report_dir, filename)

    writeLines(report_lines, filepath)
    cat("Report saved to:", filepath, "\n")
  }

  return(report_lines)
}

#' @export
create_comprehensive_report.DEMATEL_Complete <- function(enhanced_results, save_report = FALSE, report_dir = "reports") {
  # For DEMATEL_Complete objects, use the same logic as DEMATEL_Enhanced
  # Just need to adjust the structure slightly

  # Convert to DEMATEL_Enhanced-like structure for compatibility
  enhanced_format <- list(
    spectral = enhanced_results$spectral,
    sensitivity = enhanced_results$sensitivity,
    case_name = enhanced_results$case_name,
    matrices = enhanced_results$matrices
  )
  class(enhanced_format) <- "DEMATEL_Enhanced"

  # Call the existing method
  create_comprehensive_report.DEMATEL_Enhanced(enhanced_format, save_report, report_dir)
}

#' Complete DEMATEL Analysis from Original Direct Influence Matrix
#'
#' Performs comprehensive DEMATEL analysis starting from the original direct
#' influence matrix A, including both spectral and sensitivity analysis.
#'
#' @param a_file Path to original direct influence matrix (A) CSV file
#' @param case_name Optional identifier for your analysis
#' @param header Whether CSV file contains headers (default: FALSE)
#' @param verbose Whether to print progress messages (default: TRUE)
#' @param include_sensitivity Whether to include sensitivity analysis (default: TRUE)
#' @param sensitivity_method Method for sensitivity computation: "numerical" or "analytical" (default: "numerical")
#' @param sensitivity_epsilon Step size for numerical sensitivity (default: 0.01)
#' @param factor_names Optional vector of factor names. If NULL, will use "F1", "F2", etc.
#'
#' @return List containing comprehensive analysis results
#'
#' @details
#' This function provides the most mathematically accurate approach by:
#' \itemize{
#'   \item Starting with the original direct influence matrix A
#'   \item Computing normalized matrix D and total relations matrix T internally
#'   \item Performing spectral analysis on T
#'   \item Performing sensitivity analysis on the original A matrix
#'   \item Providing integrated results and reporting
#' }
#'
#' @examples
#' # Basic analysis with sensitivity
#' results <- analyze_dematel_from_A("A_matrix.csv")
#'
#' # With custom factor names
#' results <- analyze_dematel_from_A("A_matrix.csv",
#'                                  factor_names = c("Education", "Leadership", "Risk"))
#'
#' # Access results
#' summary(results)
#' plots <- visualize_sensitivity(results$sensitivity)
#'
#' @export
analyze_dematel_from_A <- function(a_file, case_name = NULL, header = FALSE,
                                   verbose = TRUE, include_sensitivity = TRUE,
                                   sensitivity_method = "numerical",
                                   sensitivity_epsilon = 0.01,
                                   factor_names = NULL) {

  # Input validation
  if (!file.exists(a_file)) {
    stop("A matrix file not found: ", a_file)
  }

  if (!sensitivity_method %in% c("numerical", "analytical")) {
    stop("sensitivity_method must be 'numerical' or 'analytical'")
  }

  if (verbose) {
    cat("Complete DEMATEL Analysis from Original Matrix A\n")
    cat("===============================================\n")
    if (!is.null(case_name)) {
      cat("Case:", case_name, "\n")
    }
    cat("A matrix file:", a_file, "\n")
    cat("Sensitivity analysis:", ifelse(include_sensitivity, "Yes", "No"), "\n")
    if (include_sensitivity) {
      cat("Sensitivity method:", sensitivity_method, "\n")
    }
    cat("\n")
  }

  # Load original matrix A
  if (verbose) cat("Loading original direct influence matrix A...\n")

  # Read the CSV file
  A_raw <- read.csv(a_file, header = header, stringsAsFactors = FALSE)

  # Handle case where row names were saved as first column
  if (header && ncol(A_raw) > nrow(A_raw)) {
    # Remove the first column (row names) and convert to matrix
    A_matrix <- as.matrix(A_raw[, -1])
    mode(A_matrix) <- "numeric"
  } else if (header) {
    # Standard case with headers
    A_matrix <- as.matrix(A_raw)
    mode(A_matrix) <- "numeric"
  } else {
    # No headers case
    A_matrix <- as.matrix(A_raw)
  }

  # Validation
  if (nrow(A_matrix) != ncol(A_matrix)) {
    stop("Matrix A must be square")
  }

  if (any(is.na(A_matrix)) || any(!is.finite(A_matrix))) {
    stop("Matrix A must contain only finite numeric values")
  }

  n <- nrow(A_matrix)

  # Set up factor names
  if (is.null(factor_names)) {
    if (!is.null(rownames(A_matrix))) {
      factor_names <- rownames(A_matrix)
    } else {
      factor_names <- paste0("F", 1:n)
    }
  }

  if (length(factor_names) != n) {
    stop("Length of factor_names must equal number of rows/columns in A")
  }

  # Compute DEMATEL matrices from A
  if (verbose) cat("Computing DEMATEL matrices (D and T)...\n")
  dematel_matrices <- compute_dematel_matrices(A_matrix)
  D_matrix <- dematel_matrices$D
  T_matrix <- dematel_matrices$T
  lambda_max <- dematel_matrices$lambda_max

  # Add row and column names
  rownames(A_matrix) <- colnames(A_matrix) <- factor_names
  rownames(D_matrix) <- colnames(D_matrix) <- factor_names
  rownames(T_matrix) <- colnames(T_matrix) <- factor_names

  # Perform spectral analysis
  if (verbose) cat("Performing spectral analysis...\n")

  # Use existing spectral analysis function if available
  if (exists("dematel_spectral_analysis", mode = "function")) {
    spectral_results <- dematel_spectral_analysis(D_matrix, T_matrix, case_name, verbose = FALSE)
  } else {
    # Fallback spectral analysis
    eigenvals <- eigen(T_matrix, only.values = TRUE)$values
    spectral_radius <- max(abs(eigenvals))

    # Additional spectral metrics
    lambda_sorted <- sort(Re(eigenvals), decreasing = TRUE)
    condition_number <- ifelse(length(lambda_sorted) > 1 && lambda_sorted[length(lambda_sorted)] != 0,
                               lambda_sorted[1] / abs(lambda_sorted[length(lambda_sorted)]),
                               Inf)

    spectral_results <- list(
      lambda_max = lambda_max,
      spectral_radius = spectral_radius,
      condition_number = condition_number,
      eigenvalues = eigenvals,
      case_name = case_name,
      D_matrix = D_matrix,
      T_matrix = T_matrix,
      A_matrix = A_matrix,
      n = n,
      factor_names = factor_names
    )
  }

  # Add sensitivity analysis if requested
  sensitivity_results <- NULL
  if (include_sensitivity) {
    if (verbose) cat("Performing sensitivity analysis on original matrix A...\n")

    # Create sensitivity analysis object using original A matrix
    sensitivity_obj <- DEMATEL_Sensitivity(A_matrix, factor_names)

    # Compute sensitivity matrix
    if (sensitivity_method == "numerical") {
      sensitivity_obj <- compute_sensitivity_numerical(sensitivity_obj, epsilon = sensitivity_epsilon)
    } else {
      sensitivity_obj <- compute_sensitivity_analytical(sensitivity_obj)
    }

    sensitivity_results <- sensitivity_obj

    if (verbose) {
      cat("Sensitivity analysis completed.\n")
      cat("Method used:", sensitivity_method, "\n")

      # Quick summary
      stats <- get_sensitivity_stats(sensitivity_obj)
      cat("Mean absolute sensitivity:", round(stats$mean_abs, 6), "\n")
      cat("Amplifying relationships:", stats$n_positive, "\n")
      cat("Dampening relationships:", stats$n_negative, "\n")
    }
  }

  # Combine results
  complete_results <- list(
    spectral = spectral_results,
    sensitivity = sensitivity_results,
    matrices = list(A = A_matrix, D = D_matrix, T = T_matrix),
    case_name = case_name,
    factor_names = factor_names,
    include_sensitivity = include_sensitivity,
    analysis_type = "complete_from_A"
  )

  class(complete_results) <- "DEMATEL_Complete"

  if (verbose) {
    cat("\nComplete analysis finished successfully!\n")
    cat("Matrix dimensions:", n, "×", n, "\n")
    cat("Dominant eigenvalue (λmax):", round(lambda_max, 6), "\n")

    if (include_sensitivity) {
      critical <- identify_critical_relationships(sensitivity_results, 90)
      cat("Critical relationships (90th percentile):", nrow(critical), "\n")
      cat("\nUse summary(results) for detailed analysis.\n")
      cat("Use visualize_sensitivity(results$sensitivity) to create plots.\n")
    }
  }

  return(complete_results)
}

#' Create Example Data with Original A Matrix
#'
#' Enhanced version that creates example A, D, and T matrices for testing.
#'
#' @param n Number of factors (default: 5)
#' @param case_name Optional case name for file naming
#'
#' @return Invisibly returns the matrices
#'
#' @examples
#' create_example_A_matrix(n = 4, case_name = "test")
#' results <- analyze_dematel_from_A("A_test.csv")
#'
#' @export
create_example_A_matrix <- function(n = 5, case_name = "example") {

  # Create example original matrix A
  set.seed(42)  # For reproducibility
  A <- matrix(0, nrow = n, ncol = n)

  # Fill with random values (typical DEMATEL scale 0-4)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {  # No self-influence
        A[i, j] <- sample(0:4, 1, prob = c(0.2, 0.3, 0.3, 0.15, 0.05))
      }
    }
  }

  # Create factor names
  factor_names <- paste0("Factor_", LETTERS[1:n])
  rownames(A) <- colnames(A) <- factor_names

  # Compute D and T matrices
  dematel_matrices <- compute_dematel_matrices(A)
  D <- dematel_matrices$D
  T <- dematel_matrices$T

  # Add names to computed matrices
  rownames(D) <- colnames(D) <- factor_names
  rownames(T) <- colnames(T) <- factor_names

  # Save all matrices (without row names or column names to match DEMATEL convention)
  write.table(A, paste0("A_", case_name, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(D, paste0("D_", case_name, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(T, paste0("T_", case_name, ".csv"), row.names = FALSE, col.names = FALSE, sep = ",")

  cat("Created example matrices:\n")
  cat("- A_", case_name, ".csv (original direct influence matrix)\n", sep = "")
  cat("- D_", case_name, ".csv (normalized matrix)\n", sep = "")
  cat("- T_", case_name, ".csv (total relations matrix)\n", sep = "")
  cat("\nUse: results <- analyze_dematel_from_A('A_", case_name, ".csv')\n", sep = "")

  invisible(list(A = A, D = D, T = T))
}
