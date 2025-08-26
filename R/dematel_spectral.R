# DEMATEL Spectral Analysis Package
# File: R/dematel_spectral.R
#
# A clean, reusable R package for DEMATEL spectral analysis
# Based on "Towards Elevating DEMATEL with Spectral Analysis" by Delias & Kalkitsa

#' DEMATEL Spectral Analysis
#'
#' Performs spectral analysis on DEMATEL total relations matrix to extract
#' enhanced insights into system dynamics and stability characteristics.
#'
#' @param D Normalized direct influence matrix (n x n numeric matrix)
#' @param T Total relations matrix (n x n numeric matrix)
#' @param case_name Optional name/identifier for this analysis
#' @param return_eigenvalues Logical, whether to return all eigenvalues (default: FALSE)
#' @param verbose Logical, whether to print detailed output (default: TRUE)
#'
#' @return List containing spectral analysis results
#' @export
#'
#' @examples
#' # Create example matrices
#' D <- matrix(runif(25, 0, 0.15), 5, 5)
#' diag(D) <- 0
#' D <- D / max(rowSums(D), colSums(D))  # Normalize
#' T <- D %*% solve(diag(5) - D)
#'
#' # Perform analysis
#' results <- dematel_spectral_analysis(D, T, case_name = "Example")
#'
#' # Access specific results
#' print(results$lambda_max)
#' print(results$is_diagonalizable)
dematel_spectral_analysis <- function(D, T,
                                      case_name = NULL,
                                      return_eigenvalues = FALSE,
                                      verbose = TRUE) {

  # Input validation
  if (!is.matrix(D) || !is.matrix(T)) {
    stop("D and T must be matrices")
  }

  if (nrow(D) != ncol(D) || nrow(T) != ncol(T)) {
    stop("Matrices must be square")
  }

  if (nrow(D) != nrow(T)) {
    stop("Matrices D and T must have the same dimensions")
  }

  if (any(is.na(D)) || any(is.na(T))) {
    stop("Matrices cannot contain NA values")
  }

  n <- nrow(T)

  if (verbose && !is.null(case_name)) {
    cat(sprintf("\n=== DEMATEL Spectral Analysis: %s ===\n", case_name))
  }

  # Compute eigenstructure
  eigen_result <- eigen(T)
  eigenvalues <- eigen_result$values
  eigenvectors <- eigen_result$vectors

  # Check diagonalizability
  eigenvector_rank <- qr(eigenvectors)$rank
  is_diagonalizable <- (eigenvector_rank == n)

  # Extract key eigenvalues (real parts for interpretation)
  eigenvalues_real <- Re(eigenvalues)
  lambda_max <- max(eigenvalues_real)
  lambda_min <- min(eigenvalues_real)

  # Second largest eigenvalue
  sorted_eigenvalues <- sort(eigenvalues_real, decreasing = TRUE)
  lambda_2 <- if(n > 1) sorted_eigenvalues[2] else NA_real_

  # Spectral radius (maximum absolute value)
  spectral_radius <- max(abs(eigenvalues))

  # Condition number
  condition_number <- if(abs(lambda_min) > 1e-12) lambda_max / lambda_min else Inf

  # Amplification factor
  D_spectral_radius <- max(abs(eigen(D)$values))
  amplification_factor <- if(D_spectral_radius < 1) {
    1 / (1 - D_spectral_radius)
  } else {
    NA_real_
  }

  # Convergence rate
  convergence_rate <- if(!is.na(lambda_2) && abs(lambda_max) > 1e-12) {
    -log(abs(lambda_2 / lambda_max))
  } else {
    NA_real_
  }

  # Eigenvalue concentration ratio
  eigenvalue_sum <- sum(eigenvalues_real)
  concentration_ratio <- if(abs(eigenvalue_sum) > 1e-12) {
    lambda_max / eigenvalue_sum
  } else {
    NA_real_
  }

  # Dominant eigenvector analysis
  max_eigen_idx <- which.max(eigenvalues_real)
  dominant_eigenvector <- abs(Re(eigenvectors[, max_eigen_idx]))
  eigenvector_sd <- sd(dominant_eigenvector)
  eigenvector_range <- max(dominant_eigenvector) - min(dominant_eigenvector)

  # Create results object
  results <- structure(list(
    # Case information
    case_name = case_name,
    n_factors = n,

    # Diagonalizability
    is_diagonalizable = is_diagonalizable,

    # Key eigenvalues
    lambda_max = lambda_max,
    lambda_2 = lambda_2,
    lambda_min = lambda_min,
    spectral_radius = spectral_radius,

    # System metrics
    condition_number = condition_number,
    amplification_factor = amplification_factor,
    convergence_rate = convergence_rate,
    concentration_ratio = concentration_ratio,

    # Eigenvector analysis
    eigenvector_sd = eigenvector_sd,
    eigenvector_range = eigenvector_range,

    # Optional: all eigenvalues
    all_eigenvalues = if(return_eigenvalues) eigenvalues_real else NULL,
    dominant_eigenvector = if(return_eigenvalues) dominant_eigenvector else NULL
  ), class = "dematel_spectral")

  if (verbose) {
    print(results)
  }

  return(results)
}

#' Print method for dematel_spectral objects
#' @export
print.dematel_spectral <- function(x, ...) {

  cat("\nDEMATEL Spectral Analysis Results\n")
  cat(rep("=", 40), "\n", sep = "")

  if (!is.null(x$case_name)) {
    cat(sprintf("Case: %s\n", x$case_name))
  }

  cat(sprintf("Number of factors: %d\n", x$n_factors))
  cat(sprintf("Is diagonalizable: %s\n", ifelse(x$is_diagonalizable, "Yes", "No")))

  cat("\nKey Eigenvalues:\n")
  cat(sprintf("  λmax (maximum):     %8.4f\n", x$lambda_max))
  if (!is.na(x$lambda_2)) {
    cat(sprintf("  λ2 (second largest): %8.4f\n", x$lambda_2))
  }
  cat(sprintf("  λmin (minimum):     %8.4f\n", x$lambda_min))
  cat(sprintf("  Spectral radius:    %8.4f\n", x$spectral_radius))

  cat("\nSystem Dynamics:\n")
  cat(sprintf("  Condition number:       %8.2f\n", x$condition_number))
  if (!is.na(x$amplification_factor)) {
    cat(sprintf("  Amplification factor:   %8.4f\n", x$amplification_factor))
  }
  if (!is.na(x$convergence_rate)) {
    cat(sprintf("  Convergence rate:       %8.4f\n", x$convergence_rate))
  }
  cat(sprintf("  Concentration ratio:    %8.4f\n", x$concentration_ratio))

  cat("\nDominant Eigenvector:\n")
  cat(sprintf("  Standard deviation:     %8.4f\n", x$eigenvector_sd))
  cat(sprintf("  Range (max - min):      %8.4f\n", x$eigenvector_range))

  # Interpretation
  cat("\nInterpretation:\n")
  if (x$lambda_max > 1) {
    cat("  • System shows potential for influence amplification\n")
  } else {
    cat("  • System is stable with bounded influence\n")
  }

  if (x$condition_number > 100) {
    cat("  • High sensitivity - use precise measurements\n")
  }

  if (x$concentration_ratio > 0.5) {
    cat("  • Dominant influence pattern - predictable behavior\n")
  }

  cat("\n")
}

#' Load DEMATEL matrices from CSV files
#'
#' Convenience function to load D and T matrices from CSV files
#'
#' @param d_file Path to CSV file containing normalized direct influence matrix D
#' @param t_file Path to CSV file containing total relations matrix T
#' @param header Logical, whether CSV files have headers (default: FALSE)
#'
#' @return List with D and T matrices
#' @export
load_dematel_matrices <- function(d_file, t_file, header = FALSE) {

  if (!file.exists(d_file)) {
    stop(sprintf("File not found: %s", d_file))
  }

  if (!file.exists(t_file)) {
    stop(sprintf("File not found: %s", t_file))
  }

  # Read CSV files with proper settings for DEMATEL matrices
  D <- as.matrix(read.csv(d_file, header = header, row.names = NULL, check.names = FALSE))
  T <- as.matrix(read.csv(t_file, header = header, row.names = NULL, check.names = FALSE))

  # Ensure numeric and handle any conversion issues
  mode(D) <- "numeric"
  mode(T) <- "numeric"

  # Check for and remove any row/column names if they exist
  dimnames(D) <- NULL
  dimnames(T) <- NULL

  # Validate that matrices are square
  if (nrow(D) != ncol(D)) {
    stop(sprintf("Matrix D is not square: %d x %d. Check if header=TRUE is needed.", nrow(D), ncol(D)))
  }

  if (nrow(T) != ncol(T)) {
    stop(sprintf("Matrix T is not square: %d x %d. Check if header=TRUE is needed.", nrow(T), ncol(T)))
  }

  list(D = D, T = T)
}

#' Analyze DEMATEL from files
#'
#' Complete workflow: load matrices from files and perform spectral analysis
#'
#' @param d_file Path to CSV file containing matrix D
#' @param t_file Path to CSV file containing matrix T
#' @param case_name Optional case identifier
#' @param ... Additional arguments passed to dematel_spectral_analysis
#'
#' @return dematel_spectral object
#' @export
analyze_dematel_files <- function(d_file, t_file, case_name = NULL, ...) {

  # Extract case name from filename if not provided
  if (is.null(case_name)) {
    case_name <- tools::file_path_sans_ext(basename(t_file))
  }

  # Load matrices
  matrices <- load_dematel_matrices(d_file, t_file)

  # Perform analysis
  dematel_spectral_analysis(matrices$D, matrices$T, case_name = case_name, ...)
}

#' Extract key metrics as a data frame row
#'
#' Useful for collecting results across multiple cases
#'
#' @param spectral_result A dematel_spectral object
#'
#' @return Single-row data frame with key metrics
#' @export
extract_metrics <- function(spectral_result) {

  data.frame(
    case_name = spectral_result$case_name %||% NA_character_,
    n_factors = spectral_result$n_factors,
    is_diagonalizable = spectral_result$is_diagonalizable,
    lambda_max = spectral_result$lambda_max,
    lambda_2 = spectral_result$lambda_2,
    lambda_min = spectral_result$lambda_min,
    spectral_radius = spectral_result$spectral_radius,
    condition_number = spectral_result$condition_number,
    amplification_factor = spectral_result$amplification_factor,
    convergence_rate = spectral_result$convergence_rate,
    concentration_ratio = spectral_result$concentration_ratio,
    eigenvector_sd = spectral_result$eigenvector_sd,
    eigenvector_range = spectral_result$eigenvector_range,
    stringsAsFactors = FALSE
  )
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create example DEMATEL matrices
#'
#' Generate example matrices for testing and demonstration
#'
#' @param n Size of matrices (default: 5)
#' @param seed Random seed for reproducibility
#' @param save_files Logical, whether to save as CSV files (default: TRUE)
#'
#' @return List with D and T matrices
#' @export
create_example_dematel <- function(n = 5, seed = 123, save_files = TRUE) {

  set.seed(seed)

  # Create random direct influence matrix
  D <- matrix(runif(n^2, 0, 0.2), n, n)
  diag(D) <- 0  # No self-influence

  # Normalize to ensure convergence
  s <- max(rowSums(D), colSums(D))
  D <- D / s

  # Compute total relations matrix
  I <- diag(n)
  T <- D %*% solve(I - D)

  if (save_files) {
    # Save CSV files without headers and ensure proper line endings
    write.table(D, "D_example.csv", sep = ",", row.names = FALSE, col.names = FALSE,
                quote = FALSE, eol = "\n")
    write.table(T, "T_example.csv", sep = ",", row.names = FALSE, col.names = FALSE,
                quote = FALSE, eol = "\n")

    cat("Example matrices saved as 'D_example.csv' and 'T_example.csv'\n")
    cat("Both files saved without headers (use header=FALSE when reading)\n")
  }

  list(D = D, T = T)
}

# ===== PACKAGE SETUP INSTRUCTIONS =====
#
# To create this as a proper R package:
#
# 1. Create package structure:
#    usethis::create_package("DEMATELSpectral")
#
# 2. Add this code to R/dematel_spectral.R
#
# 3. Create DESCRIPTION file:
#    usethis::use_description(
#      fields = list(
#        Title = "Spectral Analysis Extension for DEMATEL",
#        Description = "Enhanced DEMATEL analysis through eigenvalue decomposition",
#        `Authors@R` = 'person("Your", "Name", email = "your@email.com", role = c("aut", "cre"))'
#      )
#    )
#
# 4. Generate documentation:
#    devtools::document()
#
# 5. Install package:
#    devtools::install()
#
# ===== USAGE EXAMPLES =====

# Example 1: Basic usage with matrices
example_usage_matrices <- function() {
  # Create example data
  matrices <- create_example_dematel(n = 6)

  # Perform analysis
  results <- dematel_spectral_analysis(
    D = matrices$D,
    T = matrices$T,
    case_name = "Example Case"
  )

  # Extract metrics for data collection
  metrics <- extract_metrics(results)
  print(metrics)
}

# Example 2: Working with files
example_usage_files <- function() {
  # Analyze from files
  results <- analyze_dematel_files(
    d_file = "case1_D.csv",
    t_file = "case1_T.csv",
    case_name = "Case 1"
  )

  return(results)
}

# Example 3: Collecting results across multiple cases
example_collect_results <- function() {
  # Initialize results collector
  all_results <- data.frame()

  # Process multiple cases
  case_files <- list(
    list(d = "case1_D.csv", t = "case1_T.csv", name = "Case 1"),
    list(d = "case2_D.csv", t = "case2_T.csv", name = "Case 2")
    # ... add more cases
  )

  for (case in case_files) {
    if (file.exists(case$d) && file.exists(case$t)) {
      result <- analyze_dematel_files(case$d, case$t, case$name, verbose = FALSE)
      metrics <- extract_metrics(result)
      all_results <- rbind(all_results, metrics)
    }
  }

  # Save collected results
  write.csv(all_results, "dematel_spectral_results.csv", row.names = FALSE)

  return(all_results)
}
