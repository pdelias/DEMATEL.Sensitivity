#' Identify Critical Relationships
#'
#' Identifies the most sensitive relationships in the DEMATEL system
#' based on sensitivity matrix values.
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param threshold_percentile Numeric. Percentile threshold for identifying
#'   critical relationships (default: 90)
#'
#' @return Data frame with critical relationships sorted by absolute sensitivity
#'
#' @details
#' Returns relationships where the absolute sensitivity value is above
#' the specified percentile threshold. Each row contains:
#' \itemize{
#'   \item from_factor, to_factor: Factor names
#'   \item from_index, to_index: Matrix indices
#'   \item sensitivity: Raw sensitivity value
#'   \item abs_sensitivity: Absolute sensitivity value
#'   \item interpretation: "Amplifying" (positive) or "Dampening" (negative)
#' }
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#' critical <- identify_critical_relationships(sens_obj, threshold_percentile = 80)
#'
#' @export
identify_critical_relationships <- function(obj, threshold_percentile = 90) {
  UseMethod("identify_critical_relationships")
}

#' @export
identify_critical_relationships.DEMATEL_Sensitivity <- function(obj, threshold_percentile = 90) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first using compute_sensitivity_numerical() or compute_sensitivity_analytical()")
  }

  if (threshold_percentile < 0 || threshold_percentile > 100) {
    stop("threshold_percentile must be between 0 and 100")
  }

  # Get absolute values
  abs_sensitivity <- abs(obj$sensitivity_matrix)
  threshold <- quantile(abs_sensitivity, threshold_percentile / 100, na.rm = TRUE)

  # Find critical relationships
  critical_indices <- which(abs_sensitivity >= threshold, arr.ind = TRUE)

  if (nrow(critical_indices) == 0) {
    warning("No relationships found above the specified threshold")
    return(data.frame())
  }

  critical_relationships <- data.frame(
    from_factor = obj$factor_names[critical_indices[, 1]],
    to_factor = obj$factor_names[critical_indices[, 2]],
    from_index = critical_indices[, 1],
    to_index = critical_indices[, 2],
    sensitivity = obj$sensitivity_matrix[critical_indices],
    abs_sensitivity = abs_sensitivity[critical_indices],
    interpretation = ifelse(obj$sensitivity_matrix[critical_indices] > 0, "Amplifying", "Dampening"),
    stringsAsFactors = FALSE
  )

  # Sort by absolute sensitivity
  critical_relationships <- critical_relationships[order(critical_relationships$abs_sensitivity, decreasing = TRUE), ]
  rownames(critical_relationships) <- NULL

  return(critical_relationships)
}

#' Intervention Analysis
#'
#' Analyzes potential interventions to achieve a target change in the
#' dominant eigenvalue (λmax).
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param target_lambda_change Numeric. Desired change in λmax (can be positive or negative)
#' @param feasibility_check Logical. Whether to check if resulting values are non-negative (default: TRUE)
#'
#' @return Data frame with potential interventions sorted by efficiency
#'
#' @details
#' For each relationship (i,j), computes the required change in aij to achieve
#' the target change in λmax using: Δaij = target_change / sensitivity_ij
#'
#' The efficiency metric represents how small the required change is relative
#' to the target effect. Higher efficiency means smaller intervention needed.
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#' interventions <- intervention_analysis(sens_obj, target_lambda_change = -0.1)
#'
#' @export
intervention_analysis <- function(obj, target_lambda_change, feasibility_check = TRUE) {
  UseMethod("intervention_analysis")
}

#' @export
intervention_analysis.DEMATEL_Sensitivity <- function(obj, target_lambda_change, feasibility_check = TRUE) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }

  if (!is.numeric(target_lambda_change) || length(target_lambda_change) != 1) {
    stop("target_lambda_change must be a single numeric value")
  }

  interventions <- data.frame()

  for (i in 1:obj$n) {
    for (j in 1:obj$n) {
      sensitivity <- obj$sensitivity_matrix[i, j]

      if (!is.na(sensitivity) && abs(sensitivity) > 1e-6) {  # Avoid division by near-zero
        required_change <- target_lambda_change / sensitivity
        new_aij <- obj$A[i, j] + required_change

        # Feasibility check
        feasible <- TRUE
        if (feasibility_check && new_aij < 0) {
          feasible <- FALSE
        }

        intervention <- data.frame(
          from_factor = obj$factor_names[i],
          to_factor = obj$factor_names[j],
          from_index = i,
          to_index = j,
          current_aij = obj$A[i, j],
          required_change = required_change,
          new_aij = new_aij,
          sensitivity = sensitivity,
          efficiency = abs(target_lambda_change) / abs(required_change),
          feasible = feasible,
          stringsAsFactors = FALSE
        )

        interventions <- rbind(interventions, intervention)
      }
    }
  }

  if (nrow(interventions) == 0) {
    warning("No valid interventions found")
    return(data.frame())
  }

  # Sort by efficiency (highest efficiency = smallest required change for target effect)
  interventions <- interventions[order(interventions$efficiency, decreasing = TRUE), ]
  rownames(interventions) <- NULL

  return(interventions)
}

#' Get Sensitivity Summary Statistics
#'
#' Computes summary statistics for the sensitivity matrix
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#'
#' @return List with summary statistics
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#' stats <- get_sensitivity_stats(sens_obj)
#'
#' @export
get_sensitivity_stats <- function(obj) {
  UseMethod("get_sensitivity_stats")
}

#' @export
get_sensitivity_stats.DEMATEL_Sensitivity <- function(obj) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }

  sens_values <- as.vector(obj$sensitivity_matrix)
  sens_values <- sens_values[!is.na(sens_values)]

  if (length(sens_values) == 0) {
    stop("No valid sensitivity values found")
  }

  stats <- list(
    mean = mean(sens_values),
    median = median(sens_values),
    sd = sd(sens_values),
    min = min(sens_values),
    max = max(sens_values),
    mean_abs = mean(abs(sens_values)),
    median_abs = median(abs(sens_values)),
    n_positive = sum(sens_values > 0),
    n_negative = sum(sens_values < 0),
    n_zero = sum(abs(sens_values) < 1e-6),
    total_elements = length(sens_values)
  )

  return(stats)
}

#' Print Method for DEMATEL_Sensitivity
#'
#' @param x DEMATEL_Sensitivity object
#' @param ... Additional arguments (not used)
#'
#' @export
print.DEMATEL_Sensitivity <- function(x, ...) {
  cat("DEMATEL Sensitivity Analysis Object\n")
  cat("===================================\n")
  cat(sprintf("Number of factors: %d\n", x$n))
  cat(sprintf("Factor names: %s\n", paste(x$factor_names, collapse = ", ")))
  cat(sprintf("Dominant eigenvalue (λmax): %.6f\n", x$lambda_max))

  if (!is.null(x$sensitivity_matrix)) {
    cat(sprintf("Sensitivity matrix: Computed (%s method)\n", x$computation_method %||% "unknown"))

    stats <- get_sensitivity_stats(x)
    cat(sprintf("  Range: [%.6f, %.6f]\n", stats$min, stats$max))
    cat(sprintf("  Mean absolute sensitivity: %.6f\n", stats$mean_abs))
    cat(sprintf("  Amplifying relationships: %d\n", stats$n_positive))
    cat(sprintf("  Dampening relationships: %d\n", stats$n_negative))
  } else {
    cat("Sensitivity matrix: Not computed\n")
    cat("Use compute_sensitivity_numerical() or compute_sensitivity_analytical()\n")
  }

  invisible(x)
}

#' Summary Method for DEMATEL_Sensitivity
#'
#' @param object DEMATEL_Sensitivity object
#' @param ... Additional arguments (not used)
#'
#' @export
summary.DEMATEL_Sensitivity <- function(object, ...) {
  cat("DEMATEL Sensitivity Analysis Summary\n")
  cat("====================================\n")
  cat(sprintf("System size: %d × %d\n", object$n, object$n))
  cat(sprintf("Dominant eigenvalue: %.6f\n", object$lambda_max))

  if (!is.null(object$sensitivity_matrix)) {
    cat(sprintf("\nSensitivity Analysis (%s method):\n", object$computation_method %||% "unknown"))

    stats <- get_sensitivity_stats(object)
    cat(sprintf("  Total relationships: %d\n", stats$total_elements))
    cat(sprintf("  Amplifying (positive): %d (%.1f%%)\n",
                stats$n_positive, 100 * stats$n_positive / stats$total_elements))
    cat(sprintf("  Dampening (negative): %d (%.1f%%)\n",
                stats$n_negative, 100 * stats$n_negative / stats$total_elements))
    cat(sprintf("  Near-zero: %d (%.1f%%)\n",
                stats$n_zero, 100 * stats$n_zero / stats$total_elements))

    cat(sprintf("\nSensitivity Statistics:\n"))
    cat(sprintf("  Mean: %.6f (SD: %.6f)\n", stats$mean, stats$sd))
    cat(sprintf("  Range: [%.6f, %.6f]\n", stats$min, stats$max))
    cat(sprintf("  Mean absolute: %.6f\n", stats$mean_abs))

    # Most critical relationships
    critical <- identify_critical_relationships(object, threshold_percentile = 95)
    if (nrow(critical) > 0) {
      cat(sprintf("\nTop 3 most critical relationships (95th percentile):\n"))
      for (i in 1:min(3, nrow(critical))) {
        cat(sprintf("  %d. %s → %s: %.6f (%s)\n",
                    i, critical$from_factor[i], critical$to_factor[i],
                    critical$sensitivity[i], critical$interpretation[i]))
      }
    }
  } else {
    cat("\nSensitivity matrix not computed.\n")
    cat("Use compute_sensitivity_numerical() or compute_sensitivity_analytical() first.\n")
  }

  invisible(object)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
