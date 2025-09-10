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

#' Enhanced Intervention Analysis with Proper Boundary Handling
#'
#' Analyzes potential interventions that respect DEMATEL scale boundaries.
#' Only suggests feasible changes: +1 when current < max, -1 when current > min.
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param target_lambda_change Numeric. Desired change in λmax
#' @param intervention_type Character. "continuous" or "discrete" (default: "discrete")
#' @param max_value Numeric. Maximum allowed value (default: 4)
#' @param min_value Numeric. Minimum allowed value (default: 0)
#'
#' @return Data frame with boundary-respecting interventions
#'
#' @examples
#' # Proper discrete interventions with boundary respect
#' interventions <- intervention_analysis_enhanced(sens_obj, 
#'                                               target_lambda_change = -0.1)
#'
#' @export
intervention_analysis_enhanced <- function(obj, target_lambda_change, 
                                           intervention_type = "discrete",
                                           max_value = 4,
                                           min_value = 0) {
  UseMethod("intervention_analysis_enhanced")
}

#' @export
intervention_analysis_enhanced.DEMATEL_Sensitivity <- function(obj, target_lambda_change, 
                                                               intervention_type = "discrete",
                                                               max_value = 4,
                                                               min_value = 0) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }
  
  if (!intervention_type %in% c("continuous", "discrete")) {
    stop("intervention_type must be 'continuous' or 'discrete'")
  }
  
  interventions <- data.frame()
  
  if (intervention_type == "continuous") {
    # Original continuous method (unchanged)
    for (i in 1:obj$n) {
      for (j in 1:obj$n) {
        sensitivity <- obj$sensitivity_matrix[i, j]
        
        if (!is.na(sensitivity) && abs(sensitivity) > 1e-6) {
          required_change <- target_lambda_change / sensitivity
          new_aij <- obj$A[i, j] + required_change
          
          # Feasibility: must be within bounds
          feasible <- (new_aij >= min_value && new_aij <= max_value)
          
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
            intervention_type = "continuous",
            actual_lambda_change = sensitivity * required_change,
            stringsAsFactors = FALSE
          )
          
          interventions <- rbind(interventions, intervention)
        }
      }
    }
    
  } else {
    # CORRECTED discrete method with proper boundary handling
    for (i in 1:obj$n) {
      for (j in 1:obj$n) {
        sensitivity <- obj$sensitivity_matrix[i, j]
        current_value <- obj$A[i, j]
        
        if (!is.na(sensitivity) && abs(sensitivity) > 1e-6) {
          
          # Determine valid discrete changes based on current value and boundaries
          valid_changes <- c()
          
          # Can we increase by 1? Only if current < max
          if (current_value < max_value) {
            valid_changes <- c(valid_changes, 1)
          }
          
          # Can we decrease by 1? Only if current > min  
          if (current_value > min_value) {
            valid_changes <- c(valid_changes, -1)
          }
          
          # Skip this relationship if no valid changes possible
          if (length(valid_changes) == 0) {
            next
          }
          
          # Create interventions for each valid change
          for (change in valid_changes) {
            new_aij <- current_value + change
            
            # Calculate actual lambda change for this discrete step
            actual_lambda_change <- sensitivity * change
            
            # Calculate how close this gets us to target
            target_achievement <- abs(actual_lambda_change) / abs(target_lambda_change)
            error_from_target <- abs(actual_lambda_change - target_lambda_change)
            
            # All discrete interventions within bounds are feasible by definition
            feasible <- TRUE
            
            intervention <- data.frame(
              from_factor = obj$factor_names[i],
              to_factor = obj$factor_names[j],
              from_index = i,
              to_index = j,
              current_aij = current_value,
              required_change = change,
              new_aij = new_aij,
              sensitivity = sensitivity,
              actual_lambda_change = actual_lambda_change,
              target_achievement = target_achievement,
              error_from_target = error_from_target,
              feasible = feasible,
              intervention_type = "discrete",
              # Additional helpful fields
              boundary_constrained = (current_value == min_value && change == -1) || 
                (current_value == max_value && change == 1),
              change_direction = ifelse(change > 0, "Increase", "Decrease"),
              stringsAsFactors = FALSE
            )
            
            interventions <- rbind(interventions, intervention)
          }
        }
      }
    }
  }
  
  if (nrow(interventions) == 0) {
    warning("No valid interventions found within the specified boundaries")
    return(data.frame())
  }
  
  # Sort results
  if (intervention_type == "continuous") {
    # Sort by efficiency, then feasibility
    interventions <- interventions[order(-interventions$feasible, interventions$efficiency, decreasing = TRUE), ]
  } else {
    # Sort by target achievement (best first), then by error from target (smallest first)
    interventions <- interventions[order(-interventions$target_achievement, interventions$error_from_target), ]
  }
  
  rownames(interventions) <- NULL
  return(interventions)
}

#' Get Boundary Analysis Summary
#'
#' Analyzes how boundary constraints affect intervention options
#'
#' @param obj DEMATEL_Sensitivity object
#' @param max_value Maximum scale value
#' @param min_value Minimum scale value
#' @return List with boundary constraint analysis
#'
#' @export
analyze_boundary_constraints <- function(obj, max_value = 4, min_value = 0) {
  UseMethod("analyze_boundary_constraints")
}

#' @export
analyze_boundary_constraints.DEMATEL_Sensitivity <- function(obj, max_value = 4, min_value = 0) {
  
  # Create off-diagonal mask to exclude self-influence relationships
  n <- obj$n
  off_diagonal <- !diag(n)  # TRUE for all non-diagonal positions
  
  # Count relationships at boundaries (excluding diagonal)
  at_max <- sum(obj$A[off_diagonal] == max_value)
  at_min <- sum(obj$A[off_diagonal] == min_value)
  at_boundaries <- at_max + at_min
  
  # Count relationships that can be increased/decreased (excluding diagonal)
  can_increase <- sum(obj$A[off_diagonal] < max_value)
  can_decrease <- sum(obj$A[off_diagonal] > min_value)
  
  # Total relationships (excluding diagonal)
  total_relationships <- n^2 - n
  
  # Analysis of sensitivity for boundary-constrained relationships
  # Note: these still include diagonal for sensitivity analysis consistency
  sensitivity_at_max <- obj$sensitivity_matrix[obj$A == max_value]
  sensitivity_at_min <- obj$sensitivity_matrix[obj$A == min_value]
  
  boundary_analysis <- list(
    total_relationships = total_relationships,
    relationships_at_max = at_max,
    relationships_at_min = at_min,
    relationships_at_boundaries = at_boundaries,
    can_increase = can_increase,
    can_decrease = can_decrease,
    flexibility_score = (can_increase + can_decrease) / (2 * total_relationships),
    
    # Sensitivity analysis for constrained relationships
    high_sensitivity_at_max = sum(abs(sensitivity_at_max) > quantile(abs(obj$sensitivity_matrix), 0.9, na.rm = TRUE), na.rm = TRUE),
    high_sensitivity_at_min = sum(abs(sensitivity_at_min) > quantile(abs(obj$sensitivity_matrix), 0.9, na.rm = TRUE), na.rm = TRUE),
    
    # Recommendations
    recommendations = list()
  )
  
  # Generate recommendations
  if (boundary_analysis$flexibility_score < 0.5) {
    boundary_analysis$recommendations <- c(boundary_analysis$recommendations,
                                           "⚠️ Low system flexibility due to many boundary values")
  }
  
  if (boundary_analysis$high_sensitivity_at_max > 0) {
    boundary_analysis$recommendations <- c(boundary_analysis$recommendations,
                                           paste("🔴", boundary_analysis$high_sensitivity_at_max, 
                                                 "high-sensitivity relationships at maximum value (can only be decreased)"))
  }
  
  if (boundary_analysis$high_sensitivity_at_min > 0) {
    boundary_analysis$recommendations <- c(boundary_analysis$recommendations,
                                           paste("🔵", boundary_analysis$high_sensitivity_at_min, 
                                                 "high-sensitivity relationships at minimum value (can only be increased)"))
  }
  
  return(boundary_analysis)
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
    cat(sprintf("Sensitivity matrix: Computed (analytical method)\n"))
    
    # Show assumption check results if available
    if (!is.null(x$assumptions_check)) {
      cat("\nTheorem 1 Assumptions:\n")
      cat(sprintf("  Status: %s\n", ifelse(x$assumptions_check$valid, "✓ Satisfied", "✗ Not satisfied")))
      if (!is.null(x$assumptions_check$eigenvalue_gaps)) {
        cat(sprintf("  Eigenvalue gap: %.6f\n", x$assumptions_check$eigenvalue_gaps))
      }
      if (!is.null(x$assumptions_check$condition_number)) {
        cat(sprintf("  Condition number: %.2e\n", x$assumptions_check$condition_number))
      }
    }
    
    stats <- get_sensitivity_stats(x)
    cat(sprintf("\nSensitivity Statistics:\n"))
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


#' #' Print Method for DEMATEL_Sensitivity
#' #'
#' #' @param x DEMATEL_Sensitivity object
#' #' @param ... Additional arguments (not used)
#' #'
#' #' @export
#' print.DEMATEL_Sensitivity <- function(x, ...) {
#'   cat("DEMATEL Sensitivity Analysis Object\n")
#'   cat("===================================\n")
#'   cat(sprintf("Number of factors: %d\n", x$n))
#'   cat(sprintf("Factor names: %s\n", paste(x$factor_names, collapse = ", ")))
#'   cat(sprintf("Dominant eigenvalue (λmax): %.6f\n", x$lambda_max))
#' 
#'   if (!is.null(x$sensitivity_matrix)) {
#'     cat(sprintf("Sensitivity matrix: Computed (%s method)\n", x$computation_method %||% "unknown"))
#' 
#'     stats <- get_sensitivity_stats(x)
#'     cat(sprintf("  Range: [%.6f, %.6f]\n", stats$min, stats$max))
#'     cat(sprintf("  Mean absolute sensitivity: %.6f\n", stats$mean_abs))
#'     cat(sprintf("  Amplifying relationships: %d\n", stats$n_positive))
#'     cat(sprintf("  Dampening relationships: %d\n", stats$n_negative))
#'   } else {
#'     cat("Sensitivity matrix: Not computed\n")
#'     cat("Use compute_sensitivity_numerical() or compute_sensitivity_analytical()\n")
#'   }
#' 
#'   invisible(x)
#' }

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

#' Enhanced Intervention Analysis with Proper Boundary Handling
#'
#' @param obj DEMATEL_Sensitivity object with computed sensitivity matrix
#' @param target_lambda_change Numeric. Desired change in λmax
#' @param intervention_type Character. "continuous" or "discrete" (default: "discrete")
#' @param max_value Numeric. Maximum allowed value (default: 4)
#' @param min_value Numeric. Minimum allowed value (default: 0)
#' @return Data frame with boundary-respecting interventions
#' @export
intervention_analysis_enhanced <- function(obj, target_lambda_change, 
                                           intervention_type = "discrete",
                                           max_value = 4,
                                           min_value = 0) {
  UseMethod("intervention_analysis_enhanced")
}

#' @export
intervention_analysis_enhanced.DEMATEL_Sensitivity <- function(obj, target_lambda_change, 
                                                               intervention_type = "discrete",
                                                               max_value = 4,
                                                               min_value = 0) {
  if (is.null(obj$sensitivity_matrix)) {
    stop("Please compute sensitivity matrix first")
  }
  
  interventions <- data.frame()
  
  if (intervention_type == "continuous") {
    # Original continuous method
    for (i in 1:obj$n) {
      for (j in 1:obj$n) {
        sensitivity <- obj$sensitivity_matrix[i, j]
        
        if (!is.na(sensitivity) && abs(sensitivity) > 1e-6) {
          required_change <- target_lambda_change / sensitivity
          new_aij <- obj$A[i, j] + required_change
          
          feasible <- (new_aij >= min_value && new_aij <= max_value)
          
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
            intervention_type = "continuous",
            actual_lambda_change = sensitivity * required_change,
            stringsAsFactors = FALSE
          )
          
          interventions <- rbind(interventions, intervention)
        }
      }
    }
    
  } else {
    # Discrete method with proper boundary handling
    for (i in 1:obj$n) {
      for (j in 1:obj$n) {
        sensitivity <- obj$sensitivity_matrix[i, j]
        current_value <- obj$A[i, j]
        
        if (!is.na(sensitivity) && abs(sensitivity) > 1e-6) {
          
          # Determine valid discrete changes based on boundaries
          valid_changes <- c()
          
          # Can increase by 1? Only if current < max
          if (current_value < max_value) {
            valid_changes <- c(valid_changes, 1)
          }
          
          # Can decrease by 1? Only if current > min
          if (current_value > min_value) {
            valid_changes <- c(valid_changes, -1)
          }
          
          # Skip if no valid changes
          if (length(valid_changes) == 0) {
            next
          }
          
          # Create interventions for each valid change
          for (change in valid_changes) {
            new_aij <- current_value + change
            actual_lambda_change <- sensitivity * change
            target_achievement <- abs(actual_lambda_change) / abs(target_lambda_change)
            error_from_target <- abs(actual_lambda_change - target_lambda_change)
            
            intervention <- data.frame(
              from_factor = obj$factor_names[i],
              to_factor = obj$factor_names[j],
              from_index = i,
              to_index = j,
              current_aij = current_value,
              required_change = change,
              new_aij = new_aij,
              sensitivity = sensitivity,
              actual_lambda_change = actual_lambda_change,
              target_achievement = target_achievement,
              error_from_target = error_from_target,
              feasible = TRUE,
              intervention_type = "discrete",
              change_direction = ifelse(change > 0, "Increase", "Decrease"),
              stringsAsFactors = FALSE
            )
            
            interventions <- rbind(interventions, intervention)
          }
        }
      }
    }
  }
  
  if (nrow(interventions) == 0) {
    warning("No valid interventions found within the specified boundaries")
    return(data.frame())
  }
  
  # Sort results
  if (intervention_type == "continuous") {
    interventions <- interventions[order(-interventions$feasible, interventions$efficiency, decreasing = TRUE), ]
  } else {
    interventions <- interventions[order(-interventions$target_achievement, interventions$error_from_target), ]
  }
  
  rownames(interventions) <- NULL
  return(interventions)
}
