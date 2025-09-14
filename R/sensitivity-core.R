#' DEMATEL Sensitivity Analysis Core Functions
#' 
#' This file contains the core functions for DEMATEL sensitivity analysis,
#' including the main constructor and matrix computation functions.

#' DEMATEL Sensitivity Analysis Constructor
#'
#' Creates a DEMATEL sensitivity analysis object for examining how changes
#' in direct influence relationships affect the system's dominant eigenvalue.
#'
#' @param A Numeric matrix. Direct influence matrix (square matrix)
#' @param factor_names Character vector. Names for the factors. If NULL,
#'   default names "F1", "F2", ... will be used
#'
#' @return Object of class "DEMATEL_Sensitivity"
#'
#' @details
#' This function initializes a DEMATEL sensitivity analysis by:
#' \itemize{
#'   \item Computing the normalized direct influence matrix (D)
#'   \item Computing the total relations matrix (T)
#'   \item Finding the dominant eigenvalue (lambda_max)
#'   \item Setting up the structure for sensitivity analysis
#' }
#'
#' @examples
#' # Create sample direct influence matrix
#' A <- matrix(c(0, 3, 2, 1,
#'               2, 0, 3, 2,
#'               1, 2, 0, 3,
#'               2, 1, 2, 0), nrow = 4, byrow = TRUE)
#'
#' # Create sensitivity analysis object
#' sens_obj <- DEMATEL_Sensitivity(A, c("Factor1", "Factor2", "Factor3", "Factor4"))
#'
#' @export
DEMATEL_Sensitivity <- function(A, factor_names = NULL) {
  
  # Input validation
  if (!is.matrix(A)) {
    stop("A must be a matrix")
  }
  
  if (nrow(A) != ncol(A)) {
    stop("A must be a square matrix")
  }
  
  if (any(is.na(A)) || any(!is.finite(A))) {
    stop("A must contain only finite numeric values")
  }
  
  if (any(diag(A) != 0)) {
    warning("Diagonal elements of A should typically be zero in DEMATEL analysis")
  }
  
  # Initialize
  n <- nrow(A)
  if (is.null(factor_names)) {
    factor_names <- paste0("F", 1:n)
  }
  
  if (length(factor_names) != n) {
    stop("Length of factor_names must equal number of rows/columns in A")
  }
  
  # Compute DEMATEL matrices
  dematel_matrices <- compute_dematel_matrices(A)
  
  # Create object
  obj <- list(
    A = A,
    D = dematel_matrices$D,
    T = dematel_matrices$T,
    lambda_max = dematel_matrices$lambda_max,
    factor_names = factor_names,
    n = n,
    sensitivity_matrix = NULL,
    computation_method = NULL
  )
  
  class(obj) <- "DEMATEL_Sensitivity"
  return(obj)
}

#' Compute DEMATEL Matrices
#'
#' Internal function to compute D and T matrices and dominant eigenvalue
#'
#' @param A Direct influence matrix
#' @return List containing D, T matrices and lambda_max
#' @keywords internal
compute_dematel_matrices <- function(A) {
  n <- nrow(A)
  
  # Normalization
  row_sums <- rowSums(A)
  col_sums <- colSums(A)
  s <- max(max(row_sums), max(col_sums))
  
  if (s == 0) {
    stop("Matrix A cannot have all zero elements")
  }
  
  D <- A / s
  
  # Total relation matrix
  I <- diag(n)
  
  # Check if (I - D) is invertible
  det_val <- det(I - D)
  if (abs(det_val) < 1e-12) {
    stop("Matrix (I - D) is not invertible. Check your input matrix A.")
  }
  
  T <- solve(I - D) - I
  
  # Dominant eigenvalue
  eigenvals <- eigen(T, only.values = TRUE)$values
  eigenvals_real <- Re(eigenvals)
  lambda_max <- max(eigenvals_real)
  
  return(list(D = D, T = T, lambda_max = lambda_max))
}

#' Compute Numerical Sensitivity Matrix
#'
#' Computes the sensitivity matrix using numerical differentiation.
#' Each element (i,j) represents ∂λmax/∂aij.
#'
#' @param obj DEMATEL_Sensitivity object
#' @param epsilon Numeric. Step size for numerical differentiation (default: 0.01)
#'
#' @return Updated DEMATEL_Sensitivity object with sensitivity matrix
#'
#' @details
#' Uses forward finite differences to compute:
#' ∂λmax/∂aij ≈ (λmax(A + ε·eij) - λmax(A)) / ε
#' where eij is a matrix with 1 at position (i,j) and 0 elsewhere.
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_numerical(sens_obj)
#'
#' @export
compute_sensitivity_numerical <- function(obj, epsilon = 0.01) {
  UseMethod("compute_sensitivity_numerical")
}

#' @export
compute_sensitivity_numerical.DEMATEL_Sensitivity <- function(obj, epsilon = 0.01) {
  
  if (epsilon <= 0) {
    stop("epsilon must be positive")
  }
  
  n <- obj$n
  sensitivity_matrix <- matrix(0, nrow = n, ncol = n)
  
  cat("Computing sensitivity matrix using numerical method...\n")
  cat("This may take a moment for large matrices.\n")
  
  pb <- txtProgressBar(min = 0, max = n^2, style = 3)
  
  for (i in 1:n) {
    for (j in 1:n) {
      # Create perturbed matrix
      A_pert <- obj$A
      A_pert[i, j] <- A_pert[i, j] + epsilon
      
      tryCatch({
        # Compute perturbed system
        dematel_pert <- compute_dematel_matrices(A_pert)
        lambda_max_pert <- dematel_pert$lambda_max
        
        # Numerical derivative
        sensitivity_matrix[i, j] <- (lambda_max_pert - obj$lambda_max) / epsilon
      }, error = function(e) {
        warning(paste("Could not compute sensitivity for element (", i, ",", j, "): ", e$message))
        sensitivity_matrix[i, j] <- NA
      })
      
      setTxtProgressBar(pb, (i-1)*n + j)
    }
  }
  close(pb)
  
  # Add row and column names
  rownames(sensitivity_matrix) <- obj$factor_names
  colnames(sensitivity_matrix) <- obj$factor_names
  
  obj$sensitivity_matrix <- sensitivity_matrix
  obj$computation_method <- "numerical"
  
  cat("\nSensitivity matrix computation completed.\n")
  
  return(obj)
}

#' Check Assumptions for Theorem 1
#'
#' Verifies that the conditions required for Theorem 1 are satisfied:
#' 1. The dominant eigenvalue λmax is simple (non-repeated)
#' 2. The matrix D is irreducible (strongly connected influence graph)
#' 3. The system matrices are well-conditioned
#'
#' @param obj DEMATEL_Sensitivity object
#' @param tolerance Numerical tolerance for eigenvalue multiplicity check
#'
#' @return List with validity status and diagnostic information
#' @keywords internal
check_theorem1_assumptions <- function(obj, tolerance = 1e-10) {
  
  result <- list(
    valid = FALSE,
    message = "",
    dominant_is_simple = FALSE,
    matrix_is_irreducible = FALSE,
    well_conditioned = FALSE,
    eigenvalue_gaps = NULL,
    condition_number = NULL
  )
  
  tryCatch({
    # Check 1: Dominant eigenvalue is simple (non-repeated)
    eigenvalues <- eigen(obj$T, only.values = TRUE)$values
    eigenvalues_real <- Re(eigenvalues)
    
    # Sort eigenvalues by magnitude
    sorted_eigenvalues <- sort(eigenvalues_real, decreasing = TRUE)
    lambda_max <- sorted_eigenvalues[1]
    
    # Check if dominant eigenvalue is simple
    multiplicity <- sum(abs(eigenvalues_real - lambda_max) < tolerance)
    if (multiplicity == 1) {
      result$dominant_is_simple <- TRUE
    } else {
      result$message <- paste("Dominant eigenvalue has multiplicity", multiplicity, 
                              "(should be 1 for simple eigenvalue)")
      return(result)
    }
    
    # Store eigenvalue gaps for diagnostics
    if (length(sorted_eigenvalues) > 1) {
      result$eigenvalue_gaps <- sorted_eigenvalues[1] - sorted_eigenvalues[2]
    }
    
    # Check 2: Matrix D is irreducible (strongly connected)
    D_abs <- abs(obj$D)
    n <- nrow(D_abs)
    I <- diag(n)
    
    # Compute (I + |D|)^(n-1)
    power_matrix <- I + D_abs
    if (n > 1) {
      for (k in 2:(n-1)) {
        power_matrix <- power_matrix %*% (I + D_abs)
      }
    }
    
    # Check if all entries are positive
    if (all(power_matrix > tolerance)) {
      result$matrix_is_irreducible <- TRUE
    } else {
      result$message <- "Matrix D is not irreducible (influence graph not strongly connected)"
      return(result)
    }
    
    # Check 3: System is well-conditioned
    I_minus_D <- I - obj$D
    condition_num <- kappa(I_minus_D)
    result$condition_number <- condition_num
    
    if (condition_num < 1e12) {
      result$well_conditioned <- TRUE
    } else {
      result$message <- paste("System is ill-conditioned (κ =", 
                              format(condition_num, scientific = TRUE), ")")
      return(result)
    }
    
    # All checks passed
    result$valid <- TRUE
    result$message <- "All Theorem 1 assumptions satisfied"
    
  }, error = function(e) {
    result$message <- paste("Error checking assumptions:", e$message)
  })
  
  return(result)
}

#' Compute Analytical Sensitivity Matrix
#'
#' Computes the sensitivity matrix using analytical differentiation
#' based on eigenvalue perturbation theory.
#'
#' @param obj DEMATEL_Sensitivity object
#'
#' @return Updated DEMATEL_Sensitivity object with sensitivity matrix
#'
#' @details
#' Uses the formula: ∂λmax/∂aij = v^T (∂T/∂aij) u
#' where v and u are the left and right eigenvectors corresponding
#' to the dominant eigenvalue λmax.
#'
#' @examples
#' A <- matrix(c(0, 3, 2, 2, 0, 3, 1, 2, 0), nrow = 3, byrow = TRUE)
#' sens_obj <- DEMATEL_Sensitivity(A)
#' sens_obj <- compute_sensitivity_analytical(sens_obj)
#'
#' @export
compute_sensitivity_analytical <- function(obj) {
  UseMethod("compute_sensitivity_analytical")
}

#' @export
compute_sensitivity_analytical.DEMATEL_Sensitivity <- function(obj) {
  n <- obj$n
  
  # Assumption checks (same as before)
  assumption_check <- check_theorem1_assumptions(obj)
  if (!assumption_check$valid) {
    warning(paste("Theorem assumptions not satisfied:", assumption_check$message,
                  "\nFalling back to numerical method."))
    return(compute_sensitivity_numerical(obj))
  }
  
  cat("Theorem 2 assumptions satisfied. Computing full analytical sensitivity...\n")
  
  tryCatch({
    # Eigendecomposition of T
    eigen_result <- eigen(obj$T)
    eigenvalues <- eigen_result$values
    eigenvectors <- eigen_result$vectors
    
    eigenvalues_real <- Re(eigenvalues)
    dominant_idx <- which.max(eigenvalues_real)
    lambda_max <- Re(eigenvalues[dominant_idx])
    
    # Extract right eigenvector
    u <- Re(eigenvectors[, dominant_idx])
    
    # Left eigenvector
    eigen_result_T <- eigen(t(obj$T))
    eigenvalues_T_real <- Re(eigen_result_T$values)
    left_dominant_idx <- which.min(abs(eigenvalues_T_real - lambda_max))
    v <- Re(eigen_result_T$vectors[, left_dominant_idx])
    
    # Normalize so that v^T u = 1
    inner_product <- as.numeric(t(v) %*% u)
    if (inner_product < 0) {
      v <- -v
      inner_product <- -inner_product
    }
    v <- v / inner_product
    
    # Scaling factor s
    row_sums <- rowSums(obj$A)
    col_sums <- colSums(obj$A)
    max_row <- max(row_sums)
    max_col <- max(col_sums)
    s <- max(max_row, max_col)
    
    if (s == 0) stop("Scaling factor is zero")
    
    # Local effect (outer product form)
    I <- diag(n)
    S <- I + obj$T
    left  <- as.numeric(t(v) %*% S)   # row effect
    right <- as.numeric(S %*% u)      # column effect
    local_matrix <- (1/s) * outer(left, right)
    
    # Normalization penalty term
    C <- as.numeric(t(v) %*% (S %*% obj$A %*% S) %*% u)
    
    # δ_ij mask: 1 if in critical row or column, 0 otherwise
    delta <- matrix(0, n, n)
    if (s == max_row) {
      critical_rows <- which(row_sums == max_row)
      delta[critical_rows, ] <- 1
    }
    if (s == max_col) {
      critical_cols <- which(col_sums == max_col)
      delta[, critical_cols] <- 1
    }
    
    normalization_matrix <- -(C / s^2) * delta
    
    # Full sensitivity = local effect + normalization effect
    sensitivity_matrix <- local_matrix + normalization_matrix
    
    # Add row and column names
    rownames(sensitivity_matrix) <- obj$factor_names
    colnames(sensitivity_matrix) <- obj$factor_names
    
    obj$sensitivity_matrix <- sensitivity_matrix
    obj$computation_method <- "analytical_theorem2"
    obj$assumptions_check <- assumption_check
    
    cat("\nAnalytical sensitivity computation completed successfully (Theorem 2).\n")
    cat("Sensitivity range:", range(as.vector(sensitivity_matrix)), "\n")
    
    return(obj)
    
  }, error = function(e) {
    warning(paste("Analytical method failed:", e$message, 
                  "\nFalling back to numerical method."))
    return(compute_sensitivity_numerical(obj))
  })
}

# compute_sensitivity_analytical.DEMATEL_Sensitivity <- function(obj) {
#   n <- obj$n
#   
#   # Check assumptions for Theorem 1
#   assumption_check <- check_theorem1_assumptions(obj)
#   if (!assumption_check$valid) {
#     warning(paste("Theorem 1 assumptions not satisfied:", assumption_check$message,
#                   "\nFalling back to numerical method."))
#     return(compute_sensitivity_numerical(obj))
#   }
#   
#   cat("Theorem 1 assumptions satisfied. Computing analytical sensitivity...\n")
#   
#   tryCatch({
#     # Get eigendecomposition of T
#     eigen_result <- eigen(obj$T)
#     eigenvalues <- eigen_result$values
#     eigenvectors <- eigen_result$vectors
#     
#     # Find dominant eigenvalue by real part
#     eigenvalues_real <- Re(eigenvalues)
#     dominant_idx <- which.max(eigenvalues_real)
#     lambda_max_complex <- eigenvalues[dominant_idx]
#     lambda_max <- Re(lambda_max_complex)
#     
#     # Check if dominant eigenvalue is real
#     if (abs(Im(lambda_max_complex)) > 1e-12) {
#       cat("WARNING: Dominant eigenvalue has imaginary part. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     # Extract right eigenvector
#     u_complex <- eigenvectors[, dominant_idx]
#     
#     # Verify eigenvector is essentially real
#     max_imag_u <- max(abs(Im(u_complex)))
#     
#     if (max_imag_u > 1e-10) {
#       cat("WARNING: Right eigenvector has significant imaginary part. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     u <- Re(u_complex)
#     
#     # For left eigenvector, find the eigenvector of T^T corresponding to the same eigenvalue
#     eigen_result_T <- eigen(t(obj$T))
#     eigenvalues_T <- eigen_result_T$values
#     eigenvectors_T <- eigen_result_T$vectors
#     
#     eigenvalues_T_real <- Re(eigenvalues_T)
#     
#     # Find closest match to lambda_max
#     diff_from_lambda_max <- abs(eigenvalues_T_real - lambda_max)
#     left_dominant_idx <- which.min(diff_from_lambda_max)
#     
#     if (min(diff_from_lambda_max) > 1e-10) {
#       cat("WARNING: Cannot find matching left eigenvalue. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     v_complex <- eigenvectors_T[, left_dominant_idx]
#     
#     # Verify left eigenvector is essentially real
#     max_imag_v <- max(abs(Im(v_complex)))
#     
#     if (max_imag_v > 1e-10) {
#       cat("WARNING: Left eigenvector has significant imaginary part. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     v <- Re(v_complex)
#     
#     # Ensure proper orientation of eigenvectors
#     inner_product_raw <- as.numeric(t(v) %*% u)
#     
#     if (inner_product_raw < 0) {
#       v <- -v
#       inner_product_raw <- -inner_product_raw
#     }
#     
#     if (abs(inner_product_raw) < 1e-12) {
#       cat("ERROR: Eigenvectors are orthogonal after sign correction\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     # Normalize so that v^T u = 1
#     v <- v / inner_product_raw
#     
#     # Verify normalization
#     final_inner_product <- as.numeric(t(v) %*% u)
#     
#     if (abs(final_inner_product - 1) > 1e-10) {
#       cat("WARNING: Normalization verification failed\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     # Verify eigenvector equations
#     Tu <- obj$T %*% u
#     lambda_u <- lambda_max * u
#     right_eigen_error <- max(abs(Tu - lambda_u))
#     
#     Tv <- t(obj$T) %*% v
#     lambda_v <- lambda_max * v
#     left_eigen_error <- max(abs(Tv - lambda_v))
#     
#     if (right_eigen_error > 1e-8 || left_eigen_error > 1e-8) {
#       cat("WARNING: Eigenvector verification failed. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     # Get scaling factor s from normalization
#     s <- max(max(rowSums(obj$A)), max(colSums(obj$A)))
#     if (s == 0) {
#       stop("Scaling factor is zero")
#     }
#     
#     
#     # NEW: use sandwich formula
#     I <- diag(n)
#     S <- I + T
#     
#     left  <- as.numeric(t(v) %*% S)   # length-n row vector
#     right <- as.numeric(S %*% u)      # length-n column vector
#     
#     sensitivity_matrix <- (1/s) * outer(left, right)  # outer product: sens[i,j] = left[i] * right[j]
#     
#    
#     # Final verification: sensitivity matrix should not be all zeros
#     sens_range <- range(sensitivity_matrix)
#     
#     if (diff(sens_range) < 1e-15) {
#       cat("WARNING: Sensitivity matrix is essentially zero. Using numerical method.\n")
#       return(compute_sensitivity_numerical(obj))
#     }
#     
#     # Add row and column names
#     rownames(sensitivity_matrix) <- obj$factor_names
#     colnames(sensitivity_matrix) <- obj$factor_names
#     
#     obj$sensitivity_matrix <- sensitivity_matrix
#     obj$computation_method <- "analytical_theorem1"
#     obj$assumptions_check <- assumption_check
#     
#     cat("\nAnalytical sensitivity computation completed successfully.\n")
#     cat("Sensitivity range:", range(as.vector(sensitivity_matrix)), "\n")
#     
#     return(obj)
#     
#   }, error = function(e) {
#     warning(paste("Analytical method failed:", e$message, 
#                   "\nFalling back to numerical method."))
#     return(compute_sensitivity_numerical(obj))
#   })
# }