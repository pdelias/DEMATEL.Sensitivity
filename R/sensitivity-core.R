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

  # Every number in this object comes from spectralDEMATEL. This function
  # arranges them into the shape the rest of the application already expects;
  # it computes nothing. The previous version implemented the DEMATEL pipeline
  # a second time here, and the two implementations drifted apart.

  d <- spectralDEMATEL::spectral_diagnostics(A)
  m <- spectralDEMATEL::dematel(A)

  if (is.null(m)) {
    # Inadmissible input. Return the object with empty numbers and the checks
    # attached, so the caller can say why rather than crash.
    obj <- list(A = A, D = NULL, T = NULL, lambda_max = NA_real_,
                factor_names = factor_names, n = if (is.matrix(A)) nrow(A) else NA_integer_,
                sensitivity_matrix = NULL, computation_method = NULL,
                assumptions_check = d$checks, diagnostics = d)
    class(obj) <- "DEMATEL_Sensitivity"
    return(obj)
  }

  n <- nrow(A)
  if (is.null(factor_names)) factor_names <- paste0("F", seq_len(n))
  if (length(factor_names) != n) factor_names <- paste0("F", seq_len(n))

  obj <- list(
    A                  = A,
    D                  = m$D,
    T                  = m$T,
    lambda_max         = d$lambda_max,
    factor_names       = factor_names,
    n                  = n,
    sensitivity_matrix = NULL,
    computation_method = NULL,
    assumptions_check  = d$checks,
    diagnostics        = d
  )

  class(obj) <- "DEMATEL_Sensitivity"
  obj
}

#' Compute DEMATEL Matrices
#'
#' Internal function to compute D and T matrices and dominant eigenvalue
#'
#' @param A Direct influence matrix
#' @return List containing D, T matrices and lambda_max
#' @keywords internal
compute_dematel_matrices <- function(A) {
  # Kept only so older scripts calling it keep working. The pipeline lives in
  # spectralDEMATEL::dematel(); nothing is computed here.
  m <- spectralDEMATEL::dematel(A)
  if (is.null(m)) return(NULL)
  list(D = m$D, T = m$T,
       lambda_max = spectralDEMATEL::spectral_diagnostics(A, checks = FALSE)$lambda_max)
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
#' sens_obj <- compute_sensitivity_analytical(sens_obj)
#'
#' @export
# compute_sensitivity_numerical <- function(obj, epsilon = 0.01) {
#   UseMethod("compute_sensitivity_numerical")
# }

#' @export
# compute_sensitivity_numerical.DEMATEL_Sensitivity <- function(obj, epsilon = 0.01) {
#   
#   if (epsilon <= 0) {
#     stop("epsilon must be positive")
#   }
#   
#   n <- obj$n
#   sensitivity_matrix <- matrix(0, nrow = n, ncol = n)
#   
#   cat("Computing sensitivity matrix using numerical method...\n")
#   cat("This may take a moment for large matrices.\n")
#   
#   pb <- txtProgressBar(min = 0, max = n^2, style = 3)
#   
#   for (i in 1:n) {
#     for (j in 1:n) {
#       # Create perturbed matrix
#       A_pert <- obj$A
#       A_pert[i, j] <- A_pert[i, j] + epsilon
#       
#       tryCatch({
#         # Compute perturbed system
#         dematel_pert <- compute_dematel_matrices(A_pert)
#         lambda_max_pert <- dematel_pert$lambda_max
#         
#         # Numerical derivative
#         sensitivity_matrix[i, j] <- (lambda_max_pert - obj$lambda_max) / epsilon
#       }, error = function(e) {
#         warning(paste("Could not compute sensitivity for element (", i, ",", j, "): ", e$message))
#         sensitivity_matrix[i, j] <- NA
#       })
#       
#       setTxtProgressBar(pb, (i-1)*n + j)
#     }
#   }
#   close(pb)
#   
#   # Add row and column names
#   rownames(sensitivity_matrix) <- obj$factor_names
#   colnames(sensitivity_matrix) <- obj$factor_names
#   
#   obj$sensitivity_matrix <- sensitivity_matrix
#   obj$computation_method <- "numerical"
#   
#   cat("\nSensitivity matrix computation completed.\n")
#   
#   return(obj)
# }

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

  # The closed form lives in spectralDEMATEL::sensitivity_matrix(), where it is
  # checked against finite differences by the package test suite. There is no
  # numerical fallback any more: the engine returns NULL for a matrix it cannot
  # process, and the assumption checks say why.

  s <- spectralDEMATEL::sensitivity_matrix(obj$A)
  if (is.null(s)) {
    obj$sensitivity_matrix <- NULL
    obj$computation_method <- "not computable"
    return(obj)
  }

  dimnames(s$total) <- dimnames(s$local) <- dimnames(s$normalization) <-
    list(obj$factor_names, obj$factor_names)

  obj$sensitivity_matrix        <- s$total
  obj$sensitivity_local         <- s$local
  obj$sensitivity_normalization <- s$normalization
  obj$computation_method        <- "analytical (spectralDEMATEL)"

  # The condition number travels with the ranking. A first-order estimate with
  # a large condition number is locally uninformative, and a ranking shown
  # without that caveat misleads. The two ship together or neither ships.
  obj$ev_condition <- obj$diagnostics$ev_condition

  obj
}
