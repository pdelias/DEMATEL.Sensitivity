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

  T <- D %*% solve(I - D)

  # Dominant eigenvalue
  eigenvals <- eigen(T, only.values = TRUE)$values
  lambda_max <- max(Re(eigenvals))

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

  tryCatch({
    # Get eigenvectors
    eigen_result <- eigen(obj$T)
    max_idx <- which.max(Re(eigen_result$values))

    # Right eigenvector
    u <- Re(eigen_result$vectors[, max_idx])
    u <- u / sqrt(sum(u^2))  # Normalize

    # Left eigenvector (for non-symmetric matrices)
    eigen_result_T <- eigen(t(obj$T))
    max_idx_T <- which.max(Re(eigen_result_T$values))
    v <- Re(eigen_result_T$vectors[, max_idx_T])

    # Normalize so that v^T u = 1
    normalization_factor <- as.numeric(t(v) %*% u)
    if (abs(normalization_factor) < 1e-12) {
      stop("Cannot normalize eigenvectors - they may be orthogonal")
    }
    v <- v / normalization_factor

    sensitivity_matrix <- matrix(0, nrow = n, ncol = n)

    cat("Computing sensitivity matrix using analytical method...\n")
    pb <- txtProgressBar(min = 0, max = n^2, style = 3)

    for (i in 1:n) {
      for (j in 1:n) {
        # Compute dT/da_ij using finite differences for chain rule
        dT_daij <- compute_dT_daij(obj, i, j)
        sensitivity_matrix[i, j] <- as.numeric(t(v) %*% dT_daij %*% u)

        setTxtProgressBar(pb, (i-1)*n + j)
      }
    }
    close(pb)

    # Add row and column names
    rownames(sensitivity_matrix) <- obj$factor_names
    colnames(sensitivity_matrix) <- obj$factor_names

    obj$sensitivity_matrix <- sensitivity_matrix
    obj$computation_method <- "analytical"

    cat("\nAnalytical sensitivity matrix computation completed.\n")

    return(obj)

  }, error = function(e) {
    warning("Analytical method failed, falling back to numerical method: ", e$message)
    return(compute_sensitivity_numerical(obj))
  })
}

#' Compute Derivative of T Matrix
#'
#' Helper function to compute dT/da_ij using finite differences
#'
#' @param obj DEMATEL_Sensitivity object
#' @param i Row index
#' @param j Column index
#' @param epsilon Step size for finite differences
#' @return Matrix representing dT/da_ij
#' @keywords internal
compute_dT_daij <- function(obj, i, j, epsilon = 1e-8) {
  A_pert <- obj$A
  A_pert[i, j] <- A_pert[i, j] + epsilon

  dematel_pert <- compute_dematel_matrices(A_pert)
  dT_daij <- (dematel_pert$T - obj$T) / epsilon

  return(dT_daij)
}
