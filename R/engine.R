# The interface's only contact with the mathematics.
#
# Every spectral quantity this application shows comes from the spectralDEMATEL
# package, which is the single implementation of every definition. Nothing in
# this file computes a diagnostic; it calls the engine and shapes the answer for
# the screen. That is the whole point of the boundary: the same numbers serve a
# script, a batch job and this app, and a second implementation is exactly how
# they drifted apart before.
#
# Install with:
#   install.packages("spectralDEMATEL",
#                    repos = c("https://pdelias.r-universe.dev",
#                              "https://cloud.r-project.org"))

#' Diagnose one matrix.
#'
#' @param A square numeric matrix, already parsed by the interface.
#' @param factor_names character vector, or NULL for F1..Fn.
#' @param type "A" for a direct influence matrix, "T" for a published
#'   total-relation matrix.
#' @param case_name label carried through to the display.
#'
#' @return A list holding every diagnostic, the assumption checks, the engine
#'   version, and the matrices the display needs. Always the same shape, even
#'   when the matrix is inadmissible: in that case the numbers are NA and the
#'   checks say why.
run_diagnosis <- function(A, factor_names = NULL, type = c("A", "T"),
                          case_name = "Current Analysis") {
  type <- match.arg(type)

  d <- spectralDEMATEL::spectral_diagnostics(A, type = type)

  n <- if (is.matrix(A) && nrow(A) == ncol(A)) nrow(A) else NA_integer_
  if (is.null(factor_names) && !is.na(n)) factor_names <- paste0("F", seq_len(n))

  # D and T are needed by the display and by the sensitivity object. For a
  # published T matrix there is no normalisation to undo, so D is unavailable.
  m <- if (type == "A") spectralDEMATEL::dematel(A) else NULL
  D_matrix <- if (!is.null(m)) m$D else NULL
  T_matrix <- if (!is.null(m)) m$T else if (type == "T") A else NULL

  c(d, list(
    case_name    = case_name,
    factor_names = factor_names,
    A_matrix     = A,
    D_matrix     = D_matrix,
    T_matrix     = T_matrix,
    input_type   = type,
    computable   = !is.na(d$mu_max)
  ))
}

#' The diagnostics table.
#'
#' Every row is a quantity defined in the source paper. Quantities the previous
#' version of this application showed and this one does not -- the spectral
#' radius, the minimum eigenvalue, a convergence rate, a concentration ratio,
#' an eigenvector range -- were not part of any definition and two of them were
#' computed wrongly. They are gone rather than corrected.
diagnostics_table <- function(res) {
  if (!isTRUE(res$computable)) {
    return(data.frame(
      Metric = "Not computable",
      Value = "-",
      Description = "See the assumption checks below for the reason.",
      stringsAsFactors = FALSE))
  }

  num <- function(x, digits = 4) formatC(x, format = "f", digits = digits)

  data.frame(
    Metric = c(
      "Coupling (μₘₐₓ)",
      "Total-effect multiplier",
      "Dominant eigenvalue (λₘₐₓ)",
      "Indirect effects dominant",
      "Mode dominance",
      "Hierarchy — SD (high = concentrated)",
      "Hierarchy — Gini (high = concentrated)",
      "Hierarchy — participation ratio (LOW = concentrated)",
      "Eigenvalue condition number"),
    Value = c(
      num(res$mu_max),
      num(res$multiplier, 2),
      num(res$lambda_max),
      if (isTRUE(res$indirect_dominant)) "Yes" else "No",
      num(res$dominance),
      num(res$hierarchy_sd),
      num(res$hierarchy_gini),
      num(res$hierarchy_pr),
      num(res$ev_condition, 2)),
    Description = c(
      "Distance to criticality, in (0,1). Higher means more strongly coupled.",
      "Total effects relative to direct ones. Equals 1/(1-μ) and 1+λ.",
      "Largest eigenvalue of the total-relation matrix.",
      "True when λ > 1, equivalently μ > 0.5. Indirect propagation outweighs direct.",
      "|λ₂|/λₘₐₓ, largest modulus below the dominant one. Low means a single propagation mode.",
      "Standard deviation of the entry profile. HIGH means influence enters at a few factors.",
      "Gini of the same vector, size-free. HIGH means influence enters at a few factors.",
      "Participation ratio over n. LOW means influence enters at a few factors — this one runs the other way.",
      "Bounds how far the per-link sensitivity estimates can be trusted. At least 1; large values mean the derivative is locally uninformative."),
    stringsAsFactors = FALSE
  )
}

#' The assumption checks, shaped for display.
#'
#' `skipped` is rendered distinctly from `pass`, because it is not one: it means
#' a prerequisite failed and the check was never evaluated.
checks_table <- function(res) {
  ck <- res$checks
  symbol <- c(pass = "✅ pass", warn = "⚠️ warn",
              fail = "❌ fail", skipped = "– not evaluated")

  factors <- vapply(ck$factors, function(f) {
    if (length(f) == 0) "" else paste(f, collapse = ", ")
  }, character(1))

  data.frame(
    Check   = gsub("_", " ", ck$check),
    Verdict = unname(symbol[ck$verdict]),
    Factors = factors,
    Reason  = ck$reason,
    stringsAsFactors = FALSE
  )
}

#' One line summarising whether the matrix is in scope, for a header.
checks_summary <- function(res) {
  ck <- res$checks
  n_fail <- sum(ck$verdict == "fail")
  n_warn <- sum(ck$verdict == "warn")

  if (n_fail > 0) {
    sprintf(paste("%d assumption%s not met. The diagnostics below are computed",
                  "regardless, as published studies do, but read them with that",
                  "in mind."), n_fail, if (n_fail == 1) "" else "s")
  } else if (n_warn > 0) {
    sprintf("In scope, with %d point%s worth noting.",
            n_warn, if (n_warn == 1) "" else "s")
  } else {
    "Every assumption is met."
  }
}

#' Whether the per-link sensitivity ranking may be shown at face value.
#'
#' The ranking and the condition number ship together or neither ships: a
#' first-order estimate with a large condition number is locally uninformative,
#' and a ranking presented without that caveat misleads.
sensitivity_is_reliable <- function(res) {
  isTRUE(res$computable) && res$ev_condition < 5
}

#' The caveat that accompanies the sensitivity ranking, always shown with it.
sensitivity_caveat <- function(res) {
  if (!isTRUE(res$computable)) return("")
  if (sensitivity_is_reliable(res)) {
    sprintf(paste("Eigenvalue condition number %.2f: these first-order",
                  "estimates can be read at face value."), res$ev_condition)
  } else {
    sprintf(paste("⚠️ Eigenvalue condition number %.1f. First-order",
                  "estimates lose roughly that factor of accuracy, so treat the",
                  "ranking below as indicative rather than exact. Long chains",
                  "and deep hierarchies produce this."), res$ev_condition)
  }
}

#' A long-format export carrying the diagnostics, the checks and the engine
#' version together.
#'
#' The checks travel with the numbers deliberately. Someone will run this over
#' fifty matrices scraped from papers and publish the result, and a reader has
#' to be able to see which of the fifty were in scope.
export_frame <- function(res) {
  quant <- c("n", "mu_max", "multiplier", "lambda_max", "indirect_dominant",
             "dominance", "hierarchy_sd", "hierarchy_pr", "hierarchy_gini",
             "ev_condition")

  out <- data.frame(
    item     = quant,
    kind     = "diagnostic",
    value    = vapply(quant, function(q) as.numeric(res[[q]]), numeric(1)),
    verdict  = NA_character_,
    detail   = c("factors",
                 "coupling in (0,1)",
                 "total effects relative to direct",
                 "dominant eigenvalue of T",
                 "1 = indirect effects exceed direct",
                 "|lambda_2|/lambda_max; low = single mode",
                 "hierarchy, HIGH = concentrated",
                 "hierarchy, LOW = concentrated",
                 "hierarchy, HIGH = concentrated, size-free",
                 "sensitivity reliability, >= 1"),
    stringsAsFactors = FALSE)

  ck <- res$checks
  checks <- data.frame(
    item    = ck$check,
    kind    = "check",
    value   = ck$value,
    verdict = ck$verdict,
    detail  = ck$reason,
    stringsAsFactors = FALSE)

  meta <- data.frame(
    item = c("engine_version", "input_type"),
    kind = "provenance",
    value = NA_real_,
    verdict = NA_character_,
    detail = c(res$engine_version, res$input_type),
    stringsAsFactors = FALSE)

  out <- rbind(out, checks, meta)
  rownames(out) <- NULL
  out
}

#' The assumption checks as plain text, for the exported reports.
checks_text <- function(res, indent = "  ") {
  ck <- res$checks
  mark <- c(pass = "PASSED", warn = "WARNING",
            fail = "FAILED", skipped = "not evaluated")

  lines <- vapply(seq_len(nrow(ck)), function(i) {
    who <- ck$factors[[i]]
    paste0(indent, sprintf("%-26s %s", gsub("_", " ", ck$check[i]),
                           mark[ck$verdict[i]]),
           if (length(who)) paste0("  [factors: ", paste(who, collapse = ", "), "]") else "",
           "\n", indent, "    ", ck$reason[i], "\n")
  }, character(1))

  paste0("ASSUMPTION CHECKS\n", paste(rep("-", 35), collapse = ""), "\n",
         paste(lines, collapse = ""), "\n")
}

#' The diagnostics as plain text, for the exported reports.
diagnostics_text <- function(res, indent = "  ") {
  if (!isTRUE(res$computable)) {
    return("DIAGNOSTICS\n  Not computable for this matrix; see the checks above.\n")
  }
  tbl <- diagnostics_table(res)
  paste0("DIAGNOSTICS\n", paste(rep("-", 35), collapse = ""), "\n",
         paste(sprintf("%s%-52s %s\n", indent, tbl$Metric, tbl$Value),
               collapse = ""),
         indent, "engine: spectralDEMATEL ", res$engine_version, "\n\n")
}

#' Entry and accumulation as two rankings, never one blended score.
#'
#' Prominence, the standard DEMATEL deliverable, adds dispatch to absorption.
#' Showing all three side by side is what lets a user see them disagree.
profile_table <- function(res) {
  if (!isTRUE(res$computable) || is.null(res$T_matrix)) return(NULL)

  prominence <- rowSums(res$T_matrix) + colSums(res$T_matrix)
  data.frame(
    Factor            = res$factor_names,
    Entry             = round(res$entry_points, 4),
    Entry_rank        = rank(-res$entry_points, ties.method = "min"),
    Accumulation      = round(res$accumulation, 4),
    Accumulation_rank = rank(-res$accumulation, ties.method = "min"),
    Prominence        = round(prominence, 4),
    Prominence_rank   = rank(-prominence, ties.method = "min"),
    stringsAsFactors  = FALSE
  )
}
