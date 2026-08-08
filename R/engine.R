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

#' A matrix in long form, one row per cell.
#'
#' Replaces reshape2::melt(), which was the only reason this application loaded
#' reshape2, and reshape2 pulled plyr, Rcpp, stringr and stringi in behind it.
#' Measured against the shinylive export: 41.0 MB of packages before, 27.5 MB
#' after, for two calls.
#'
#' Output is identical() to reshape2::melt() for a matrix with dimnames, which
#' is every matrix that reaches it here. The explicit levels are the part that
#' matters: the data.frame default sorts them alphabetically, so a matrix
#' labelled Cost/Quality/Speed/Risk would come back with Risk before Speed.
#' unique() is what makes duplicate factor names -- a pasted CSV with a repeated
#' header -- an ordinary matrix rather than an error.
melt_matrix <- function(m) {
  data.frame(
    Var1  = factor(rep(rownames(m), times = ncol(m)), levels = unique(rownames(m))),
    Var2  = factor(rep(colnames(m), each  = nrow(m)), levels = unique(colnames(m))),
    value = as.vector(m)
  )
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

#' The diagnosis in plain language, before any symbol.
#'
#' A user arriving at this application has a matrix and a question, not a taste
#' for spectral graph theory. Everything the mathematics establishes can be said
#' in four sentences, and those four sentences come first. The symbols are still
#' there, one scroll down, for anyone who wants them.
#'
#' Written in the second person on purpose: this is the one place in the
#' application that addresses the user directly, because it is describing their
#' system rather than stating a general result.
plain_verdict <- function(res) {
  if (!isTRUE(res$computable)) {
    return(list(
      headline = "This matrix cannot be diagnosed yet",
      lines = "Something in it needs fixing first. The checks below say what."))
  }

  amplifies <- isTRUE(res$indirect_dominant)
  concentrated <- res$hierarchy_sd > 0.10
  single_mode <- res$dominance < 0.10

  headline <- if (amplifies && concentrated)
    "A system with one dominant entry point, and strong knock-on effects"
  else if (amplifies && !concentrated)
    "A system that amplifies, with no single dominant factor"
  else if (!amplifies && concentrated)
    "A system with one dominant entry point, and little amplification"
  else
    "A system where effects stay close to where they are applied"

  lines <- c(
    sprintf(paste("**Knock-on effects.** Counting every indirect path, total",
                  "influence runs %.1f times the ratings you entered. %s"),
            res$multiplier,
            if (amplifies) paste("Most of what an intervention achieves here",
                                 "arrives indirectly, so options have to be",
                                 "judged on their total effect rather than on",
                                 "the direct ratings.")
            else paste("Direct influence carries most of what follows, so the",
                       "ratings you entered already show most of what an",
                       "intervention would produce.")),

    sprintf(paste("**Where to push.** %s"),
            if (concentrated) paste("Influence enters the system through a few",
                                    "factors rather than spreading evenly, so",
                                    "there is a preferred place to act. The entry",
                                    "ranking below names it.")
            else paste("Influence enters fairly evenly across factors. No single",
                       "one offers materially more leverage than another, so a",
                       "coordinated set of changes will do more than any one",
                       "of them.")),

    sprintf(paste("**Is one ranking enough?** %s"),
            if (single_mode) paste("One pattern of propagation governs this",
                                   "system, so a single ordering of factors",
                                   "describes it well.")
            else paste("A second pattern of propagation competes with the first,",
                       "so any single ranking of factors is hiding a",
                       "disagreement. Worth looking at before acting on one."))
  )

  list(headline = headline, lines = lines)
}

#' Plain-English glossary, so a user never has to leave to look a term up.
#'
#' Kept beside the numbers rather than in a separate document: a glossary a user
#' has to go and find is a glossary nobody reads.
metric_glossary <- function() {
  data.frame(
    Term = c("Coupling (μ max)",
             "Total-effect multiplier",
             "Indirect effects dominant",
             "Mode dominance",
             "Hierarchy",
             "Entry profile",
             "Accumulation profile",
             "Prominence",
             "Eigenvalue condition number",
             "Surrogate baseline",
             "Structural type"),
    `In plain words` = c(
      "How much the system feeds back into itself. Near 0, influence dies out quickly; near 1, it circulates and builds.",
      "How much bigger total influence is than the ratings you entered. A multiplier of 4 means indirect paths carry three times as much as the direct ones.",
      "Whether the knock-on effects outweigh the direct ones. True whenever the multiplier is above 2.",
      "Whether one pattern of propagation governs the system or two compete. Low is one pattern; high means a single ranking of factors hides a disagreement.",
      "How unevenly influence enters. High means a few factors are the way in; low means it enters everywhere at once.",
      "How much influence each factor injects into the system. Where to apply pressure.",
      "How much influence lands on each factor. Where effects end up.",
      "The standard DEMATEL score: what a factor dispatches plus what it absorbs. Adding those two together can rank an absorber above the factor actually driving the system.",
      "How far the per-link estimates can be trusted. 1 is ideal; large values mean the estimate is locally uninformative.",
      "Your own ratings, shuffled at random many times. If your numbers look like the shuffles, they follow from how you rated rather than from what you connected.",
      "Which of four corners of the map your system falls in, from its coupling and its hierarchy."),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' How firmly the type is held, in words.
#'
#' The engine returns the margin to each cut and stops there, because how much
#' to hedge is a presentation decision. This is that decision, in one place.
#'
#' The scale is the margin expressed in corpus interquartile ranges, so it means
#' the same thing on both axes.
type_confidence <- function(st) {
  if (is.null(st)) return(list(level = "none", phrase = ""))
  m <- st$nearest_margin_scaled
  axis <- if (st$nearest_cut == "coupling") "coupling" else "hierarchy"

  if (m < 0.10) {
    list(level = "borderline", phrase = sprintf(paste(
      "This system sits almost exactly on the %s boundary. The type below is",
      "the side it falls on, not a reading you should rely on — a small",
      "change to one rating would move it."), axis))
  } else if (m < 0.35) {
    list(level = "close", phrase = sprintf(paste(
      "This system is close to the %s boundary. The type below holds, but not",
      "with much room."), axis))
  } else {
    list(level = "clear", phrase = sprintf(paste(
      "This system is well clear of both boundaries; the nearest is %s."),
      axis))
  }
}

#' The type card: what kind of system this is, and how much to trust that.
#'
#' Everything here is quoted or computed. The interventions are the source
#' paper's own wording, and the caveat travels with them because a pairing of
#' structure with a prescription is a hypothesis, not a validated result, and an
#' interface that renders it as an instruction will be read as stronger than the
#' evidence.
type_card <- function(res) {
  if (!isTRUE(res$computable)) return(NULL)
  st <- spectralDEMATEL::structural_type(res)
  if (is.null(st)) return(NULL)

  conf <- type_confidence(st)
  stab <- spectralDEMATEL::type_stability(res)
  tr   <- spectralDEMATEL::tradeoff_residual(res)

  list(
    type        = st$type,
    confidence  = conf$level,
    headline    = conf$phrase,
    logic       = st$intervention_logic,
    caveat      = st$caveat,
    margins = sprintf(paste(
      "Coupling %.3f, %s the 0.50 cut by %.3f. Hierarchy %.3f, %s the %.2f cut",
      "by %.3f."),
      res$mu_max, if (st$coupling_margin >= 0) "above" else "below",
      abs(st$coupling_margin),
      res$hierarchy_sd, if (st$hierarchy_margin >= 0) "above" else "below",
      st$cuts$hierarchy, abs(st$hierarchy_margin)),
    corpus = sprintf(paste(
      "%.0f%% of the 117 reference systems are of this type%s. Their median",
      "total-effect multiplier is %.2f; this system's is %.2f."),
      100 * st$corpus_share,
      if (st$corpus_share < 0.15) ", so this is an uncommon corner of the map" else "",
      st$corpus_multiplier, res$multiplier),
    stability = if (is.null(stab)) "" else if (isTRUE(stab$stable))
      sprintf(paste("The type holds across every hierarchy cut from %.3f to",
                    "%.3f, so it does not depend on where that line is drawn."),
              min(stab$by_cut$cut), max(stab$by_cut$cut))
      else sprintf(paste("The type changes between hierarchy cuts %.3f and",
                         "%.3f. Since that cut is a recommendation rather than",
                         "a fitted constant, treat this reading as borderline."),
                   stab$flips_between[1], stab$flips_between[2]),
    tradeoff = if (is.null(tr)) "" else sprintf(paste(
      "Against the corpus trade-off, hierarchy sits %.2f residual standard",
      "deviations %s what this system's coupling predicts (%.3f expected,",
      "%.3f observed). %s"),
      abs(tr$residual_sd), tr$direction, tr$expected, res$hierarchy_sd,
      tr$caveat),
    cut_note = if (isTRUE(st$cuts$hierarchy_is_default))
      paste("The coupling cut at 0.50 is the indirect-dominance threshold and",
            "follows from the algebra. The hierarchy cut at 0.10 is a",
            "recommendation, not a fitted constant.")
      else sprintf(paste("Classified with a user-defined hierarchy cut of %.3f",
                         "rather than the recommended 0.10."), st$cuts$hierarchy)
  )
}

#' The structure map: the user's system against the corpus reference band.
#'
#' The 117 individual systems are not plotted, because they are not ours to
#' publish. What is shown is the corpus spread on each axis and the fitted
#' trade-off, which is what a user actually needs to know where they sit.
structure_map <- function(res) {
  if (!isTRUE(res$computable)) return(NULL)
  cp <- spectralDEMATEL:::CORPUS
  st <- spectralDEMATEL::structural_type(res)

  mu <- seq(0.05, 0.98, length.out = 100)
  line <- data.frame(mu_max = mu,
                     hierarchy = cp$tradeoff$intercept + cp$tradeoff$slope * mu)
  line$lo <- line$hierarchy - cp$tradeoff$resid_sd
  line$hi <- line$hierarchy + cp$tradeoff$resid_sd

  ytop <- max(0.30, res$hierarchy_sd * 1.15)
  hcut <- st$cuts$hierarchy
  ccut <- st$cuts$coupling

  # Name every quadrant, not just the one the system landed in. A user needs to
  # see what the other three would have meant, and how uneven the corpus is
  # across them -- a system in the 7% corner deserves to know it is unusual.
  share <- function(ty) sprintf("%s\n%.0f%% of corpus",
                               ty, 100 * CORPUS_SHARE[[ty]])
  # Placed in the outer corner of each quadrant rather than its centre: the
  # trade-off band runs diagonally through every centre, and so does the point
  # itself. A label a user has to read through a ribbon is not a label.
  pad <- 0.02
  quad <- data.frame(
    x     = c(pad,    1 - pad, pad,        1 - pad),
    y     = c(pad * ytop, pad * ytop, ytop * (1 - pad), ytop * (1 - pad)),
    hjust = c(0, 1, 0, 1),
    vjust = c(0, 0, 1, 1),
    label = c(share("diffuse-dampened"), share("diffuse-amplified"),
              share("hierarchical-dampened"), share("hierarchical-amplified")),
    stringsAsFactors = FALSE
  )
  quad$here <- quad$label == share(st$type)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = line,
      ggplot2::aes(x = mu_max, ymin = pmax(lo, 0), ymax = hi),
      fill = "grey85", alpha = 0.55) +
    ggplot2::geom_line(data = line,
      ggplot2::aes(x = mu_max, y = hierarchy), colour = "grey45",
      linetype = "22") +
    ggplot2::geom_text(data = quad,
      ggplot2::aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust,
                   fontface = ifelse(here, "bold", "plain"),
                   colour = here),
      size = 3.1, lineheight = 0.95, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#8C2B21", `FALSE` = "grey72")) +
    ggplot2::geom_vline(xintercept = ccut, colour = "grey30") +
    ggplot2::geom_hline(yintercept = hcut, colour = "grey30", linetype = "31") +
    ggplot2::geom_point(
      data = data.frame(x = res$mu_max, y = res$hierarchy_sd),
      ggplot2::aes(x = x, y = y), size = 5, shape = 21,
      fill = "#C0392B", colour = "white", stroke = 1.2) +
    ggplot2::labs(
      x = sprintf("Coupling  (μ max)   —   dampened  |  amplified   at %.2f", ccut),
      y = sprintf("Hierarchy  (SD of entry profile)\ndiffuse  |  hierarchical   at %.2f", hcut),
      subtitle = paste("Your system in red. Shaded band: one residual SD around",
                       "the corpus\ncoupling–hierarchy trade-off (dashed).")) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, ytop)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#8C2B21"),
      plot.subtitle = ggplot2::element_text(colour = "grey40", size = 8.5),
      axis.title = ggplot2::element_text(size = 9, colour = "grey30"),
      panel.grid.minor = ggplot2::element_blank())
}

# Corpus shares, read from the engine so this file holds no constant of its own.
CORPUS_SHARE <- local({
  cp <- spectralDEMATEL:::CORPUS
  as.list(cp$count / cp$n)
})

#' The robustness panel: is this type worth anything?
#'
#' Two different doubts, kept apart because they have different answers.
#' The surrogate baseline asks whether the rating distribution alone would have
#' produced these numbers. Measurement stability asks whether the type survives
#' the noise expert ratings carry. A type can pass one and fail the other.
#'
#' Run on demand, never on page load: the surrogate ensemble is the one part of
#' this that is not instant.
robustness_report <- function(res, B = 200, tolerance = 0.5, seed = 42) {
  if (!isTRUE(res$computable)) return(NULL)
  A <- res$A_matrix

  sp <- spectralDEMATEL::surrogate_position(A, B = B, seed = seed)
  ms <- spectralDEMATEL::measurement_stability(A, tolerance = tolerance,
                                               B = B, seed = seed)
  list(surrogate = sp, measurement = ms, B = B, tolerance = tolerance,
       seed = seed)
}

#' The surrogate baseline as a table a person can read.
surrogate_table <- function(rr) {
  if (is.null(rr) || is.null(rr$surrogate)) return(NULL)
  m <- rr$surrogate$metrics

  label <- c(mu_max = "Coupling", hierarchy_sd = "Hierarchy (SD)",
             dominance = "Mode dominance")
  verdict <- vapply(seq_len(nrow(m)), function(i) {
    if (m$outside[i]) {
      sprintf("outside the whole ensemble (%s every draw)",
              if (m$share_ge[i] == 0) "above" else "below")
    } else if (m$share_ge[i] > 0.9 || m$share_ge[i] < 0.1) {
      "near the edge of the ensemble"
    } else {
      "inside the ensemble — the shuffle reproduces this"
    }
  }, character(1))

  data.frame(
    Diagnostic = unname(label[m$metric]),
    Observed   = formatC(m$observed, format = "f", digits = 4),
    `Surrogate range` = sprintf("%.4f – %.4f", m$min, m$max),
    `Surrogate median` = formatC(m$median, format = "f", digits = 4),
    Position   = verdict,
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

#' What the robustness panel means, in prose.
robustness_text <- function(rr, res) {
  if (is.null(rr)) return("")
  out <- character(0)

  sp <- rr$surrogate
  if (is.null(sp)) {
    out <- c(out, paste(
      "SURROGATE BASELINE\n  Not available. The shuffle has to preserve strong",
      "connectivity, and this matrix does not have it to begin with — see the",
      "assumption checks. Fix the disconnected factors and the baseline becomes",
      "computable."))
  } else {
    n_outside <- sum(sp$metrics$outside)
    n_total <- nrow(sp$metrics)
    reading <- if (n_outside == 0) paste(
      "None of the three falls outside the ensemble. On this evidence the",
      "structure is what this matrix's rating distribution implies on its own:",
      "shuffling which factor influences which reproduces all three",
      "diagnostics. That is worth knowing before drawing conclusions from",
      "them.")
    else if (n_outside == n_total) paste(
      "All three fall outside the whole ensemble. None of them is something",
      "the rating distribution produces on its own, so the structure is",
      "something the analyst built.")
    else sprintf(paste(
      "%d of the %d fall outside the whole ensemble, and are therefore not",
      "something the rating distribution produces on its own. The",
      "%s that does not is reproduced by shuffling, so read it with more",
      "caution than the others."),
      n_outside, n_total,
      if (n_total - n_outside == 1) "one" else "others")

    out <- c(out, sprintf(paste(
      "SURROGATE BASELINE  (%d shuffles, seed %d)\n",
      " Shuffling this matrix's own ratings at random, holding the number of",
      "factors, the density and the exact set of values fixed.\n  %s"),
      rr$B, rr$seed, reading))

    # The honest caveat: for a common type this says almost nothing.
    common <- sp$type_share > 0.5
    out <- c(out, sprintf(paste(
      "  The same type arises in %.0f%% of shuffles.%s"),
      100 * sp$type_share,
      if (common) paste(" That is weak evidence either way: nearly 70% of the",
                        "reference corpus is diffuse-amplified, so a shuffled",
                        "matrix usually lands there too. Read the per-diagnostic",
                        "positions above instead.")
      else " The observed type is an uncommon one, so this is informative."))
  }

  ms <- rr$measurement
  if (!is.null(ms)) {
    out <- c(out, sprintf(paste(
      "\nMEASUREMENT STABILITY  (%d draws, +/- %.2f rating points)\n",
      " The type is %s in %.0f%% of perturbed matrices.%s"),
      ms$B_admissible, ms$tolerance, ms$observed_type, 100 * ms$share_same,
      if (ms$share_same >= 0.95)
        " Half a rating point of noise cannot move this system."
      else if (ms$share_same >= 0.75)
        " Mostly stable, but the reading is not immune to rating noise."
      else paste(" This system sits close enough to a boundary that expert",
                 "noise alone decides which side it falls on. Treat the type",
                 "as undetermined rather than borderline.")))
    if (length(ms$type_table) > 1) {
      out <- c(out, paste0("  Types across the draws: ",
                           paste(names(ms$type_table), ms$type_table,
                                 sep = " ", collapse = ",  ")))
    }
    out <- c(out, sprintf(paste(
      "  Recorded zeros were left alone (%s): a rating of zero is a judgement",
      "that there is no influence, not a one with noise on it."), ms$perturb))
  }

  paste(out, collapse = "\n")
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
