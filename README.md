# DEMATEL.Sensitivity

A Shiny application for diagnosing DEMATEL influence matrices: the structural
type of the system, whether the matrix is in scope, where influence enters and
accumulates, and which relationship moves the system most.

## Requires spectralDEMATEL

**Every spectral quantity this application shows is computed by the
[spectralDEMATEL](https://github.com/pdelias/spectralDEMATEL) package**, which
is the single implementation of every definition. Install it first:

```r
install.packages("spectralDEMATEL",
                 repos = c("https://pdelias.r-universe.dev",
                           "https://cloud.r-project.org"))
```

Then the interface packages:

```r
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "DT",
                   "plotly", "ggplot2", "ggrepel", "viridis", "reshape2"))
```

## Run

```r
shiny::runApp()
```

## What changed, and why it matters

Until 2026-08 this application implemented the mathematics itself, in
`R/dematel_spectral.R` and `R/sensitivity-core.R`. That second implementation
had drifted from the definitions in the research code:

* **Mode dominance** took the subdominant eigenvalue by *real part* rather than
  by *modulus*. For a non-negative total-relation matrix the negative end of the
  spectrum usually carries the larger modulus, so the ratio was understated —
  by a factor of three on one published matrix.
* **"Condition number"** meant `lambda_max / lambda_min`, which is not the
  eigenvalue condition number that bounds how far the per-link sensitivity
  estimates can be trusted. It could be negative.
* Coupling, the three hierarchy readings, the accumulation profile and the
  assumption checks were not computed at all.

Those were removed rather than patched. Five further quantities the old
interface displayed — the spectral radius, the minimum eigenvalue, a convergence
rate, a concentration ratio and an eigenvector range — were not part of any
definition and are gone too.

`R/engine.R` is now the only file that touches the mathematics, and it computes
nothing: it calls the package and shapes the answer for the screen.

## Reading the numbers in the right direction

Hierarchy has three readings and **they do not run the same way**.
`hierarchy_sd` and `hierarchy_gini` are high when influence enters at a few
factors; the participation ratio is **low**. Every display in this application
carries the direction.

## Licence

MIT © Pavlos Delias
