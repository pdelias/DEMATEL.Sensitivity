# Debug Script for DEMATEL Shiny App
# File: debug_app.R
#
# Run this script to check for common issues before running the Shiny app

cat("DEMATEL Shiny App Debug Script\n")
cat("==============================\n\n")

# 1. Check if all required packages are installed
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly",
  "ggplot2", "viridis", "reshape2", "gridExtra"
)

cat("1. Checking required packages...\n")
missing_packages <- character()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
    cat("   ❌", pkg, "- NOT INSTALLED\n")
  } else {
    cat("   ✅", pkg, "- OK\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\n⚠️  Missing packages detected. Install with:\n")
  cat("install.packages(c('", paste(missing_packages, collapse = "', '"), "'))\n\n")
} else {
  cat("\n✅ All required packages are installed!\n\n")
}

# 2. Check if all R source files exist
cat("2. Checking source files...\n")
source_files <- c(
  "R/dematel_spectral.R",
  "R/sensitivity-core.R", 
  "R/sensitivity-methods.R",
  "R/sensitivity-visualization.R",
  "R/ui_components.R"
)

missing_files <- character()
for (file in source_files) {
  if (!file.exists(file)) {
    missing_files <- c(missing_files, file)
    cat("   ❌", file, "- FILE NOT FOUND\n")
  } else {
    cat("   ✅", file, "- OK\n")
  }
}

if (length(missing_files) > 0) {
  cat("\n⚠️  Missing source files detected!\n")
  cat("Make sure all files are in the correct R/ subdirectory.\n\n")
} else {
  cat("\n✅ All source files found!\n\n")
}

# 3. Try loading the source files to check for syntax errors
cat("3. Checking for syntax errors in source files...\n")
syntax_errors <- character()

for (file in source_files) {
  if (file.exists(file)) {
    tryCatch({
      source(file, local = TRUE)
      cat("   ✅", file, "- No syntax errors\n")
    }, error = function(e) {
      syntax_errors <- c(syntax_errors, file)
      cat("   ❌", file, "- SYNTAX ERROR:", e$message, "\n")
    })
  }
}

if (length(syntax_errors) > 0) {
  cat("\n⚠️  Syntax errors detected in source files!\n")
  cat("Fix these errors before running the app.\n\n")
} else {
  cat("\n✅ No syntax errors found!\n\n")
}

# 4. Check if key functions exist
cat("4. Checking for key functions...\n")
key_functions <- c(
  "DEMATEL_Sensitivity",
  "compute_sensitivity_numerical", 
  "compute_sensitivity_analytical",
  "identify_critical_relationships",
  "intervention_analysis",
  "get_sensitivity_stats",
  "plot_dematel_network",
  "create_dematel_interrelationship_map",
  "visualize_sensitivity"
)

missing_functions <- character()
for (func in key_functions) {
  if (!exists(func, mode = "function")) {
    missing_functions <- c(missing_functions, func)
    cat("   ❌", func, "- FUNCTION NOT FOUND\n")
  } else {
    cat("   ✅", func, "- OK\n")
  }
}

if (length(missing_functions) > 0) {
  cat("\n⚠️  Missing functions detected!\n")
  cat("Check that all source files loaded correctly.\n\n")
} else {
  cat("\n✅ All key functions found!\n\n")
}

# 5. Test basic functionality with example data
cat("5. Testing basic functionality...\n")
tryCatch({
  # Create test matrix
  set.seed(42)
  A <- matrix(0, nrow = 3, ncol = 3)
  for (i in 1:3) {
    for (j in 1:3) {
      if (i != j) {
        A[i, j] <- sample(0:4, 1)
      }
    }
  }
  rownames(A) <- colnames(A) <- c("F1", "F2", "F3")
  
  # Test DEMATEL_Sensitivity
  sens_obj <- DEMATEL_Sensitivity(A, c("F1", "F2", "F3"))
  cat("   ✅ DEMATEL_Sensitivity object created\n")
  
  # Test numerical sensitivity
  sens_obj <- compute_sensitivity_numerical(sens_obj, epsilon = 0.01)
  cat("   ✅ Numerical sensitivity computed\n")
  
  # Test critical relationships
  critical <- identify_critical_relationships(sens_obj, threshold_percentile = 80)
  cat("   ✅ Critical relationships identified\n")
  
  # Test visualization
  if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("reshape2", quietly = TRUE)) {
    plots <- visualize_sensitivity(sens_obj, save_plots = FALSE)
    cat("   ✅ Visualizations created\n")
  }
  
  cat("\n✅ Basic functionality test passed!\n\n")
  
}, error = function(e) {
  cat("   ❌ Basic functionality test FAILED:", e$message, "\n\n")
})

# 6. Summary
cat("6. Summary\n")
cat("==========\n")

total_issues <- length(missing_packages) + length(missing_files) + length(syntax_errors) + length(missing_functions)

if (total_issues == 0) {
  cat("🎉 ALL CHECKS PASSED! Your app should run without issues.\n")
  cat("\nTo start the app, run:\n")
  cat("library(shiny)\n")
  cat("runApp()\n")
} else {
  cat("⚠️  Found", total_issues, "issue(s) that need to be fixed:\n")
  
  if (length(missing_packages) > 0) {
    cat("- Install missing packages:", paste(missing_packages, collapse = ", "), "\n")
  }
  if (length(missing_files) > 0) {
    cat("- Add missing files:", paste(missing_files, collapse = ", "), "\n")
  }
  if (length(syntax_errors) > 0) {
    cat("- Fix syntax errors in:", paste(syntax_errors, collapse = ", "), "\n")
  }
  if (length(missing_functions) > 0) {
    cat("- Missing functions:", paste(missing_functions, collapse = ", "), "\n")
  }
  
  cat("\nFix these issues then re-run this debug script.\n")
}

cat("\n" , rep("=", 50), "\n")
cat("Debug script completed at", as.character(Sys.time()), "\n")