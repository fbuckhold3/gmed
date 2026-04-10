#!/usr/bin/env Rscript

# =============================================================================
# GMED INTERNAL DEPENDENCY ANALYZER
# =============================================================================
# This script analyzes internal dependencies within the gmed package to
# identify which "unused" functions are actually internal helpers needed by
# other gmed functions.
#
# USAGE:
#   # First run the external usage analysis:
#   Rscript analyze_gmed_usage.R
#
#   # Then run this internal dependency analysis:
#   Rscript analyze_internal_dependencies.R
#
# OUTPUT:
#   - gmed_internal_dependencies.txt: Dependency analysis report
#   - gmed_dependency_tree.csv: Full dependency matrix
#   - gmed_final_classification.txt: Final 3-tier classification
# =============================================================================

library(stringr)

# Configuration
GMED_DIR <- "/Users/fredbuckhold/Documents/GitHub/gmed"
R_DIR <- file.path(GMED_DIR, "R")

# =============================================================================
# Helper Functions
# =============================================================================

#' Extract function definitions from an R file
extract_function_definitions <- function(file_path) {
  if (!file.exists(file_path)) return(character(0))

  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    content <- paste(content, collapse = "\n")

    # Match: function_name <- function(
    # or:    function_name = function(
    pattern <- "([a-zA-Z][a-zA-Z0-9_\\.]*)\\s*(<-|=)\\s*function\\s*\\("
    matches <- str_match_all(content, pattern)[[1]]

    if (nrow(matches) > 0) {
      return(unique(matches[, 2]))
    }

    return(character(0))
  }, error = function(e) {
    return(character(0))
  })
}

#' Extract all function calls from an R file
extract_function_calls <- function(file_path, defined_functions) {
  if (!file.exists(file_path)) return(character(0))

  tryCatch({
    content <- readLines(file_path, warn = FALSE)

    # Remove comments
    content <- gsub("#.*$", "", content)
    content <- paste(content, collapse = "\n")

    # Remove strings to avoid false positives
    content <- gsub('"[^"]*"', '""', content)
    content <- gsub("'[^']*'", "''", content)

    # Find all function calls: function_name(
    pattern <- "([a-zA-Z][a-zA-Z0-9_\\.]*)\\s*\\("
    matches <- str_extract_all(content, pattern)[[1]]

    # Clean up - remove the opening paren and whitespace
    functions <- str_replace(matches, "\\s*\\($", "")
    functions <- unique(functions)

    # Only keep functions that are defined in gmed
    functions <- functions[functions %in% defined_functions]

    return(functions)
  }, error = function(e) {
    return(character(0))
  })
}

#' Get all exported functions from NAMESPACE
get_exports <- function(gmed_dir) {
  namespace_file <- file.path(gmed_dir, "NAMESPACE")

  if (!file.exists(namespace_file)) {
    stop("Cannot find NAMESPACE file at: ", namespace_file)
  }

  content <- readLines(namespace_file, warn = FALSE)
  export_lines <- content[grepl("^export\\(", content)]

  functions <- str_extract(export_lines, "export\\((.+)\\)", group = 1)
  functions <- functions[!is.na(functions)]

  return(sort(unique(functions)))
}

#' Load external usage results from previous analysis
load_external_usage <- function() {
  usage_file <- "gmed_usage_report.csv"

  if (!file.exists(usage_file)) {
    cat("WARNING: Could not find", usage_file, "\n")
    cat("Please run analyze_gmed_usage.R first!\n\n")
    return(NULL)
  }

  usage_data <- read.csv(usage_file, stringsAsFactors = FALSE)

  # Extract functions with total_repos > 0
  externally_used <- usage_data$function_name[usage_data$total_repos > 0]

  return(sort(externally_used))
}

#' Build dependency map for all gmed functions
build_dependency_map <- function(r_dir, all_functions) {
  r_files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)

  cat("Analyzing", length(r_files), "R files in gmed/R/\n")

  # Map of function -> file
  function_to_file <- list()

  # First pass: identify which functions are defined in which files
  for (r_file in r_files) {
    defined <- extract_function_definitions(r_file)
    for (func in defined) {
      if (func %in% all_functions) {
        function_to_file[[func]] <- basename(r_file)
      }
    }
  }

  # Second pass: for each function, find what it calls
  dependency_map <- list()

  for (func in all_functions) {
    # Find which file defines this function
    file_name <- function_to_file[[func]]

    if (is.null(file_name)) {
      # Function exported but definition not found (might be in another package)
      dependency_map[[func]] <- character(0)
      next
    }

    file_path <- file.path(r_dir, file_name)

    # Parse the file to find all function calls
    # This is a simplification - ideally we'd parse only the body of this function
    # but that requires full R parsing which is complex
    calls <- extract_function_calls(file_path, all_functions)

    # Remove self-references
    calls <- calls[calls != func]

    dependency_map[[func]] <- sort(unique(calls))
  }

  return(dependency_map)
}

#' Find all functions that depend on a given function (reverse lookup)
find_dependents <- function(target_func, dependency_map) {
  dependents <- character(0)

  for (func in names(dependency_map)) {
    if (target_func %in% dependency_map[[func]]) {
      dependents <- c(dependents, func)
    }
  }

  return(sort(unique(dependents)))
}

#' Recursively find all dependencies of a function
get_all_dependencies <- function(func, dependency_map, visited = character(0)) {
  if (func %in% visited) {
    return(character(0))  # Avoid circular dependencies
  }

  visited <- c(visited, func)
  direct_deps <- dependency_map[[func]]

  if (length(direct_deps) == 0) {
    return(character(0))
  }

  all_deps <- direct_deps

  for (dep in direct_deps) {
    indirect_deps <- get_all_dependencies(dep, dependency_map, visited)
    all_deps <- c(all_deps, indirect_deps)
  }

  return(sort(unique(all_deps)))
}

#' Calculate dependency depth (how many levels deep)
calculate_dependency_depth <- function(func, dependency_map, depth = 0, visited = character(0)) {
  if (func %in% visited) {
    return(depth)  # Circular dependency
  }

  visited <- c(visited, func)
  deps <- dependency_map[[func]]

  if (length(deps) == 0) {
    return(depth)
  }

  max_depth <- depth

  for (dep in deps) {
    dep_depth <- calculate_dependency_depth(dep, dependency_map, depth + 1, visited)
    max_depth <- max(max_depth, dep_depth)
  }

  return(max_depth)
}

# =============================================================================
# Main Analysis
# =============================================================================

cat("=============================================================================\n")
cat("GMED INTERNAL DEPENDENCY ANALYZER\n")
cat("=============================================================================\n\n")

# Step 1: Load external usage data
cat("Step 1: Loading external usage data...\n")
externally_used <- load_external_usage()

if (is.null(externally_used)) {
  stop("Cannot proceed without external usage data. Run analyze_gmed_usage.R first!")
}

cat("Found", length(externally_used), "functions used in external repos\n\n")

# Step 2: Get all exported functions
cat("Step 2: Loading NAMESPACE...\n")
all_exports <- get_exports(GMED_DIR)
cat("Found", length(all_exports), "exported functions\n\n")

# Step 3: Build dependency map
cat("Step 3: Building internal dependency map...\n")
dependency_map <- build_dependency_map(R_DIR, all_exports)
cat("Dependency map complete!\n\n")

# Step 4: Classify functions into tiers
cat("Step 4: Classifying functions into tiers...\n\n")

tier1 <- externally_used
tier2 <- character(0)
tier3 <- character(0)

# Find all functions not in Tier 1
not_tier1 <- setdiff(all_exports, tier1)

# For each Tier 1 function, find all its dependencies
tier1_dependencies <- character(0)
for (func in tier1) {
  deps <- get_all_dependencies(func, dependency_map)
  tier1_dependencies <- c(tier1_dependencies, deps)
}
tier1_dependencies <- unique(tier1_dependencies)

# Tier 2: Functions needed by Tier 1 but not in Tier 1 themselves
tier2 <- intersect(tier1_dependencies, not_tier1)

# Tier 3: Functions not in Tier 1 or Tier 2
tier3 <- setdiff(not_tier1, tier2)

cat("Classification complete!\n")
cat("  Tier 1 (Externally used):        ", length(tier1), "functions\n")
cat("  Tier 2 (Internal dependencies):  ", length(tier2), "functions\n")
cat("  Tier 3 (Truly unused):           ", length(tier3), "functions\n\n")

# =============================================================================
# Generate Detailed Report
# =============================================================================

report_file <- "gmed_internal_dependencies.txt"
sink(report_file)

cat("=============================================================================\n")
cat("GMED INTERNAL DEPENDENCY ANALYSIS\n")
cat("=============================================================================\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("SUMMARY\n")
cat("-----------------------------------------------------------------------------\n")
cat("Total exported functions:          ", length(all_exports), "\n")
cat("Tier 1 (Externally used):          ", length(tier1), "\n")
cat("Tier 2 (Internal dependencies):    ", length(tier2), "\n")
cat("Tier 3 (Truly unused):             ", length(tier3), "\n")
cat("Safe to deprecate (Tier 3):        ", length(tier3), "\n\n")

cat("TIER 1: EXTERNALLY USED FUNCTIONS (", length(tier1), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
cat("These functions are directly used in your 8 repositories.\n")
cat("DO NOT DEPRECATE - These are your core API!\n\n")

for (func in tier1) {
  deps <- dependency_map[[func]]
  depth <- calculate_dependency_depth(func, dependency_map)

  cat(sprintf("%-40s", func))

  if (length(deps) > 0) {
    cat(sprintf("  [calls %d functions, depth %d]\n", length(deps), depth))
    if (length(deps) <= 5) {
      cat("    Calls:", paste(deps, collapse = ", "), "\n")
    } else {
      cat("    Calls:", paste(deps[1:5], collapse = ", "), "...\n")
    }
  } else {
    cat("  [no internal dependencies]\n")
  }
}

cat("\n\n")
cat("TIER 2: INTERNAL DEPENDENCIES (", length(tier2), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
cat("These functions are NOT used in external repos, but ARE needed by Tier 1.\n")
cat("DO NOT DEPRECATE - Required for Tier 1 functions to work!\n\n")

for (func in tier2) {
  # Find which Tier 1 functions depend on this
  dependents <- find_dependents(func, dependency_map)
  tier1_dependents <- intersect(dependents, tier1)

  deps <- dependency_map[[func]]

  cat(sprintf("%-40s", func))

  if (length(tier1_dependents) > 0) {
    cat(sprintf("  [needed by %d Tier 1 functions]\n", length(tier1_dependents)))
    cat("    Needed by:", paste(tier1_dependents[1:min(3, length(tier1_dependents))], collapse = ", "))
    if (length(tier1_dependents) > 3) cat(", ...")
    cat("\n")
  }

  if (length(deps) > 0) {
    cat("    Calls:", paste(deps[1:min(3, length(deps))], collapse = ", "))
    if (length(deps) > 3) cat(", ...")
    cat("\n")
  }
}

cat("\n\n")
cat("TIER 3: TRULY UNUSED FUNCTIONS (", length(tier3), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
cat("These functions are NOT used externally AND NOT needed internally.\n")
cat("SAFE TO DEPRECATE - Review and remove these!\n\n")

if (length(tier3) > 0) {
  for (func in tier3) {
    deps <- dependency_map[[func]]
    dependents <- find_dependents(func, dependency_map)

    cat(sprintf("%-40s", func))

    if (length(deps) > 0) {
      # This function calls other functions but isn't called itself
      cat(sprintf("  [orphan - calls %d functions but unused]\n", length(deps)))
    } else if (length(dependents) > 0) {
      # This shouldn't happen if our tier logic is correct
      cat("  [WARNING: Has dependents but not in Tier 2?]\n")
    } else {
      cat("  [completely isolated]\n")
    }
  }

  cat("\n")
  cat("RECOMMENDATION: Create deprecation warnings for these", length(tier3), "functions.\n")
  cat("They can be safely removed in a future version.\n")
} else {
  cat("(none - all exported functions are being used!)\n")
}

cat("\n")
cat("=============================================================================\n")
cat("DEPENDENCY STATISTICS\n")
cat("=============================================================================\n\n")

# Calculate some interesting stats
total_deps <- sum(sapply(dependency_map, length))
avg_deps <- round(total_deps / length(all_exports), 1)

most_dependencies <- names(sort(sapply(dependency_map, length), decreasing = TRUE))[1:10]
most_depended_on <- names(sort(table(unlist(dependency_map)), decreasing = TRUE))[1:10]

cat("Average dependencies per function:     ", avg_deps, "\n")
cat("Total internal function calls:         ", total_deps, "\n\n")

cat("TOP 10 FUNCTIONS WITH MOST DEPENDENCIES (calls many functions):\n")
cat("-----------------------------------------------------------------------------\n")
for (func in most_dependencies) {
  count <- length(dependency_map[[func]])
  tier <- if (func %in% tier1) "Tier 1" else if (func %in% tier2) "Tier 2" else "Tier 3"
  cat(sprintf("%-40s %2d deps  [%s]\n", func, count, tier))
}

cat("\n")
cat("TOP 10 MOST DEPENDED-ON FUNCTIONS (called by many functions):\n")
cat("-----------------------------------------------------------------------------\n")
dep_counts <- table(unlist(dependency_map))
for (func in most_depended_on) {
  count <- dep_counts[func]
  tier <- if (func %in% tier1) "Tier 1" else if (func %in% tier2) "Tier 2" else "Tier 3"
  cat(sprintf("%-40s %2d callers  [%s]\n", func, count, tier))
}

cat("\n")
cat("=============================================================================\n")
cat("END OF REPORT\n")
cat("=============================================================================\n")

sink()

# =============================================================================
# Generate CSV Dependency Matrix
# =============================================================================

csv_file <- "gmed_dependency_tree.csv"

dep_matrix <- data.frame(
  function_name = all_exports,
  tier = sapply(all_exports, function(f) {
    if (f %in% tier1) return(1)
    if (f %in% tier2) return(2)
    return(3)
  }),
  tier_label = sapply(all_exports, function(f) {
    if (f %in% tier1) return("EXTERNALLY USED")
    if (f %in% tier2) return("INTERNAL DEPENDENCY")
    return("UNUSED")
  }),
  num_dependencies = sapply(all_exports, function(f) length(dependency_map[[f]])),
  num_dependents = sapply(all_exports, function(f) length(find_dependents(f, dependency_map))),
  dependency_depth = sapply(all_exports, function(f) calculate_dependency_depth(f, dependency_map)),
  stringsAsFactors = FALSE
)

# Add list of dependencies and dependents
dep_matrix$calls_functions <- sapply(all_exports, function(f) {
  paste(dependency_map[[f]], collapse = ", ")
})

dep_matrix$called_by_functions <- sapply(all_exports, function(f) {
  paste(find_dependents(f, dependency_map), collapse = ", ")
})

# Sort by tier, then by number of dependents
dep_matrix <- dep_matrix[order(dep_matrix$tier, -dep_matrix$num_dependents), ]

write.csv(dep_matrix, csv_file, row.names = FALSE)

# =============================================================================
# Generate Final Classification List
# =============================================================================

classification_file <- "gmed_final_classification.txt"
sink(classification_file)

cat("GMED FUNCTION CLASSIFICATION\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

cat("=== TIER 1: EXTERNALLY USED (", length(tier1), " functions) ===\n")
cat("DO NOT DEPRECATE - Core API functions\n\n")
for (func in sort(tier1)) {
  cat("  ", func, "\n")
}

cat("\n\n")
cat("=== TIER 2: INTERNAL DEPENDENCIES (", length(tier2), " functions) ===\n")
cat("DO NOT DEPRECATE - Needed by Tier 1 functions\n\n")
for (func in sort(tier2)) {
  cat("  ", func, "\n")
}

cat("\n\n")
cat("=== TIER 3: TRULY UNUSED (", length(tier3), " functions) ===\n")
cat("SAFE TO DEPRECATE - Not used anywhere\n\n")
for (func in sort(tier3)) {
  cat("  ", func, "\n")
}

sink()

# =============================================================================
# Print Summary
# =============================================================================

cat("=============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================================\n\n")

cat("FINAL CLASSIFICATION:\n")
cat("  Tier 1 (Externally used):        ", length(tier1), " functions - DO NOT DEPRECATE\n")
cat("  Tier 2 (Internal dependencies):  ", length(tier2), " functions - DO NOT DEPRECATE\n")
cat("  Tier 3 (Truly unused):           ", length(tier3), " functions - SAFE TO DEPRECATE\n\n")

cat("OUTPUT FILES:\n")
cat("  1.", report_file, "- Detailed dependency analysis\n")
cat("  2.", csv_file, "- Full dependency matrix (open in Excel)\n")
cat("  3.", classification_file, "- Simple 3-tier classification\n\n")

cat("KEY INSIGHTS:\n")
cat("  - Total functions to keep:       ", length(tier1) + length(tier2), "\n")
cat("  - Functions safe to deprecate:   ", length(tier3), "\n")
cat("  - Cleanup potential:             ", round(100 * length(tier3) / length(all_exports), 1), "%\n\n")

if (length(tier3) > 0) {
  cat("⚠️  NEXT STEPS:\n")
  cat("  1. Review", classification_file, "for the", length(tier3), "functions to deprecate\n")
  cat("  2. Add .Deprecated() warnings to Tier 3 functions\n")
  cat("  3. Update documentation\n")
  cat("  4. Plan removal for next major version\n\n")
}

cat("=============================================================================\n")
