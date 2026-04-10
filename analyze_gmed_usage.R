#!/usr/bin/env Rscript

# =============================================================================
# GMED Function Usage Analyzer
# =============================================================================
# This script analyzes all local repositories to identify which gmed functions
# are actually being used in your projects.
#
# USAGE:
#   Rscript analyze_gmed_usage.R
#
# OUTPUT:
#   - gmed_usage_report.txt: Comprehensive text report
#   - gmed_usage_report.csv: CSV file with all function usage details
#   - gmed_unused_functions.txt: List of functions not found in any repo
# =============================================================================

library(stringr)

# Configuration
GITHUB_DIR <- "/Users/fredbuckhold/Documents/GitHub"
GMED_DIR <- file.path(GITHUB_DIR, "gmed")

REPOS <- c(
  "imslu.ccc.dashboard",
  "imslu-resident-self-assessment",
  "imslu.coach.dash",
  "imslu.at.noon",
  "imslu.res.dash",
  "resident.assessment",
  "gme_mvp",
  "imslu.facultyeval"
)

# =============================================================================
# Helper Functions
# =============================================================================

#' Extract all gmed:: function calls from a file
extract_gmed_calls <- function(file_path) {
  if (!file.exists(file_path)) return(character(0))

  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    content <- paste(content, collapse = "\n")

    # Match gmed::function_name patterns
    matches <- str_extract_all(content, "gmed::([a-zA-Z0-9_\\.]+)")[[1]]

    # Remove the gmed:: prefix
    functions <- str_replace(matches, "gmed::", "")

    return(unique(functions))
  }, error = function(e) {
    return(character(0))
  })
}

#' Check if file loads gmed library
uses_gmed_library <- function(file_path) {
  if (!file.exists(file_path)) return(FALSE)

  tryCatch({
    content <- readLines(file_path, warn = FALSE)
    content <- paste(content, collapse = "\n")

    # Check for library(gmed), require(gmed), or library("gmed")
    has_library <- grepl("library\\s*\\(\\s*['\"]?gmed['\"]?\\s*\\)", content)
    has_require <- grepl("require\\s*\\(\\s*['\"]?gmed['\"]?\\s*\\)", content)

    return(has_library || has_require)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Get all R files in a directory recursively
get_r_files <- function(dir_path) {
  if (!dir.exists(dir_path)) return(character(0))

  list.files(
    path = dir_path,
    pattern = "\\.[Rr]$|\\.[Rr]md$",
    recursive = TRUE,
    full.names = TRUE
  )
}

#' Get all exported functions from gmed NAMESPACE
get_gmed_exports <- function(gmed_dir) {
  namespace_file <- file.path(gmed_dir, "NAMESPACE")

  if (!file.exists(namespace_file)) {
    stop("Cannot find gmed NAMESPACE file at: ", namespace_file)
  }

  content <- readLines(namespace_file, warn = FALSE)
  export_lines <- content[grepl("^export\\(", content)]

  functions <- str_extract(export_lines, "export\\((.+)\\)", group = 1)
  functions <- functions[!is.na(functions)]

  return(sort(unique(functions)))
}

#' Analyze a single repository
analyze_repo <- function(repo_name, github_dir) {
  repo_path <- file.path(github_dir, repo_name)

  cat("Analyzing", repo_name, "...\n")

  if (!dir.exists(repo_path)) {
    cat("  WARNING: Directory not found:", repo_path, "\n")
    return(list(
      repo = repo_name,
      exists = FALSE,
      r_files = 0,
      files_with_gmed = character(0),
      functions_used = character(0)
    ))
  }

  r_files <- get_r_files(repo_path)

  if (length(r_files) == 0) {
    cat("  No R files found\n")
    return(list(
      repo = repo_name,
      exists = TRUE,
      r_files = 0,
      files_with_gmed = character(0),
      functions_used = character(0)
    ))
  }

  # Analyze each R file
  all_functions <- character(0)
  files_with_gmed <- character(0)

  for (r_file in r_files) {
    functions <- extract_gmed_calls(r_file)
    uses_lib <- uses_gmed_library(r_file)

    if (length(functions) > 0 || uses_lib) {
      files_with_gmed <- c(files_with_gmed, r_file)
      all_functions <- c(all_functions, functions)
    }
  }

  all_functions <- sort(unique(all_functions))

  cat("  Found", length(r_files), "R files\n")
  cat("  Found", length(files_with_gmed), "files using gmed\n")
  cat("  Found", length(all_functions), "unique gmed functions\n")

  return(list(
    repo = repo_name,
    exists = TRUE,
    r_files = length(r_files),
    files_with_gmed = files_with_gmed,
    functions_used = all_functions
  ))
}

# =============================================================================
# Main Analysis
# =============================================================================

cat("=============================================================================\n")
cat("GMED FUNCTION USAGE ANALYZER\n")
cat("=============================================================================\n\n")

cat("Checking gmed directory...\n")
if (!dir.exists(GMED_DIR)) {
  stop("Cannot find gmed directory at: ", GMED_DIR)
}

cat("Loading gmed NAMESPACE...\n")
all_exports <- get_gmed_exports(GMED_DIR)
cat("Found", length(all_exports), "exported functions in gmed\n\n")

cat("Analyzing repositories...\n")
cat("=============================================================================\n\n")

# Analyze all repos
results <- lapply(REPOS, function(repo) {
  analyze_repo(repo, GITHUB_DIR)
})

cat("\n=============================================================================\n")
cat("COMPILING RESULTS\n")
cat("=============================================================================\n\n")

# Compile all used functions across all repos
all_used_functions <- unique(unlist(lapply(results, function(x) x$functions_used)))
all_used_functions <- sort(all_used_functions)

# Identify unused functions
unused_functions <- setdiff(all_exports, all_used_functions)
unused_functions <- sort(unused_functions)

# Create detailed usage matrix
usage_matrix <- data.frame(
  function_name = all_exports,
  stringsAsFactors = FALSE
)

for (result in results) {
  if (result$exists && result$r_files > 0) {
    usage_matrix[[result$repo]] <- ifelse(
      usage_matrix$function_name %in% result$functions_used,
      "✓",
      ""
    )
  }
}

usage_matrix$total_repos <- rowSums(usage_matrix[, -1] == "✓")
usage_matrix$status <- ifelse(
  usage_matrix$total_repos == 0,
  "UNUSED",
  ifelse(usage_matrix$total_repos == 1, "SINGLE REPO", "MULTIPLE REPOS")
)

# Sort by usage (most used first)
usage_matrix <- usage_matrix[order(-usage_matrix$total_repos, usage_matrix$function_name), ]

# =============================================================================
# Generate Text Report
# =============================================================================

report_file <- "gmed_usage_report.txt"
sink(report_file)

cat("=============================================================================\n")
cat("GMED FUNCTION USAGE ANALYSIS REPORT\n")
cat("=============================================================================\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Gmed Directory:", GMED_DIR, "\n")
cat("\n")

cat("SUMMARY\n")
cat("-----------------------------------------------------------------------------\n")
cat("Total exported functions:    ", length(all_exports), "\n")
cat("Functions used in repos:     ", length(all_used_functions), "\n")
cat("Functions NOT used anywhere: ", length(unused_functions), "\n")
cat("Usage rate:                  ", round(100 * length(all_used_functions) / length(all_exports), 1), "%\n")
cat("\n")

cat("REPOSITORY ANALYSIS\n")
cat("-----------------------------------------------------------------------------\n")
for (result in results) {
  cat("\n", result$repo, "\n")
  if (!result$exists) {
    cat("  STATUS: Directory not found\n")
  } else if (result$r_files == 0) {
    cat("  STATUS: No R files found\n")
  } else {
    cat("  R files found:             ", result$r_files, "\n")
    cat("  Files using gmed:          ", length(result$files_with_gmed), "\n")
    cat("  Unique gmed functions used:", length(result$functions_used), "\n")

    if (length(result$functions_used) > 0) {
      cat("\n  Functions used:\n")
      for (func in result$functions_used) {
        cat("    -", func, "\n")
      }
    }
  }
}

cat("\n\n")
cat("FUNCTION USAGE DETAILS\n")
cat("=============================================================================\n\n")

cat("FUNCTIONS USED IN MULTIPLE REPOS (", sum(usage_matrix$total_repos > 1), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
multi_repo <- usage_matrix[usage_matrix$total_repos > 1, ]
if (nrow(multi_repo) > 0) {
  for (i in 1:nrow(multi_repo)) {
    cat(sprintf("%-40s (%d repos)\n", multi_repo$function_name[i], multi_repo$total_repos[i]))
  }
} else {
  cat("(none)\n")
}

cat("\n")
cat("FUNCTIONS USED IN SINGLE REPO (", sum(usage_matrix$total_repos == 1), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
single_repo <- usage_matrix[usage_matrix$total_repos == 1, ]
if (nrow(single_repo) > 0) {
  for (i in 1:nrow(single_repo)) {
    repo_name <- names(single_repo)[which(single_repo[i, ] == "✓")[1]]
    cat(sprintf("%-40s [%s]\n", single_repo$function_name[i], repo_name))
  }
} else {
  cat("(none)\n")
}

cat("\n")
cat("UNUSED FUNCTIONS (", length(unused_functions), " functions)\n")
cat("-----------------------------------------------------------------------------\n")
if (length(unused_functions) > 0) {
  cat("The following functions are exported but not found in any analyzed repo:\n\n")
  for (func in unused_functions) {
    cat("  -", func, "\n")
  }
  cat("\n")
  cat("RECOMMENDATION: Review these functions for potential deprecation.\n")
  cat("They may be:\n")
  cat("  1. Used only in internal scripts not in these repos\n")
  cat("  2. Legacy functions no longer needed\n")
  cat("  3. Internal helper functions that shouldn't be exported\n")
  cat("  4. Recently added and not yet adopted\n")
} else {
  cat("(none - all exported functions are being used!)\n")
}

cat("\n")
cat("=============================================================================\n")
cat("END OF REPORT\n")
cat("=============================================================================\n")

sink()

# =============================================================================
# Generate CSV Report
# =============================================================================

csv_file <- "gmed_usage_report.csv"
write.csv(usage_matrix, csv_file, row.names = FALSE)

# =============================================================================
# Generate Unused Functions List
# =============================================================================

unused_file <- "gmed_unused_functions.txt"
writeLines(unused_functions, unused_file)

# =============================================================================
# Print Summary to Console
# =============================================================================

cat("\n=============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================================\n\n")

cat("SUMMARY:\n")
cat("  Total exported functions:     ", length(all_exports), "\n")
cat("  Functions used in repos:      ", length(all_used_functions), "\n")
cat("  Functions NOT used anywhere:  ", length(unused_functions), "\n")
cat("  Usage rate:                   ", round(100 * length(all_used_functions) / length(all_exports), 1), "%\n\n")

cat("OUTPUT FILES:\n")
cat("  1.", report_file, "- Detailed text report\n")
cat("  2.", csv_file, "- CSV matrix of function usage\n")
cat("  3.", unused_file, "- List of unused functions\n\n")

cat("NEXT STEPS:\n")
cat("  1. Review", report_file, "for complete analysis\n")
cat("  2. Open", csv_file, "in Excel/R for interactive exploration\n")
cat("  3. Consider deprecating functions in", unused_file, "\n\n")

if (length(unused_functions) > 0) {
  cat("⚠️  WARNING: Found", length(unused_functions), "unused functions\n")
  cat("Review", unused_file, "carefully before removing any functions.\n\n")
}

cat("=============================================================================\n")
