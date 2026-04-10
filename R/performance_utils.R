#' @title Performance Optimization Utilities
#' @description Functions for profiling and caching to improve performance
#' @name performance_utils
NULL

# Simple in-memory cache
.rdm_cache <- new.env(parent = emptyenv())

#' Clear RDM Cache
#'
#' Clears all cached data (data dictionary, milestone medians, etc.)
#'
#' @export
#' @examples
#' \dontrun{
#' clear_rdm_cache()
#' }
clear_rdm_cache <- function() {
  rm(list = ls(envir = .rdm_cache), envir = .rdm_cache)
  message("RDM cache cleared")
  invisible(NULL)
}

#' Get Cached Item
#'
#' Retrieves an item from cache if it exists
#'
#' @param key Character string cache key
#' @return Cached value or NULL if not found
#' @keywords internal
get_cached <- function(key) {
  if (exists(key, envir = .rdm_cache)) {
    return(get(key, envir = .rdm_cache))
  }
  return(NULL)
}

#' Set Cached Item
#'
#' Stores an item in cache
#'
#' @param key Character string cache key
#' @param value Value to cache
#' @keywords internal
set_cached <- function(key, value) {
  assign(key, value, envir = .rdm_cache)
  invisible(value)
}

#' Get Cache Info
#'
#' Returns information about what's currently cached
#'
#' @export
#' @return Data frame with cache contents
#' @examples
#' \dontrun{
#' get_cache_info()
#' }
get_cache_info <- function() {
  cached_items <- ls(envir = .rdm_cache)

  if (length(cached_items) == 0) {
    return(data.frame(
      item = character(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  info <- lapply(cached_items, function(item) {
    obj <- get(item, envir = .rdm_cache)
    size_bytes <- as.numeric(object.size(obj))
    data.frame(
      item = item,
      size_mb = round(size_bytes / 1024^2, 2),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, info)
}

#' Profile RDM Data Loading
#'
#' Profiles the load_rdm_complete function to identify performance bottlenecks.
#' Uses base R's Rprof for profiling.
#'
#' @param rdm_token REDCap API token
#' @param redcap_url REDCap API URL
#' @param verbose Print timing information (default: TRUE)
#'
#' @return List with timing information for each major step
#' @export
#'
#' @examples
#' \dontrun{
#' timing <- profile_rdm_loading(rdm_token = Sys.getenv("RDM_TOKEN"))
#' print(timing)
#' }
profile_rdm_loading <- function(rdm_token = NULL,
                               redcap_url = "https://redcapsurvey.slu.edu/api/",
                               verbose = TRUE) {

  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
  }

  if (rdm_token == "" || is.null(rdm_token)) {
    stop("RDM_TOKEN must be provided or set as environment variable")
  }

  timing <- list()

  # Step 1: Load raw data
  if (verbose) message("Profiling Step 1: Loading raw data...")
  start_time <- Sys.time()
  raw_data <- load_data_by_forms(
    rdm_token = rdm_token,
    redcap_url = redcap_url,
    filter_archived = FALSE,
    calculate_levels = FALSE,
    verbose = FALSE
  )
  timing$load_raw_data <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$load_raw_data))

  # Step 2: Load data dictionary
  if (verbose) message("Profiling Step 2: Loading data dictionary...")
  start_time <- Sys.time()
  data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  timing$load_data_dict <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$load_data_dict))

  # Step 3: Calculate milestone medians
  if (verbose) message("Profiling Step 3: Calculating milestone medians...")
  start_time <- Sys.time()
  tryCatch({
    milestone_medians <- calculate_all_milestone_medians(raw_data, verbose = FALSE)
    timing$calculate_medians <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$calculate_medians))
  }, error = function(e) {
    timing$calculate_medians <- NA
    if (verbose) message("  ✗ Failed: ", e$message)
  })

  # Step 4: Filter archived residents
  if (verbose) message("Profiling Step 4: Filtering archived residents...")
  start_time <- Sys.time()
  filtered_data <- filter_archived_residents(raw_data, verbose = FALSE)
  timing$filter_archived <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$filter_archived))

  # Step 5: Calculate resident levels
  if (verbose) message("Profiling Step 5: Calculating resident levels...")
  start_time <- Sys.time()
  residents <- filtered_data$forms$resident_data
  if (!is.null(residents)) {
    residents <- tryCatch({
      calculate_resident_level(residents)
    }, error = function(e) {
      residents
    })
  }
  timing$calculate_levels <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$calculate_levels))

  # Step 6: Create milestone workflow
  if (verbose) message("Profiling Step 6: Creating milestone workflow...")
  start_time <- Sys.time()
  tryCatch({
    milestone_workflow <- create_milestone_workflow_from_dict(
      all_forms = filtered_data$forms,
      data_dict = data_dict,
      resident_data = residents,
      verbose = FALSE
    )
    timing$create_workflow <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (verbose) message(sprintf("  ✓ Completed in %.2f seconds", timing$create_workflow))
  }, error = function(e) {
    timing$create_workflow <- NA
    if (verbose) message("  ✗ Failed: ", e$message)
  })

  # Calculate total time
  timing$total <- sum(unlist(timing), na.rm = TRUE)

  if (verbose) {
    message("\n=== PROFILING SUMMARY ===")
    message(sprintf("Total time: %.2f seconds", timing$total))
    message("\nBreakdown:")
    for (step in names(timing)) {
      if (step != "total" && !is.na(timing[[step]])) {
        pct <- (timing[[step]] / timing$total) * 100
        message(sprintf("  %s: %.2f sec (%.1f%%)",
                       gsub("_", " ", step), timing[[step]], pct))
      }
    }
  }

  class(timing) <- c("rdm_profile", "list")
  return(timing)
}

#' Print Profiling Results
#'
#' @param x Object of class rdm_profile
#' @param ... Additional arguments (unused)
#' @export
print.rdm_profile <- function(x, ...) {
  cat("=== RDM Loading Profile ===\n\n")
  cat(sprintf("Total time: %.2f seconds\n\n", x$total))
  cat("Breakdown:\n")

  steps <- names(x)[names(x) != "total"]
  for (step in steps) {
    if (!is.na(x[[step]])) {
      pct <- (x[[step]] / x$total) * 100
      cat(sprintf("  %-25s: %6.2f sec (%5.1f%%)\n",
                 gsub("_", " ", step), x[[step]], pct))
    }
  }
  invisible(x)
}
