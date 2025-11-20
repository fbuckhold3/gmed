# ============================================================================
# SIMPLE RDM 2.0 DATA LOADER FOR GMED PACKAGE
# R/load_rdm_simple.R - Add this to your gmed package
# ============================================================================

#' Simple RDM 2.0 Data Loader for Shiny Apps
#'
#' One-stop function to load all RDM 2.0 data with sensible defaults.
#' Designed to replace complex app-specific data loading with a single function call.
#' 
#' This function handles:
#' - Token configuration (environment vs config file)
#' - Complete data loading with proper filtering
#' - Level-at-time calculations
#' - Historical milestone medians
#' - Ready-to-use data structure for gmed modules
#'
#' @param rdm_token Character. RDM REDCap token. If NULL, tries environment variable RDM_TOKEN
#' @param redcap_url Character. REDCap API URL (default: SLU)
#' @param for_app Character. App type hint for optimizations ("individual", "coaching", "ccc", "general")
#' @param verbose Logical. Print loading progress (default: TRUE)
#' @param config_fallback Logical. Try config.yml if environment variables missing (default: TRUE)
#'
#' @return List with standardized structure:
#'   - residents: Clean resident data with levels
#'   - assessment_data: All assessment records properly formatted for gmed modules  
#'   - all_forms: All form data organized by name
#'   - historical_medians: Milestone medians from complete historical data
#'   - milestone_data: Processed milestone data for visualizations
#'   - data_dict: RDM data dictionary
#'   - config: Configuration used (for debugging)
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple usage - auto-detects token
#' app_data <- load_rdm_simple()
#' 
#' # With explicit token
#' app_data <- load_rdm_simple(rdm_token = "your_token_here")
#' 
#' # For specific app type
#' app_data <- load_rdm_simple(for_app = "individual")
#' 
#' # Then use in your Shiny app:
#' residents <- app_data$residents
#' assessment_data <- app_data$assessment_data
#' }
load_rdm_simple <- function(rdm_token = NULL,
                            redcap_url = "https://redcapsurvey.slu.edu/api/",
                            for_app = "general",
                            verbose = TRUE,
                            config_fallback = TRUE) {

  # =========================================================================
  # CONFIGURATION AUTO-DETECTION
  # =========================================================================

  # Try to get token from various sources
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN", unset = "")

    # If environment variable is empty and config fallback is enabled
    if (rdm_token == "" && config_fallback && file.exists("config.yml")) {
      tryCatch({
        config_data <- config::get()
        rdm_token <- config_data$rdm_token %||% ""
      }, error = function(e) {
        if (verbose) cat("Config file load failed: ", e$message, "\n")
      })
    }
  }

  # Validate token
  if (is.null(rdm_token) || rdm_token == "") {
    stop("RDM_TOKEN is required. Set it as:\n",
         "  - Environment variable: Sys.setenv(RDM_TOKEN='your_token')\n",
         "  - Function parameter: load_rdm_simple(rdm_token='your_token')\n",
         "  - Config file: config.yml with rdm_token field")
  }

  # =========================================================================
  # STEP 1: LOAD RAW DATA
  # =========================================================================

  # Use your existing proven functions
  raw_data <- load_data_by_forms(
    rdm_token = rdm_token,
    redcap_url = redcap_url,
    filter_archived = FALSE,  # Don't filter yet - need historical data first
    calculate_levels = FALSE, # Don't calculate yet - do it in proper order
    verbose = verbose
  )

  # =========================================================================
  # STEP 2: HISTORICAL CALCULATIONS (BEFORE FILTERING)
  # =========================================================================

  historical_medians <- tryCatch({
    calculate_all_milestone_medians(raw_data)
  }, error = function(e) {
    if (verbose) cat("Historical medians calculation failed: ", e$message, "\n")
    list()
  })

  # =========================================================================
  # STEP 3: FILTER AND PROCESS ACTIVE DATA
  # =========================================================================

  clean_data <- filter_archived_residents(raw_data, verbose = verbose)

  data_with_levels <- add_level_at_time_to_forms(clean_data)

  milestone_data <- tryCatch({
    prepare_milestone_app_data(data_with_levels)
  }, error = function(e) {
    if (verbose) cat("Milestone data prep failed: ", e$message, "\n")
    NULL
  })
  
  # =========================================================================
  # STEP 4: EXTRACT AND STANDARDIZE KEY DATASETS
  # =========================================================================

  # Extract residents data
  residents <- data_with_levels$forms$resident_data
  if (is.null(residents)) {
    # Try alternatives
    residents <- data_with_levels$forms$residents %||%
      data_with_levels$forms$demographic_data %||%
      data_with_levels$forms[[1]]
  }

  # Ensure standard columns exist
  if (!is.null(residents)) {
    residents <- ensure_standard_resident_columns(residents, verbose = verbose)
  }

  # Extract assessment data - CRITICAL: this is what gmed modules expect
  assessment_data <- NULL
  if ("assessment" %in% names(data_with_levels$forms)) {
    assessment_data <- data_with_levels$forms$assessment
  } else {
    if (verbose) cat("WARNING: No assessment form found\n")
    assessment_data <- data.frame()
  }

  # =========================================================================
  # STEP 5: RETURN STANDARDIZED STRUCTURE
  # =========================================================================

  result <- list(
    # Key datasets that apps will use
    residents = residents,
    assessment_data = assessment_data,  # Raw assessment data for gmed modules
    all_forms = data_with_levels$forms,

    # Supporting data
    historical_medians = historical_medians,
    milestone_data = milestone_data,
    data_dict = raw_data$data_dict,

    # Configuration info (for debugging)
    config = list(
      rdm_token = rdm_token,
      redcap_url = redcap_url,
      for_app = for_app,
      loaded_at = Sys.time()
    ),

    # Metadata
    metadata = list(
      total_residents = nrow(residents),
      total_assessments = nrow(assessment_data),
      forms_available = names(data_with_levels$forms),
      app_ready = TRUE
    )
  )

  return(result)
}

#' Ensure Standard Resident Columns
#'
#' Helper to add standard columns that apps expect
#'
#' @param residents Residents data frame
#' @param verbose Print messages
#' @return Residents data with standard columns
ensure_standard_resident_columns <- function(residents, verbose = TRUE) {

  # Standard name column
  if (!"name" %in% names(residents)) {
    name_alternatives <- c("Name", "resident_name", "full_name", "first_name")
    found_col <- intersect(name_alternatives, names(residents))[1]

    if (!is.na(found_col)) {
      residents$name <- residents[[found_col]]
    } else {
      residents$name <- paste("Resident", residents$record_id)
    }
  }

  # Standard Level column
  if (!"Level" %in% names(residents)) {
    level_alternatives <- c("level", "resident_level", "training_level", "pgy", "PGY", "year")
    found_col <- intersect(level_alternatives, names(residents))[1]

    if (!is.na(found_col)) {
      residents$Level <- residents[[found_col]]
    } else {
      residents$Level <- "Unknown"
    }
  }

  # Standard access_code column
  if (!"access_code" %in% names(residents)) {
    code_alternatives <- c("accesscode", "code", "resident_code")
    found_col <- intersect(code_alternatives, names(residents))[1]

    if (!is.na(found_col)) {
      residents$access_code <- residents[[found_col]]
    }
  }

  return(residents)
}

#' Quick App Data Loader for Development
#'
#' Ultra-simple loader for testing/development - just provide token and go
#'
#' @param rdm_token Your RDM token
#' @return Basic data structure ready for gmed modules
#' @export
load_rdm_quick <- function(rdm_token) {
  # Basic load
  data <- load_data_by_forms(rdm_token = rdm_token, verbose = FALSE)
  clean_data <- filter_archived_residents(data, verbose = FALSE)
  final_data <- add_level_at_time_to_forms(clean_data)

  list(
    residents = final_data$forms$resident_data,
    assessment_data = final_data$forms$assessment %||% data.frame(),
    all_forms = final_data$forms,
    data_loaded = TRUE
  )
}