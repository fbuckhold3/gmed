#' Fixed RDM 2.0 Data Loader - Uses Existing gmed Functions
#'
#' Streamlined data loading that preserves historical data for medians while using
#' your existing gmed functions. Calculates medians BEFORE filtering archived residents.
#'
#' @param rdm_token Character. RDM REDCap API token. If NULL, attempts to retrieve from
#'   environment variable RDM_TOKEN.
#' @param redcap_url Character. REDCap API URL. Default: "https://redcapsurvey.slu.edu/api/"
#' @param verbose Logical. Whether to print detailed progress messages during loading.
#' @param ensure_gmed_columns Logical. Whether to verify and create required columns for
#'   gmed module compatibility.
#' @param use_cache Logical. Whether to use cached data dictionary and milestone medians
#'   to speed up loading. Default: TRUE. Use clear_rdm_cache() to clear cached data.
#'
#' @return List containing RDM 2.0 data structure with historical medians preserved
#' @export
load_rdm_complete <- function(rdm_token = NULL,
                              redcap_url = "https://redcapsurvey.slu.edu/api/",
                              verbose = TRUE,
                              ensure_gmed_columns = TRUE,
                              raw_or_label = "label",
                              use_cache = TRUE) {
  
  # TOKEN HANDLING
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
  }
  
  if (rdm_token == "" || is.null(rdm_token)) {
    stop("RDM_TOKEN must be provided or set as environment variable")
  }

  # STEP 1: LOAD ALL RAW DATA (INCLUDING ARCHIVED) USING EXISTING FUNCTION
  if (verbose) message("Loading raw data from REDCap...")
  raw_data <- load_data_by_forms(
    rdm_token = rdm_token,
    redcap_url = redcap_url,
    filter_archived = FALSE,
    calculate_levels = FALSE,
    raw_or_label = raw_or_label,
    verbose = FALSE  # Suppress verbose from load_data_by_forms
  )

  # STEP 2: LOAD DATA DICTIONARY (WITH CACHING)
  dict_cache_key <- paste0("data_dict_", redcap_url)
  if (use_cache) {
    data_dict <- get_cached(dict_cache_key)
    if (!is.null(data_dict)) {
      if (verbose) message("Using cached data dictionary")
    }
  } else {
    data_dict <- NULL
  }

  if (is.null(data_dict)) {
    if (verbose) message("Loading data dictionary from REDCap...")
    data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
    if (use_cache) {
      set_cached(dict_cache_key, data_dict)
    }
  }

  # STEP 3: CALCULATE HISTORICAL MILESTONE MEDIANS (BEFORE FILTERING!)
  # Create cache key based on number of records (invalidates when data changes)
  medians_cache_key <- paste0("milestone_medians_", nrow(raw_data$raw_data))

  if (use_cache) {
    historical_milestone_medians <- get_cached(medians_cache_key)
    if (!is.null(historical_milestone_medians)) {
      if (verbose) message("Using cached milestone medians")
    }
  } else {
    historical_milestone_medians <- NULL
  }

  if (is.null(historical_milestone_medians)) {
    if (verbose) message("Calculating milestone medians from historical data...")
    historical_milestone_medians <- tryCatch({
      calculate_all_milestone_medians(raw_data, verbose = FALSE)
    }, error = function(e) {
      if (verbose) message("Milestone median calculation failed: ", e$message)
      list()
    })
    if (use_cache && length(historical_milestone_medians) > 0) {
      set_cached(medians_cache_key, historical_milestone_medians)
    }
  }

  # STEP 4: NOW FILTER ARCHIVED RESIDENTS USING EXISTING FUNCTION
  if (verbose) message("Filtering archived residents...")
  filtered_data <- filter_archived_residents(raw_data, verbose = FALSE)

  # STEP 5: PROCESS RESIDENT LEVELS ON FILTERED DATA
  residents <- filtered_data$forms$resident_data
  if (is.null(residents)) {
    residents <- filtered_data$forms$residents %||%
      filtered_data$forms$demographic_data %||%
      filtered_data$forms[[1]]
  }
  
  # Process resident levels and names (inline to avoid helper function)
  if (!is.null(residents)) {
    # Add name if missing
    if (!"name" %in% names(residents)) {
      name_alternatives <- c("Name", "resident_name", "full_name", "first_name")
      found_name_col <- intersect(name_alternatives, names(residents))[1]

      if (!is.na(found_name_col)) {
        residents$name <- residents[[found_name_col]]
      } else {
        residents$name <- paste("Resident", residents$record_id)
      }
    }

    # Calculate current levels
    if ("type" %in% names(residents) && "grad_yr" %in% names(residents)) {
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      academic_year <- ifelse(as.numeric(format(Sys.Date(), "%m")) >= 7, current_year, current_year - 1)

      residents <- residents %>%
        dplyr::mutate(
          Level = dplyr::case_when(
            # Handle Preliminary residents - always Intern
            type == "Preliminary" ~ "Intern",

            # Handle Categorical residents - calculate based on grad_yr
            type == "Categorical" & !is.na(grad_yr) ~ {
              years_to_grad <- as.numeric(grad_yr) - academic_year
              dplyr::case_when(
                years_to_grad == 3 ~ "Intern",
                years_to_grad == 2 ~ "PGY2",
                years_to_grad == 1 ~ "PGY3",
                years_to_grad == 0 ~ "Graduating",
                years_to_grad < 0 ~ "Graduated",
                years_to_grad > 3 ~ "Pre-Intern",
                TRUE ~ "Unknown"
              )
            },
            TRUE ~ "Unknown"
          )
        )
    } else {
      residents$Level <- "Unknown"
    }
  }
  
  # STEP 6: PROCESS ASSESSMENT DATA
  assessment_data <- filtered_data$forms$assessment %||% data.frame()

  if (ensure_gmed_columns && nrow(assessment_data) > 0) {
    # Ensure compatibility columns exist
    if (!"fac_eval_level" %in% names(assessment_data)) {
      assessment_data$fac_eval_level <- assessment_data$ass_level
    }
    if (!"q_level" %in% names(assessment_data)) {
      assessment_data$q_level <- assessment_data$ass_level
    }
  }

  # STEP 7: CREATE MILESTONE WORKFLOW FROM FILTERED DATA
  if (verbose) message("Creating milestone workflow...")
  milestone_workflow <- tryCatch({
    create_milestone_workflow_from_dict(
      all_forms = filtered_data$forms,
      data_dict = data_dict,
      resident_data = residents,
      verbose = FALSE
    )
  }, error = function(e) {
    if (verbose) message("Milestone workflow creation failed: ", e$message)
    NULL
  })

  if (verbose) message("Data loading complete!")
  
  # STEP 8: RETURN COMPREHENSIVE DATA STRUCTURE
  result <- list(
    # Filtered data for current displays
    residents = residents,
    assessment = assessment_data,
    
    # Individual form data (filtered to active residents)
    milestone_entry = filtered_data$forms$milestone_entry %||% data.frame(),
    milestone_selfevaluation_c33c = filtered_data$forms$milestone_selfevaluation_c33c %||% data.frame(),
    acgme_miles = filtered_data$forms$acgme_miles %||% data.frame(),
    
    # Complete data dictionary
    data_dict = data_dict,
    
    # Historical milestone medians (calculated from ALL data including archived)
    historical_medians = historical_milestone_medians,
    
    # Processed milestone workflow (from filtered data)
    milestone_workflow = milestone_workflow,
    
    # All filtered forms for advanced use
    all_forms = filtered_data$forms,
    raw_data = raw_data$raw_data,
    
    # Metadata
    data_loaded = TRUE,
    load_timestamp = Sys.time(),
    active_resident_count = nrow(residents),
    rdm_token = rdm_token,
    redcap_url = redcap_url
  )

  return(result)
}
