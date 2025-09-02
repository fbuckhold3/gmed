# ============================================================================
# DEBUG: Show the exact structure your load_rdm_complete function needs
# ============================================================================

# This shows you exactly where the data_dict variable should be placed
# Copy this structure into your load_rdm_complete function

load_rdm_complete <- function(rdm_token = NULL, 
                                    redcap_url = "https://redcapsurvey.slu.edu/api/",
                                    verbose = TRUE,
                                    ensure_gmed_columns = TRUE,
                                    cache = FALSE) {
  
  # TOKEN HANDLING
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
  }
  
  if (rdm_token == "" || is.null(rdm_token)) {
    stop("RDM_TOKEN must be provided or set as environment variable")
  }
  
  if (verbose) message("=== COMPLETE RDM 2.0 DATA LOADING ===")
  
  # STEP 1: LOAD RAW DATA BY FORMS
  if (verbose) message("Loading RDM 2.0 data...")
  data <- load_data_by_forms(rdm_token = rdm_token)
  if (verbose) message("Raw data loaded")
  
  # STEP 1.5: LOAD DATA DICTIONARY - MOVE THIS RIGHT AFTER DATA LOADING
  if (verbose) message("Loading data dictionary...")
  data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  if (verbose) message("Data dictionary loaded with ", nrow(data_dict), " fields")
  
  # STEP 2: CALCULATE HISTORICAL MEDIANS BEFORE FILTERING
  if (verbose) message("Calculating milestone medians from historical data...")
  historical_milestone_medians <- calculate_all_milestone_medians(data)
  if (verbose) message("Historical milestone medians calculated")
  
  # STEP 3: FILTER ARCHIVED RESIDENTS FOR ACTIVE DATA
  if (verbose) message("Filtering archived residents...")
  clean_data <- filter_archived_residents(data, verbose = verbose)
  
  # STEP 4: ADD LEVELS AT TIME TO ACTIVE RESIDENTS
  if (verbose) message("Adding levels at time...")
  data_with_levels <- add_level_at_time_to_forms(clean_data)
  if (verbose) message("Added levels to active residents")
  
  # STEP 5: PREPARE MILESTONE DATA FROM ACTIVE RESIDENTS
  if (verbose) message("Preparing milestone app data...")
  mile_data <- prepare_milestone_app_data(data_with_levels)
  if (verbose) message("Prepared milestone data")
  
  # STEP 6: EXTRACT AND STANDARDIZE RESIDENTS DATA
  residents <- data_with_levels$forms$resident_data
  all_forms <- data_with_levels$forms
  
  if (is.null(residents)) {
    residents <- all_forms$residents %||% 
      all_forms$demographic_data %||%
      all_forms[[1]]
    if (verbose) message("Used alternative form for resident data")
  }
  
  # Column standardization (your existing logic)
  if (!is.null(residents)) {
    # Add name if missing
    if (!"name" %in% names(residents)) {
      name_alternatives <- c("Name", "resident_name", "full_name", "first_name")
      found_name_col <- intersect(name_alternatives, names(residents))[1]
      
      if (!is.na(found_name_col)) {
        residents$name <- residents[[found_name_col]]
        if (verbose) message("Using ", found_name_col, " as name column")
      } else {
        residents$name <- paste("Resident", residents$record_id)
        if (verbose) message("No name column found, using 'Resident [ID]'")
      }
    }
    
    # Add Level if missing  
    if (!"Level" %in% names(residents)) {
      level_alternatives <- c("level", "resident_level", "training_level", "pgy", "PGY", "year")
      found_level_col <- intersect(level_alternatives, names(residents))[1]
      
      if (!is.na(found_level_col)) {
        residents$Level <- residents[[found_level_col]]
        if (verbose) message("Using ", found_level_col, " as Level column")
      } else {
        residents$Level <- "Unknown"
        if (verbose) message("No Level column found, using 'Unknown'")
      }
    }
  }
  
  # STEP 7: PREPARE ASSESSMENT DATA
  assessment_data <- all_forms$assessment %||% data.frame()
  
  if (ensure_gmed_columns && nrow(assessment_data) > 0) {
    # Your existing column creation logic here...
    if (verbose) message("Assessment data has required columns for gmed modules")
  }
  
  # STEP 8: RETURN COMPLETE DATA STRUCTURE
  # NOW data_dict is properly scoped and available
  result <- list(
    # Core data
    residents = residents,
    all_forms = all_forms,
    assessment_data = assessment_data,
    milestone_data = mile_data,
    
    # Data dictionary and raw data for dict-driven workflow
    data_dict = data_dict,  # This should now work
    raw_data = data$raw_data,
    
    # Historical comparison data
    historical_medians = historical_milestone_medians,
    
    # Metadata
    data_loaded = TRUE,
    load_timestamp = Sys.time(),
    rdm_token = rdm_token,
    redcap_url = redcap_url
  )
  
  if (verbose) {
    message("Complete data loading finished!")
    message("Residents: ", nrow(residents))
    message("Assessment records: ", nrow(assessment_data))
    message("Data dictionary fields: ", nrow(data_dict))
    message("=== Data ready for gmed modules ===")
  }
  
  return(result)
}
