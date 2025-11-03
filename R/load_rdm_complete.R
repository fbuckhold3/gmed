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
#'
#' @return List containing RDM 2.0 data structure with historical medians preserved
#' @export
load_rdm_complete <- function(rdm_token = NULL, 
                              redcap_url = "https://redcapsurvey.slu.edu/api/",
                              verbose = TRUE,
                              ensure_gmed_columns = TRUE,
                              raw_or_label = "label") {
  
  # TOKEN HANDLING
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
  }
  
  if (rdm_token == "" || is.null(rdm_token)) {
    stop("RDM_TOKEN must be provided or set as environment variable")
  }
  
  if (verbose) message("=== SIMPLIFIED RDM 2.0 DATA LOADING ===")
  
  # STEP 1: LOAD ALL RAW DATA (INCLUDING ARCHIVED) USING EXISTING FUNCTION
  if (verbose) message("📊 Loading REDCap data organized by forms...")
raw_data <- load_data_by_forms(
  rdm_token = rdm_token,
  redcap_url = redcap_url,
  filter_archived = FALSE,
  calculate_levels = FALSE,
  raw_or_label = raw_or_label,  # ADD THIS
  verbose = verbose
)
  
  if (verbose) {
    # Count total records from raw data
    total_records <- nrow(raw_data$forms$resident_data %||% data.frame())
    total_forms <- length(raw_data$forms)
    message("Successfully pulled data with ", total_records, " residents across ", total_forms, " forms")
    message("✅ Loaded ", total_records, " total records")
    message("📋 Found ", total_forms, " forms in data:")
    for (form_name in names(raw_data$forms)) {
      if (!is.null(raw_data$forms[[form_name]]) && nrow(raw_data$forms[[form_name]]) > 0) {
        message("   - ", form_name)
      }
    }
  }
  
  # STEP 2: LOAD DATA DICTIONARY
  if (verbose) message("📖 Loading data dictionary...")
  data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  if (verbose) message("Retrieved data dictionary with ", nrow(data_dict), " fields")
  
  # STEP 3: CALCULATE HISTORICAL MILESTONE MEDIANS (BEFORE FILTERING!)
  if (verbose) message("Calculating milestone medians from all historical data...")
  if (verbose) message("=== CALCULATING MILESTONE MEDIANS FOR ALL FORMS ===")
  
  # Use your existing function but on the RAW (unfiltered) data
  historical_milestone_medians <- tryCatch({
    calculate_all_milestone_medians(raw_data, verbose = verbose)
  }, error = function(e) {
    if (verbose) message("❌ Milestone median calculation failed: ", e$message)
    list()
  })
  
  if (verbose && length(historical_milestone_medians) > 0) {
    message("=== MILESTONE MEDIANS SUMMARY ===")
    for (form_name in names(historical_milestone_medians)) {
      result <- historical_milestone_medians[[form_name]]
      if (!is.null(result$medians)) {
        message(form_name, " :")
        message("  Type: ", result$type %||% "unknown", " ")
        message("  Columns: ", length(result$columns %||% character()), " ")
        message("  Periods: ", nrow(result$medians), " ")
        message("  ✅ SUCCESS")
      }
    }
  }
  
  # STEP 4: NOW FILTER ARCHIVED RESIDENTS USING EXISTING FUNCTION
  if (verbose) message("🗂️  Filtering archived residents...")
  
  # Get archive summary before filtering
  if (verbose && !is.null(raw_data$forms$resident_data)) {
    total_residents <- nrow(raw_data$forms$resident_data)
    
    # Count archived residents manually
    archived_count <- 0
    if ("res_archive" %in% names(raw_data$forms$resident_data)) {
      archive_values <- c("Yes", "Y", "1", 1, "true", "True", "TRUE")
      archived_count <- sum(raw_data$forms$resident_data$res_archive %in% archive_values, na.rm = TRUE)
    }
    
    active_count <- total_residents - archived_count
    message("📊 Total residents: ", total_residents, " ")
    message("🗂️  Archived residents: ", archived_count, " ")
    message("✅ Active residents: ", active_count, " ")
  }
  
  # Use your existing filter function
  filtered_data <- filter_archived_residents(raw_data, verbose = verbose)
  
  if (verbose) message("✅ Archive filtering complete")
  
  # STEP 5: PROCESS RESIDENT LEVELS ON FILTERED DATA
  if (verbose) message("🎓 Calculating resident levels...")
  
  residents <- filtered_data$forms$resident_data
  if (is.null(residents)) {
    residents <- filtered_data$forms$residents %||% 
      filtered_data$forms$demographic_data %||%
      filtered_data$forms[[1]]
    if (verbose) message("Used alternative form for resident data")
  }
  
  # Process resident levels and names (inline to avoid helper function)
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
    
    # Calculate current levels
    if ("type" %in% names(residents) && "grad_yr" %in% names(residents)) {
      if (verbose) message("Using type/grad_yr calculation method (RDM 2.0 pattern)")
      
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      academic_year <- ifelse(as.numeric(format(Sys.Date(), "%m")) >= 7, current_year, current_year + 1)
      
      if (verbose) message("Current academic year: ", academic_year)
      
      residents <- residents %>%
        dplyr::mutate(
          Level = dplyr::case_when(
            # Handle Categorical residents (type = 2)
            type == 2 & !is.na(grad_yr) ~ {
              years_to_grad <- as.numeric(grad_yr) - academic_year
              dplyr::case_when(
                years_to_grad == 3 ~ "Intern",
                years_to_grad == 2 ~ "PGY2", 
                years_to_grad == 1 ~ "PGY3",
                years_to_grad == 0 ~ "Graduating",
                years_to_grad < 0 ~ "Graduated",
                TRUE ~ "Unknown"
              )
            },
            # Handle Preliminary residents (type = 1) - always Intern
            type == 1 ~ "Intern",
            TRUE ~ "Unknown"
          )
        )
      
      if (verbose) {
        level_counts <- table(residents$Level, useNA = "always")
        message("Level distribution: ", paste(names(level_counts), level_counts, sep = ": ", collapse = ", "))
      }
    } else {
      residents$Level <- "Unknown"
      if (verbose) message("No type/grad_yr columns found, using 'Unknown'")
    }
  }
  
  # STEP 6: PROCESS ASSESSMENT DATA
  assessment_data <- filtered_data$forms$assessment %||% data.frame()
  
  if (ensure_gmed_columns && nrow(assessment_data) > 0) {
    if (verbose) message("Assessment data already has correct ass_level values")
    
    # Ensure compatibility columns exist
    if (!"fac_eval_level" %in% names(assessment_data)) {
      assessment_data$fac_eval_level <- assessment_data$ass_level
    }
    if (!"q_level" %in% names(assessment_data)) {
      assessment_data$q_level <- assessment_data$ass_level
    }
    
    if (verbose) {
      ass_level_counts <- table(assessment_data$ass_level, useNA = "always")
      message("Using existing assessment levels: ", paste(names(ass_level_counts), "=", ass_level_counts, collapse = ", "))
    }
  }
  
  # STEP 7: CREATE MILESTONE WORKFLOW FROM FILTERED DATA
  if (verbose) message("Creating milestone workflow with clean data structure...")
  milestone_workflow <- tryCatch({
    create_milestone_workflow_from_dict(
      all_forms = filtered_data$forms,  # Use filtered forms for current workflow
      data_dict = data_dict,
      resident_data = residents,
      verbose = verbose
    )
  }, error = function(e) {
    if (verbose) message("Milestone workflow creation failed: ", e$message)
    NULL
  })
  
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
  
  if (verbose) {
    message("=== Data ready for gmed modules (simplified approach) ===")
    message("Simplified data loading finished!")
    message("Residents: ", nrow(residents))
    message("Assessment records: ", nrow(assessment_data))
    message("Data dictionary fields: ", nrow(data_dict))
  }
  
  return(result)
}
