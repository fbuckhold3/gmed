#' Simplified RDM 2.0 Data Loader - Most Recent Data Approach
#'
#' Streamlined data loading function that focuses on getting the most recent data
#' for each resident without complex level-at-time calculations. Uses actual data
#' presence rather than calculated levels to determine what to display.
#'
#' @param rdm_token Character. RDM REDCap API token. If NULL, attempts to retrieve from 
#'   environment variable RDM_TOKEN.
#' @param redcap_url Character. REDCap API URL. Default: "https://redcapsurvey.slu.edu/api/"
#' @param verbose Logical. Whether to print detailed progress messages during loading.
#' @param ensure_gmed_columns Logical. Whether to verify and create required columns for
#'   gmed module compatibility.
#'
#' @return List containing simplified RDM 2.0 data structure
#' @export
load_rdm_complete <- function(rdm_token = NULL, 
                              redcap_url = "https://redcapsurvey.slu.edu/api/",
                              verbose = TRUE,
                              ensure_gmed_columns = TRUE) {
  
  # TOKEN HANDLING
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
  }
  
  if (rdm_token == "" || is.null(rdm_token)) {
    stop("RDM_TOKEN must be provided or set as environment variable")
  }
  
  if (verbose) message("=== SIMPLIFIED RDM 2.0 DATA LOADING ===")
  
  # STEP 1: LOAD RAW DATA BY FORMS
  if (verbose) message("Loading RDM 2.0 data...")
  data <- load_data_by_forms(rdm_token = rdm_token)
  if (verbose) message("Raw data loaded")
  
  # STEP 2: LOAD DATA DICTIONARY
  if (verbose) message("Loading data dictionary...")
  data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  if (verbose) message("Data dictionary loaded with ", nrow(data_dict), " fields")
  
  # STEP 3: FILTER ARCHIVED RESIDENTS FOR ACTIVE DATA
  if (verbose) message("Filtering archived residents...")
  clean_data <- filter_archived_residents(data, verbose = verbose)
  
  # STEP 4: EXTRACT AND STANDARDIZE RESIDENTS DATA (SIMPLE APPROACH)
  residents <- clean_data$forms$resident_data
  all_forms <- clean_data$forms
  
  if (is.null(residents)) {
    residents <- all_forms$residents %||% 
      all_forms$demographic_data %||%
      all_forms[[1]]
    if (verbose) message("Used alternative form for resident data")
  }
  
  # SIMPLIFIED RESIDENT LEVEL CALCULATION
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
    
    # SIMPLE LEVEL CALCULATION - Just current level, no time-based complexity
    if ("type" %in% names(residents) && "grad_yr" %in% names(residents)) {
      if (verbose) message("Calculating current resident levels from type and grad_yr...")
      
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      academic_year <- ifelse(as.numeric(format(Sys.Date(), "%m")) >= 7, current_year, current_year - 1)
      
      residents <- residents %>%
        dplyr::mutate(
          Level = dplyr::case_when(
            type == "Resident" & !is.na(grad_yr) ~ {
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
            TRUE ~ "Unknown"
          )
        )
      
      if (verbose) {
        level_counts <- table(residents$Level, useNA = "always")
        message("Current resident levels: ", paste(names(level_counts), "=", level_counts, collapse = ", "))
      }
    } else {
      residents$Level <- "Unknown"
      if (verbose) message("No type/grad_yr columns found, using 'Unknown'")
    }
  }
  
  # STEP 5: SIMPLE ASSESSMENT DATA PREPARATION
  assessment_data <- all_forms$assessment %||% data.frame()
  
  if (ensure_gmed_columns && nrow(assessment_data) > 0) {
    if (verbose) message("Preparing assessment data for gmed modules...")
    
    # SIMPLE APPROACH: Add most recent level for each resident to assessments
    # Get the most recent assessment level for each resident
    if (!is.null(residents) && nrow(residents) > 0) {
      resident_levels <- residents %>%
        dplyr::select(record_id, Level) %>%
        dplyr::mutate(record_id = as.character(record_id))
      
      # Add current resident level to assessment data
      assessment_data <- assessment_data %>%
        dplyr::mutate(record_id = as.character(record_id)) %>%
        dplyr::left_join(resident_levels, by = "record_id") %>%
        dplyr::mutate(
          # Simple level assignment based on current resident level
          ass_level = dplyr::case_when(
            Level == "Intern" ~ 1L,
            Level == "PGY2" ~ 2L,
            Level == "PGY3" ~ 3L,
            TRUE ~ NA_integer_
          ),
          fac_eval_level = ass_level,  # Same for faculty evaluations
          q_level = ass_level          # Same for questions
        )
      
      if (verbose) {
        ass_level_counts <- table(assessment_data$ass_level, useNA = "always")
        message("Assessment levels assigned: ", paste(names(ass_level_counts), "=", ass_level_counts, collapse = ", "))
      }
    }
  }
  
  # STEP 6: CALCULATE MILESTONE MEDIANS FOR COMPARISON (HISTORICAL)
  if (verbose) message("Calculating milestone medians from all historical data...")
  historical_milestone_medians <- tryCatch({
    calculate_all_milestone_medians(data)  # Use all data including archived for medians
  }, error = function(e) {
    if (verbose) message("Milestone median calculation failed: ", e$message)
    NULL
  })
  
  # STEP 7: PREPARE MILESTONE DATA (SIMPLE APPROACH)
  if (verbose) message("Preparing milestone app data...")
  mile_data <- tryCatch({
    # Simple milestone data preparation without complex level calculations
    list(
      milestone_individual = all_forms,  # Just use the forms as-is
      milestone_medians = historical_milestone_medians,
      residents = residents
    )
  }, error = function(e) {
    if (verbose) message("Milestone app data preparation failed: ", e$message)
    list(residents = residents)
  })
  
  # STEP 8: RETURN SIMPLIFIED DATA STRUCTURE
  result <- list(
    # Core data
    residents = residents,
    all_forms = all_forms,
    assessment_data = assessment_data,
    milestone_data = mile_data,
    
    # Data dictionary and raw data
    data_dict = data_dict,
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
    message("Simplified data loading finished!")
    message("Residents: ", nrow(residents))
    message("Assessment records: ", nrow(assessment_data))
    message("Data dictionary fields: ", nrow(data_dict))
    message("=== Data ready for gmed modules (simplified approach) ===")
  }
  
  return(result)
}
