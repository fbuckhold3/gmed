#' @title REDCap Submission Functions for GMED
#' @description Complete REDCap submission functions handling both OVERWRITE and ADDITIVE patterns
#' @name redcap_submission
NULL

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Escape JSON strings for REDCap
#' @param x Character string to escape
#' @return Escaped string safe for JSON
escape_json_string <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  x <- as.character(x)
  x <- gsub('\\\\', '\\\\\\\\', x)
  x <- gsub('"', '\\\\"', x)
  x <- gsub('\n', '\\\\n', x)
  x <- gsub('\r', '\\\\r', x)
  x <- gsub('\t', '\\\\t', x)
  return(x)
}

#' Null-coalescing operator
#' @param a First value
#' @param b Second value (fallback)
#' @return a if not NULL, otherwise b
`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# CORE INSTANCE MAPPING FUNCTIONS
# ============================================================================

#' Get REDCap Instance Number - MASTER FUNCTION
#' 
#' This is the primary function for getting REDCap instances.
#' Handles both standard periods (1-7) and interim reviews (9+)
#'
#' @param level Resident level (Intern, PGY2, PGY3)
#' @param period Period name (can be app format, milestone format, or direct)
#' @param review_type Type of review ("scheduled" or "interim")
#' @param redcap_url REDCap API URL (only needed for interim reviews)
#' @param redcap_token REDCap token (only needed for interim reviews)
#' @param record_id Record ID (only needed for interim reviews)
#' @return Numeric instance number
#' @export
get_redcap_instance <- function(level, period, review_type = "scheduled", 
                                redcap_url = NULL, redcap_token = NULL, record_id = NULL) {
  
  message("get_redcap_instance called with level: ", level, ", period: ", period, ", review_type: ", review_type)
  
  # Handle interim reviews differently
  if (review_type == "interim") {
    if (is.null(redcap_url) || is.null(redcap_token) || is.null(record_id)) {
      warning("Interim review requires redcap_url, redcap_token, and record_id")
      return(9)  # Default to first interim instance
    }
    
    # Get next available interim instance (9+)
    return(get_next_interim_instance(redcap_url, redcap_token, record_id, "ccc_review"))
  }
  
  # STANDARD SCHEDULED REVIEWS - Direct mapping table
  instance_map <- list(
    "Intern" = list(
      # App format periods
      "Intern Intro" = 7,
      "Mid Review" = 1,
      "End Review" = 2,
      # Direct milestone format periods
      "Mid Intern" = 1,
      "End Intern" = 2
    ),
    "PGY2" = list(
      # App format periods  
      "Intern Intro" = 8,  # Fallback
      "Mid Review" = 3,
      "End Review" = 4,
      # Direct milestone format periods
      "Mid PGY2" = 3,
      "End PGY2" = 4
    ),
    "PGY3" = list(
      # App format periods
      "Intern Intro" = 8,  # Fallback  
      "Mid Review" = 5,
      "End Review" = 6,
      "Graduation" = 6,
      "Graduating" = 6,
      # Direct milestone format periods
      "Mid PGY3" = 5,
      "Graduation" = 6,
      "Graduating" = 6
    )
  )
  
  # First check direct mapping
  if (level %in% names(instance_map) && period %in% names(instance_map[[level]])) {
    result <- instance_map[[level]][[period]]
    message(paste("Found direct mapping:", result))
    return(result)
  }
  
  # CCC session values mapping (fallback)
  ccc_to_instance <- c(
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduation" = 6,
    "Intern Intro" = 7
  )
  
  if (period %in% names(ccc_to_instance)) {
    result <- ccc_to_instance[period]
    message(paste("Mapped via CCC session name:", result))
    return(result)
  }
  
  # Try parsing period like "End PGY2" into components
  period_parts <- strsplit(as.character(period), " ")[[1]]
  if (length(period_parts) >= 2) {
    period_prefix <- period_parts[1]  # "End" or "Mid"
    
    # Check if this is a valid combination
    combined_period <- paste(period_prefix, level)
    if (combined_period %in% names(ccc_to_instance)) {
      result <- ccc_to_instance[combined_period]
      message(paste("Mapped via combined period-level:", result))
      return(result)
    }
  }
  
  # Default fallback - return interim (8)
  message("Using default value 8 (Interim)")
  return(8)
}

#' Get Next Available Interim Instance
#' 
#' Finds the next available instance number for interim reviews (starts at 9)
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Record ID
#' @param form_name REDCap form name (default: "ccc_review")
#' @return Numeric instance number (9 or higher)
#' @export
get_next_interim_instance <- function(redcap_url, redcap_token, record_id, form_name = "ccc_review") {
  
  message("Finding next interim instance for record ", record_id, " in form ", form_name)
  
  tryCatch({
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        forms = form_name,
        fields = "record_id,redcap_repeat_instance",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (is.data.frame(data) && nrow(data) > 0) {
        existing_instances <- as.numeric(data$redcap_repeat_instance)
        existing_instances <- existing_instances[!is.na(existing_instances)]
        
        if (length(existing_instances) > 0) {
          max_instance <- max(existing_instances)
          next_instance <- max(max_instance + 1, 9)  # Ensure it's at least 9
          message("Found existing instances: ", paste(existing_instances, collapse = ", "))
          message("Using next available instance: ", next_instance)
          return(next_instance)
        }
      }
    }
    
    # Default to instance 9 for first interim review
    message("No existing instances found, using default instance 9 for interim review")
    return(9)
    
  }, error = function(e) {
    message("Error getting interim instance: ", e$message)
    return(9)
  })
}

# ============================================================================
# PATTERN 1: OVERWRITE SUBMISSIONS (Coaching, CCC, Self-Eval)
# ============================================================================

#' Submit Data Using OVERWRITE Pattern
#' 
#' For forms that use SET PERIODS (coaching reviews, CCC reviews, self-evaluations)
#' These forms OVERWRITE previous data for the same period/instance
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param form_data List of form fields
#' @param instrument_name REDCap instrument name (e.g., "coach_rev", "ccc_review")
#' @param period Review period
#' @param level Resident level
#' @param review_type "scheduled" or "interim"
#' @return List with success status and message
#' @export
submit_overwrite_data <- function(redcap_url, redcap_token, record_id, 
                                  form_data, instrument_name, period, level, 
                                  review_type = "scheduled") {
  
  tryCatch({
    # Get the fixed instance number for this period/level
    instance_number <- get_redcap_instance(
      level = level,
      period = period, 
      review_type = review_type,
      redcap_url = redcap_url,
      redcap_token = redcap_token,
      record_id = record_id
    )
    
    # Prepare data for REDCap
    redcap_data <- list(
      record_id = as.character(record_id),
      redcap_repeat_instrument = instrument_name,
      redcap_repeat_instance = as.character(instance_number)
    )
    
    # Add all form fields
    redcap_data <- c(redcap_data, form_data)
    
    # Mark as complete
    complete_field <- paste0(instrument_name, "_complete")
    redcap_data[[complete_field]] <- "2"
    
    # Build JSON manually for better control
    json_data <- build_redcap_json(record_id, instance_number, redcap_data, instrument_name)
    
    # Submit to REDCap
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        format = "json",
        type = "flat",
        data = json_data
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      message(instrument_name, " data submitted successfully to instance ", instance_number)
      return(list(success = TRUE, instance = instance_number, 
                  message = paste(instrument_name, "submitted successfully")))
    } else {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      return(list(success = FALSE, message = paste("REDCap error:", error_text)))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Submission error:", e$message)))
  })
}

#' Submit Coach Review Data (OVERWRITE TYPE)
#' 
#' Wrapper for submit_overwrite_data specifically for coaching reviews
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param coach_data List of coaching review fields
#' @param period Review period
#' @param level Resident level
#' @param review_type "scheduled" or "interim"
#' @return List with success status and message
#' @export
submit_coach_review_data <- function(redcap_url, redcap_token, record_id, 
                                     coach_data, period, level, review_type = "scheduled") {
  
  return(submit_overwrite_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = coach_data,
    instrument_name = "coach_rev",
    period = period,
    level = level,
    review_type = review_type
  ))
}

#' Submit CCC Review Data (OVERWRITE TYPE)
#' 
#' Wrapper for submit_overwrite_data specifically for CCC reviews
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param ccc_data List of CCC review fields
#' @param period Review period
#' @param level Resident level
#' @param review_type "scheduled" or "interim"
#' @return List with success status and message
#' @export
submit_ccc_review_data <- function(redcap_url, redcap_token, record_id, 
                                   ccc_data, period, level, review_type = "scheduled") {
  
  return(submit_overwrite_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = ccc_data,
    instrument_name = "ccc_review",
    period = period,
    level = level,
    review_type = review_type
  ))
}

# ============================================================================
# PATTERN 2: ADDITIVE SUBMISSIONS (Faculty Evals, Scholarship, etc.)
# ============================================================================

#' Submit Data Using ADDITIVE Pattern
#' 
#' For forms that ADD NEW INSTANCES (faculty evals, resident evals, scholarship)
#' These forms always increment and never overwrite existing data
#'
#' @param redcap_url REDCap API URL  
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param form_data List of form fields
#' @param instrument_name REDCap instrument name (e.g., "faculty_evaluation", "scholarship")
#' @return List with success status and message
#' @export
submit_additive_data <- function(redcap_url, redcap_token, record_id, 
                                 form_data, instrument_name) {
  
  tryCatch({
    # Get NEXT AVAILABLE instance number (always incrementing)
    next_instance <- get_next_additive_instance(redcap_url, redcap_token, record_id, instrument_name)
    
    # Prepare data for REDCap
    redcap_data <- list(
      record_id = as.character(record_id),
      redcap_repeat_instrument = instrument_name,
      redcap_repeat_instance = as.character(next_instance)
    )
    
    # Add all form fields
    redcap_data <- c(redcap_data, form_data)
    
    # Mark as complete
    complete_field <- paste0(instrument_name, "_complete")
    redcap_data[[complete_field]] <- "2"
    
    # Build JSON manually for better control
    json_data <- build_redcap_json(record_id, next_instance, redcap_data, instrument_name)
    
    # Submit to REDCap
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record", 
        format = "json",
        type = "flat",
        data = json_data
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      message(instrument_name, " data submitted successfully to instance ", next_instance)
      return(list(success = TRUE, instance = next_instance,
                  message = paste(instrument_name, "submitted successfully")))
    } else {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      return(list(success = FALSE, message = paste("REDCap error:", error_text)))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Submission error:", e$message)))
  })
}

#' Get Next Available Instance for Additive Forms
#' 
#' Helper function to find next available instance for forms that add new records
#' @param redcap_url REDCap API URL  
#' @param redcap_token REDCap API token
#' @param record_id Record ID
#' @param instrument_name REDCap instrument name
#' @return Numeric instance number (1 or higher)
#' @export
get_next_additive_instance <- function(redcap_url, redcap_token, record_id, instrument_name) {
  
  tryCatch({
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export", 
        format = "json",
        type = "flat",
        records = as.character(record_id),
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (is.data.frame(data) && nrow(data) > 0) {
        # Filter for this specific instrument
        instrument_data <- data[
          !is.na(data$redcap_repeat_instrument) & 
            data$redcap_repeat_instrument == instrument_name, 
        ]
        
        if (nrow(instrument_data) > 0) {
          instances <- as.numeric(instrument_data$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          
          if (length(instances) > 0) {
            max_instance <- max(instances)
            next_instance <- max_instance + 1
            message("Found existing ", instrument_name, " instances: ", paste(instances, collapse = ", "))
            message("Using next available instance: ", next_instance)
            return(next_instance)
          }
        }
      }
    }
    
    # Default to instance 1 for first evaluation
    message("No existing ", instrument_name, " instances found, using instance 1")
    return(1)
    
  }, error = function(e) {
    message("Error getting additive instance: ", e$message)
    return(1)
  })
}

#' Submit Faculty Evaluation (ADDITIVE TYPE)
#' 
#' Wrapper for submit_additive_data specifically for faculty evaluations
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param eval_data List of evaluation fields
#' @return List with success status and message
#' @export
submit_faculty_evaluation <- function(redcap_url, redcap_token, record_id, eval_data) {
  
  return(submit_additive_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = eval_data,
    instrument_name = "faculty_evaluation"
  ))
}

#' Submit Scholarship Data (ADDITIVE TYPE)
#' 
#' Wrapper for submit_additive_data specifically for scholarship entries
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param scholarship_data List of scholarship fields
#' @return List with success status and message
#' @export
submit_scholarship_data <- function(redcap_url, redcap_token, record_id, scholarship_data) {
  
  return(submit_additive_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = scholarship_data,
    instrument_name = "scholarship"
  ))
}

#' Submit Resident Evaluation (ADDITIVE TYPE)
#' 
#' Wrapper for submit_additive_data specifically for resident evaluations
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param eval_data List of evaluation fields
#' @return List with success status and message
#' @export
submit_resident_evaluation <- function(redcap_url, redcap_token, record_id, eval_data) {
  
  return(submit_additive_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = eval_data,
    instrument_name = "resident_evaluation"
  ))
}

# ============================================================================
# MILESTONE SUBMISSION (Special Case - OVERWRITE)
# ============================================================================

#' Submit Milestone Data
#' 
#' Special submission function for milestone assessments
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param selected_period Review period
#' @param resident_level Resident level
#' @param milestone_scores List of milestone scores
#' @param milestone_desc List of milestone descriptions (optional)
#' @return List with success status and message
#' @export
submit_milestone_data <- function(redcap_url, redcap_token, record_id, selected_period, 
                                  resident_level, milestone_scores, milestone_desc = list()) {
  
  # Validate record_id first
  if (is.null(record_id) || length(record_id) == 0) {
    error_msg <- "ERROR: Cannot submit milestone data - record_id is NULL or empty"
    message(error_msg)
    return(list(success = FALSE, outcome_message = error_msg))
  }
  
  # Convert record_id to character and ensure it's a single value
  record_id <- as.character(record_id)[1]
  
  # Use the consolidated instance function
  instance_number <- get_redcap_instance(
    level = resident_level,
    period = selected_period,
    review_type = "scheduled"  # Milestones are always scheduled reviews
  )
  
  message("Using instance number: ", instance_number, " for milestone submission")
  
  if (is.na(instance_number) || is.null(instance_number)) {
    return(list(
      success = FALSE,
      outcome_message = paste("Could not map period", selected_period, 
                              "with level", resident_level, "to a REDCap instance.")
    ))
  }
  
  # Format today's date properly for REDCap as YYYY-MM-DD
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Build fields list with all milestone data
  fields <- list(
    prog_mile_date = today_date,
    prog_mile_period = as.character(instance_number)
  )
  
  # Add milestone scores
  if (!is.null(milestone_scores) && length(milestone_scores) > 0) {
    for(key in names(milestone_scores)) {
      fields[[key]] <- as.character(milestone_scores[[key]])
      
      # Add description if it exists
      if (!is.null(milestone_desc) && length(milestone_desc) > 0 && 
          key %in% names(milestone_desc) && 
          !is.null(milestone_desc[[key]]) &&
          nzchar(trimws(milestone_desc[[key]]))) {
        desc_field <- paste0(key, "_desc")
        fields[[desc_field]] <- as.character(milestone_desc[[key]])
      }
    }
  }
  
  # Use the overwrite pattern for milestones
  return(submit_overwrite_data(
    redcap_url = redcap_url,
    redcap_token = redcap_token,
    record_id = record_id,
    form_data = fields,
    instrument_name = "milestone_entry",
    period = selected_period,
    level = resident_level,
    review_type = "scheduled"
  ))
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Build JSON data for REDCap submission
#'
#' @param record_id Record ID
#' @param instance Instance number
#' @param fields List of fields and values to submit
#' @param instrument REDCap instrument name
#' @return JSON string for REDCap submission
build_redcap_json <- function(record_id, instance, fields, instrument) {
  
  # Start JSON with required fields
  json_data <- paste0(
    '[{"record_id":"', escape_json_string(as.character(record_id)), '"',
    ',"redcap_repeat_instrument":"', escape_json_string(instrument), '"',
    ',"redcap_repeat_instance":"', escape_json_string(as.character(instance)), '"'
  )
  
  # Add all fields with proper escaping
  for (field_name in names(fields)) {
    # Skip record_id, redcap_repeat_instrument, redcap_repeat_instance (already added)
    if (field_name %in% c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance")) next
    
    # Only add non-NULL, non-NA values
    if (!is.null(fields[[field_name]]) && !is.na(fields[[field_name]])) {
      value <- escape_json_string(as.character(fields[[field_name]]))
      json_data <- paste0(json_data, ',"', field_name, '":"', value, '"')
    }
  }
  
  # Close the JSON array
  json_data <- paste0(json_data, "}]")
  
  return(json_data)
}

#' Map form inputs to coach review fields
#'
#' @param inputs Form input values from Shiny
#' @param is_intern_intro Whether this is for an intern in intro period
#' @return List of mapped fields for REDCap
#' @export
map_inputs_to_coach_rev <- function(inputs, is_intern_intro = FALSE) {
  
  # Initialize result with empty values
  result <- list(
    # Form metadata fields
    coach_date = format(Sys.Date(), "%Y-%m-%d"),
    
    # Tab 1: Pre-review
    coach_pre_rev = "",
    
    # Tab 2: Wellness
    coach_intro_back = "",
    coach_coping = "",
    coach_wellness = "",
    
    # Tab 3: Evaluations
    coach_evaluations = "",
    coach_p_d_comments = "",
    
    # Tab 4: Knowledge
    coach_ls_and_topic = "",
    coach_step_board = "",
    
    # Tab 5: Scholarship
    coach_scholarship = "",
    
    # Tab 6: ILP (Milestone Goals)
    coach_mile_goal = "",
    
    # Tab 7: Career
    coach_career = "",
    
    # Tab 8: Summary
    coach_summary = "",
    coach_ilp_final = ""
  )
  
  # Map input fields to coach_rev fields
  
  # Tab 1: Pre-review
  if (!is.null(inputs$discussion_points) && nzchar(trimws(inputs$discussion_points))) {
    result$coach_pre_rev <- inputs$discussion_points
  }
  
  # Tab 2: Wellness
  if (is_intern_intro && !is.null(inputs$resident_background) && nzchar(trimws(inputs$resident_background))) {
    result$coach_intro_back <- inputs$resident_background
  }
  
  if (!is.null(inputs$dealing_with_residency) && nzchar(trimws(inputs$dealing_with_residency))) {
    result$coach_coping <- inputs$dealing_with_residency
  }
  
  if (!is.null(inputs$resident_wellbeing) && nzchar(trimws(inputs$resident_wellbeing))) {
    result$coach_wellness <- inputs$resident_wellbeing
  }
  
  # Tab 3: Evaluations
  if (!is.null(inputs$evaluations_assessment) && nzchar(trimws(inputs$evaluations_assessment))) {
    result$coach_evaluations <- inputs$evaluations_assessment
  }
  
  if (!is.null(inputs$evaluations_comments) && nzchar(trimws(inputs$evaluations_comments))) {
    result$coach_p_d_comments <- inputs$evaluations_comments
  }
  
  # Tab 4: Knowledge
  if (!is.null(inputs$knowledge_topics_comments) && nzchar(trimws(inputs$knowledge_topics_comments))) {
    result$coach_ls_and_topic <- inputs$knowledge_topics_comments
  }
  
  if (!is.null(inputs$board_prep_comments) && nzchar(trimws(inputs$board_prep_comments))) {
    result$coach_step_board <- inputs$board_prep_comments
  }
  
  # Tab 5: Scholarship
  if (!is.null(inputs$scholarship_comments) && nzchar(trimws(inputs$scholarship_comments))) {
    result$coach_scholarship <- inputs$scholarship_comments
  }
  
  # Tab 6: ILP (Milestone Goals)
  if (!is.null(inputs$milestone_goals_comments) && nzchar(trimws(inputs$milestone_goals_comments))) {
    result$coach_mile_goal <- inputs$milestone_goals_comments
  }
  
  # Tab 7: Career
  if (!is.null(inputs$career_comments) && nzchar(trimws(inputs$career_comments))) {
    result$coach_career <- inputs$career_comments
  }
  
  # Tab 8: Summary
  if (!is.null(inputs$discussion_topics_comments) && nzchar(trimws(inputs$discussion_topics_comments))) {
    result$coach_summary <- inputs$discussion_topics_comments
  }
  
  if (!is.null(inputs$summary_comments) && nzchar(trimws(inputs$summary_comments))) {
    result$coach_ilp_final <- inputs$summary_comments
  }
  
  return(result)
}

#' Validate if record exists in REDCap
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Record ID to check
#' @return Logical indicating if record exists
check_record_exists <- function(redcap_url, redcap_token, record_id) {
  
  tryCatch({
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        fields = "record_id",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      return(is.data.frame(data) && nrow(data) > 0)
    }
    
    return(FALSE)
    
  }, error = function(e) {
    message("Error checking record existence: ", e$message)
    return(FALSE)
  })
}

#' Validate coach review inputs for required fields
#'
#' @param mapped_inputs Mapped input values
#' @param tab_order Order of tabs in the app
#' @return List with validation status and any missing fields
#' @export
validate_coach_review_inputs <- function(mapped_inputs, tab_order = NULL) {
  
  # Define required fields for each tab
  required_fields <- list(
    # Tab 1: Pre-review
    pre_review = c("coach_pre_rev"),
    
    # Tab 2: Wellness
    wellness = c("coach_coping", "coach_wellness"),
    
    # Tab 3: Evaluations
    evaluations = c("coach_evaluations", "coach_p_d_comments"),
    
    # Tab 4: Knowledge
    knowledge = c("coach_ls_and_topic", "coach_step_board"),
    
    # Tab 5: Scholarship 
    scholarship = c("coach_scholarship"),
    
    # Tab 6: ILP
    ilp = c("coach_mile_goal"),
    
    # Tab 7: Career
    career = c("coach_career"),
    
    # Tab 8: Summary
    summary = c("coach_ilp_final")
  )
  
  # Check if each required field has a value
  missing_fields <- list()
  for (tab in names(required_fields)) {
    tab_missing <- character(0)
    for (field in required_fields[[tab]]) {
      if (is.null(mapped_inputs[[field]]) || 
          trimws(mapped_inputs[[field]]) == "") {
        tab_missing <- c(tab_missing, field)
      }
    }
    if (length(tab_missing) > 0) {
      missing_fields[[tab]] <- tab_missing
    }
  }
  
  # If no missing fields, validation is successful
  if (length(missing_fields) == 0) {
    return(list(
      valid = TRUE,
      message = "All required fields are completed.",
      missing_tabs = integer(0)
    ))
  }
  
  # Create validation message with missing fields
  message_parts <- character(0)
  missing_tabs <- integer(0)
  
  for (tab in names(missing_fields)) {
    message_parts <- c(message_parts, 
                       paste0("Tab '", tab, "': Missing ", 
                              paste(missing_fields[[tab]], collapse = ", ")))
    
    # Find tab index in tab_order if provided
    if (!is.null(tab_order)) {
      tab_index <- match(tab, tab_order)
      if (!is.na(tab_index)) {
        missing_tabs <- c(missing_tabs, tab_index)
      }
    }
  }
  
  return(list(
    valid = FALSE,
    message = paste("Please complete the following required fields:", 
                    paste(message_parts, collapse = "; ")),
    missing_tabs = missing_tabs
  ))
}

# ============================================================================
# LEGACY COMPATIBILITY FUNCTIONS
# ============================================================================
# These functions maintain compatibility with existing apps while using the new patterns

#' Legacy Submit Coach Review Function
#' 
#' Maintains compatibility with existing coaching apps
#'
#' @param record_id Resident record ID
#' @param resident_name Resident name (for logging)
#' @param instance REDCap instance number (optional, will be calculated if NULL)
#' @param inputs Form input values
#' @param app_data App data containing REDCap configuration
#' @param is_intern_intro Whether this is for an intern in intro period
#' @return List with submission result
#' @export
submit_coach_review <- function(record_id, resident_name, instance = NULL, inputs, app_data, 
                                is_intern_intro = FALSE) {
  
  # Validate record_id
  if (is.null(record_id) || length(record_id) == 0) {
    error_msg <- "ERROR: Cannot submit to REDCap - record_id is NULL or empty"
    message(error_msg)
    return(list(success = FALSE, outcome_message = error_msg))
  }
  
  # Get REDCap configuration
  redcap_url <- app_data$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
  token <- app_data$rdm_token
  
  message("Attempting to submit coach review for resident: ", resident_name)
  message("Using record_id: ", record_id)
  
  # Map inputs to REDCap fields
  mapped_inputs <- map_inputs_to_coach_rev(inputs, is_intern_intro = is_intern_intro)
  
  # If instance not provided, determine from current period and level
  if (is.null(instance)) {
    # You'll need to provide level and period from app_data or inputs
    level <- app_data$resident_level %||% "Intern"  # Default fallback
    period <- app_data$current_period %||% get_current_period()  # Use the new function
    
    message("No instance provided, calculating from level: ", level, " and period: ", period)
    
    instance <- get_redcap_instance(
      level = level,
      period = period,
      review_type = "scheduled"
    )
  }
  
  message("Using instance: ", instance)
  
  # Use the new pattern function
  result <- submit_coach_review_data(
    redcap_url = redcap_url,
    redcap_token = token,
    record_id = record_id,
    coach_data = mapped_inputs,
    period = app_data$current_period %||% get_current_period(),
    level = app_data$resident_level %||% "Intern",
    review_type = "scheduled"
  )
  
  # Convert to legacy format
  if (result$success) {
    return(list(
      success = TRUE,
      outcome_message = result$message,
      instance = result$instance
    ))
  } else {
    return(list(
      success = FALSE,
      outcome_message = result$message
    ))
  }
}

#' Legacy Submit to REDCap Function
#'
#' Generic legacy submission function for backwards compatibility
#'
#' @param url REDCap API URL
#' @param token REDCap API token
#' @param json_data JSON data to submit
#' @param dev_mode Whether to skip actual submission (for development)
#' @return List with submission result
#' @export
submit_to_redcap <- function(url, token, json_data, dev_mode = FALSE) {
  
  # Debug log
  message("Submitting to REDCap API at: ", url)
  message("JSON data (first 200 chars): ", substr(json_data, 1, 200))
  
  # Skip submission in dev mode
  if (dev_mode) {
    message("DEV MODE: Skipping actual REDCap submission")
    return(list(
      success = TRUE,
      outcome_message = "Development mode - submission skipped but would have succeeded"
    ))
  }
  
  tryCatch({
    # Submit to REDCap
    result <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = json_data,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    # Process response
    status_code <- httr::status_code(result)
    response_content <- httr::content(result, "text", encoding = "UTF-8")
    
    message("REDCap API response status: ", status_code)
    message("REDCap API response: ", response_content)
    
    # Check if submission was successful
    if (status_code == 200) {
      # Try to parse the response as JSON to get count
      count_result <- tryCatch({
        jsonlite::fromJSON(response_content)
      }, error = function(e) {
        response_content
      })
      
      return(list(
        success = TRUE,
        outcome_message = paste("Successfully submitted to REDCap. Records updated:", count_result)
      ))
    } else {
      # Special handling for Form Status field error (which is expected and can be ignored)
      if (grepl("Form Status field", response_content)) {
        return(list(
          success = TRUE,
          outcome_message = "Data saved (ignoring form status field warning)"
        ))
      } else {
        return(list(
          success = FALSE,
          outcome_message = paste("REDCap Error:", response_content)
        ))
      }
    }
  }, error = function(e) {
    # Catch any errors in the HTTP request
    error_msg <- paste("Error submitting to REDCap:", e$message)
    message(error_msg)
    return(list(success = FALSE, outcome_message = error_msg))
  })
}

# ============================================================================
# DEBUGGING AND TESTING FUNCTIONS
# ============================================================================

#' Test REDCap Connection
#'
#' Simple function to test if REDCap API is accessible
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @return Logical indicating if connection is successful
#' @export
test_redcap_connection <- function(redcap_url, redcap_token) {
  
  tryCatch({
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "project"
      ),
      encode = "form",
      httr::timeout(10)
    )
    
    if (httr::status_code(response) == 200) {
      message("✅ REDCap connection successful")
      return(TRUE)
    } else {
      message("❌ REDCap connection failed with status: ", httr::status_code(response))
      return(FALSE)
    }
    
  }, error = function(e) {
    message("❌ REDCap connection error: ", e$message)
    return(FALSE)
  })
}

#' Inspect Repeating Instrument Data
#'
#' Debug function to see what instances exist for a record and instrument
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Record ID to inspect
#' @param instrument_name Instrument name to inspect
#' @return Data frame of existing instances
#' @export
inspect_repeating_instrument <- function(redcap_url, redcap_token, record_id, instrument_name) {
  
  tryCatch({
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        forms = instrument_name,
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (is.data.frame(data) && nrow(data) > 0) {
        message("Found ", nrow(data), " instances for record ", record_id, " in instrument ", instrument_name)
        
        # Show instance summary
        if ("redcap_repeat_instance" %in% names(data)) {
          instances <- as.numeric(data$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          if (length(instances) > 0) {
            message("Existing instances: ", paste(sort(instances), collapse = ", "))
          }
        }
        
        return(data)
      } else {
        message("No data found for record ", record_id, " in instrument ", instrument_name)
        return(data.frame())
      }
    } else {
      message("Failed to inspect instrument. Status: ", httr::status_code(response))
      return(data.frame())
    }
    
  }, error = function(e) {
    message("Error inspecting instrument: ", e$message)
    return(data.frame())
  })
}