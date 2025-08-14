#' @title REDCap Submission Pattern Functions
#' @description Functions that handle the two different REDCap submission patterns:
#' OVERWRITE (coaching/CCC reviews) vs ADDITIVE (evaluations/scholarship)
#' @name submission_patterns
NULL

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
    # Use existing gmed function to get the fixed instance number
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
    
    # Convert to data frame for REDCap
    redcap_df <- data.frame(redcap_data, stringsAsFactors = FALSE)
    
    # Submit to REDCap
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        format = "json",
        type = "flat",
        data = jsonlite::toJSON(redcap_df, auto_unbox = TRUE)
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
    
    # Convert to data frame for REDCap
    redcap_df <- data.frame(redcap_data, stringsAsFactors = FALSE)
    
    # Submit to REDCap
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record", 
        format = "json",
        type = "flat",
        data = jsonlite::toJSON(redcap_df, auto_unbox = TRUE)
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
            return(next_instance)
          }
        }
      }
    }
    
    # Default to instance 1 for first evaluation
    return(1)
    
  }, error = function(e) {
    message("Error getting additive instance: ", e$message)
    return(1)
  })
}