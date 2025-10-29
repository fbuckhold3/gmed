#' Validate and Authenticate Resident by Access Code
#'
#' @param access_code Character string of the access code to validate
#' @param residents_df Data frame containing resident data with access_code column
#' @param allow_record_id Logical, whether to also allow record_id for authentication (default: TRUE)
#' @param debug Logical, whether to print debug messages (default: FALSE)
#'
#' @return List with authentication result:
#'   - success: Logical indicating if authentication passed
#'   - resident_info: List with resident data if successful, NULL if failed
#'   - message: Character string with status message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In your Shiny server
#' auth_result <- gmed::authenticate_resident(
#'   access_code = trimws(input$access_code_input),
#'   residents_df = app_data$residents,
#'   debug = TRUE
#' )
#'
#' if (auth_result$success) {
#'   values$resident_info <- auth_result$resident_info
#'   values$current_resident <- auth_result$resident_info$record_id
#' }
#' }
authenticate_resident <- function(access_code, 
                                  residents_df, 
                                  allow_record_id = TRUE,
                                  debug = FALSE) {
  
  # Input validation
  if (is.null(access_code) || !nzchar(trimws(access_code))) {
    return(list(
      success = FALSE,
      resident_info = NULL,
      message = "No access code provided"
    ))
  }
  
  if (is.null(residents_df) || nrow(residents_df) == 0) {
    return(list(
      success = FALSE,
      resident_info = NULL,
      message = "No resident data available"
    ))
  }
  
  # Clean the access code
  access_code_clean <- trimws(access_code)
  
  if (debug) {
    message("=== AUTHENTICATION ATTEMPT ===")
    message("Access code: ", access_code_clean)
    message("Residents data rows: ", nrow(residents_df))
    message("Has access_code column: ", "access_code" %in% names(residents_df))
  }
  
  # Check for demo/test codes
  if (access_code_clean %in% c("demo", "test", "88")) {
    if (debug) message("Demo access granted")
    
    return(list(
      success = TRUE,
      resident_info = list(
        record_id = "88",
        name = "Demo Resident",
        type = "demo"
      ),
      message = "Demo access granted"
    ))
  }
  
  # Try to find matching resident
  tryCatch({
    
    # Build filter condition
    if ("access_code" %in% names(residents_df) && allow_record_id) {
      # Check both access_code and record_id
      matched_resident <- residents_df %>%
        dplyr::filter(
          (!is.na(access_code) & access_code == !!access_code_clean) | 
            record_id == !!access_code_clean
        ) %>%
        dplyr::slice(1)
        
    } else if ("access_code" %in% names(residents_df)) {
      # Check only access_code
      matched_resident <- residents_df %>%
        dplyr::filter(!is.na(access_code) & access_code == !!access_code_clean) %>%
        dplyr::slice(1)
        
    } else if (allow_record_id) {
      # Fallback: check only record_id
      if (debug) {
        message("WARNING: No access_code column found, checking record_id only")
      }
      matched_resident <- residents_df %>%
        dplyr::filter(record_id == !!access_code_clean) %>%
        dplyr::slice(1)
        
    } else {
      return(list(
        success = FALSE,
        resident_info = NULL,
        message = "No access_code column in resident data"
      ))
    }
    
    # Check if match found
    if (nrow(matched_resident) == 0) {
      if (debug) message("No matching resident found")
      
      return(list(
        success = FALSE,
        resident_info = NULL,
        message = "Invalid access code"
      ))
    }
    
    # Success!
    resident_info <- as.list(matched_resident)
    
    if (debug) {
      message("Authentication successful!")
      message("Record ID: ", resident_info$record_id)
      if (!is.null(resident_info$name)) {
        message("Name: ", resident_info$name)
      }
    }
    
    return(list(
      success = TRUE,
      resident_info = resident_info,
      message = "Authentication successful"
    ))
    
  }, error = function(e) {
    if (debug) {
      message("ERROR during authentication: ", e$message)
    }
    
    return(list(
      success = FALSE,
      resident_info = NULL,
      message = paste("Authentication error:", e$message)
    ))
  })
}


#' Quick Validation of Access Code
#'
#' Simpler function that just checks if an access code exists
#'
#' @param access_code Character string to validate
#' @param residents_df Data frame with resident data
#' @param allow_record_id Logical, whether to also check record_id
#'
#' @return Logical - TRUE if valid, FALSE if not
#' @export
validate_access_code <- function(access_code, 
                                 residents_df, 
                                 allow_record_id = TRUE) {
  
  if (is.null(access_code) || !nzchar(trimws(access_code))) {
    return(FALSE)
  }
  
  if (is.null(residents_df) || !"access_code" %in% names(residents_df)) {
    return(FALSE)
  }
  
  access_code_clean <- trimws(access_code)
  
  # Check demo codes
  if (access_code_clean %in% c("demo", "test", "88")) {
    return(TRUE)
  }
  
  # Check if exists in data
  if (allow_record_id) {
    return(access_code_clean %in% residents_df$access_code | 
           access_code_clean %in% residents_df$record_id)
  } else {
    return(access_code_clean %in% residents_df$access_code)
  }
}