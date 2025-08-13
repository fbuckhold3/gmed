#' @title RDM 2.0 Data Management Functions
#' @description Core functions for loading and organizing RDM 2.0 data structure
#' @name rdm2_data_functions
NULL

#' Initialize Application Configuration
#'
#' Sets up configuration for REDCap API access and application settings.
#' Handles both environment variables and config file approaches.
#'
#' @return List containing configuration parameters
#' @export
#'
#' @examples
#' \dontrun{
#' config <- initialize_app_config()
#' }
initialize_app_config <- function() {
  
  # Try environment variables first (production)
  rdm_token <- Sys.getenv("RDM_TOKEN", unset = NA)
  fac_token <- Sys.getenv("FAC_TOKEN", unset = NA)
  access_code <- Sys.getenv("ACCESS_CODE", unset = NA)
  
  # If environment variables not available, try config file
  if (is.na(rdm_token) || is.na(fac_token)) {
    tryCatch({
      if (requireNamespace("config", quietly = TRUE)) {
        config_data <- config::get()
        rdm_token <- config_data$rdm_token %||% rdm_token
        fac_token <- config_data$fac_token %||% fac_token
        access_code <- config_data$access_code %||% access_code
      }
    }, error = function(e) {
      message("Config file not found, using environment variables only")
    })
  }
  
  # Default values
  config <- list(
    url = "https://redcapsurvey.slu.edu/api/",
    rdm_token = rdm_token,
    fac_token = fac_token,
    access_code = access_code
  )
  
  # Validate required tokens
  if (is.na(config$rdm_token)) {
    stop("RDM_TOKEN is required but not found in environment or config file")
  }
  
  if (is.na(config$fac_token)) {
    stop("FAC_TOKEN is required but not found in environment or config file")
  }
  
  return(config)
}

#' Pull All REDCap Data for RDM 2.0
#'
#' Retrieves all necessary data from REDCap using a single API call
#' for the unified RDM 2.0 database structure. Uses CSV format with
#' labels for better data processing.
#'
#' @param token REDCap API token
#' @param url REDCap API URL
#'
#' @return Data frame with all REDCap records
#' @export
#'
#' @examples
#' \dontrun{
#' config <- initialize_app_config()
#' all_data <- pull_all_redcap_data(config$rdm_token, config$url)
#' }
pull_all_redcap_data <- function(token, url) {
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for REDCap API calls")
  }
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CSV processing")
  }
  
  message("Making REDCap API call...")
  
  # Prepare form data for API call (matches your working version exactly)
  form_data <- list(
    token = token,
    content = "record",
    action = "export", 
    format = "csv",
    type = "flat",
    rawOrLabel = "label",
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "true",
    exportSurveyFields = "true",
    exportDataAccessGroups = "false",
    returnFormat = "csv",
    csvDelimiter = ""
  )
  
  # Make API call with explicit httr namespace (matches your working version)
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  
  response <- httr::POST(
    url = url,
    body = form_data,
    encode = "form",
    httr::timeout(60)  # 60 second timeout
  )
  
  # Check response status
  if (httr::http_status(response)$category != "Success") {
    stop("REDCap API call failed. Status: ", httr::status_code(response))
  }
  
  # Parse CSV response
  csv_content <- httr::content(response, as = "text", encoding = "UTF-8")
  
  data <- tryCatch({
    readr::read_csv(csv_content, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  }, error = function(e) {
    stop("Failed to parse CSV content from REDCap: ", e$message)
  })
  
  # Ensure record_id is character
  if ("record_id" %in% names(data)) {
    data$record_id <- as.character(data$record_id)
  }
  
  message("Successfully pulled ", nrow(data), " rows and ", ncol(data), " columns")
  
  return(data)
}

#' Organize REDCap Data for RDM 2.0 Structure
#'
#' Takes raw REDCap data and organizes it into structured components
#' for easy access in Shiny applications. Includes filtering for archived
#' residents and proper separation of repeating instruments.
#'
#' @param all_data Raw data frame from REDCap API
#'
#' @return List containing organized data components
#' @export
#'
#' @examples
#' \dontrun{
#' config <- initialize_app_config()
#' raw_data <- pull_all_redcap_data(config$rdm_token, config$url)
#' organized_data <- organize_redcap_data(raw_data)
#' }
organize_redcap_data <- function(all_data) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data organization")
  }
  
  if (nrow(all_data) == 0) {
    warning("No data to organize")
    return(list(resident_data = data.frame()))
  }
  
  message("Organizing data by instrument type...")
  
  # Show what repeat instruments we have
  if ("redcap_repeat_instrument" %in% names(all_data)) {
    repeat_counts <- table(all_data$redcap_repeat_instrument, useNA = "ifany")
    message("Found repeat instruments:")
    print(repeat_counts)
  }
  
  # Keep ALL data together, just filter out archived residents
  all_resident_data <- all_data %>%
    # Filter out archived residents if column exists
    {if ("res_archive" %in% names(.)) {
      dplyr::filter(., is.na(res_archive) | res_archive != "Yes")
    } else .}
  
  message("Found ", length(unique(all_resident_data$record_id)), " active residents")
  message("Total rows of data: ", nrow(all_resident_data))
  
  # Create separate views for specific analyses if needed, but keep raw data intact
  milestone_program <- all_resident_data %>%
    dplyr::filter(redcap_repeat_instrument == "Milestone Entry")
  
  milestone_self <- all_resident_data %>%
    dplyr::filter(redcap_repeat_instrument == "Milestone Selfevaluation C33c")
  
  assessment_data <- all_resident_data %>%
    dplyr::filter(redcap_repeat_instrument == "Assessment")
  
  message("Data breakdown:")
  message("  Program milestones: ", nrow(milestone_program), " records")
  message("  Self assessments: ", nrow(milestone_self), " records") 
  message("  Assessment data: ", nrow(assessment_data), " records")
  
  # Add a quick data validation message:
  message("Data validation:")
  message("  Non-repeating rows: ", nrow(all_resident_data %>% dplyr::filter(is.na(redcap_repeat_instrument))))
  message("  Assessment rows: ", nrow(assessment_data))
  message("  Total instrument rows: ", nrow(all_resident_data %>% dplyr::filter(!is.na(redcap_repeat_instrument))))
  
  # Return organized list with full data as primary (matches your working version)
  return(list(
    resident_data = all_resident_data,  # ALL data, including repeating instruments
    milestone_program = milestone_program,  # Filtered views for convenience
    milestone_self = milestone_self,
    assessment_data = assessment_data,
    raw_data = all_data  # Original unfiltered data
  ))
}

#' Get Evaluation Dictionary from REDCap
#'
#' Retrieves the data dictionary for field metadata and validation.
#'
#' @param token REDCap API token
#' @param url REDCap API URL
#'
#' @return Data frame containing the data dictionary
#' @export
#'
#' @examples
#' \dontrun{
#' config <- initialize_app_config()
#' dict <- get_evaluation_dictionary(config$rdm_token, config$url)
#' }
get_evaluation_dictionary <- function(token, url) {
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for REDCap API calls")
  }
  
  response <- tryCatch({
    httr::POST(
      url = url,
      body = list(
        token = token,
        content = "metadata",
        format = "json",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(60)
    )
  }, error = function(e) {
    stop("Failed to retrieve data dictionary: ", e$message)
  })
  
  if (httr::status_code(response) != 200) {
    stop("Dictionary request failed with status: ", httr::status_code(response))
  }
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  dict <- jsonlite::fromJSON(content, flatten = TRUE)
  
  # Standardize column names
  if ("field_name" %in% names(dict)) {
    dict$Variable <- dict$field_name
  }
  if ("form_name" %in% names(dict)) {
    dict$Form <- dict$form_name
  }
  
  message("Retrieved data dictionary with ", nrow(dict), " fields")
  return(dict)
}

#' Find Record ID for a Resident
#'
#' Looks up the record_id for a given resident name in the organized data.
#'
#' @param organized_data List of organized REDCap data
#' @param resident_name Character string of resident name
#'
#' @return Character string of record_id or NULL if not found
#' @export
#'
#' @examples
#' \dontrun{
#' record_id <- find_record_id(organized_data, "John Doe")
#' }
find_record_id <- function(organized_data, resident_name) {
  
  if (is.null(organized_data$resident_data) || nrow(organized_data$resident_data) == 0) {
    warning("No resident data available")
    return(NULL)
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required")
  }
  
  # Look for exact match first
  record <- organized_data$resident_data %>%
    dplyr::filter(name == resident_name) %>%
    dplyr::slice(1)  # Take first match if multiple
  
  if (nrow(record) == 0) {
    # Try case-insensitive match
    record <- organized_data$resident_data %>%
      dplyr::filter(tolower(name) == tolower(resident_name)) %>%
      dplyr::slice(1)
  }
  
  if (nrow(record) == 0) {
    warning("Resident '", resident_name, "' not found in data")
    return(NULL)
  }
  
  return(as.character(record$record_id))
}

#' Generate Plus-Delta Data for RDM 2.0
#'
#' Filters and processes assessment data to extract plus-delta evaluations for a specific resident.
#' Maps assessment types to readable rotation names. This is the tested version from the coach app.
#'
#' @param data A data frame containing ALL RDM 2.0 data (including repeating instruments)
#' @param resident_record_id Character string specifying the resident's record_id
#'
#' @return A data frame containing Date, Rotation, Plus, Delta, Faculty, and Specialty
#' @export
generate_plus_delta_rdm2 <- function(data, resident_record_id) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    message("No data provided to generate_plus_delta_rdm2")
    return(data.frame(
      Date = character(0),
      Rotation = character(0), 
      Plus = character(0),
      Delta = character(0),
      Faculty = character(0),
      Specialty = character(0)
    ))
  }
  
  if (is.null(resident_record_id) || is.na(resident_record_id)) {
    message("No valid record_id provided to generate_plus_delta_rdm2")
    return(data.frame(
      Date = character(0),
      Rotation = character(0),
      Plus = character(0), 
      Delta = character(0),
      Faculty = character(0),
      Specialty = character(0)
    ))
  }
  
  message("Generating plus/delta data for record_id: ", resident_record_id)
  
  # Filter for the specific resident AND Assessment repeating instrument
  assessment_data <- data %>%
    dplyr::filter(
      record_id == !!resident_record_id,
      redcap_repeat_instrument == "Assessment"  # Only assessment rows
    )
  
  message("Found ", nrow(assessment_data), " assessment rows for resident")
  
  # Filter for rows that have plus/delta content
  plus_delta_data <- assessment_data %>%
    dplyr::filter(
      !is.na(ass_date) | 
        (!is.na(ass_plus) & ass_plus != "") | 
        (!is.na(ass_delta) & ass_delta != "")
    )
  
  message("Found ", nrow(plus_delta_data), " rows with assessment content")
  
  if (nrow(plus_delta_data) == 0) {
    message("No plus/delta assessment data found for this resident")
    return(data.frame(
      Date = character(0),
      Rotation = character(0),
      Plus = character(0),
      Delta = character(0), 
      Faculty = character(0),
      Specialty = character(0)
    ))
  }
  
  # Enhanced rotation mapping function based on field patterns
  map_rotation_type <- function(row) {
    # Get all column names for this row
    col_names <- names(row)
    
    # Helper function to check if any fields matching a pattern have data
    has_data_in_pattern <- function(pattern) {
      matching_fields <- col_names[grepl(pattern, col_names)]
      any(sapply(matching_fields, function(f) !is.na(row[[f]]) && row[[f]] != ""))
    }
    
    # Check specific observation types FIRST (most specific)
    if (has_data_in_pattern("^ass_obs_cdm")) return("Observation: Clinical Decision Making")
    if (has_data_in_pattern("^ass_obs_acp")) return("Observation: Advance Care Planning")
    if (has_data_in_pattern("^ass_educat")) return("Observation: Education")
    if (has_data_in_pattern("^ass_obs_pe")) return("Observation: Physical Exam")
    if (has_data_in_pattern("^ass_obs_pres")) return("Observation: Presentation")
    if (has_data_in_pattern("^ass_obs_writehp")) return("Observation: Written H&P")
    if (has_data_in_pattern("^ass_obs_daily")) return("Observation: Daily Note")
    if (has_data_in_pattern("^ass_obs_dc")) return("Observation: Discharge Summary")
    if (has_data_in_pattern("^ass_obs_meet")) return("Observation: Family Meeting")
    if (has_data_in_pattern("^ass_obs_senior")) return("Observation: Supervising Resident")
    if (has_data_in_pattern("^ass_obs_proc")) return("Observation: Procedure")
    if (has_data_in_pattern("^ass_obs_mdr")) return("Observation: MDR")
    if (has_data_in_pattern("^ass_obs_emer")) return("Observation: Emergency Situation")
    
    # Check broader rotation types (less specific)
    if (has_data_in_pattern("^ass_cc")) return("Continuity Clinic")
    if (has_data_in_pattern("^ass_cons")) return("Consult Rotation")
    if (has_data_in_pattern("^ass_int_ip")) return("Intern Inpatient")
    if (has_data_in_pattern("^ass_res_ip")) return("Resident Inpatient")
    if (has_data_in_pattern("^ass_bridge")) return("Bridge Clinic")
    if (has_data_in_pattern("^ass_day")) return("Single Day Clinic")
    
    # Default if no pattern matches
    return("General Assessment")
  }
  
  # Process and format the data
  result <- plus_delta_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Date = ifelse(!is.na(ass_date), as.character(as.Date(ass_date)), "Not specified"),
      Rotation = map_rotation_type(dplyr::pick(dplyr::everything())),
      Plus = ifelse(!is.na(ass_plus) & ass_plus != "", ass_plus, "Not provided"),
      Delta = ifelse(!is.na(ass_delta) & ass_delta != "", ass_delta, "Not provided"),
      Faculty = ifelse(!is.na(ass_faculty) & ass_faculty != "", ass_faculty, "Not specified"),
      Specialty = ifelse(!is.na(ass_specialty) & ass_specialty != "", ass_specialty, "Not specified")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Date, Rotation, Plus, Delta, Faculty, Specialty) %>%
    dplyr::arrange(dplyr::desc(Date))
  
  message("Generated plus/delta table with ", nrow(result), " rows")
  return(result)
}

#' Helper operator for NULL coalescing
#'
#' @param x First value
#' @param y Second value to use if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}