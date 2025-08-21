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
  
  # Filter out archived residents with improved logic
  all_resident_data <- all_data %>%
    {if ("res_archive" %in% names(.)) {
      # Debug: show what archive values exist
      archive_values <- unique(.$res_archive)
      message("DEBUG: res_archive values found: ", paste(archive_values, collapse = ", "))
      
      # Count before filtering
      before_count <- nrow(.)
      
      # Filter out archived residents - handle multiple possible values
      filtered_data <- dplyr::filter(., 
                                     is.na(res_archive) | 
                                       res_archive == "" | 
                                       !(res_archive %in% c("Yes", "Y", "yes", "YES", "y", "1", 1, "True", "TRUE", "true"))
      )
      
      # Count after filtering
      after_count <- nrow(filtered_data)
      filtered_count <- before_count - after_count
      
      if (filtered_count > 0) {
        message("SUCCESS: Filtered out ", filtered_count, " archived residents (", before_count, " -> ", after_count, " rows)")
      } else {
        message("INFO: No archived residents found to filter")
      }
      
      return(filtered_data)
    } else {
      message("INFO: No res_archive column found - keeping all residents")
      return(.)
    }}
  
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

#' Filter Archived Residents (Standalone Helper Function)
#'
#' Filters out archived residents from a dataset, handling multiple possible
#' archive indicator values (Yes, Y, 1, True, etc.)
#'
#' @param data Data frame with potential res_archive column
#' @param archive_column Name of the archive column (default: "res_archive")
#' @param verbose Whether to print filtering messages (default: TRUE)
#'
#' @return Data frame with archived residents removed
#' @export
#'
#' @examples
#' \dontrun{
#' clean_data <- filter_archived_residents(raw_data)
#' }
filter_archived_residents <- function(data, archive_column = "res_archive", verbose = TRUE) {
  
  if (!archive_column %in% names(data)) {
    if (verbose) message("No ", archive_column, " column found - returning data unchanged")
    return(data)
  }
  
  before_count <- nrow(data)
  
  # Show what archive values exist
  if (verbose) {
    archive_values <- unique(data[[archive_column]])
    message("DEBUG: ", archive_column, " values found: ", paste(archive_values, collapse = ", "))
  }
  
  # Define all possible "archived" values
  archived_indicators <- c("Yes", "Y", "yes", "YES", "y", "1", 1, "True", "TRUE", "true")
  
  # Filter out archived residents
  data <- data %>%
    dplyr::filter(
      is.na(.data[[archive_column]]) | 
        .data[[archive_column]] == "" | 
        !.data[[archive_column]] %in% archived_indicators
    )
  
  after_count <- nrow(data)
  filtered_count <- before_count - after_count
  
  if (verbose) {
    if (filtered_count > 0) {
      message("SUCCESS: Filtered out ", filtered_count, " archived residents (", before_count, " -> ", after_count, " rows)")
    } else {
      message("INFO: No archived residents found to filter")
    }
  }
  
  return(data)
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

#' Get Record ID from Resident Name
#' 
#' Looks up resident record_id from name in REDCap data
#' @param resident_name Name of resident
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @return Character record_id or NULL if not found
#' @export
get_record_id_from_name <- function(resident_name, redcap_url, redcap_token) {
  
  tryCatch({
    # Query REDCap for resident data
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat", 
        fields = "record_id,name",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "text", encoding = "UTF-8")
      data <- jsonlite::fromJSON(content)
      
      if (is.data.frame(data) && nrow(data) > 0) {
        # Look for exact name match
        matching_record <- data[data$name == resident_name, ]
        
        if (nrow(matching_record) > 0) {
          return(as.character(matching_record$record_id[1]))
        }
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    message("Error looking up record ID: ", e$message)
    return(NULL)
  })
}

#' Load Application Data for RDM 2.0
#'
#' Comprehensive data loading function that retrieves and organizes all necessary
#' data from REDCap for IMSLU applications using the unified RDM 2.0 database structure.
#' This function handles connection testing, data dictionary loading, resident filtering,
#' level calculations, and data organization with robust error handling.
#'
#' @param app_config List containing application configuration with rdm_token and other settings.
#'   If NULL, will attempt to load from global app_config or environment variables.
#' @param redcap_url Character string of REDCap API URL. Defaults to 
#'   "https://redcapsurvey.slu.edu/api/"
#' @param filter_archived Logical indicating whether to filter out archived residents.
#'   Defaults to TRUE.
#' @param calculate_levels Logical indicating whether to calculate resident training levels.
#'   Defaults to TRUE.
#' @param timeout_seconds Numeric value for API call timeout in seconds. Defaults to 90.
#'
#' @return Named list containing organized application data:
#' \describe{
#'   \item{url}{REDCap API URL used}
#'   \item{rdm_token}{REDCap token used (first 8 characters for security)}
#'   \item{resident_data}{Data frame of active residents with calculated levels}
#'   \item{s_eval}{Data frame of self-evaluation records}
#'   \item{rdm_dict}{Data frame containing REDCap data dictionary}
#'   \item{milestone_program}{Data frame of program milestone assessments}
#'   \item{milestone_self}{Data frame of self-assessment milestones}
#'   \item{assessment_data}{Data frame of clinical assessment records}
#'   \item{raw_data}{Data frame containing all raw REDCap data}
#' }
#'
#' @details
#' This function performs the following operations in sequence:
#' \enumerate{
#'   \item Validates REDCap token availability
#'   \item Tests REDCap connection
#'   \item Loads data dictionary using gmed functions or direct API calls
#'   \item Retrieves all RDM 2.0 data
#'   \item Filters to active (non-archived) residents
#'   \item Calculates resident training levels (Intern, PGY2, PGY3)
#'   \item Organizes data into structured components by instrument type
#'   \item Provides debugging information for critical data elements
#' }
#'
#' The function includes fallback mechanisms for when gmed functions are not
#' available, ensuring compatibility across different deployment environments.
#'
#' @section Error Handling:
#' The function includes comprehensive error handling:
#' \itemize{
#'   \item Stops execution if REDCap token is missing
#'   \item Validates REDCap connectivity before data loading
#'   \item Provides fallback calculations if gmed functions fail
#'   \item Includes manual data organization if gmed organize functions fail
#'   \item Logs detailed progress and debugging information
#' }
#'
#' @section Performance:
#' Optimized for Posit Connect deployment with extended timeouts for large
#' datasets and efficient data processing using dplyr operations.
#'
#' @examples
#' \dontrun{
#' # Basic usage with global configuration
#' app_data <- load_app_data()
#' 
#' # Custom configuration
#' config <- list(rdm_token = "your_token_here")
#' app_data <- load_app_data(app_config = config)
#' 
#' # Access organized data components
#' residents <- app_data$resident_data
#' evaluations <- app_data$s_eval
#' milestones <- app_data$milestone_program
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select case_when
#' @importFrom httr POST status_code content timeout
#' @importFrom jsonlite fromJSON
#'
#' @seealso 
#' \code{\link{pull_all_redcap_data}}, \code{\link{organize_redcap_data}}, 
#' \code{\link{calculate_resident_level}}, \code{\link{get_evaluation_dictionary}}
load_app_data <- function(app_config = NULL, 
                          redcap_url = "https://redcapsurvey.slu.edu/api/",
                          filter_archived = TRUE,
                          calculate_levels = TRUE,
                          timeout_seconds = 90) {
  
  message("=== LOADING APPLICATION DATA ===")
  
  # Use provided config or try to get from environment
  if (is.null(app_config)) {
    if (exists("app_config", envir = .GlobalEnv)) {
      app_config <- get("app_config", envir = .GlobalEnv)
    } else {
      app_config <- list(
        rdm_token = Sys.getenv("RDM_TOKEN"),
        access_code = Sys.getenv("ACCESS_CODE")
      )
    }
  }
  
  # Validate token
  if (is.null(app_config$rdm_token) || app_config$rdm_token == "") {
    stop("❌ RDM_TOKEN not found. Please set the RDM_TOKEN environment variable.")
  }
  
  # Test REDCap connection
  if (!test_redcap_connection(redcap_url, app_config$rdm_token)) {
    stop("❌ Cannot connect to REDCap. Check your RDM_TOKEN and network connection.")
  }
  
  # Load data dictionary
  message("Loading REDCap data dictionary...")
  rdm_dict <- tryCatch({
    if (exists("get_evaluation_dictionary", where = "package:gmed")) {
      message("Using gmed::get_evaluation_dictionary")
      gmed::get_evaluation_dictionary(app_config$rdm_token, redcap_url)
    } else {
      message("Using direct API call for data dictionary")
      response <- httr::POST(
        redcap_url,
        body = list(
          token = app_config$rdm_token,
          content = "metadata",
          format = "json",
          returnFormat = "json"
        ),
        encode = "form",
        httr::timeout(45)  # Longer timeout for production
      )
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        dict <- jsonlite::fromJSON(content, flatten = TRUE)
        message("✅ Data dictionary loaded: ", nrow(dict), " fields")
        dict
      } else {
        stop("❌ Data dictionary API call failed with status: ", httr::status_code(response))
      }
    }
  }, error = function(e) {
    message("❌ Data dictionary loading failed: ", e$message)
    stop("Cannot load data dictionary: ", e$message)
  })
  
  # Load all RDM data
  message("Loading all RDM 2.0 data...")
  all_rdm_data <- tryCatch({
    if (exists("pull_all_redcap_data", where = "package:gmed")) {
      message("Using gmed::pull_all_redcap_data")
      gmed::pull_all_redcap_data(app_config$rdm_token, redcap_url)
    } else {
      message("Using direct REDCap API call")
      response <- httr::POST(
        redcap_url,
        body = list(
          token = app_config$rdm_token,
          content = "record",
          action = "export", 
          format = "json",
          type = "flat",
          returnFormat = "json"
        ),
        encode = "form",
        httr::timeout(timeout_seconds)  # Extended timeout for large datasets
      )
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content)
        message("✅ RDM data loaded: ", nrow(data), " total rows")
        data
      } else {
        stop("❌ RDM data API call failed with status: ", httr::status_code(response))
      }
    }
  }, error = function(e) {
    message("❌ RDM data loading failed: ", e$message)
    stop("Cannot load RDM data: ", e$message)
  })
  
  # Process resident data
  message("=== PROCESSING RESIDENT DATA ===")
  
  # Get base resident data (main form only, no repeat instruments)
  if ("redcap_repeat_instrument" %in% names(all_rdm_data)) {
    resident_data <- all_rdm_data %>%
      dplyr::filter(is.na(redcap_repeat_instrument) | redcap_repeat_instrument == "")
    message("Filtered to ", nrow(resident_data), " base resident records")
  } else {
    resident_data <- all_rdm_data
    message("No repeat instrument column found, using all ", nrow(resident_data), " records")
  }
  
  # Filter out archived residents if requested
  if (filter_archived && "res_archive" %in% names(resident_data)) {
    before_count <- nrow(resident_data)
    resident_data <- resident_data %>%
      dplyr::filter(is.na(res_archive) | (!res_archive %in% c("Yes", "1")))
    message("Archive filter: ", before_count, " -> ", nrow(resident_data), " active residents")
  }
  
  # Calculate resident levels if requested
  if (calculate_levels) {
    message("Calculating resident levels...")
    resident_data <- tryCatch({
      gmed::calculate_resident_level(resident_data)
    }, error = function(e) {
      message("⚠️ gmed level calculation failed, using fallback: ", e$message)
      
      # Manual fallback calculation
      current_date <- Sys.Date()
      current_academic_year <- ifelse(
        format(current_date, "%m-%d") >= "07-01",
        as.numeric(format(current_date, "%Y")),
        as.numeric(format(current_date, "%Y")) - 1
      )
      
      resident_data %>%
        dplyr::mutate(
          grad_yr_numeric = suppressWarnings(as.numeric(grad_yr)),
          Level = dplyr::case_when(
            type == "Preliminary" ~ "Intern",
            type == "Categorical" & grad_yr_numeric == current_academic_year + 3 ~ "Intern",
            type == "Categorical" & grad_yr_numeric == current_academic_year + 2 ~ "PGY2",
            type == "Categorical" & grad_yr_numeric == current_academic_year + 1 ~ "PGY3",
            TRUE ~ "Unknown"
          )
        ) %>%
        dplyr::select(-grad_yr_numeric)
    })
    
    # Log level distribution
    if ("Level" %in% names(resident_data)) {
      level_counts <- table(resident_data$Level, useNA = "always")
      message("Level distribution: ", paste(names(level_counts), "=", level_counts, collapse = ", "))
    }
  }
  
  # Organize other data components
  message("Organizing additional data components...")
  
  organized_data <- tryCatch({
    if (exists("organize_redcap_data", where = "package:gmed")) {
      # Try using gmed function first
      result <- gmed::organize_redcap_data(all_rdm_data)
      # Override with our filtered resident data
      result$resident_data <- resident_data
      
      # CRITICAL FIX: Manually extract s_eval if it's missing
      if (is.null(result$s_eval) || nrow(result$s_eval) == 0) {
        message("⚠️ s_eval missing from gmed organization, extracting manually...")
        result$s_eval <- all_rdm_data %>%
          dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "s_eval")
        message("✅ Manually extracted ", nrow(result$s_eval), " s_eval records")
      }
      
      result
    } else {
      # Manual organization
      message("Using manual data organization...")
      
      list(
        resident_data = resident_data,
        s_eval = all_rdm_data %>%
          dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "S Eval"),
        milestone_program = all_rdm_data %>%
          dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_entry"),
        milestone_self = all_rdm_data %>%
          dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_selfevaluation_c33c"),
        assessment_data = all_rdm_data %>%
          dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "assessment"),
        raw_data = all_rdm_data
      )
    }
  }, error = function(e) {
    message("⚠️ organize_redcap_data issues: ", e$message)
    
    # Fallback manual organization
    message("Using fallback manual organization...")
    
    list(
      resident_data = resident_data,
      s_eval = all_rdm_data %>%
        dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "s_eval"),
      milestone_program = all_rdm_data %>%
        dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_entry"),
      milestone_self = all_rdm_data %>%
        dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_selfevaluation_c33c"),
      assessment_data = all_rdm_data %>%
        dplyr::filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "assessment"),
      raw_data = all_rdm_data
    )
  })
  
  # Add debugging for s_eval after organization
  message("=== S_EVAL DEBUG ===")
  if (!is.null(organized_data$s_eval)) {
    message("✅ s_eval found with ", nrow(organized_data$s_eval), " records")
    
    # Check for Claire Boehm specifically (example debugging)
    claire_data <- organized_data$s_eval %>%
      dplyr::filter(name == "Claire Boehm")
    
    if (nrow(claire_data) > 0) {
      message("✅ Found s_eval data for Claire Boehm")
      
      # Check specific fields
      goal_fields <- c("s_e_ume_goal1", "s_e_ume_goal2", "s_e_ume_goal3")
      for (field in goal_fields) {
        if (field %in% names(claire_data)) {
          value <- claire_data[[field]][1]
          message("  ", field, ": ", ifelse(is.na(value), "NA", as.character(value)))
        }
      }
    } else {
      message("❌ No s_eval data found for Claire Boehm")
    }
  } else {
    message("❌ s_eval is NULL in organized data")
  }
  message("===================")
  
  message("✅ Data loading completed successfully")
  message("✅ Loaded ", nrow(organized_data$resident_data), " resident records")
  
  # Return organized data with metadata
  return(list(
    url = redcap_url,
    rdm_token = substr(app_config$rdm_token, 1, 8),  # Only first 8 chars for security
    resident_data = organized_data$resident_data,
    s_eval = organized_data$s_eval,
    rdm_dict = rdm_dict,
    milestone_program = organized_data$milestone_program,
    milestone_self = organized_data$milestone_self,
    assessment_data = organized_data$assessment_data,
    raw_data = organized_data$raw_data
  ))
}
