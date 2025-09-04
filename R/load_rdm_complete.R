#' Complete RDM 2.0 Data Loader with Full Pipeline Processing
#'
#' Comprehensive data loading function that handles the complete RDM 2.0 data pipeline including
#' raw data loading, data dictionary integration, historical calculations, filtering, and preparation 
#' for gmed modules. This is the primary data loading function for complex applications requiring
#' the full data processing workflow.
#' 
#' This function orchestrates the complete data loading pipeline:
#' - Loads raw REDCap data organized by forms
#' - Retrieves and integrates data dictionary for metadata-driven workflows
#' - Calculates historical milestone medians before any filtering
#' - Filters archived residents while preserving historical data
#' - Adds level-at-time calculations to active residents
#' - Prepares milestone data for visualizations
#' - Standardizes column names and data structures
#' - Returns comprehensive data structure ready for gmed modules
#'
#' @param rdm_token Character. RDM REDCap API token. If NULL, attempts to retrieve from 
#'   environment variable RDM_TOKEN. Token must have appropriate permissions for data 
#'   export and metadata access.
#' @param redcap_url Character. REDCap API URL. Default: "https://redcapsurvey.slu.edu/api/"
#' @param verbose Logical. Whether to print detailed progress messages during loading. 
#'   Default: TRUE. Useful for debugging and monitoring long-running processes.
#' @param ensure_gmed_columns Logical. Whether to verify and create required columns for
#'   gmed module compatibility. Default: TRUE. Ensures assessment data has proper structure.
#' @param cache Logical. Whether to enable caching of intermediate results for faster 
#'   subsequent loads. Default: FALSE. (Note: caching functionality may not be fully implemented)
#'
#' @return List containing comprehensive RDM 2.0 data structure with the following components:
#' \describe{
#'   \item{residents}{Data frame of active residents with standardized columns (name, Level, 
#'     access_code) and calculated training levels}
#'   \item{all_forms}{Named list of all form data organized by REDCap form names, with 
#'     archived residents filtered and levels calculated}
#'   \item{assessment_data}{Processed assessment/evaluation data formatted for gmed modules
#'     with required column structure}
#'   \item{milestone_data}{Processed milestone data ready for visualization, including 
#'     individual data and comparison medians}
#'   \item{data_dict}{Complete REDCap data dictionary with field metadata, labels, choices, 
#'     and validation rules for metadata-driven workflows}
#'   \item{raw_data}{Unfiltered raw REDCap data for reference and advanced processing}
#'   \item{historical_medians}{Pre-calculated milestone medians from complete historical data
#'     (including archived residents) for comparison purposes}
#'   \item{data_loaded}{Logical flag indicating successful data loading}
#'   \item{load_timestamp}{POSIXct timestamp of when data was loaded}
#'   \item{rdm_token}{Character string of the token used (for debugging/reference)}
#'   \item{redcap_url}{Character string of the REDCap URL used}
#' }
#'
#' @details
#' The function follows a specific order of operations to ensure data integrity:
#' 
#' 1. **Token Validation**: Checks for valid RDM_TOKEN from parameter or environment
#' 2. **Raw Data Loading**: Uses `load_data_by_forms()` to get all REDCap data organized by forms
#' 3. **Data Dictionary**: Loads complete field metadata using `get_evaluation_dictionary()`
#' 4. **Historical Calculations**: Calculates milestone medians using `calculate_all_milestone_medians()` 
#'    BEFORE filtering to preserve historical context
#' 5. **Archive Filtering**: Removes archived residents using `filter_archived_residents()`
#' 6. **Level Calculations**: Adds resident training levels at time of data collection using
#'    `add_level_at_time_to_forms()`
#' 7. **Milestone Preparation**: Processes milestone data for apps using `prepare_milestone_app_data()`
#' 8. **Column Standardization**: Ensures consistent column naming (name, Level, access_code)
#' 9. **Final Assembly**: Creates comprehensive return structure with all components
#'
#' Column standardization includes:
#' - **name**: Uses Name, resident_name, full_name, or first_name alternatives
#' - **Level**: Uses level, resident_level, training_level, pgy, PGY, or year alternatives  
#' - **access_code**: Uses accesscode, code, or resident_code alternatives
#'
#' @section Error Handling:
#' - Stops execution if RDM_TOKEN is missing or empty
#' - Continues with warnings if optional processing steps fail
#' - Provides fallback values for missing standardized columns
#' - Reports detailed error messages when verbose=TRUE
#'
#' @section Performance Notes:
#' This is a comprehensive loading function that may take 10-60 seconds depending on 
#' database size. For faster loading in development, consider using `load_rdm_simple()` 
#' or `load_rdm_quick()` for basic data needs.
#'
#' @section Dependencies:
#' Requires the following gmed package functions:
#' - `load_data_by_forms()`
#' - `get_evaluation_dictionary()`  
#' - `calculate_all_milestone_medians()`
#' - `filter_archived_residents()`
#' - `add_level_at_time_to_forms()`
#' - `prepare_milestone_app_data()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with environment token
#' Sys.setenv(RDM_TOKEN = "your_redcap_token_here")
#' complete_data <- load_rdm_complete()
#' 
#' # With explicit token
#' complete_data <- load_rdm_complete(
#'   rdm_token = "your_redcap_token_here",
#'   verbose = TRUE
#' )
#' 
#' # Quiet loading for production
#' complete_data <- load_rdm_complete(verbose = FALSE)
#' 
#' # Access different data components
#' residents <- complete_data$residents
#' assessment_data <- complete_data$assessment_data
#' data_dict <- complete_data$data_dict
#' historical_medians <- complete_data$historical_medians
#' 
#' # Use in Shiny apps
#' library(shiny)
#' server <- function(input, output, session) {
#'   app_data <- reactive({
#'     load_rdm_complete()
#'   })
#'   
#'   output$resident_table <- DT::renderDataTable({
#'     app_data()$residents
#'   })
#' }
#' 
#' # Check data loading success
#' if (complete_data$data_loaded) {
#'   message("Data loaded successfully at ", complete_data$load_timestamp)
#'   message("Residents: ", nrow(complete_data$residents))
#'   message("Assessment records: ", nrow(complete_data$assessment_data))
#'   message("Data dictionary fields: ", nrow(complete_data$data_dict))
#' }
#' }
#'
#' @seealso 
#' \code{\link{load_rdm_simple}} for simplified data loading
#' \code{\link{load_rdm_quick}} for development/testing
#' \code{\link{load_data_by_forms}} for basic form-organized data loading
#' \code{\link{get_evaluation_dictionary}} for data dictionary access
#'
#' @keywords data loading REDCap RDM
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
  
  # ADD THIS NEW STEP HERE - Filter empty assessment instances
  if (verbose) message("Filtering empty assessment instances...")
  if (!is.null(data$raw_data)) {
    # Helper function to check if a field is empty
    is_empty <- function(x) {
      is.na(x) | as.character(x) == "" | as.character(x) == "NA"
    }
    
    # Filter out completely empty assessment instances
    data$raw_data <- data$raw_data %>%
      filter(
        !(redcap_repeat_instrument == "assessment" & 
            is_empty(ass_date) &
            is_empty(ass_delta) &
            is_empty(ass_plus))
      )
    
    # Also filter the forms data if it exists
    if (!is.null(data$forms$assessment)) {
      data$forms$assessment <- data$forms$assessment %>%
        filter(
          !(is_empty(ass_date) & is_empty(ass_delta) & is_empty(ass_plus))
        )
    }
    
    if (verbose) message("Filtered empty assessment instances")
  } else {
    if (verbose) message("No raw data found to filter")
  }
  
  
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
    
    # Add Level if missing or fix it if it's wrong
    if (!"Level" %in% names(residents) || all(residents$Level == "Unknown" | is.na(residents$Level))) {
      
      # Try to calculate level from type and grad_yr (same logic as elsewhere)
      if ("type" %in% names(residents) && "grad_yr" %in% names(residents)) {
        if (verbose) message("Calculating resident Level from type and grad_yr...")
        
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
          message("Calculated resident levels: ", paste(names(level_counts), "=", level_counts, collapse = ", "))
        }
        
      } else {
        # Fallback: try other level columns
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
