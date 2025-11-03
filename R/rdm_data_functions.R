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
#' @param raw_or_label Character. "label" for human-readable labels (default), "raw" for codes/checkbox fields
#'
#' @return Data frame with all REDCap records
#' @export
#'
#' @examples
#' \dontrun{
#' config <- initialize_app_config()
#' all_data <- pull_all_redcap_data(config$rdm_token, config$url)
#' }
pull_all_redcap_data <- function(token, url, raw_or_label = "label") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for REDCap API calls")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CSV processing")
  }
  
  message("Making REDCap API call...")
  
  # Prepare form data for API call
  form_data <- list(
    token = token,
    content = "record",
    action = "export", 
    format = "csv",
    type = "flat",
    rawOrLabel = raw_or_label,  # CHANGED: now parameterized
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "true",
    exportSurveyFields = "true",
    exportDataAccessGroups = "false",
    returnFormat = "csv",
    csvDelimiter = ""
  )
  
  # Make API call with explicit httr namespace
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  response <- httr::POST(
    url = url,
    body = form_data,
    encode = "form",
    httr::timeout(60)
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


#' Helper operator for NULL coalescing
#'
#' @param x First value
#' @param y Second value to use if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Load REDCap Data Organized by Forms (Simplified)
#'
#' Loads REDCap data and organizes it by actual form names from the data dictionary.
#' No complex mapping - just direct organization by form labels.
#'
#' @param rdm_token Character. REDCap API token for RDM database
#' @param redcap_url Character. REDCap API URL (default: SLU REDCap)
#' @param filter_archived Boolean. Remove archived residents (default: TRUE)
#' @param calculate_levels Boolean. Calculate resident levels (default: TRUE)
#' @param raw_or_label Character. "label" for labels (default), "raw" for codes
#' @param verbose Boolean. Print detailed loading messages (default: TRUE)
#'
#' @return List containing data organized by form names
#' @export
load_data_by_forms <- function(rdm_token = NULL,
                               redcap_url = "https://redcapsurvey.slu.edu/api/",
                               filter_archived = TRUE,
                               calculate_levels = TRUE,
                               raw_or_label = "label",  # ADD THIS PARAMETER
                               verbose = TRUE) {
  
  if (verbose) message("📊 Loading REDCap data organized by forms...")
  
  # Validate token
  if (is.null(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
    if (rdm_token == "") {
      stop("❌ RDM_TOKEN not provided and not found in environment variables")
    }
  }
  
  # Load data dictionary
  if (verbose) message("📖 Loading data dictionary...")
  data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  
  # Load raw data - PASS THROUGH PARAMETER
  if (verbose) message("📊 Loading raw REDCap data...")
  raw_data <- pull_all_redcap_data(
    token = rdm_token, 
    url = redcap_url,
    raw_or_label = raw_or_label  # ADD THIS
  )
  
  if (verbose) {
    message("✅ Loaded ", nrow(raw_data), " total records")
  }
  
  # Get all unique form names from data dictionary
  form_names <- unique(data_dict$form_name)
  form_names <- form_names[!is.na(form_names)]
  
  if (verbose) {
    message("📋 Found ", length(form_names), " forms in data dictionary:")
    for (form in sort(form_names)) {
      message("   - ", form)
    }
  }
  
  # Initialize result structure
  result <- list(
    raw_data = raw_data,
    data_dict = data_dict,
    forms = list()
  )
  
  # Extract base resident data (non-repeating)
  resident_data <- raw_data %>%
    dplyr::filter(is.na(redcap_repeat_instrument) | redcap_repeat_instrument == "")
  
  # Filter archived residents if requested
  if (filter_archived && "res_archive" %in% names(resident_data)) {
    before_count <- nrow(resident_data)
    resident_data <- resident_data %>%
      dplyr::filter(is.na(res_archive) | (!res_archive %in% c("Yes", "1")))
    if (verbose) {
      message("🗂️  Filtered archived residents: ", before_count, " -> ", nrow(resident_data))
    }
  }
  
  # Calculate levels if requested
  if (calculate_levels) {
    if (verbose) message("🎓 Calculating resident levels...")
    resident_data <- tryCatch({
      calculate_resident_level(resident_data)
    }, error = function(e) {
      if (verbose) message("⚠️  Using fallback level calculation")
      resident_data # Return as-is if calculation fails
    })
  }
  
  result$resident_data <- resident_data
  
  # For each form, extract relevant fields and data
  for (current_form in form_names) {
    
    # Get all field names for this form from data dictionary
    form_fields <- data_dict %>%
      dplyr::filter(form_name == current_form) %>%
      dplyr::pull(field_name)
    
    # Always include REDCap metadata fields
    metadata_fields <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", 
                         "redcap_event_name", "redcap_survey_identifier")
    
    # Combine metadata + form fields, keep only those that exist in data
    all_fields <- c(metadata_fields, form_fields)
    existing_fields <- intersect(all_fields, names(raw_data))
    
    # Extract data for this form
    if (length(existing_fields) > length(metadata_fields)) {
      # Filter data to only include records with data in this form's fields
      form_data <- raw_data %>%
        dplyr::select(all_of(existing_fields))
      
      # Remove rows where all form-specific fields are NA/empty
      form_specific_fields <- setdiff(existing_fields, metadata_fields)
      if (length(form_specific_fields) > 0) {
        # Keep rows where at least one form field has data
        form_data <- form_data %>%
          dplyr::filter(
            if_any(all_of(form_specific_fields), ~ !is.na(.) & . != "")
          )
      }
      
      # Store with clean form name
      clean_form_name <- tolower(gsub("[^a-zA-Z0-9_]", "_", current_form))
      clean_form_name <- gsub("_+", "_", clean_form_name)  # Remove multiple underscores
      clean_form_name <- gsub("^_|_$", "", clean_form_name)  # Remove leading/trailing underscores
      
      result$forms[[clean_form_name]] <- form_data
      
      if (verbose) {
        message("   ", current_form, " (", clean_form_name, "): ", 
                nrow(form_data), " records, ", length(form_specific_fields), " fields")
      }
    }
  }
  
  # Add metadata summary
  result$metadata <- list(
    total_records = nrow(raw_data),
    residents = nrow(resident_data),
    forms_with_data = length(result$forms),
    all_form_names = form_names,
    loaded_at = Sys.time()
  )
  
  if (verbose) {
    message("✅ Data organized by forms successfully!")
    message("📋 Available forms with data: ", paste(names(result$forms), collapse = ", "))
  }
  
  return(result)
}

#' Quick Form Data Access
#'
#' Helper function to quickly access data for a specific form
#'
#' @param data_list List returned from load_data_by_forms()
#' @param form_name Character. Name of form to extract (partial matching supported)
#' @return Data frame with form data, or NULL if not found
#' @export
get_form_data <- function(data_list, form_name) {
  
  # Direct match first
  if (form_name %in% names(data_list$forms)) {
    return(data_list$forms[[form_name]])
  }
  
  # Partial match
  matches <- grep(form_name, names(data_list$forms), value = TRUE, ignore.case = TRUE)
  
  if (length(matches) == 1) {
    return(data_list$forms[[matches[1]]])
  } else if (length(matches) > 1) {
    message("Multiple matches found: ", paste(matches, collapse = ", "))
    message("Using first match: ", matches[1])
    return(data_list$forms[[matches[1]]])
  } else {
    message("No form found matching: ", form_name)
    message("Available forms: ", paste(names(data_list$forms), collapse = ", "))
    return(NULL)
  }
}

#' List Available Forms
#'
#' Helper to see what forms are available in your loaded data
#'
#' @param data_list List returned from load_data_by_forms()
#' @return Data frame with form info
#' @export
list_forms <- function(data_list) {
  
  if (length(data_list$forms) == 0) {
    return(data.frame(
      form_name = character(0),
      n_records = integer(0),
      n_fields = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  
  form_summary <- data.frame(
    form_name = names(data_list$forms),
    n_records = sapply(data_list$forms, nrow),
    n_fields = sapply(data_list$forms, function(x) {
      # Count non-metadata fields
      metadata_fields <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance", 
                           "redcap_event_name", "redcap_survey_identifier")
      ncol(x) - length(intersect(names(x), metadata_fields))
    }),
    stringsAsFactors = FALSE
  )
  
  # Sort by number of records
  form_summary <- form_summary[order(form_summary$n_records, decreasing = TRUE), ]
  rownames(form_summary) <- NULL
  
  return(form_summary)
}

#' Debug Data Dictionary Structure
#'
#' Helper function to see what columns are in your data dictionary
#'
#' @param data_dict Data dictionary from get_evaluation_dictionary()
#' @export
debug_data_dict <- function(data_dict) {
  cat("=== DATA DICTIONARY DEBUG ===\n")
  cat("Total rows:", nrow(data_dict), "\n")
  cat("Column names:\n")
  for (i in 1:length(names(data_dict))) {
    cat("  ", i, ". '", names(data_dict)[i], "'\n", sep = "")
  }
  
  # Show sample form names
  cat("\nSample Form Names:\n")
  sample_forms <- unique(data_dict$form_name)[1:10]
  for (form in sample_forms) {
    cat("  - ", form, "\n")
  }
  
  # Show sample field names
  cat("\nSample Field Names:\n")
  sample_fields <- data_dict$field_name[1:5]
  cat("  ", paste(sample_fields, collapse = ", "), "\n")
}

# Example usage:
# 
# # Load data organized by forms
# data <- load_data_by_forms(rdm_token = "your_token")
# 
# # See what forms are available
# list_forms(data)
# 
# # Get specific form data
# acgme_data <- get_form_data(data, "acgme_miles")
# ccc_data <- get_form_data(data, "ccc_review")
# milestone_data <- get_form_data(data, "milestone_entry")
# 
# # Or access directly
# acgme_data <- data$forms$acgme_miles
# ccc_data <- data$forms$ccc_review


#' Filter Archived Residents from RDM Data
#'
#' Removes residents marked as archived (res_archive = "Yes", "Y", "1", etc.) 
#' from all forms in the data structure.
#'
#' @param data List containing forms data (output from load_data_by_forms)
#' @param archive_field Character string name of archive field (default: "res_archive")
#' @param verbose Logical, whether to print filtering summary (default: TRUE)
#'
#' @return Data structure with archived residents filtered out
#' @export
#'
#' @examples
#' \dontrun{
#' # Load data then filter archives
#' data <- load_data_by_forms(rdm_token)
#' clean_data <- filter_archived_residents(data)
#' 
#' # Quiet filtering
#' clean_data <- filter_archived_residents(data, verbose = FALSE)
#' 
#' # Custom archive field name
#' clean_data <- filter_archived_residents(data, archive_field = "archived")
#' }
filter_archived_residents <- function(data, archive_field = "res_archive", verbose = TRUE) {
  
  # Validate input
  if (!is.list(data) || !"forms" %in% names(data)) {
    stop("Data must be a list containing 'forms' element (output from load_data_by_forms)")
  }
  
  if (!"resident_data" %in% names(data$forms)) {
    if (verbose) cat("⚠️ No resident_data form found - returning data unchanged\n")
    return(data)
  }
  
  resident_data <- data$forms$resident_data
  
  if (!archive_field %in% names(resident_data)) {
    if (verbose) cat("⚠️ No", archive_field, "column found - returning data unchanged\n")
    return(data)
  }
  
  # Find archived record_ids
  # Archive values: "Yes", "Y", "1", 1, "true", "True", "TRUE"
  archive_values <- c("Yes", "Y", "1", 1, "true", "True", "TRUE")
  
  archived_record_ids <- resident_data %>%
    dplyr::filter(!!rlang::sym(archive_field) %in% archive_values) %>%
    dplyr::pull(record_id)
  
  # Summary before filtering
  if (verbose) {
    total_residents <- nrow(resident_data)
    archived_count <- length(archived_record_ids)
    active_count <- total_residents - archived_count
    
    cat("=== FILTERING ARCHIVED RESIDENTS ===\n")
    cat("📊 Total residents:", total_residents, "\n")
    cat("🗂️  Archived residents:", archived_count, "\n") 
    cat("✅ Active residents:", active_count, "\n")
    
    if (archived_count > 0) {
      cat("🔄 Removing archived residents from all forms...\n")
    }
  }
  
  # If no archived residents, return unchanged
  if (length(archived_record_ids) == 0) {
    if (verbose) cat("✅ No archived residents to filter\n")
    return(data)
  }
  
  # Filter archived residents from all forms
  data$forms <- lapply(names(data$forms), function(form_name) {
    form_data <- data$forms[[form_name]]
    
    # Only filter forms that have record_id column
    if ("record_id" %in% names(form_data)) {
      before_count <- nrow(form_data)
      
      # Remove archived record_ids
      filtered_data <- form_data %>% 
        dplyr::filter(!record_id %in% archived_record_ids)
      
      after_count <- nrow(filtered_data)
      
      # Report filtering for this form
      if (verbose && before_count != after_count) {
        records_removed <- before_count - after_count
        cat("   ", form_name, ":", before_count, "→", after_count, 
            "(removed", records_removed, ")\n")
      }
      
      return(filtered_data)
      
    } else {
      # Forms without record_id remain unchanged
      if (verbose && nrow(form_data) > 0) {
        cat("   ", form_name, ": no record_id column - unchanged\n")
      }
      return(form_data)
    }
    
  }) %>% setNames(names(data$forms))
  
  if (verbose) {
    cat("✅ Archive filtering complete\n")
  }
  
  return(data)
}

#' Get Archive Summary
#'
#' Returns summary information about archived residents without filtering
#'
#' @param data List containing forms data
#' @param archive_field Character string name of archive field (default: "res_archive")
#'
#' @return List with archive summary information
#' @export
get_archive_summary <- function(data, archive_field = "res_archive") {
  
  if (!is.list(data) || !"forms" %in% names(data) || !"resident_data" %in% names(data$forms)) {
    return(list(error = "Invalid data structure"))
  }
  
  resident_data <- data$forms$resident_data
  
  if (!archive_field %in% names(resident_data)) {
    return(list(error = paste("Archive field", archive_field, "not found")))
  }
  
  # Count archive values
  archive_table <- table(resident_data[[archive_field]], useNA = "always")
  
  # Find archived residents
  archive_values <- c("Yes", "Y", "1", 1, "true", "True", "TRUE")
  archived_count <- sum(resident_data[[archive_field]] %in% archive_values, na.rm = TRUE)
  total_count <- nrow(resident_data)
  active_count <- total_count - archived_count
  
  return(list(
    total_residents = total_count,
    archived_residents = archived_count, 
    active_residents = active_count,
    archive_field_values = as.list(archive_table),
    archive_percentage = round((archived_count / total_count) * 100, 1)
  ))
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

#' Example usage in your data pipeline:
#' 
#' # Standard pipeline
#' data <- load_data_by_forms(rdm_token = rdm_token)
#' clean_data <- filter_archived_residents(data)  # <-- Use this instead
#' data_with_levels <- add_level_at_time_to_forms(clean_data)
#' mile_data <- prepare_milestone_app_data(data_with_levels)
#'
#' # Check archive status without filtering
#' archive_info <- get_archive_summary(data)
#' print(archive_info)
#'
#' # Quiet filtering 
#' clean_data <- filter_archived_residents(data, verbose = FALSE)




# 
#' Calculate Resident Level at Time of Data Collection
#'
#' Calculates what level a resident was at the specific date when data was collected,
#' based on their type (Categorical/Preliminary), graduation year, and the date field.
#'
#' @param data Dataframe with resident data including type, grad_yr, and a date column
#' @param resident_lookup Dataframe with record_id, type, and grad_yr for each resident
#' @param date_col_name Name of the date column to use (e.g., "ass_date", "fac_eval_date")
#'
#' @return Dataframe with added 'level' column showing level at time of data collection
#' @export
calculate_level_at_time <- function(data, resident_lookup, date_col_name) {
  
  # Check if date column exists
  if (!date_col_name %in% names(data)) {
    warning("Date column '", date_col_name, "' not found in data")
    data$level <- NA_character_
    return(data)
  }
  
  # Join with resident lookup to get type and grad_yr
  data_with_resident_info <- data %>%
    dplyr::left_join(
      resident_lookup %>% dplyr::select(record_id, type, grad_yr),
      by = "record_id"
    )
  
  # Calculate level at time of data collection
  data_with_level <- data_with_resident_info %>%
    dplyr::mutate(
      # Convert date column to Date type
      collection_date = as.Date(.data[[date_col_name]]),
      
      # Convert grad_yr to numeric
      grad_yr_numeric = suppressWarnings(as.numeric(grad_yr)),
      
      # Calculate level based on date, type, and graduation year
      level = dplyr::case_when(
        # Missing date -> missing level
        is.na(collection_date) ~ NA_character_,
        
        # Missing type or grad_yr -> missing level  
        is.na(type) | is.na(grad_yr_numeric) ~ NA_character_,
        
        # Preliminary residents are always Intern
        type == "Preliminary" ~ "Intern",
        
        # Categorical residents - calculate based on academic year at collection date
        type == "Categorical" ~ {
          # Determine academic year of the collection date
          # Academic year starts July 1st
          academic_year <- ifelse(
            format(collection_date, "%m-%d") >= "07-01",
            as.numeric(format(collection_date, "%Y")),
            as.numeric(format(collection_date, "%Y")) - 1
          )
          
          # Calculate level based on how many years before graduation
          years_to_grad <- grad_yr_numeric - academic_year
          
          dplyr::case_when(
            years_to_grad == 3 ~ "Intern",    # 3 years until graduation
            years_to_grad == 2 ~ "PGY2",      # 2 years until graduation  
            years_to_grad == 1 ~ "PGY3",      # 1 year until graduation
            years_to_grad == 0 ~ "Graduating", # Graduation year
            years_to_grad < 0 ~ "Graduated",   # Past graduation
            years_to_grad > 3 ~ "Pre-Intern",  # Before starting residency
            TRUE ~ "Unknown"
          )
        },
        
        # Other types
        TRUE ~ "Unknown"
      )
    ) %>%
    # Remove temporary columns
    dplyr::select(-collection_date, -grad_yr_numeric, -type, -grad_yr)
  
  return(data_with_level)
}

#' Add Level at Time to Specified Forms with Date Fields
#'
#' Automatically adds level calculation to specified forms that have date fields
#'
#' @param data_list List from load_data_by_forms() 
#' @param forms Named vector or list specifying forms and their date columns
#'   e.g., c("assessment" = "ass_date", "faculty_evaluation" = "fac_eval_date")
#' @param verbose Print progress messages
#'
#' @return Same data structure with level columns added to specified forms
#' @export
add_level_at_time_to_forms <- function(data_list, 
                                       forms = c("assessment" = "ass_date", 
                                                 "faculty_evaluation" = "fac_eval_date", 
                                                 "questions" = "q_date"),
                                       verbose = TRUE) {
  
  if (verbose) cat("=== ADDING LEVEL AT TIME TO SPECIFIED FORMS ===\n")
  
  # Get resident lookup data
  resident_lookup <- data_list$resident_data %>%
    dplyr::select(record_id, type, grad_yr)
  
  if (verbose) cat("Using resident lookup with", nrow(resident_lookup), "residents\n")
  
  # Process each specified form
  for (form_name in names(forms)) {
    date_col <- forms[[form_name]]
    
    if (form_name %in% names(data_list$forms)) {
      form_data <- data_list$forms[[form_name]]
      
      if (nrow(form_data) > 0 && date_col %in% names(form_data)) {
        
        if (verbose) cat("Adding level to", form_name, "using", date_col, "\n")
        
        # Calculate level at time
        form_data_with_level <- calculate_level_at_time(
          form_data, 
          resident_lookup, 
          date_col
        )
        
        # Update the form data
        data_list$forms[[form_name]] <- form_data_with_level
        
        if (verbose) {
          level_counts <- table(form_data_with_level$level, useNA = "always")
          cat("  Level distribution:", paste(names(level_counts), "=", level_counts, collapse = ", "), "\n")
        }
        
      } else {
        if (verbose) cat("Skipping", form_name, "- no data or no", date_col, "column\n")
      }
    } else {
      if (verbose) cat("Form", form_name, "not found in data\n")
    }
  }
  
  if (verbose) cat("Level at time calculation complete\n")
  
  return(data_list)
}

# Example usage:
#
# # Default: Apply to assessment, faculty_evaluation, and questions forms
# data_with_levels <- add_level_at_time_to_forms(clean_data)
# 
# # Custom: Apply only to assessment form
# data_with_levels <- add_level_at_time_to_forms(
#   clean_data, 
#   forms = c("assessment" = "ass_date")
# )
# 
# # Custom: Apply to different forms/date combinations
# data_with_levels <- add_level_at_time_to_forms(
#   clean_data,
#   forms = c(
#     "assessment" = "ass_date",
#     "s_eval" = "s_e_date",
#     "ccc_review" = "ccc_date"
#   )
# )
#
# # Check results
# table(data_with_levels$forms$assessment$level, useNA = "always")
# 
