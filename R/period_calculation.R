#' Calculate Current PGY Year and Self-Evaluation Period
#'
#' Determines resident's current PGY year and appropriate self-evaluation period
#' based on graduation year, residency type, and current date within academic year.
#'
#' @param grad_yr Numeric. Year of expected graduation (e.g., 2026)
#' @param type Numeric or Character. Residency type: 1/"Preliminary" or 2/"Categorical"
#' @param current_date Date. Current date for calculation (default: Sys.Date())
#' @param data_dict Optional. Data dictionary data frame to pull labels from REDCap
#'
#' @return List with elements:
#'   \item{pgy_year}{Numeric. Current PGY year (1, 2, or 3)}
#'   \item{period_number}{Numeric. Self-evaluation period number (7, 1-6, or NA)}
#'   \item{period_name}{Character. Self-evaluation period name}
#'   \item{academic_year}{Character. Current academic year (e.g., "2024-2025")}
#'   \item{months_into_year}{Numeric. Months into current academic year}
#'   \item{is_valid}{Logical. TRUE if resident is currently in training}
#'   
#' @details
#' Self-evaluation periods (note: starts at 7, not 0):
#' \itemize{
#'   \item 7 = Entering Residency (July 1 - Sept 30, PGY1)
#'   \item 1 = Mid Intern (Oct 1 - Jan 31, PGY1)
#'   \item 2 = End Intern (Feb 1 - June 30, PGY1)
#'   \item 3 = Mid PGY2 (Oct 1 - Jan 31, PGY2)
#'   \item 4 = End PGY2 (Feb 1 - June 30, PGY2)
#'   \item 5 = Mid PGY3 (Oct 1 - Jan 31, PGY3)
#'   \item 6 = Graduating (Feb 1 - June 30, PGY3)
#' }
#'
#' Preliminary residents only experience periods 7, 1, 2.
#' After graduation, period_number = NA and is_valid = FALSE.
#' 
#' If data_dict is provided, period names and type labels will be pulled from
#' REDCap data dictionary. Otherwise, falls back to hardcoded labels.
#'
#' @export
#'
#' @examples
#' # Categorical resident graduating 2026, checked in December 2024
#' calculate_pgy_and_period(2026, 2, as.Date("2024-12-15"))
#' # Returns: PGY2, period 3 (Mid PGY2)
#'
#' # With data dictionary for dynamic labels
#' calculate_pgy_and_period(2026, 2, as.Date("2024-12-15"), data_dict = my_data_dict)
#'
#' # Preliminary resident graduating 2025, checked in January 2025
#' calculate_pgy_and_period(2025, 1, as.Date("2025-01-15"))
#' # Returns: PGY1, period 1 (Mid Intern)
#'
#' # Recent graduate (after June 30)
#' calculate_pgy_and_period(2024, 2, as.Date("2024-08-01"))
#' # Returns: Graduated, period NA, is_valid = FALSE
calculate_pgy_and_period <- function(grad_yr, 
                                     type = 2,
                                     current_date = Sys.Date(),
                                     data_dict = NULL) {
  
  # Validate inputs
  if (is.na(grad_yr) || is.null(grad_yr)) {
    return(list(
      pgy_year = NA,
      period_number = NA,
      period_name = "Unknown",
      academic_year = NA,
      months_into_year = NA,
      is_valid = FALSE,
      error = "Missing graduation year"
    ))
  }
  
  # Standardize type - convert to numeric if needed
  type_numeric <- as.numeric(type)
  if (is.na(type_numeric)) {
    # Try string matching
    type_str <- tolower(trimws(as.character(type)))
    type_numeric <- if (grepl("prelim", type_str)) 1 else 2
  }
  
  is_preliminary <- (type_numeric == 1)
  
  # Calculate years of training
  training_years <- if (is_preliminary) 1 else 3
  
  # Calculate start year (year they began residency)
  start_year <- grad_yr - training_years
  
  # Current year and month
  current_year <- as.numeric(format(current_date, "%Y"))
  current_month <- as.numeric(format(current_date, "%m"))
  current_day <- as.numeric(format(current_date, "%d"))
  
  # Determine current academic year
  if (current_month >= 7) {
    academic_year_start_year <- current_year
    academic_year_end_year <- current_year + 1
  } else {
    academic_year_start_year <- current_year - 1
    academic_year_end_year <- current_year
  }
  
  academic_year <- paste0(academic_year_start_year, "-", academic_year_end_year)
  
  # Calculate PGY year
  pgy_year <- academic_year_start_year - start_year + 1
  
  # Calculate months into academic year
  ay_start <- as.Date(paste0(academic_year_start_year, "-07-01"))
  months_into_year <- as.numeric(difftime(current_date, ay_start, units = "days")) / 30.44
  
  # Check if graduated
  grad_date <- as.Date(paste0(grad_yr, "-06-30"))
  if (current_date > grad_date) {
    return(list(
      pgy_year = NA,
      period_number = NA,
      period_name = "Graduated",
      academic_year = academic_year,
      months_into_year = NA,
      is_valid = FALSE,
      message = "Resident has graduated"
    ))
  }
  
  # Check if before residency start
  residency_start <- as.Date(paste0(start_year, "-07-01"))
  if (current_date < residency_start) {
    return(list(
      pgy_year = 0,
      period_number = NA,
      period_name = "Pre-residency",
      academic_year = academic_year,
      months_into_year = NA,
      is_valid = FALSE,
      message = "Resident has not started yet"
    ))
  }
  
  # Determine period based on PGY year and date within year
  period_number <- NA
  
  if (pgy_year == 1) {
    # PGY1 periods
    if (current_month >= 7 && current_month <= 9) {
      period_number <- 7
    } else if (current_month >= 10 || current_month == 1) {
      period_number <- 1
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 2
    }
  } else if (pgy_year == 2 && !is_preliminary) {
    # PGY2 periods (categorical only)
    if (current_month >= 10 || current_month == 1) {
      period_number <- 3
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 4
    } else if (current_month >= 7 && current_month <= 9) {
      # Early PGY2 - use previous period (End Intern)
      period_number <- 2
    }
  } else if (pgy_year == 3 && !is_preliminary) {
    # PGY3 periods (categorical only)
    if (current_month >= 10 || current_month == 1) {
      period_number <- 5
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 6
    } else if (current_month >= 7 && current_month <= 9) {
      # Early PGY3 - use previous period (End PGY2)
      period_number <- 4
    }
  } else if (is_preliminary && pgy_year > 1) {
    # Preliminary residents shouldn't be beyond PGY1
    return(list(
      pgy_year = pgy_year,
      period_number = NA,
      period_name = "Invalid - Prelim beyond PGY1",
      academic_year = academic_year,
      months_into_year = months_into_year,
      is_valid = FALSE,
      error = "Preliminary resident should have graduated"
    ))
  }
  
  # Get period name from data dictionary or fallback
  period_name <- get_period_label(period_number, data_dict = data_dict)
  
  # Get type label from data dictionary or fallback
  type_label <- translate_resident_type(type_numeric, data_dict = data_dict)
  
  return(list(
    pgy_year = pgy_year,
    period_number = period_number,
    period_name = period_name,
    academic_year = academic_year,
    months_into_year = round(months_into_year, 1),
    is_valid = !is.na(period_number),
    grad_yr = grad_yr,
    type = type_label,
    type_code = type_numeric
  ))
}


#' Get Period Label from Period Number
#'
#' Simple helper to convert period number to readable label.
#' If data dictionary is provided, pulls labels from REDCap.
#'
#' @param period_number Numeric period number (7, 1-6)
#' @param data_dict Optional data dictionary to pull labels from
#' @return Character period label
#' @export
#'
#' @examples
#' get_period_label(7)  # "Entering Residency"
#' get_period_label(1)  # "Mid Intern"
#' 
#' # With data dictionary
#' get_period_label(1, data_dict = my_data_dict)
get_period_label <- function(period_number, data_dict = NULL) {
  
  if (is.na(period_number)) return("Unknown")
  
  # If data dictionary provided, try to use it
  if (!is.null(data_dict)) {
    # Look for period field (could be s_e_period, prog_mile_period, etc.)
    period_fields <- data_dict %>%
      dplyr::filter(
        grepl("period", field_name, ignore.case = TRUE) &
          field_type %in% c("dropdown", "radio")
      )
    
    if (nrow(period_fields) > 0) {
      # Use first period field found
      period_field <- period_fields %>% dplyr::slice(1)
      
      if (!is.na(period_field$select_choices_or_calculations)) {
        choices <- parse_redcap_choices(period_field$select_choices_or_calculations)
        
        period_code_str <- as.character(period_number)
        if (period_code_str %in% names(choices)) {
          return(choices[[period_code_str]])
        }
      }
    }
  }
  
  # Fallback to hardcoded labels
  labels <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduating",
    "7" = "Entering Residency"
  )
  
  labels[as.character(period_number)] %||% "Unknown"
}


#' Translate Resident Type Code to Label
#'
#' Converts numeric type code (1 or 2) to readable label.
#' If data dictionary is provided, pulls labels from REDCap.
#'
#' @param type_code Numeric type code (1 or 2) or character
#' @param data_dict Optional data dictionary to pull labels from
#'
#' @return Character string with type label
#' @export
#'
#' @examples
#' translate_resident_type(1)  # "Preliminary"
#' translate_resident_type(2)  # "Categorical"
#' 
#' # With data dictionary
#' translate_resident_type(2, data_dict = my_data_dict)
translate_resident_type <- function(type_code, data_dict = NULL) {
  
  # If data dictionary provided, use it
  if (!is.null(data_dict)) {
    type_field <- data_dict %>%
      dplyr::filter(field_name == "type") %>%
      dplyr::slice(1)
    
    if (nrow(type_field) > 0 && !is.na(type_field$select_choices_or_calculations)) {
      # Parse choices
      choices_string <- type_field$select_choices_or_calculations
      choices <- parse_redcap_choices(choices_string)
      
      # Find matching label
      type_code_str <- as.character(type_code)
      if (type_code_str %in% names(choices)) {
        return(choices[[type_code_str]])
      }
    }
  }
  
  # Fallback to hardcoded (if data dict not available or doesn't have it)
  switch(as.character(type_code),
         "1" = "Preliminary",
         "2" = "Categorical",
         "Unknown")
}


#' Parse REDCap Choices String
#'
#' Helper to parse "1, Label 1 | 2, Label 2" format from REDCap data dictionary
#'
#' @param choices_string Character string from data dictionary select_choices_or_calculations
#'
#' @return Named character vector where names are codes and values are labels
#' @export
#'
#' @examples
#' parse_redcap_choices("1, Preliminary | 2, Categorical")
#' # Returns: c("1" = "Preliminary", "2" = "Categorical")
parse_redcap_choices <- function(choices_string) {
  if (is.na(choices_string) || nchar(trimws(choices_string)) == 0) {
    return(NULL)
  }
  
  # Split by |
  items <- strsplit(choices_string, "\\|")[[1]]
  items <- trimws(items)
  
  # Parse each item (format: "code, label")
  result <- sapply(items, function(item) {
    parts <- strsplit(item, ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      code <- trimws(parts[1])
      label <- trimws(paste(parts[-1], collapse = ","))  # Handle labels with commas
      return(c(code = label))
    }
    return(NULL)
  }, USE.NAMES = FALSE, simplify = FALSE)
  
  # Remove NULLs and combine
  result <- result[!sapply(result, is.null)]
  result <- unlist(result)
  
  # Create named vector: names = codes, values = labels
  codes <- names(result)
  labels <- unname(result)
  
  setNames(labels, codes)
}


#' Get All Possible Periods for a Resident
#'
#' Returns vector of all period numbers a resident will experience
#' based on their type
#'
#' @param type Numeric or Character. Residency type: 1/"Preliminary" or 2/"Categorical"
#' @return Numeric vector of period numbers
#' @export
#'
#' @examples
#' get_resident_periods(2)  # c(7, 1, 2, 3, 4, 5, 6)
#' get_resident_periods("Categorical")  # c(7, 1, 2, 3, 4, 5, 6)
#' get_resident_periods(1)  # c(7, 1, 2)
#' get_resident_periods("Preliminary")  # c(7, 1, 2)
get_resident_periods <- function(type) {
  # Handle both numeric and string type
  type_numeric <- as.numeric(type)
  if (is.na(type_numeric)) {
    type_str <- tolower(trimws(as.character(type)))
    type_numeric <- if (grepl("prelim", type_str)) 1 else 2
  }
  
  is_preliminary <- (type_numeric == 1)
  
  if (is_preliminary) {
    return(c(7, 1, 2))
  } else {
    return(c(7, 1, 2, 3, 4, 5, 6))
  }
}