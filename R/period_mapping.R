#' @title Period and Level Mapping Functions for GMED
#' @description Functions for mapping between app periods, REDCap periods, and milestone periods
#' @name period_mapping
NULL

#' Map Application Period to Milestone Period
#'
#' Converts application-friendly period names to the milestone period format
#' used in REDCap. Handles different contexts (milestone vs coach review).
#'
#' @param level Character string indicating resident level
#' @param period Character string indicating the period in app format
#' @param form_context Optional context ("milestone", "coach", etc.) for specialized mapping
#'
#' @return Character string of mapped period or NA if no mapping exists
#' @export
#'
#' @examples
#' \dontrun{
#' # Map for milestone assessment
#' map_to_milestone_period("Intern", "End Review")
#' 
#' # Map for coach review
#' map_to_milestone_period("PGY2", "Mid Year Review", "coach")
#' }
map_to_milestone_period <- function(level, period, form_context = NULL) {
  
  if (is.null(level) || is.null(period) || level == "" || period == "") {
    return(NA_character_)
  }
  
  # Define period mappings by level
  intern_periods <- c(
    "Entering Residency" = "Intern Intro",
    "Mid Year Review" = "Mid Intern", 
    "End Review" = "End Intern"
  )
  
  pgy2_periods <- c(
    "Mid Year Review" = "Mid PGY2",
    "End Review" = "End PGY2"
  )
  
  pgy3_periods <- c(
    "Mid Year Review" = "Mid PGY3", 
    "End Review" = "Graduation"
  )
  
  # Get appropriate mapping based on level
  period_map <- switch(level,
                       "Intern" = intern_periods,
                       "PGY2" = pgy2_periods,
                       "PGY3" = pgy3_periods,
                       character(0)
  )
  
  # Handle special context mappings
  if (!is.null(form_context)) {
    if (form_context == "milestone" && level == "Intern" && period == "Entering Residency") {
      # Entering residents don't have milestone assessments yet
      return(NA_character_)
    }
  }
  
  # Return mapped period or the original if no mapping found
  mapped_period <- period_map[period]
  
  if (is.na(mapped_period) || is.null(mapped_period)) {
    # Try direct period name if no mapping found
    if (period %in% c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2", "Mid PGY3", "Graduation")) {
      return(period)
    }
    return(NA_character_)
  }
  
  return(unname(mapped_period))
}

#' Get Previous Period for Comparison
#'
#' Determines the previous evaluation period for a given current period and level.
#' Used for comparing current vs. previous milestone assessments.
#'
#' @param current_period Character string of current period in app format
#' @param level Character string indicating resident level
#' @param form_context Optional context for specialized logic
#'
#' @return Character string of previous period or NA if no previous period
#' @export
#'
#' @examples
#' \dontrun{
#' get_previous_period("End Review", "Intern")
#' get_previous_period("Mid Year Review", "PGY2")
#' }
get_previous_period <- function(current_period, level, form_context = NULL) {
  
  if (is.null(current_period) || is.null(level)) {
    return(NA_character_)
  }
  
  # Define previous period relationships
  previous_map <- list(
    "Intern" = list(
      "Mid Year Review" = "Entering Residency",
      "End Review" = "Mid Year Review"
    ),
    "PGY2" = list(
      "Mid Year Review" = "End Review",  # Previous year's end review
      "End Review" = "Mid Year Review"
    ),
    "PGY3" = list(
      "Mid Year Review" = "End Review",  # Previous year's end review  
      "End Review" = "Mid Year Review"
    )
  )
  
  # Get previous period for this level
  level_map <- previous_map[[level]]
  if (is.null(level_map)) {
    return(NA_character_)
  }
  
  previous_period <- level_map[[current_period]]
  
  # Handle special cases
  if (!is.null(form_context) && form_context == "milestone") {
    # For milestone context, "Entering Residency" doesn't have milestone data
    if (!is.null(previous_period) && previous_period == "Entering Residency") {
      return(NA_character_)
    }
  }
  
  return(previous_period)
}

#' Get Current Period Based on Date
#'
#' Determines the current evaluation period based on the current date
#' and academic year calendar.
#'
#' @param reference_date Date object (defaults to current date)
#'
#' @return Character string indicating current period
#' @export
#'
#' @examples
#' \dontrun{
#' get_current_period()
#' get_current_period(as.Date("2024-02-15"))
#' }
get_current_period <- function(reference_date = Sys.Date()) {
  
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }
  
  # Convert to Date if needed
  if (!inherits(reference_date, "Date")) {
    reference_date <- as.Date(reference_date)
  }
  
  # Get month and day
  month <- as.numeric(format(reference_date, "%m"))
  day <- as.numeric(format(reference_date, "%d"))
  
  # Define period boundaries (adjust based on your academic calendar)
  if (month >= 7 && month <= 12) {
    # July through December - first half of academic year
    return("Mid Year Review")
  } else if (month >= 1 && month <= 6) {
    # January through June - second half of academic year
    return("End Review")
  } else {
    # Fallback
    return("Mid Year Review")
  }
}

#' Calculate Resident Level Based on Data
#'
#' Determines or calculates the resident level based on available data fields.
#' Handles both explicit Level fields and calculated levels based on year.
#'
#' @param resident_data Data frame containing resident information
#'
#' @return Data frame with Level column added or updated
#' @export
#'
#' @examples
#' \dontrun{
#' residents_with_levels <- calculate_resident_level(resident_data)
#' }
calculate_resident_level <- function(resident_data) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(resident_data) || nrow(resident_data) == 0) {
    return(resident_data)
  }
  
  # If Level column already exists and is populated, use it
  if ("Level" %in% names(resident_data)) {
    # Fill in missing levels if we have year data
    if ("year" %in% names(resident_data)) {
      resident_data <- resident_data %>%
        dplyr::mutate(
          Level = dplyr::case_when(
            !is.na(Level) & Level != "" ~ Level,  # Keep existing levels
            year == 1 ~ "Intern",
            year == 2 ~ "PGY2", 
            year == 3 ~ "PGY3",
            TRUE ~ Level  # Keep original if no mapping possible
          )
        )
    }
    return(resident_data)
  }
  
  # Calculate Level based on year if Level column doesn't exist
  if ("year" %in% names(resident_data)) {
    resident_data <- resident_data %>%
      dplyr::mutate(
        Level = dplyr::case_when(
          year == 1 ~ "Intern",
          year == 2 ~ "PGY2",
          year == 3 ~ "PGY3", 
          TRUE ~ "Unknown"
        )
      )
  } else {
    # If no year column, add placeholder Level
    resident_data$Level <- "Unknown"
  }
  
  return(resident_data)
}

#' Get REDCap Instance Number for Form Submission
#'
#' Maps level, period, and review type to the appropriate REDCap instance number
#' for repeating instruments. This is critical for proper data storage in RDM 2.0.
#'
#' @param level Character string indicating resident level
#' @param period Character string indicating period
#' @param review_type Character string indicating review type ("scheduled", "interim", etc.)
#'
#' @return Numeric instance number or NA if no mapping exists
#' @export
#'
#' @examples
#' \dontrun{
#' get_redcap_instance("Intern", "End Review", "scheduled")
#' get_redcap_instance("PGY2", "Mid Year Review", "interim")
#' }
get_redcap_instance <- function(level, period, review_type = "scheduled") {
  
  # Define instance mappings for scheduled reviews
  if (review_type == "scheduled") {
    instance_map <- list(
      "Intern" = list(
        "Entering Residency" = 7,
        "Mid Year Review" = 1,
        "End Review" = 2
      ),
      "PGY2" = list(
        "Mid Year Review" = 3,
        "End Review" = 4
      ),
      "PGY3" = list(
        "Mid Year Review" = 5,
        "End Review" = 6
      )
    )
  } else if (review_type == "interim") {
    # Interim reviews use different instance numbers
    instance_map <- list(
      "Intern" = list(
        "Interim Review" = 8
      ),
      "PGY2" = list(
        "Interim Review" = 9
      ),
      "PGY3" = list(
        "Interim Review" = 10
      )
    )
  } else {
    return(NA_integer_)
  }
  
  # Get instance number
  level_map <- instance_map[[level]]
  if (is.null(level_map)) {
    return(NA_integer_)
  }
  
  instance_num <- level_map[[period]]
  if (is.null(instance_num)) {
    return(NA_integer_)
  }
  
  return(as.integer(instance_num))
}

#' Map Milestone Period to Instance (Legacy Support)
#'
#' Maps milestone period text to numeric instance for backward compatibility.
#' Used when working with older milestone data structures.
#'
#' @param period_text Character string of milestone period
#'
#' @return Numeric instance number
#' @export
map_milestone_period_to_instance <- function(period_text) {
  
  period_instance_map <- c(
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduation" = 6,
    "Intern Intro" = 7
  )
  
  instance <- period_instance_map[period_text]
  
  if (is.na(instance) || is.null(instance)) {
    return(1)  # Default to instance 1
  }
  
  return(unname(instance))
}