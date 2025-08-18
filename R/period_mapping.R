# UPDATES for existing gmed R/period_mapping.R file
# Replace/update these functions to match your working coach app patterns

#' Get Current Period Based on Date - UPDATED VERSION
#'
#' Determines the current evaluation period based on the current date
#' and academic year calendar. Updated to match coaching app periods.
#'
#' @param reference_date Date object (defaults to current date)  
#' @return Character string indicating current period ("Intern Intro", "Mid Review", "End Review")
#' @export
get_current_period <- function(reference_date = Sys.Date()) {
  
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }
  
  # Convert to Date if needed
  if (!inherits(reference_date, "Date")) {
    reference_date <- as.Date(reference_date)
  }
  
  # Calculate academic year boundaries (July 1 - June 30)
  if (format(reference_date, "%m-%d") >= "07-01") {
    academic_year_start <- as.Date(paste0(format(reference_date, "%Y"), "-07-01"))
    academic_year_end <- as.Date(paste0(as.numeric(format(reference_date, "%Y")) + 1, "-06-30"))
  } else {
    academic_year_start <- as.Date(paste0(as.numeric(format(reference_date, "%Y")) - 1, "-07-01"))
    academic_year_end <- as.Date(paste0(format(reference_date, "%Y"), "-06-30"))
  }
  
  # Define period boundaries (matches your coaching app exactly)
  intern_intro_end <- academic_year_start + 76  # ~11 weeks for intern intro
  mid_review_end <- as.Date(paste0(format(academic_year_start + 365, "%Y"), "-01-31"))
  
  # Determine period (matches your working coach app)
  if (reference_date >= academic_year_start & reference_date <= intern_intro_end) {
    return("Intern Intro")
  } else if (reference_date >= intern_intro_end + 1 & reference_date <= mid_review_end) {
    return("Mid Review") 
  } else if (reference_date > mid_review_end & reference_date <= academic_year_end) {
    return("End Review")
  } else {
    return("Mid Review")  # Fallback
  }
}

#' Map Application Period to Milestone Period - UPDATED VERSION
#'
#' Updated to match your working coach app mapping patterns exactly.
#'
#' @param level Character string indicating resident level
#' @param period Character string indicating the period in app format
#' @param form_context Optional context for specialized mapping
#'
#' @return Character string of mapped period or NA if no mapping exists
#' @export
map_to_milestone_period <- function(level, period, form_context = NULL) {
  
  if (is.null(level) || is.null(period) || level == "" || period == "") {
    return(NA_character_)
  }
  
  # Define period mappings by level (matches your working coach app)
  intern_periods <- c(
    "Intern Intro" = "Intern Intro",
    "Mid Review" = "Mid Intern", 
    "End Review" = "End Intern"
  )
  
  pgy2_periods <- c(
    "Mid Review" = "Mid PGY2",
    "End Review" = "End PGY2"
  )
  
  pgy3_periods <- c(
    "Mid Review" = "Mid PGY3", 
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
    if (form_context == "milestone" && level == "Intern" && period == "Intern Intro") {
      # Entering residents don't have milestone assessments yet
      return(NA_character_)
    }
  }
  
  # Return mapped period or the original if no mapping found
  mapped_period <- period_map[period]
  
  if (is.na(mapped_period) || is.null(mapped_period)) {
    # Try direct period name if no mapping found (supports both formats)
    if (period %in% c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2", "Mid PGY3", "Graduation", "Intern Intro")) {
      return(period)
    }
    return(NA_character_)
  }
  
  return(unname(mapped_period))
}

#' Calculate Resident Level Based on Data - ENHANCED VERSION  
#'
#' Enhanced to handle the grad_yr/type pattern from your coaching app.
#'
#' @param resident_data Data frame containing resident information
#' @param current_date Date object for academic year calculation (defaults to current date)
#' @return Data frame with Level column added or updated
#' @export
calculate_resident_level <- function(resident_data, current_date = Sys.Date()) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(resident_data) || nrow(resident_data) == 0) {
    return(resident_data)
  }
  
  # If Level column already exists and is populated, use it
  if ("Level" %in% names(resident_data)) {
    existing_levels <- resident_data$Level[!is.na(resident_data$Level) & resident_data$Level != ""]
    if (length(existing_levels) > 0) {
      # Check if levels are already calculated properly
      unique_levels <- unique(existing_levels)
      if (all(unique_levels %in% c("Intern", "PGY2", "PGY3"))) {
        message("Level column already properly calculated")
        return(resident_data)
      }
    }
  }
  
  # METHOD 1: Complex calculation using type + grad_yr (RDM 2.0 pattern)
  if ("type" %in% names(resident_data) && "grad_yr" %in% names(resident_data)) {
    
    message("Using type/grad_yr calculation method (RDM 2.0 pattern)")
    
    # Calculate current academic year (July 1 - June 30)
    if (format(current_date, "%m-%d") >= "07-01") {
      current_academic_year <- as.numeric(format(current_date, "%Y"))
    } else {
      current_academic_year <- as.numeric(format(current_date, "%Y")) - 1
    }
    
    message("Current academic year: ", current_academic_year)
    
    # Ensure grad_yr is numeric
    resident_data <- resident_data %>%
      dplyr::mutate(
        grad_yr = suppressWarnings(as.numeric(.data$grad_yr))
      )
    
    # Calculate Level based on type and grad_yr (matches your working coach app)
    resident_data <- resident_data %>%
      dplyr::mutate(
        Level = dplyr::case_when(
          # Preliminary residents are always Interns (one year program)
          tolower(.data$type) %in% c("preliminary", "prelim") ~ "Intern",
          
          # Categorical residents based on graduation year
          tolower(.data$type) == "categorical" & .data$grad_yr == current_academic_year + 3 ~ "Intern",  # PGY1
          tolower(.data$type) == "categorical" & .data$grad_yr == current_academic_year + 2 ~ "PGY2",    # PGY2
          tolower(.data$type) == "categorical" & .data$grad_yr == current_academic_year + 1 ~ "PGY3",    # PGY3
          
          # Default for unmatched cases
          !is.na(.data$type) & !is.na(.data$grad_yr) ~ "Intern",
          TRUE ~ "Intern"
        )
      )
    
    # METHOD 2: Simple year field (legacy apps)
  } else if ("year" %in% names(resident_data)) {
    
    message("Using simple year field calculation method")
    
    resident_data <- resident_data %>%
      dplyr::mutate(
        Level = dplyr::case_when(
          .data$year == 1 ~ "Intern",
          .data$year == 2 ~ "PGY2",
          .data$year == 3 ~ "PGY3", 
          TRUE ~ "Intern"  # Default fallback
        )
      )
    
  } else {
    # Fallback - add default Level
    message("No type/grad_yr or year columns found, using default Level")
    resident_data$Level <- "Intern"
  }
  
  # Report level distribution
  if ("Level" %in% names(resident_data)) {
    level_counts <- table(resident_data$Level)
    message("Level distribution: ", 
            "Intern: ", level_counts[["Intern"]] %||% 0, ", ",
            "PGY2: ", level_counts[["PGY2"]] %||% 0, ", ",
            "PGY3: ", level_counts[["PGY3"]] %||% 0)
  }
  
  return(resident_data)
}

#' Get Previous Period for Comparison - UPDATED VERSION
#'
#' Updated to match your working coach app period patterns.
#'
#' @param current_period Character string of current period in app format
#' @param level Character string indicating resident level
#' @param form_context Optional context for specialized logic
#'
#' @return Character string of previous period or NA if no previous period
#' @export
get_previous_period <- function(current_period, level, form_context = NULL) {
  
  if (is.null(current_period) || is.null(level)) {
    return(NA_character_)
  }
  
  # Define previous period relationships (matches your working patterns)
  previous_map <- list(
    "Intern" = list(
      "Mid Review" = "Intern Intro",   # Previous to Mid Review is Intern Intro
      "End Review" = "Mid Review"      # Previous to End Review is Mid Review
    ),
    "PGY2" = list(
      "Mid Review" = "End Review",     # Previous year's end review (as Intern)
      "End Review" = "Mid Review"      # Current year's mid review
    ),
    "PGY3" = list(
      "Mid Review" = "End Review",     # Previous year's end review (as PGY2)  
      "End Review" = "Mid Review"      # Current year's mid review
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
    # For milestone context, "Intern Intro" doesn't have milestone data
    if (!is.null(previous_period) && previous_period == "Intern Intro") {
      return(NA_character_)
    }
  }
  
  return(previous_period)
}

# ============================================================================
# ADD NEW UTILITY FUNCTIONS TO GMED
# ============================================================================

#' Check if Period is Intern Intro
#'
#' Utility function to check if a given period represents the intern introduction period.
#'
#' @param period Character string of period to check
#' @return Logical indicating if period is intern intro
#' @export
is_intern_intro_period <- function(period) {
  if (is.null(period) || is.na(period)) return(FALSE)
  
  return(period %in% c("Intern Intro", "7", "Intro"))
}

#' Get Academic Year from Date
#'
#' Determines the academic year (July 1 - June 30) for a given date.
#'
#' @param date Date object (defaults to current date)
#' @return Numeric academic year (starting year)
#' @export
get_academic_year <- function(date = Sys.Date()) {
  if (format(date, "%m-%d") >= "07-01") {
    return(as.numeric(format(date, "%Y")))
  } else {
    return(as.numeric(format(date, "%Y")) - 1)
  }
}

