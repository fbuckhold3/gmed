# Update these functions in your gmed package R/period_mapping.R file

#' Get Current Period Based on Date
#'
#' Determines the current evaluation period based on the current date
#' using the academic calendar (July 1 - June 30).
#' 
#' Period Calendar:
#' - Intern Intro: July 1 - September 30 (only for Interns)
#' - Mid Review: October 1 - January 31 (all levels)
#' - End Review: February 1 - June 30 (all levels)
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
  
  month <- as.numeric(format(reference_date, "%m"))
  
  if (month >= 7 && month <= 9) {
    # July-September: Intern Intro period
    return("Intern Intro")
  } else if (month >= 10 || month <= 1) {
    # October-January: Mid Review for all levels
    return("Mid Review")
  } else {
    # February-June: End Review for all levels
    return("End Review")
  }
}

#' Map Application Period to Milestone Period
#'
#' Maps the app's period and resident level to the appropriate milestone period.
#' Correctly handles that only Interns get "Intern Intro" reviews.
#'
#' @param level Character string indicating resident level ("Intern", "PGY2", "PGY3")
#' @param period Character string indicating the period in app format
#' @param form_context Optional context for specialized mapping
#'
#' @return Character string of mapped period or NA if no mapping exists
#' @export
map_to_milestone_period <- function(level, period, form_context = NULL) {
  
  if (is.null(level) || is.null(period) || level == "" || period == "") {
    return(NA_character_)
  }
  
  # Normalize inputs
  level <- trimws(as.character(level))
  period <- trimws(as.character(period))
  
  # INTERN MAPPINGS
  if (level == "Intern") {
    if (period == "Intern Intro") {
      return("Intern Intro")
    } else if (period == "Mid Review") {
      return("Mid Intern")
    } else if (period == "End Review") {
      return("End Intern")
    }
  }
  
  # PGY2 MAPPINGS
  else if (level == "PGY2") {
    # PGY2s don't do Intern Intro reviews
    if (period == "Intern Intro") {
      # During July-Sept, PGY2s aren't reviewed
      # Return NA or their next review period depending on use case
      if (!is.null(form_context) && form_context == "display") {
        return("No Review This Period")
      }
      return(NA_character_)
    } else if (period == "Mid Review") {
      return("Mid PGY2")
    } else if (period == "End Review") {
      return("End PGY2")
    }
  }
  
  # PGY3 MAPPINGS
  else if (level == "PGY3") {
    # PGY3s don't do Intern Intro reviews
    if (period == "Intern Intro") {
      # During July-Sept, PGY3s aren't reviewed
      # Return NA or their next review period depending on use case
      if (!is.null(form_context) && form_context == "display") {
        return("No Review This Period")
      }
      return(NA_character_)
    } else if (period == "Mid Review") {
      return("Mid PGY3")
    } else if (period == "End Review") {
      return("Graduation")
    }
  }
  
  # Unknown level
  message(sprintf("Unknown level '%s' for period '%s'", level, period))
  return(NA_character_)
}

#' Map Period to REDCap Instance Number
#'
#' Converts milestone period names to REDCap instance numbers.
#'
#' REDCap Instance Mapping:
#' - 1 = Mid Intern
#' - 2 = End Intern
#' - 3 = Mid PGY2
#' - 4 = End PGY2
#' - 5 = Mid PGY3
#' - 6 = Graduation (End PGY3)
#' - 7 = Intern Intro
#'
#' @param milestone_period Character string of milestone period
#' @return Character string of instance number or NA
#' @export
map_milestone_to_instance <- function(milestone_period) {
  
  if (is.null(milestone_period) || is.na(milestone_period)) {
    return(NA_character_)
  }
  
  instance_map <- c(
    "Intern Intro" = "7",
    "Mid Intern" = "1",
    "End Intern" = "2",
    "Mid PGY2" = "3",
    "End PGY2" = "4",
    "Mid PGY3" = "5",
    "Graduation" = "6"
  )
  
  instance <- instance_map[milestone_period]
  
  if (is.na(instance)) {
    message(sprintf("No instance mapping for milestone period: %s", milestone_period))
    return(NA_character_)
  }
  
  return(unname(instance))
}

#' Map Application Period to Coach Period Instance
#'
#' Convenience function that combines level/period mapping to get REDCap instance.
#'
#' @param app_period Current period from get_current_period()
#' @param resident_level Resident level ("Intern", "PGY2", "PGY3")
#' @return Character string of REDCap instance number
#' @export
map_app_period_to_coach_period <- function(app_period, resident_level) {
  
  # Get milestone period first
  milestone_period <- map_to_milestone_period(resident_level, app_period)
  
  # If no milestone period (e.g., PGY2/3 during Intern Intro), return NA
  if (is.na(milestone_period)) {
    return(NA_character_)
  }
  
  # Convert to instance
  return(map_milestone_to_instance(milestone_period))
}

#' Check if Resident Should Be Reviewed
#'
#' Determines if a resident should have a review based on their level and current period.
#'
#' @param level Resident level ("Intern", "PGY2", "PGY3")
#' @param period Current period from get_current_period()
#' @return Logical indicating if resident should be reviewed
#' @export
should_resident_be_reviewed <- function(level, period) {
  
  if (is.null(level) || is.null(period)) {
    return(FALSE)
  }
  
  level <- trimws(as.character(level))
  period <- trimws(as.character(period))
  
  # Interns get all three reviews
  if (level == "Intern") {
    return(TRUE)
  }
  
  # PGY2 and PGY3 only get Mid and End reviews
  if (level %in% c("PGY2", "PGY3")) {
    if (period == "Intern Intro") {
      return(FALSE)  # No review during Intern Intro period
    } else {
      return(TRUE)  # Reviews during Mid and End periods
    }
  }
  
  # Unknown level - default to no review
  return(FALSE)
}

#' Get Previous Period
#'
#' Returns the previous evaluation period for a given level and current period.
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
  
  # Define previous period relationships
  previous_map <- list(
    "Intern" = list(
      "Mid Review" = "Intern Intro",
      "End Review" = "Mid Review"
    ),
    "PGY2" = list(
      "Mid Review" = "End Review",  # Previous year's end review (as Intern)
      "End Review" = "Mid Review"   # Current year's mid review
    ),
    "PGY3" = list(
      "Mid Review" = "End Review",  # Previous year's end review (as PGY2)  
      "End Review" = "Mid Review"   # Current year's mid review
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

# Add this to your gmed package, probably in R/period_mapping.R or R/helpers.R

#' Get Most Recent Available Period for Resident
#'
#' Automatically determines the most recent milestone period that has data
#' for a specific resident, following the standard evaluation hierarchy.
#' Used across gmed-based applications for consistent period selection.
#'
#' @param resident_info List containing resident information (record_id, Level, etc.)
#' @param app_data Complete app data structure (from load_rdm_complete or similar)
#' @param verbose Logical. Print selection details for debugging
#' @return Character string of the most recent period with data
#' @export
#' @examples
#' \dontrun{
#' # In a Shiny app
#' period <- get_most_recent_period_for_resident(resident_info(), app_data())
#' 
#' # With debugging
#' period <- get_most_recent_period_for_resident(resident_info(), app_data(), verbose = TRUE)
#' }
get_most_recent_period_for_resident <- function(resident_info, app_data, verbose = FALSE) {
  
  resident_level <- resident_info$Level %||% "Unknown"
  resident_id <- resident_info$record_id
  
  if (verbose) message("Finding most recent period for resident ", resident_id, " (", resident_level, ")")
  
  # Standard evaluation period hierarchy (most recent first)
  period_hierarchy <- c("Graduating", "Mid PGY3", "End PGY2", "Mid PGY2", "End Intern", "Mid Intern")
  
  # Check what periods actually have data for this resident
  available_periods <- c()
  
  # Check across all milestone forms
  if (!is.null(app_data$all_forms)) {
    milestone_forms <- c("milestone_entry", "milestone_selfevaluation_c33c", "acgme_miles", "acgme_entry")
    
    for (form_name in milestone_forms) {
      if (form_name %in% names(app_data$all_forms)) {
        form_data <- app_data$all_forms[[form_name]]
        
        if (!is.null(form_data) && nrow(form_data) > 0 && resident_id %in% form_data$record_id) {
          # Get period column (flexible naming)
          period_cols <- grep("period", names(form_data), ignore.case = TRUE, value = TRUE)
          
          if (length(period_cols) > 0) {
            resident_periods <- form_data %>%
              dplyr::filter(record_id == !!resident_id) %>%
              dplyr::pull(!!period_cols[1]) %>%
              unique() %>%
              na.omit()
            
            available_periods <- c(available_periods, resident_periods)
            
            if (verbose && length(resident_periods) > 0) {
              message("  Found periods in ", form_name, ": ", paste(resident_periods, collapse = ", "))
            }
          }
        }
      }
    }
  }
  
  # Remove duplicates and clean up
  available_periods <- unique(available_periods[!is.na(available_periods) & available_periods != ""])
  
  if (verbose) {
    message("  All available periods: ", paste(available_periods, collapse = ", "))
  }
  
  # Find the most recent period that exists in our data
  for (period in period_hierarchy) {
    if (period %in% available_periods) {
      if (verbose) message("  Selected most recent period: ", period)
      return(period)
    }
  }
  
  # Fallback based on resident level if no data periods found
  fallback_period <- dplyr::case_when(
    grepl("PGY3|Graduating", resident_level, ignore.case = TRUE) ~ "Graduating",
    grepl("PGY2", resident_level, ignore.case = TRUE) ~ "End PGY2",
    grepl("Intern", resident_level, ignore.case = TRUE) ~ "End Intern",
    TRUE ~ "End PGY2"
  )
  
  if (verbose) {
    message("  No data periods found, using level-based fallback: ", fallback_period)
  }
  
  return(fallback_period)
}



