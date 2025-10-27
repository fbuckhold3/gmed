#' Calculate Current PGY Year and Self-Evaluation Period
#'
#' Determines resident's current PGY year and appropriate self-evaluation period
#' based on graduation year, residency type, and current date within academic year.
#'
#' @param grad_yr Numeric. Year of expected graduation (e.g., 2026)
#' @param type Character. Residency type: "Categorical" or "Preliminary"
#' @param current_date Date. Current date for calculation (default: Sys.Date())
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
#' @export
#'
#' @examples
#' # Categorical resident graduating 2026, checked in December 2024
#' calculate_pgy_and_period(2026, "Categorical", as.Date("2024-12-15"))
#' # Returns: PGY2, period 3 (Mid PGY2)
#'
#' # Preliminary resident graduating 2025, checked in January 2025
#' calculate_pgy_and_period(2025, "Preliminary", as.Date("2025-01-15"))
#' # Returns: PGY1, period 1 (Mid Intern)
#'
#' # Recent graduate (after June 30)
#' calculate_pgy_and_period(2024, "Categorical", as.Date("2024-08-01"))
#' # Returns: Graduated, period NA, is_valid = FALSE
calculate_pgy_and_period <- function(grad_yr, 
                                     type = "Categorical",
                                     current_date = Sys.Date()) {
  
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
  
  # Standardize type
  type <- tolower(trimws(as.character(type)))
  is_preliminary <- grepl("prelim", type)
  
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
  # Period cutoffs:
  # July 1 - Sept 30: Period 7 (Entering)
  # Oct 1 - Jan 31: Period 1, 3, or 5 (Mid year)
  # Feb 1 - June 30: Period 2, 4, or 6 (End year)
  
  period_number <- NA
  period_name <- "Unknown"
  
  if (pgy_year == 1) {
    # PGY1 periods
    if (current_month >= 7 && current_month <= 9) {
      period_number <- 7
      period_name <- "Entering Residency"
    } else if (current_month >= 10 || current_month == 1) {
      period_number <- 1
      period_name <- "Mid Intern"
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 2
      period_name <- "End Intern"
    }
  } else if (pgy_year == 2 && !is_preliminary) {
    # PGY2 periods (categorical only)
    if (current_month >= 10 || current_month == 1) {
      period_number <- 3
      period_name <- "Mid PGY2"
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 4
      period_name <- "End PGY2"
    } else if (current_month >= 7 && current_month <= 9) {
      # Early PGY2 - use previous period (End Intern)
      period_number <- 2
      period_name <- "End Intern"
    }
  } else if (pgy_year == 3 && !is_preliminary) {
    # PGY3 periods (categorical only)
    if (current_month >= 10 || current_month == 1) {
      period_number <- 5
      period_name <- "Mid PGY3"
    } else if (current_month >= 2 && current_month <= 6) {
      period_number <- 6
      period_name <- "Graduating"
    } else if (current_month >= 7 && current_month <= 9) {
      # Early PGY3 - use previous period (End PGY2)
      period_number <- 4
      period_name <- "End PGY2"
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
  
  return(list(
    pgy_year = pgy_year,
    period_number = period_number,
    period_name = period_name,
    academic_year = academic_year,
    months_into_year = round(months_into_year, 1),
    is_valid = !is.na(period_number),
    grad_yr = grad_yr,
    type = if (is_preliminary) "Preliminary" else "Categorical"
  ))
}


#' Get Period Label from Period Number
#'
#' Simple helper to convert period number to readable label
#'
#' @param period_number Numeric period number (7, 1-6)
#' @return Character period label
#' @export
#'
#' @examples
#' get_period_label(7)  # "Entering Residency"
#' get_period_label(1)  # "Mid Intern"
get_period_label <- function(period_number) {
  labels <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduating",
    "7" = "Entering Residency"
  )
  
  if (is.na(period_number)) return("Unknown")
  
  labels[as.character(period_number)] %||% "Unknown"
}


#' Get All Possible Periods for a Resident
#'
#' Returns vector of all period numbers a resident will experience
#' based on their type
#'
#' @param type Character. "Categorical" or "Preliminary"
#' @return Numeric vector of period numbers
#' @export
#'
#' @examples
#' get_resident_periods("Categorical")  # c(7, 1, 2, 3, 4, 5, 6)
#' get_resident_periods("Preliminary")  # c(7, 1, 2)
get_resident_periods <- function(type) {
  type <- tolower(trimws(as.character(type)))
  is_preliminary <- grepl("prelim", type)
  
  if (is_preliminary) {
    return(c(7, 1, 2))
  } else {
    return(c(7, 1, 2, 3, 4, 5, 6))
  }
}