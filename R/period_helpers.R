#' Period Normalization and Comparison Helpers
#' 
#' Functions to handle both raw (numeric) and label (text) period formats
#'
#' @name period_helpers
NULL

#' Normalize Period Value
#' 
#' Converts between period codes (1-7) and labels ("Mid Intern", etc.)
#' Works with both raw and label data formats
#'
#' @param period_value Period as number or text
#' @param output_format "code" or "label"
#' @return Normalized period value
#' @export
normalize_period <- function(period_value, output_format = "code") {
  
  period_map <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduating",
    "7" = "Entering Residency"
  )
  
  # Handle vectors - return vector of results
  if (length(period_value) > 1) {
    return(sapply(period_value, normalize_period, output_format = output_format, USE.NAMES = FALSE))
  }
  
  if (is.na(period_value) || period_value == "") return(NA_character_)
  
  period_str <- as.character(period_value)
  
  if (output_format == "code") {
    # Convert to code
    if (period_str %in% names(period_map)) {
      return(period_str)  # Already a code
    } else if (period_str %in% period_map) {
      # It's a label, convert to code
      return(names(period_map)[period_map == period_str])
    }
  } else {
    # Convert to label
    if (period_str %in% period_map) {
      return(period_str)  # Already a label
    } else if (period_str %in% names(period_map)) {
      # It's a code, convert to label
      return(unname(period_map[period_str]))
    }
  }
  
  return(period_value)  # Return as-is if can't convert
}

#' Check if Period Matches
#'
#' Flexible period comparison that works with both codes and labels
#'
#' @param period1 First period (code or label)
#' @param period2 Second period (code or label)
#' @return Logical
#' @export
periods_match <- function(period1, period2) {
  code1 <- normalize_period(period1, "code")
  code2 <- normalize_period(period2, "code")
  return(!is.na(code1) && !is.na(code2) && code1 == code2)
}

#' Get Period Label
#'
#' Convert period code to human-readable label
#'
#' @param period_code Period code (1-7)
#' @return Period label
#' @export
get_period_label <- function(period_code) {
  normalize_period(period_code, "label")
}

#' Get Period Code
#'
#' Convert period label to numeric code
#'
#' @param period_label Period label
#' @return Period code
#' @export
get_period_code <- function(period_label) {
  normalize_period(period_label, "code")
}