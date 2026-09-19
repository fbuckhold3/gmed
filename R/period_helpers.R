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

#' Get Period Label
#'
#' Convert period code to human-readable label
#'
#' @param period_code Period code (1-7)
#' @return Period label
get_period_label <- function(period_code) {
  normalize_period(period_code, "label")
}
