# ============================================================================
# LEVEL-AT-TIME DATA PROCESSOR (FIXED VERSION)
# Calculates resident level at time of assessment/evaluation
# ============================================================================

#' Calculate Level at Specific Time (FIXED VERSION)
#'
#' Helper function that calculates resident level at a specific date
#' FIXED: Avoids .data[[]] dplyr issue by extracting column values first
#'
#' @param data Data frame with resident records and dates
#' @param resident_lookup Lookup table with record_id, type, grad_yr
#' @param date_col_name Name of the date column to use
#' @return Data frame with level_at_time column added
calculate_level_at_specific_time <- function(data, resident_lookup, date_col_name) {

  # Check if date column exists
  if (!date_col_name %in% names(data)) {
    warning("Date column '", date_col_name, "' not found in data")
    data$level_at_time <- NA_integer_
    return(data)
  }

  # Join with resident lookup to get type and grad_yr
  data_with_resident_info <- data %>%
    dplyr::left_join(
      resident_lookup %>% dplyr::select(record_id, type, grad_yr),
      by = "record_id"
    )

  # FIXED: Extract the date column values BEFORE the mutate call
  # This avoids the .data[[date_col_name]] issue that was causing errors
  date_values <- data_with_resident_info[[date_col_name]]

  # Calculate level at time of data collection
  data_with_level <- data_with_resident_info %>%
    dplyr::mutate(
      # Convert date column to Date type - use the extracted values
      collection_date = as.Date(date_values),

      # Convert grad_yr to numeric
      grad_yr_numeric = suppressWarnings(as.numeric(grad_yr)),

      # Calculate level based on date, type, and graduation year
      level_at_time = dplyr::case_when(
        # Missing data
        is.na(collection_date) | is.na(type) | is.na(grad_yr_numeric) ~ NA_integer_,

        # Preliminary residents are always Intern (1)
        tolower(type) == "preliminary" ~ 1L,

        # Categorical residents - calculate based on academic year at collection date
        tolower(type) == "categorical" ~ {
          # Determine academic year of the collection date (July 1 start)
          academic_year <- ifelse(
            format(collection_date, "%m-%d") >= "07-01",
            as.numeric(format(collection_date, "%Y")),
            as.numeric(format(collection_date, "%Y")) - 1
          )

          # Calculate level based on years until graduation
          years_to_grad <- grad_yr_numeric - academic_year

          dplyr::case_when(
            years_to_grad == 3 ~ 1L,        # 3 years until graduation = PGY1 (Intern)
            years_to_grad == 2 ~ 2L,        # 2 years until graduation = PGY2
            years_to_grad == 1 ~ 3L,        # 1 year until graduation = PGY3
            years_to_grad <= 0 ~ 4L,        # Past graduation (Graduated)
            years_to_grad > 3 ~ 0L,         # Before starting residency (Pre-Intern)
            TRUE ~ NA_integer_
          )
        },

        # Other types or rotators
        tolower(type) == "rotator" ~ 5L,    # Rotator = 5
        TRUE ~ NA_integer_
      )
    ) %>%
    # Remove temporary columns
    dplyr::select(-collection_date, -grad_yr_numeric, -type, -grad_yr)

  return(data_with_level)
}

#' Get Smart Level Column for Analysis
#'
#' Returns the appropriate level column to use for a given assessment record,
#' falling back to calculated current level if level-at-time is not available
#'
#' @param assessment_data Assessment data with level columns
#' @param form_type "assessment", "faculty_evaluation", or "questions"
#' @return Vector of levels to use for analysis
get_smart_level <- function(assessment_data, form_type = "assessment") {

  level_col <- switch(form_type,
                      "assessment" = "ass_level",
                      "faculty_evaluation" = "fac_eval_level",
                      "questions" = "q_level",
                      "ass_level"  # default
  )

  if (!level_col %in% names(assessment_data)) {
    warning("Level column '", level_col, "' not found, using current Level")
    return(assessment_data$Level %||% "Unknown")
  }

  # Use level-at-time if available, otherwise convert current Level to numeric
  if (!level_col %in% names(assessment_data)) {
    # Convert current Level text to numeric for consistency
    current_level_numeric <- label_to_numeric_level(assessment_data$Level %||% "Unknown")
    return(current_level_numeric)
  }

  # Use level-at-time if available, otherwise fall back to current Level (converted to numeric)
  current_level_numeric <- label_to_numeric_level(assessment_data$Level %||% "Unknown")
  smart_level <- ifelse(
    is.na(assessment_data[[level_col]]),
    current_level_numeric,
    assessment_data[[level_col]]
  )

  return(smart_level)
}

#' Convert Numeric Level to Label
#'
#' Converts numeric level codes to readable labels for display
#'
#' @param numeric_level Numeric level (1=Intern, 2=PGY2, 3=PGY3, etc.)
#' @return Character label
numeric_level_to_label <- function(numeric_level) {
  dplyr::case_when(
    is.na(numeric_level) ~ "Unknown",
    numeric_level == 0 ~ "Pre-Intern",
    numeric_level == 1 ~ "Intern",
    numeric_level == 2 ~ "PGY2",
    numeric_level == 3 ~ "PGY3",
    numeric_level == 4 ~ "Graduated",
    numeric_level == 5 ~ "Rotator",
    TRUE ~ "Unknown"
  )
}

#' Convert Label to Numeric Level
#'
#' Converts readable labels to numeric level codes for REDCap
#'
#' @param level_label Character label (Intern, PGY2, PGY3, etc.)
#' @return Numeric level code
label_to_numeric_level <- function(level_label) {
  dplyr::case_when(
    is.na(level_label) ~ NA_integer_,
    tolower(level_label) %in% c("intern", "pgy1", "pgy-1") ~ 1L,
    tolower(level_label) %in% c("pgy2", "pgy-2") ~ 2L,
    tolower(level_label) %in% c("pgy3", "pgy-3") ~ 3L,
    tolower(level_label) %in% c("graduated", "graduate") ~ 4L,
    tolower(level_label) %in% c("rotator", "rotation") ~ 5L,
    tolower(level_label) %in% c("pre-intern", "preintern") ~ 0L,
    TRUE ~ NA_integer_
  )
}
