# ============================================================================
# LEVEL-AT-TIME DATA PROCESSOR (FIXED VERSION)
# Calculates resident level at time of assessment/evaluation
# ============================================================================

#' Add Level-at-Time to Assessment Data
#'
#' Calculates what level each resident was at the time of their assessments
#' and adds ass_level, fac_eval_level, and q_level columns
#'
#' @param assessment_data Combined assessment data from all forms
#' @param residents Residents data with type, grad_yr, record_id
#' @return Assessment data with level-at-time columns added
#' @export
add_level_at_time_to_assessments <- function(assessment_data, residents) {
  
  if (is.null(assessment_data) || nrow(assessment_data) == 0) {
    return(assessment_data)
  }
  
  if (is.null(residents) || nrow(residents) == 0) {
    warning("No residents data provided for level calculation")
    return(assessment_data)
  }
  
  cat("🕐 Adding level-at-time to assessment data...\n")
  
  # Prepare resident lookup with type and grad_yr
  resident_lookup <- residents %>%
    dplyr::select(record_id, type, grad_yr) %>%
    dplyr::filter(!is.na(record_id))
  
  cat("📋 Resident lookup prepared with", nrow(resident_lookup), "residents\n")
  
  # Initialize the level columns as numeric
  assessment_data$ass_level <- NA_integer_
  assessment_data$fac_eval_level <- NA_integer_
  assessment_data$q_level <- NA_integer_
  
  # Process each assessment type with its corresponding date field
  
  # 1. Assessment forms (use ass_date)
  cat("🔄 Processing Assessment forms with ass_date...\n")
  assessment_rows <- which(!is.na(assessment_data$redcap_repeat_instrument) & 
                             assessment_data$redcap_repeat_instrument == "Assessment" &
                             !is.na(assessment_data$ass_date))
  
  if (length(assessment_rows) > 0) {
    assessment_subset <- assessment_data[assessment_rows, ]
    assessment_subset_with_level <- calculate_level_at_specific_time(
      assessment_subset, resident_lookup, "ass_date"
    )
    assessment_data$ass_level[assessment_rows] <- assessment_subset_with_level$level_at_time
    cat("✅ Added ass_level to", length(assessment_rows), "Assessment records\n")
  }
  
  # 2. Faculty Evaluation forms (use fac_eval_date or similar)
  cat("🔄 Processing Faculty Evaluation forms...\n")
  faculty_rows <- which((assessment_data$source_form == "faculty_evaluation" | 
                           (!is.na(assessment_data$redcap_repeat_instrument) & 
                              assessment_data$redcap_repeat_instrument == "Faculty Evaluation")))
  
  if (length(faculty_rows) > 0) {
    # Look for faculty evaluation date field
    faculty_date_fields <- c("fac_eval_date", "eval_date", "Date", "date")
    found_fac_date_field <- faculty_date_fields[faculty_date_fields %in% names(assessment_data)][1]
    
    if (!is.na(found_fac_date_field)) {
      faculty_subset <- assessment_data[faculty_rows, ]
      faculty_rows_with_date <- faculty_rows[!is.na(faculty_subset[[found_fac_date_field]])]
      
      if (length(faculty_rows_with_date) > 0) {
        faculty_subset_with_date <- assessment_data[faculty_rows_with_date, ]
        faculty_subset_with_level <- calculate_level_at_specific_time(
          faculty_subset_with_date, resident_lookup, found_fac_date_field
        )
        assessment_data$fac_eval_level[faculty_rows_with_date] <- faculty_subset_with_level$level_at_time
        cat("✅ Added fac_eval_level to", length(faculty_rows_with_date), "Faculty Evaluation records using", found_fac_date_field, "\n")
      }
    } else {
      cat("⚠️ No date field found for Faculty Evaluations\n")
    }
  }
  
  # 3. Questions forms (use q_date or similar)
  cat("🔄 Processing Questions forms...\n")
  questions_rows <- which(assessment_data$source_form == "questions" | 
                            (!is.na(assessment_data$redcap_repeat_instrument) & 
                               assessment_data$redcap_repeat_instrument == "Questions"))
  
  if (length(questions_rows) > 0) {
    # Look for questions date field
    questions_date_fields <- c("q_date", "questions_date", "Date", "date")
    found_q_date_field <- questions_date_fields[questions_date_fields %in% names(assessment_data)][1]
    
    if (!is.na(found_q_date_field)) {
      questions_subset <- assessment_data[questions_rows, ]
      questions_rows_with_date <- questions_rows[!is.na(questions_subset[[found_q_date_field]])]
      
      if (length(questions_rows_with_date) > 0) {
        questions_subset_with_date <- assessment_data[questions_rows_with_date, ]
        questions_subset_with_level <- calculate_level_at_specific_time(
          questions_subset_with_date, resident_lookup, found_q_date_field
        )
        assessment_data$q_level[questions_rows_with_date] <- questions_subset_with_level$level_at_time
        cat("✅ Added q_level to", length(questions_rows_with_date), "Questions records using", found_q_date_field, "\n")
      }
    } else {
      cat("⚠️ No date field found for Questions\n")
    }
  }
  
  # Summary
  ass_level_count <- sum(!is.na(assessment_data$ass_level))
  fac_level_count <- sum(!is.na(assessment_data$fac_eval_level)) 
  q_level_count <- sum(!is.na(assessment_data$q_level))
  
  cat("📊 Level-at-time summary:\n")
  cat("   ass_level:", ass_level_count, "records\n")
  cat("   fac_eval_level:", fac_level_count, "records\n") 
  cat("   q_level:", q_level_count, "records\n")
  
  return(assessment_data)
}

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
#' @export
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
#' @export
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
#' @export
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

#' Count Assessment Completions with Level-at-Time
#'
#' Modified versions of the assessment counting functions that use
#' the appropriate level-at-time columns
#'
#' @param data Data frame containing assessment data with level-at-time columns
#' @return Data frame with assessment_type, Level, and Count columns
#' @export
count_assessment_completions_with_time_level <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      assessment_type = character(0), 
      Level = character(0), 
      Count = numeric(0)
    ))
  }
  
  # Filter for Assessment repeat instrument only
  assessment_data <- data %>%
    dplyr::filter(!is.na(redcap_repeat_instrument) & 
                    redcap_repeat_instrument == "Assessment") %>%
    dplyr::mutate(
      # Use ass_level if available, otherwise fall back to current Level
      analysis_level = get_smart_level(., "assessment")
    ) %>%
    dplyr::filter(!is.na(analysis_level))
  
  if (nrow(assessment_data) == 0) {
    return(data.frame(
      assessment_type = character(0), 
      Level = character(0), 
      Count = numeric(0)
    ))
  }
  
  # Define assessment completion fields
  assessment_fields <- list(
    "Continuity Clinic" = c("ass_cc_complete", "cc_complete"),
    "Inpatient" = c("ass_int_ip_complete", "ass_res_ip_complete", "ip_complete"),
    "Observations" = c("ass_obs_complete", "obs_complete"),
    "Bridge Clinic" = c("ass_bridge_complete", "bridge_complete"),
    "Consults" = c("ass_cons_complete", "cons_complete"),
    "Single Day Clinic" = c("ass_day_complete", "day_complete")
  )
  
  # Count completions by type and level-at-time
  results <- list()
  
  for (assessment_type in names(assessment_fields)) {
    fields_to_check <- assessment_fields[[assessment_type]]
    existing_fields <- fields_to_check[fields_to_check %in% names(assessment_data)]
    
    if (length(existing_fields) > 0) {
      type_data <- assessment_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          completed = any(sapply(existing_fields, function(field) {
            val <- .data[[field]]
            !is.na(val) && val != "" && val != 0 && 
              !val %in% c("Incomplete", "No", "N/A", "0")
          }), na.rm = TRUE)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(completed) %>%
        dplyr::group_by(analysis_level) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::mutate(
          assessment_type = assessment_type,
          Level = numeric_level_to_label(analysis_level)  # Convert to readable labels
        ) %>%
        dplyr::select(assessment_type, Level, Count)
      
      if (nrow(type_data) > 0) {
        results[[assessment_type]] <- type_data
      }
    }
  }
  
  if (length(results) == 0) {
    return(data.frame(
      assessment_type = character(0), 
      Level = character(0), 
      Count = numeric(0)
    ))
  }
  
  # Combine and return
  combined_results <- dplyr::bind_rows(results) %>%
    dplyr::select(assessment_type, Level, Count) %>%
    dplyr::arrange(assessment_type, Level)
  
  return(combined_results)
}

#' Count Faculty Evaluations with Level-at-Time
#'
#' @param data Data frame containing faculty evaluation data with level-at-time columns
#' @return Data frame with rotation, Level, and Count columns
#' @export
count_faculty_evaluations_with_time_level <- function(data) {
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      rotation = character(0), 
      Level = character(0), 
      Count = numeric(0)
    ))
  }
  
  # Filter for Faculty Evaluation form
  faculty_data <- data %>%
    dplyr::filter(
      source_form == "faculty_evaluation" | 
        (!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "Faculty Evaluation")
    ) %>%
    dplyr::mutate(
      # Use fac_eval_level if available, otherwise fall back to current Level
      analysis_level = get_smart_level(., "faculty_evaluation")
    ) %>%
    dplyr::filter(!is.na(analysis_level))
  
  if (nrow(faculty_data) == 0) {
    return(data.frame(
      rotation = character(0), 
      Level = character(0), 
      Count = numeric(0)
    ))
  }
  
  # Look for rotation field
  rotation_field <- NULL
  rotation_candidates <- c("att_rot", "Rotation", "rot", "service", "rotation")
  
  for (field in rotation_candidates) {
    if (field %in% names(faculty_data)) {
      rotation_field <- field
      break
    }
  }
  
  if (is.null(rotation_field)) {
    # If no rotation field, group by level only
    result <- faculty_data %>%
      dplyr::group_by(analysis_level) %>%
      dplyr::summarise(Count = n(), .groups = "drop") %>%
      dplyr::mutate(
        rotation = "All Rotations",
        Level = numeric_level_to_label(analysis_level)  # Convert to readable labels
      ) %>%
      dplyr::select(rotation, Level, Count)
    
    return(result)
  }
  
  # Count by rotation and level-at-time
  result <- faculty_data %>%
    dplyr::filter(!is.na(!!rlang::sym(rotation_field)) & 
                    !!rlang::sym(rotation_field) != "") %>%
    dplyr::group_by(!!rlang::sym(rotation_field), analysis_level) %>%
    dplyr::summarise(Count = n(), .groups = "drop") %>%
    dplyr::mutate(
      Level = numeric_level_to_label(analysis_level)  # Convert to readable labels
    ) %>%
    dplyr::rename(rotation = !!rlang::sym(rotation_field)) %>%
    dplyr::select(rotation, Level, Count) %>%
    dplyr::arrange(rotation, Level)
  
  return(result)
}