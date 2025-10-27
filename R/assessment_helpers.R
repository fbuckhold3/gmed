# Assessment Data Helper Functions
# Add to: R/assessment_helpers.R

#' Get Assessment Fields by Type
#'
#' Returns the relevant field names for a specific assessment type
#'
#' @param data_dict Data dictionary from load_rdm_complete()
#' @param eval_type Evaluation type: "cc", "day", "cons", "int_ip", "res_ip", "bridge", "obs", or "all"
#' @param obs_subtype For observations, specify subtype: "cdm", "acp", "educat", "pe", etc.
#'
#' @return Character vector of field names
#' @export
get_assessment_fields <- function(data_dict, eval_type = "all", obs_subtype = NULL) {
  
  # Base fields always included
  base_fields <- c(
    "record_id", "ass_date", "ass_level", 
    "ass_faculty", "ass_specialty", 
    "ass_plus", "ass_delta", "ass_rotator"
  )
  
  # Return all if requested
  if (eval_type == "all") {
    all_fields <- data_dict %>%
      dplyr::filter(form_name == "assessment") %>%
      dplyr::pull(field_name)
    return(all_fields)
  }
  
  # Build pattern for specific type
  if (eval_type == "obs" && !is.null(obs_subtype) && obs_subtype != "all") {
    # Specific observation subtype
    pattern <- paste0("^ass_obs_", obs_subtype)
    extra_fields <- c("ass_obs_type")
  } else if (eval_type == "obs") {
    # All observations
    pattern <- "^ass_obs_"
    extra_fields <- c("ass_obs_type")
  } else {
    # Other evaluation types
    pattern <- paste0("^ass_", eval_type, "_")
    extra_fields <- NULL
  }
  
  # Get matching fields from data dictionary
  type_fields <- data_dict %>%
    dplyr::filter(
      form_name == "assessment",
      grepl(pattern, field_name)
    ) %>%
    dplyr::pull(field_name)
  
  # Combine and return unique fields
  all_fields <- unique(c(base_fields, extra_fields, type_fields))
  return(all_fields)
}

#' Filter Assessment Data by Type
#'
#' Filters assessment data to only include relevant fields and non-empty records
#'
#' @param assessment_data Assessment data frame from load_rdm_complete()
#' @param data_dict Data dictionary
#' @param eval_type Evaluation type
#' @param obs_subtype Observation subtype (optional)
#' @param remove_empty_rows Remove rows where all eval fields are NA (default TRUE)
#' @param remove_empty_cols Remove columns that are entirely NA (default TRUE)
#'
#' @return Filtered data frame
#' @export
filter_assessment_by_type <- function(assessment_data, 
                                     data_dict,
                                     eval_type = "all",
                                     obs_subtype = NULL,
                                     remove_empty_rows = TRUE,
                                     remove_empty_cols = TRUE) {
  
  # Get relevant fields
  eval_fields <- get_assessment_fields(data_dict, eval_type, obs_subtype)
  
  # Select only available fields
  available_fields <- intersect(eval_fields, names(assessment_data))
  
  result <- assessment_data %>%
    dplyr::select(dplyr::all_of(available_fields))
  
  # Remove empty rows if requested
  if (remove_empty_rows) {
    # Define core metadata fields
    metadata_fields <- c("record_id", "ass_date", "ass_level", 
                        "ass_faculty", "ass_specialty", 
                        "ass_plus", "ass_delta", "ass_rotator")
    
    # Get evaluation-specific fields
    eval_specific_fields <- setdiff(available_fields, metadata_fields)
    
    if (length(eval_specific_fields) > 0) {
      result <- result %>%
        dplyr::filter(
          dplyr::if_any(
            dplyr::all_of(eval_specific_fields),
            ~ !is.na(.) & . != ""
          )
        )
    }
  }
  
  # Remove empty columns if requested
  if (remove_empty_cols) {
    result <- result %>%
      dplyr::select(where(~ !all(is.na(.) | . == "")))
  }
  
  return(result)
}

#' Get Human-Readable Assessment Labels
#'
#' Creates a named vector mapping field names to human-readable labels
#'
#' @param data_dict Data dictionary from load_rdm_complete()
#' @param form_name Form name (default: "assessment")
#'
#' @return Named character vector (names = field_names, values = labels)
#' @export
get_assessment_labels <- function(data_dict, form_name = "assessment") {
  
  labels <- data_dict %>%
    dplyr::filter(form_name == !!form_name) %>%
    dplyr::select(field_name, field_label) %>%
    tibble::deframe()
  
  return(labels)
}

#' Format Assessment Data for Display
#'
#' Converts assessment data with readable column names and formatting
#'
#' @param assessment_data Assessment data frame
#' @param data_dict Data dictionary
#' @param priority_cols Column names to place first (default: date, level, faculty)
#'
#' @return Formatted data frame
#' @export
format_assessment_display <- function(assessment_data, 
                                     data_dict,
                                     priority_cols = c("ass_date", "ass_level", 
                                                      "ass_faculty", "ass_specialty")) {
  
  if (nrow(assessment_data) == 0) return(assessment_data)
  
  # Get field labels
  field_labels <- get_assessment_labels(data_dict)
  
  # Rename columns
  display_data <- assessment_data
  
  for (col in names(display_data)) {
    if (col %in% names(field_labels)) {
      new_name <- field_labels[col]
      names(display_data)[names(display_data) == col] <- new_name
    }
  }
  
  # Reorder columns - priority columns first
  priority_labels <- field_labels[priority_cols]
  priority_labels <- priority_labels[!is.na(priority_labels)]
  
  available_priority <- intersect(priority_labels, names(display_data))
  other_cols <- setdiff(names(display_data), available_priority)
  
  display_data <- display_data %>%
    dplyr::select(dplyr::all_of(c(available_priority, other_cols)))
  
  return(display_data)
}

#' Get Assessment Type Summary
#'
#' Summarizes assessment counts by type and resident
#'
#' @param assessment_data Assessment data from load_rdm_complete()
#' @param data_dict Data dictionary
#' @param by_resident Include resident-level summary (default TRUE)
#'
#' @return Data frame with summary statistics
#' @export
get_assessment_summary <- function(assessment_data, 
                                  data_dict,
                                  by_resident = TRUE) {
  
  # Define evaluation types and their patterns
  eval_types <- list(
    "Continuity Clinic" = "^ass_cc_",
    "Single Clinic Days" = "^ass_day_",
    "Consults" = "^ass_cons_",
    "Intern Inpatient" = "^ass_int_ip_",
    "Resident Inpatient" = "^ass_res_ip_",
    "Bridge Clinic" = "^ass_bridge_",
    "Observations" = "^ass_obs_"
  )
  
  # Count assessments by type
  summary_list <- list()
  
  for (eval_name in names(eval_types)) {
    pattern <- eval_types[[eval_name]]
    
    # Get fields for this type
    type_fields <- data_dict %>%
      dplyr::filter(
        form_name == "assessment",
        grepl(pattern, field_name)
      ) %>%
      dplyr::pull(field_name)
    
    # Check which fields exist in data
    available_fields <- intersect(type_fields, names(assessment_data))
    
    if (length(available_fields) > 0) {
      # Count rows with data in these fields
      type_data <- assessment_data %>%
        dplyr::filter(
          dplyr::if_any(
            dplyr::all_of(available_fields),
            ~ !is.na(.) & . != ""
          )
        )
      
      if (by_resident) {
        type_summary <- type_data %>%
          dplyr::group_by(name) %>%
          dplyr::summarise(
            n_assessments = dplyr::n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(eval_type = eval_name)
      } else {
        type_summary <- data.frame(
          eval_type = eval_name,
          n_assessments = nrow(type_data)
        )
      }
      
      summary_list[[eval_name]] <- type_summary
    }
  }
  
  # Combine all summaries
  if (length(summary_list) > 0) {
    result <- dplyr::bind_rows(summary_list)
    
    if (by_resident) {
      result <- result %>%
        dplyr::select(name, eval_type, n_assessments) %>%
        dplyr::arrange(name, eval_type)
    } else {
      result <- result %>%
        dplyr::arrange(dplyr::desc(n_assessments))
    }
  } else {
    result <- data.frame(
      eval_type = character(0),
      n_assessments = numeric(0)
    )
  }
  
  return(result)
}

#' Get Observation Subtypes
#'
#' Returns a mapping of observation codes to full names
#'
#' @return Named character vector
#' @export
get_observation_subtypes <- function() {
  c(
    "cdm" = "Clinical Decision Making",
    "acp" = "Advanced Care Planning",
    "educat" = "Teaching Session",
    "pe" = "Physical Exam",
    "pres" = "Presentation",
    "writehp" = "Written H&P",
    "daily" = "Progress Note",
    "dc" = "Discharge Summary",
    "meet" = "Family Meeting",
    "senior" = "Team Supervision",
    "proc" = "Procedure",
    "mdr" = "Multi-Disciplinary Rounds",
    "emer" = "Emergent Situation",
    "pocus" = "Point of Care Ultrasound"
  )
}

#' Extract Plus/Delta Feedback
#'
#' Extracts and formats plus/delta feedback from assessment data
#'
#' @param assessment_data Assessment data frame
#' @param resident_name Optional: filter to specific resident
#' @param date_range Optional: vector of c(start_date, end_date)
#'
#' @return Data frame with plus/delta feedback
#' @export
extract_plusdelta <- function(assessment_data, 
                              resident_name = NULL,
                              date_range = NULL) {
  
  result <- assessment_data %>%
    dplyr::filter(!is.na(ass_plus) | !is.na(ass_delta))
  
  # Filter by resident if specified
  if (!is.null(resident_name)) {
    result <- result %>%
      dplyr::filter(name == resident_name)
  }
  
  # Filter by date range if specified
  if (!is.null(date_range) && length(date_range) == 2) {
    result <- result %>%
      dplyr::filter(
        ass_date >= date_range[1],
        ass_date <= date_range[2]
      )
  }
  
  # Select relevant columns and clean up
  result <- result %>%
    dplyr::select(
      date = ass_date,
      resident = name,
      level = ass_level,
      faculty = ass_faculty,
      specialty = ass_specialty,
      plus = ass_plus,
      delta = ass_delta
    ) %>%
    dplyr::arrange(dplyr::desc(date))
  
  return(result)
}

#' Get Assessment Field Choices
#'
#' Extracts the choices/options for a specific assessment field
#'
#' @param data_dict Data dictionary
#' @param field_name Field name to get choices for
#' @param return_type "named_vector" (default), "data_frame", or "text"
#'
#' @return Choices in specified format
#' @export
get_field_choices <- function(data_dict, 
                             field_name,
                             return_type = "named_vector") {
  
  field_info <- data_dict %>%
    dplyr::filter(field_name == !!field_name)
  
  if (nrow(field_info) == 0) {
    warning("Field not found: ", field_name)
    return(NULL)
  }
  
  choices_text <- field_info$select_choices_or_calculations[1]
  
  if (is.na(choices_text) || choices_text == "") {
    return(NULL)
  }
  
  # Parse choices (format: "1, Label 1 | 2, Label 2 | ...")
  choices_split <- strsplit(choices_text, "\\|")[[1]]
  choices_split <- trimws(choices_split)
  
  choices_parsed <- lapply(choices_split, function(choice) {
    parts <- strsplit(choice, ",", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      value <- trimws(parts[1])
      label <- trimws(paste(parts[-1], collapse = ","))
      return(c(value = value, label = label))
    }
    return(NULL)
  })
  
  # Remove NULL entries
  choices_parsed <- Filter(Negate(is.null), choices_parsed)
  
  if (length(choices_parsed) == 0) {
    return(NULL)
  }
  
  # Format output
  if (return_type == "named_vector") {
    values <- sapply(choices_parsed, function(x) x["value"])
    labels <- sapply(choices_parsed, function(x) x["label"])
    result <- setNames(values, labels)
    return(result)
    
  } else if (return_type == "data_frame") {
    result <- data.frame(
      value = sapply(choices_parsed, function(x) x["value"]),
      label = sapply(choices_parsed, function(x) x["label"]),
      stringsAsFactors = FALSE
    )
    return(result)
    
  } else if (return_type == "text") {
    return(choices_text)
  }
  
  return(NULL)
}