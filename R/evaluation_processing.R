#' @title Evaluation Data Processing Functions for GMED
#' @description Functions for processing plus/delta, summative evaluations, and other assessment data
#' @name evaluation_processing
NULL

#' Process Summative Evaluation Data
#'
#' Processes summative evaluation data for a specified resident and level. The function filters,
#' renames, and selects relevant columns based on level-specific criteria for summative evaluations.
#' Migrated from function_assessment.R with enhancements.
#'
#' @param data A data frame containing evaluation data
#' @param resident_name A character string specifying the resident's name
#' @param level A character string indicating the level ("Intern", "PGY2", or "PGY3")
#'
#' @return A data frame with processed summative evaluation data if available; otherwise,
#' a data frame containing a message indicating no data or describing any error encountered
#' @export
process_summative_data <- function(data, resident_name, level) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  message("Function called with resident:", resident_name, "and level:", level)
  
  # CC field mappings for summative evaluations
  cc_names <- c(
    `Intern Presentation` = 'cc_intern_pc1_1',
    Differential = 'cc_intern_pc3_1',
    `Health Promotion` = 'cc_intern_pc5_1',
    `Chronic Management` = 'cc_intern_pc5_2',
    `Minimize unfamiliar terms (I)` = 'cc_intern_ics1_2',
    `Shared Decision-Making (I)` = 'cc_intern_ics1_1',
    Respect = 'cc_intern_prof1',
    `Takes feedback (I)` = 'cc_intern_pbl2_1',
    `Acknowledge errors` = 'cc_intern_pbl2_2',
    `Presentation PGY2` = 'cc_pgy2_pc1_1',
    Documentation = 'cc_pgy2_ics3_1',
    `Reflection on practice` = 'cc_pgy2_pbl2_2',
    `Care coordination (2)` = 'cc_pgy2_sbp2_1',
    `Use Evidence (2)` = 'cc_pgy2_pbl1',
    `Shared Decision-Making (2)` = 'cc_pgy2_ics1_1',
    `Teamwork (2)` = 'cc_pgy2_ics2_2',
    `Takes feedback (2)` = 'cc_pgy_pbl2_1',
    `Minimize unfamiliar terms (2)` = 'cc_pgy2_ics_1_2',
    `Presentation PGY3` = 'cc_pgy3_pc1_1'
  )
  
  # Filter data based on level and resident
  level_specific_cols <- switch(level,
                                "Intern" = grep("cc_intern_", names(cc_names), value = TRUE),
                                "PGY2" = grep("cc_pgy2_", names(cc_names), value = TRUE),
                                "PGY3" = grep("cc_pgy3_", names(cc_names), value = TRUE),
                                character(0)
  )
  
  if (length(level_specific_cols) == 0) {
    return(data.frame(
      Message = paste("No summative evaluation configuration for level:", level)
    ))
  }
  
  # Get the actual field names
  actual_fields <- cc_names[names(cc_names) %in% level_specific_cols]
  
  tryCatch({
    # Filter for the specific resident and select relevant columns
    resident_data <- data %>%
      dplyr::filter(name == resident_name) %>%
      dplyr::select(
        dplyr::any_of(c("Date", "Rotation", "Level", actual_fields))
      )
    
    if (nrow(resident_data) == 0) {
      return(data.frame(
        Message = paste("No summative evaluation data found for", resident_name, "at level", level)
      ))
    }
    
    # Rename columns to readable names
    field_mapping <- actual_fields
    names(field_mapping) <- names(actual_fields)
    
    for (readable_name in names(field_mapping)) {
      field_name <- field_mapping[readable_name]
      if (field_name %in% names(resident_data)) {
        names(resident_data)[names(resident_data) == field_name] <- readable_name
      }
    }
    
    return(resident_data)
    
  }, error = function(e) {
    message("Error in process_summative_data: ", e$message)
    return(data.frame(
      Message = paste("Error processing summative data:", e$message)
    ))
  })
}

#' Check Self-Evaluation Completion Status
#'
#' Determines if a resident has completed their self-evaluation for a given period
#' by checking for non-NA values in self-evaluation fields.
#'
#' @param data Resident data containing self-evaluation information
#' @param resident_name Character string of resident name
#' @param period Character string of evaluation period
#' @param level Character string of resident level
#'
#' @return Logical value indicating completion status
#' @export
check_self_eval_complete <- function(data, resident_name, period, level) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(data) || nrow(data) == 0) {
    return(FALSE)
  }
  
  # Map period to self-evaluation period format if needed
  # This should match your period mapping logic
  mapped_period <- map_to_milestone_period(level, period)
  
  # Look for self-evaluation data for this resident and period
  self_eval_data <- data %>%
    dplyr::filter(
      name == resident_name,
      redcap_repeat_instrument == "Milestone Selfevaluation C33c" | is.na(redcap_repeat_instrument)
    )
  
  if (nrow(self_eval_data) == 0) {
    return(FALSE)
  }
  
  # Check for completion based on presence of milestone ratings
  # Look for self-evaluation milestone fields (rep_*_self pattern)
  self_milestone_cols <- grep("^rep_.*_self$", names(self_eval_data), value = TRUE)
  
  if (length(self_milestone_cols) == 0) {
    return(FALSE)
  }
  
  # Check if any self-evaluation milestone fields have values
  has_ratings <- self_eval_data %>%
    dplyr::select(dplyr::all_of(self_milestone_cols)) %>%
    dplyr::summarise(
      any_rating = dplyr::if_any(dplyr::everything(), ~ !is.na(.x) & .x != "")
    ) %>%
    dplyr::pull(any_rating)
  
  return(any(has_ratings, na.rm = TRUE))
}

#' Generate Traditional Plus-Delta Data (Legacy Support)
#'
#' Filters and processes the input data to extract plus-delta evaluations for a specific resident.
#' This is the traditional version for backward compatibility with older data structures.
#'
#' @param data A data frame containing evaluation data
#' @param resident A character string specifying the resident's name
#'
#' @return A data frame containing the Date, Rotation, Level, Plus, Delta, Feedback, and Evaluator
#' @export
generate_p_d <- function(data, resident) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      Date = character(0),
      Rotation = character(0),
      Level = character(0),
      Plus = character(0),
      Delta = character(0),
      Feedback = character(0),
      Evaluator = character(0)
    ))
  }
  
  data %>%
    dplyr::filter(name == resident) %>%
    dplyr::select(Date, Rotation, Level, cc_res_does_well, res_to_improve, min_giv_feedback, Evaluator) %>%
    dplyr::rename(
      Plus = cc_res_does_well,
      Delta = res_to_improve,
      Feedback = min_giv_feedback
    ) %>%
    dplyr::filter(!(is.na(Rotation) & is.na(Plus) & is.na(Delta)))
}

#' Get CC Fields for Quarter and Level
#'
#' Retrieves the appropriate Continuity Clinic evaluation fields based on
#' the quarter and resident level for form building.
#'
#' @param quarter Character string indicating the quarter
#' @param level Character string indicating resident level
#'
#' @return Vector of field names for the specified quarter and level
#' @export
get_cc_fields_for_quarter_and_level <- function(quarter, level) {
  
  # Base field mappings by level
  intern_fields <- c(
    "ass_cc_pc1_r1", "ass_cc_pc5_r1", "ass_cc_pc5_r2", 
    "ass_cc_ics1_r1", "ass_cc_ics1_r2", "ass_cc_prof1", 
    "ass_cc_pbl2_r1", "ass_cc_pbl2_r2"
  )
  
  pgy2_fields <- c(
    "ass_cc_pc1_r1", "ass_cc_ics3_r1", "ass_cc_pbl2_r2",
    "ass_cc_sbp2_r1", "ass_cc_pbl1", "ass_cc_ics1_r1",
    "ass_cc_ics2_r2", "ass_cc_pbl2_r1", "ass_cc_ics1_r2"
  )
  
  pgy3_fields <- c(
    "ass_cc_pc1_r1", "ass_cc_advanced_fields"  # Add specific PGY3 fields as needed
  )
  
  # Return appropriate fields based on level
  level_fields <- switch(level,
                         "Intern" = intern_fields,
                         "PGY2" = pgy2_fields,
                         "PGY3" = pgy3_fields,
                         character(0)
  )
  
  # Add common fields that apply to all quarters/levels
  common_fields <- c("ass_plus", "ass_delta", "ass_date", "ass_faculty", "ass_specialty")
  
  return(c(level_fields, common_fields))
}

#' Get Observation Fields for Type
#'
#' Retrieves the appropriate observation evaluation fields based on
#' the observation type selected in the form.
#'
#' @param obs_type Character string indicating the observation type
#'
#' @return Vector of field names for the specified observation type
#' @export
get_obs_fields_for_type <- function(obs_type) {
  
  # Field mappings by observation type
  obs_field_map <- list(
    "Clinical Decision Making" = c("ass_obs_cdm_pc3", "ass_obs_cdm_mk1", "ass_obs_cdm_sbp1"),
    "Advance Care Planning" = c("ass_obs_acp_ics1", "ass_obs_acp_prof2", "ass_obs_acp_pc5"),
    "Education" = c("ass_educat_ics2", "ass_educat_pbl1", "ass_educat_prof1"),
    "Physical Exam" = c("ass_obs_pe_pc2", "ass_obs_pe_ics1"),
    "Presentation" = c("ass_obs_pres_pc1", "ass_obs_pres_ics2", "ass_obs_pres_mk1"),
    "Written H&P" = c("ass_obs_writehp_pc1", "ass_obs_writehp_pc3", "ass_obs_writehp_ics3"),
    "Daily Note" = c("ass_obs_daily_pc4", "ass_obs_daily_ics3", "ass_obs_daily_sbp2"),
    "Discharge Summary" = c("ass_obs_dc_pc4", "ass_obs_dc_ics3", "ass_obs_dc_sbp2"),
    "Family Meeting" = c("ass_obs_meet_ics1", "ass_obs_meet_prof2", "ass_obs_meet_pc5"),
    "Supervising Resident" = c("ass_obs_senior_ics2", "ass_obs_senior_prof1", "ass_obs_senior_pbl2"),
    "Procedure" = c("ass_obs_proc_pc4", "ass_obs_proc_sbp1", "ass_obs_proc_prof1"),
    "MDR" = c("ass_obs_mdr_ics2", "ass_obs_mdr_pc3", "ass_obs_mdr_mk1"),
    "Emergency Situation" = c("ass_obs_emer_pc4", "ass_obs_emer_sbp1", "ass_obs_emer_ics2")
  )
  
  # Get fields for the specified observation type
  type_fields <- obs_field_map[[obs_type]] %||% character(0)
  
  # Add common observation fields
  common_fields <- c("ass_obs_plus", "ass_obs_delta", "ass_date", "ass_faculty", "ass_specialty", "ass_obs_type")
  
  return(c(type_fields, common_fields))
}

#' Collect Core Evaluation Data
#'
#' Core function for collecting evaluation data with consistent field handling.
#' This is the main collection function used by other specialized collection functions.
#'
#' @param input Shiny input object
#' @param faculty Faculty information
#' @param resident Resident information
#' @param field_names Vector of specific field names for this evaluation type
#'
#' @return List containing collected evaluation data
#' @export
collect_evaluation_data <- function(input, faculty, resident, field_names = character(0)) {
  
  # Core evaluation data that applies to all types
  eval_data <- list(
    # Date and basic info
    ass_date = if (!is.null(input$ass_date)) as.character(input$ass_date) else as.character(Sys.Date()),
    
    # Faculty and resident info
    ass_faculty = faculty$name %||% "Unknown Faculty",
    ass_resident = resident$name %||% "Unknown Resident",
    ass_specialty = input$ass_specialty %||% "Not specified",
    
    # Core assessment fields
    ass_plus = trimws(input$ass_plus %||% ""),
    ass_delta = trimws(input$ass_delta %||% ""),
    
    # Rotation tracking - FIXED to use proper rotator field
    ass_rotator = if (!is.null(input$rotator_select)) {
      input$rotator_select
    } else if (!is.null(input$ass_rotator)) {
      input$ass_rotator  
    } else {
      "Not specified"
    },
    
    # REDCap metadata
    redcap_repeat_instrument = "Assessment",
    record_id = resident$record_id %||% resident$access_code %||% "Unknown"
  )
  
  # Add specific fields for this evaluation type
  for (field_name in field_names) {
    if (!is.null(input[[field_name]])) {
      eval_data[[field_name]] <- input[[field_name]]
    }
  }
  
  return(eval_data)
}

#' Helper operator for NULL coalescing
#'
#' @param x First value
#' @param y Second value to use if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}