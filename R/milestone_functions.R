#' @title Milestone Assessment Functions for GMED
#' @description Functions for milestone visualization, assessment, and data processing
#' @name milestone_functions
NULL

#' Create Final Milestone Spider Plot (Tested Version)
#'
#' Creates a spider/radar plot for milestone data comparing individual resident scores
#' to cohort medians. This is the tested version from your coach app.
#'
#' @param milestone_data Data frame containing milestone assessments
#' @param median_data Data frame containing calculated medians
#' @param resident_id Character string of resident record ID
#' @param period_text Character string of period (e.g., "End Intern")
#' @param milestone_type Type of milestone ("program", "self", "acgme")
#' @param resident_data Optional data frame for resident name lookup
#'
#' @return Plotly object with interactive spider plot
#' @export
create_milestone_spider_plot_final <- function(milestone_data, median_data, resident_id, period_text, 
                                               milestone_type = "program", resident_data = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required for data processing")
  }
  
  # Get milestone columns
  milestone_cols <- get_milestone_columns_simple(milestone_data, milestone_type)
  
  # Filter individual resident's scores for the period using text period
  individual_data <- milestone_data %>%
    dplyr::filter(record_id == !!resident_id, prog_mile_period == !!period_text)
  
  if (nrow(individual_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No data for resident", resident_id, "in period", period_text), 
               x = 0.5, y = 0.5,
               showarrow = FALSE
             ))
  }
  
  # Get resident name from the resident_data (non-repeating form)
  resident_name <- "Unknown Resident"
  if (!is.null(resident_data)) {
    name_lookup <- resident_data %>%
      dplyr::filter(record_id == !!resident_id) %>%
      dplyr::pull(name)
    if (length(name_lookup) > 0 && !is.na(name_lookup[1])) {
      resident_name <- name_lookup[1]
    }
  }
  
  # Extract milestone scores
  individual_scores <- individual_data %>%
    dplyr::select(dplyr::all_of(milestone_cols))
  
  # Filter median scores for the period using text period
  median_scores <- median_data %>%
    dplyr::filter(prog_mile_period == !!period_text) %>%
    dplyr::select(dplyr::all_of(milestone_cols))
  
  if (nrow(median_scores) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "No median data available for this period", 
               x = 0.5, y = 0.5,
               showarrow = FALSE
             ))
  }
  
  # Prepare data for plotting
  individual_values <- as.numeric(individual_scores[1, ])
  median_values <- as.numeric(median_scores[1, ])
  
  # Remove any NA values and corresponding labels
  valid_indices <- !is.na(individual_values) & !is.na(median_values)
  individual_values <- individual_values[valid_indices]
  median_values <- median_values[valid_indices]
  
  # Clean milestone names for display
  milestone_labels <- gsub("^rep_", "", milestone_cols[valid_indices])
  milestone_labels <- toupper(milestone_labels)
  milestone_labels <- gsub("PBL", "PBLI", milestone_labels)
  
  # Create hover text for better tooltip display
  individual_hover <- paste0(
    "<b>", milestone_labels, "</b><br>",
    resident_name, ": ", individual_values, "<br>",
    "Cohort Median: ", median_values,
    "<extra></extra>"
  )
  
  median_hover <- paste0(
    "<b>", milestone_labels, "</b><br>",
    "Cohort Median: ", median_values, "<br>",
    resident_name, ": ", individual_values,
    "<extra></extra>"
  )
  
  # Create the spider plot
  fig <- plotly::plot_ly(type = 'scatterpolar', fill = 'toself')
  
  # Add individual scores with custom hover
  fig <- fig %>% plotly::add_trace(
    r = individual_values,
    theta = milestone_labels,
    name = resident_name,
    line = list(color = '#1f77b4', width = 3),
    marker = list(color = '#1f77b4', size = 8),
    fillcolor = 'rgba(31, 119, 180, 0.1)',
    hovertemplate = individual_hover
  )
  
  # Add median scores with custom hover
  fig <- fig %>% plotly::add_trace(
    r = median_values,
    theta = milestone_labels,
    name = "Cohort Median",
    line = list(color = '#ff7f0e', width = 2, dash = 'dash'),
    marker = list(color = '#ff7f0e', size = 6),
    fillcolor = 'rgba(255, 127, 14, 0.05)',
    hovertemplate = median_hover
  )
  
  # Create title based on milestone type
  title_prefix <- switch(milestone_type,
                         "program" = "Program Milestones",
                         "self" = "Self-Assessment Milestones", 
                         "acgme" = "ACGME Milestones",
                         "Milestones"
  )
  
  # Layout with scale from 1-9
  fig <- fig %>% plotly::layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 9),
        tickmode = 'array',
        tickvals = c(1, 5, 9),
        ticktext = c('1', '5', '9'),
        tickfont = list(size = 12),
        gridcolor = 'rgba(0,0,0,0.1)'
      ),
      angularaxis = list(
        tickfont = list(size = 12),
        rotation = 90,
        direction = "clockwise"
      )
    ),
    title = list(
      text = paste0(title_prefix, " for ", resident_name, " - ", period_text),
      font = list(size = 16)
    ),
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
    margin = list(t = 80, b = 80, l = 80, r = 80)
  )
  
  return(fig)
}

#' Get Milestone Column Names (Simple Version)
#' 
#' Extracts milestone column names for a specific instrument type.
#' This is the tested version from your coach app.
#' 
#' @param data Milestone data frame
#' @param type Type of milestone instrument ("program", "self", "acgme")
#' @return Vector of milestone column names
#' @export
get_milestone_columns_simple <- function(data, type = "program") {
  
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }
  
  # Define patterns for each milestone type
  if (type == "program") {
    # Program milestones: rep_pc1, rep_pc2, etc.
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$"
  } else if (type == "self") {
    # Self milestones: rep_pc1_self, rep_pc2_self, etc.
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$"
  } else if (type == "acgme") {
    # ACGME milestones: need to check the new data dictionary
    # For now, assume similar pattern - update based on actual fields
    pattern <- "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"
  } else {
    stop("Unknown milestone type: ", type)
  }
  
  milestone_cols <- grep(pattern, names(data), value = TRUE)
  
  message("Found ", length(milestone_cols), " milestone columns for type '", type, "'")
  if (length(milestone_cols) > 0) {
    message("Sample columns: ", paste(head(milestone_cols, 5), collapse = ", "))
  }
  
  return(milestone_cols)
}

#' Process Milestone Data (Simple Version)
#' 
#' Converts milestone data to numeric and adds period names.
#' This is the tested version from your coach app.
#' 
#' @param milestone_data Raw milestone data
#' @param type Milestone type ("program", "self", "acgme")
#' @return Processed milestone data
#' @export
process_milestone_data_simple <- function(milestone_data, type = "program") {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(milestone_data) || nrow(milestone_data) == 0) {
    message("No ", type, " milestone data to process")
    return(NULL)
  }
  
  message("Processing ", type, " milestone data: ", nrow(milestone_data), " rows")
  
  # Get milestone columns
  milestone_cols <- get_milestone_columns_simple(milestone_data, type)
  
  if (length(milestone_cols) == 0) {
    message("No milestone columns found for type: ", type)
    return(NULL)
  }
  
  # Process the data
  processed_data <- milestone_data %>%
    # Convert milestone scores to numeric
    dplyr::mutate(dplyr::across(dplyr::all_of(milestone_cols), as.numeric)) %>%
    # Add readable period names based on prog_mile_period or equivalent
    dplyr::mutate(
      period_name = dplyr::case_when(
        prog_mile_period == 1 ~ "Mid Intern",
        prog_mile_period == 2 ~ "End Intern", 
        prog_mile_period == 3 ~ "Mid PGY2",
        prog_mile_period == 4 ~ "End PGY2",
        prog_mile_period == 5 ~ "Mid PGY3",
        prog_mile_period == 6 ~ "Graduation",
        prog_mile_period == 7 ~ "Intern Intro",
        TRUE ~ paste("Period", prog_mile_period)
      )
    ) %>%
    # Remove rows with all NA milestone scores
    dplyr::filter(dplyr::if_any(dplyr::all_of(milestone_cols), ~ !is.na(.x)))
  
  message("After processing: ", nrow(processed_data), " rows with valid milestone data")
  
  return(processed_data)
}

#' Calculate Milestone Medians (Simple Version)
#' 
#' Calculates median milestone scores by period.
#' This is the tested version from your coach app.
#' 
#' @param processed_milestone_data Processed milestone data
#' @return Data frame with median scores by period
#' @export
calculate_milestone_medians_simple <- function(processed_milestone_data) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(processed_milestone_data) || nrow(processed_milestone_data) == 0) {
    return(NULL)
  }
  
  # Get milestone columns (numeric ones only)
  milestone_cols <- names(processed_milestone_data)[
    grepl("^(rep_|acgme_)(pc|mk|sbp|pbl|prof|ics)\\d+", names(processed_milestone_data)) & 
      !grepl("_desc|_self$", names(processed_milestone_data))
  ]
  
  if (length(milestone_cols) == 0) {
    message("No numeric milestone columns found for median calculation")
    return(NULL)
  }
  
  medians <- processed_milestone_data %>%
    dplyr::group_by(prog_mile_period, period_name) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(milestone_cols), ~ median(.x, na.rm = TRUE)),
      n_residents = dplyr::n(),
      .groups = "drop"
    )
  
  message("Calculated medians for ", nrow(medians), " periods")
  return(medians)
}

#' Get Field Mappings for RDM 2.0 Milestone Forms
#' 
#' Returns the field mapping for writing milestone data to specific RDM 2.0 forms.
#' 
#' @param target_form Which form to write to ("milestone_entry", "milestone_selfevaluation_c33c", "acgme_miles")
#' @return Named vector mapping module format to RDM field names
#' @export
get_milestone_field_mapping_rdm2 <- function(target_form = "milestone_entry") {
  
  base_mapping <- c(
    # Patient Care
    "PC_1" = "pc1", "PC_2" = "pc2", "PC_3" = "pc3", 
    "PC_4" = "pc4", "PC_5" = "pc5", "PC_6" = "pc6",
    # Medical Knowledge  
    "MK_1" = "mk1", "MK_2" = "mk2", "MK_3" = "mk3",
    # Systems-Based Practice
    "SBP_1" = "sbp1", "SBP_2" = "sbp2", "SBP_3" = "sbp3",
    # Practice-Based Learning
    "PBLI_1" = "pbl1", "PBLI_2" = "pbl2",
    # Professionalism
    "PROF_1" = "prof1", "PROF_2" = "prof2", "PROF_3" = "prof3", "PROF_4" = "prof4",
    # Interpersonal Communication
    "ICS_1" = "ics1", "ICS_2" = "ics2", "ICS_3" = "ics3"
  )
  
  # Add appropriate prefix based on target form
  prefix <- switch(target_form,
                   "milestone_entry" = "rep_",
                   "milestone_selfevaluation_c33c" = "rep_", 
                   "acgme_miles" = "acgme_",
                   "rep_"  # default
  )
  
  # Add suffix for self-evaluation
  suffix <- if (target_form == "milestone_selfevaluation_c33c") "_self" else ""
  
  # Create final mapping
  final_mapping <- paste0(prefix, base_mapping, suffix)
  names(final_mapping) <- names(base_mapping)
  
  return(final_mapping)
}

#' Get Milestone Description Fields
#'
#' Returns the list of milestone fields that support description text.
#' These are typically used for higher ratings that require explanation.
#'
#' @return Character vector of milestone fields with description support
#' @export
get_milestone_desc_fields <- function() {
  return(c(
    "PC_1", "PC_2", "PC_3", "PC_6",
    "PBLI_1", "PBLI_2",
    "PROF_1", "PROF_2", "PROF_3", "PROF_4",
    "ICS_1", "ICS_2", "ICS_3"
  ))
}

#' Milestone Definitions for UI Display
#'
#' Organized milestone definitions for use in UI components.
#' This matches the structure used in your milestone module.
#'
#' @return List containing organized milestone definitions
#' @export
get_milestone_definitions <- function() {
  list(
    pc_mk = list(
      title = "Patient Care & Medical Knowledge",
      items = c(
        "rep_pc1" = "PC1: History",
        "rep_pc2" = "PC2: Physical Examination", 
        "rep_pc3" = "PC3: Clinical Reasoning",
        "rep_pc4" = "PC4: Patient Management - Inpatient",
        "rep_pc5" = "PC5: Patient Management - Outpatient",
        "rep_pc6" = "PC6: Digital Health",
        "rep_mk1" = "MK1: Applied Foundational Sciences",
        "rep_mk2" = "MK2: Therapeutic Knowledge",
        "rep_mk3" = "MK3: Knowledge of Diagnostic Testing"
      )
    ),
    sbp_pbl = list(
      title = "Systems-Based Practice & Practice-Based Learning",
      items = c(
        "rep_sbp1" = "SBP1: Patient Safety and Quality Improvement",
        "rep_sbp2" = "SBP2: System Navigation for Patient-Centered Care", 
        "rep_sbp3" = "SBP3: Physician Role in Health Care Systems",
        "rep_pbl1" = "PBLI1: Evidence-Based and Informed Practice",
        "rep_pbl2" = "PBLI2: Reflective Practice and Commitment to Personal Growth"
      )
    ),
    prof_ics = list(
      title = "Professionalism & Interpersonal Communication",
      items = c(
        "rep_prof1" = "PROF1: Professional Behavior",
        "rep_prof2" = "PROF2: Ethical Principles",
        "rep_prof3" = "PROF3: Accountability/Conscientiousness", 
        "rep_prof4" = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",
        "rep_ics1" = "ICS1: Patient- and Family-Centered Communication",
        "rep_ics2" = "ICS2: Interprofessional and Team Communication",
        "rep_ics3" = "ICS3: Communication within Health Care Systems"
      )
    )
  )
}