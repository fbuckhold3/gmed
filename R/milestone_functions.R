#' @title Milestone Assessment Functions for GMED - RDM 2.0 Version
#' @description Functions for milestone visualization, assessment, and data processing
#' Supports both legacy REP milestones and new ACGME milestones in RDM 2.0
#' @name milestone_functions
NULL

# ============================================================================
# CORE DATA PROCESSING FUNCTIONS
# ============================================================================

#' Create Clean Milestone Workflow Data
#'
#' Creates cleaned and organized milestone data for use in coaching and assessment workflows.
#' This function processes raw milestone data and creates separate datasets for plotting vs tables.
#' Supports both REP and ACGME milestone formats.
#'
#' @param raw_milestone_data Raw milestone data from REDCap
#' @param resident_data Resident data for name/level mapping
#' @param milestone_format Format of milestones ("rep" for legacy, "acgme" for new, "auto" to detect)
#' @return List containing clean milestone datasets
#' @export
create_clean_milestone_workflow <- function(raw_milestone_data, resident_data = NULL, milestone_format = "auto") {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for milestone processing")
  }
  
  message("=== Creating clean milestone workflow data ===")
  
  # Handle NULL or empty data
  if (is.null(raw_milestone_data) || nrow(raw_milestone_data) == 0) {
    message("No milestone data provided, returning empty structure")
    return(create_empty_milestone_workflow())
  }
  
  message("Processing ", nrow(raw_milestone_data), " rows of milestone data")
  
  # ============================================================================
  # STEP 1: Auto-detect milestone format if needed
  # ============================================================================
  
  if (milestone_format == "auto") {
    milestone_format <- detect_milestone_format(raw_milestone_data)
    message("Auto-detected milestone format: ", milestone_format)
  }
  
  # ============================================================================
  # STEP 2: Basic data cleaning
  # ============================================================================
  
  tryCatch({
    
    # Clean the data
    milestone_data <- raw_milestone_data %>%
      # Remove completely empty rows
      dplyr::filter(!is.na(record_id)) %>%
      # Convert record_id to character for consistency
      dplyr::mutate(record_id = as.character(record_id))
    
    message("After basic cleaning: ", nrow(milestone_data), " rows")
    
    # ============================================================================
    # STEP 3: Add resident information
    # ============================================================================
    
    if (!is.null(resident_data)) {
      milestone_data <- milestone_data %>%
        dplyr::left_join(
          resident_data %>% 
            dplyr::select(record_id, name, Level) %>%
            dplyr::mutate(record_id = as.character(record_id)),
          by = "record_id"
        )
      
      message("Added resident information (name, level)")
    } else {
      # Add placeholder columns if no resident data
      milestone_data <- milestone_data %>%
        dplyr::mutate(
          name = paste0("Resident_", record_id),
          Level = "Unknown"
        )
      
      message("No resident data provided - using placeholder names")
    }
    
    # ============================================================================
    # STEP 4: Period mapping and standardization
    # ============================================================================
    
    # Add period mapping
    milestone_data <- milestone_data %>%
      dplyr::mutate(
        # Convert numeric period to readable format
        period_name = dplyr::case_when(
          prog_mile_period == "1" | prog_mile_period == 1 ~ "Mid Intern",
          prog_mile_period == "2" | prog_mile_period == 2 ~ "End Intern", 
          prog_mile_period == "3" | prog_mile_period == 3 ~ "Mid PGY2",
          prog_mile_period == "4" | prog_mile_period == 4 ~ "End PGY2",
          prog_mile_period == "5" | prog_mile_period == 5 ~ "Mid PGY3",
          prog_mile_period == "6" | prog_mile_period == 6 ~ "Graduation",
          prog_mile_period == "7" | prog_mile_period == 7 ~ "Intern Intro",
          TRUE ~ as.character(prog_mile_period)
        ),
        
        # Ensure period is character
        period = as.character(prog_mile_period)
      )
    
    message("Added period mapping")
    
    # ============================================================================
    # STEP 5: Identify milestone score columns based on format
    # ============================================================================
    
    if (milestone_format == "rep") {
      # Legacy REP milestone columns
      score_cols <- get_milestone_columns_simple(milestone_data, "program")
      self_cols <- get_milestone_columns_simple(milestone_data, "self")
      desc_cols <- names(milestone_data)[
        grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_desc", names(milestone_data))
      ]
    } else if (milestone_format == "acgme") {
      # New ACGME milestone columns
      score_cols <- get_milestone_columns_simple(milestone_data, "acgme")
      self_cols <- get_milestone_columns_simple(milestone_data, "acgme_self")
      desc_cols <- names(milestone_data)[
        grepl("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+_desc", names(milestone_data))
      ]
    } else {
      # Mixed format - get both
      rep_score_cols <- get_milestone_columns_simple(milestone_data, "program")
      acgme_score_cols <- get_milestone_columns_simple(milestone_data, "acgme")
      score_cols <- c(rep_score_cols, acgme_score_cols)
      
      rep_self_cols <- get_milestone_columns_simple(milestone_data, "self")
      acgme_self_cols <- get_milestone_columns_simple(milestone_data, "acgme_self")
      self_cols <- c(rep_self_cols, acgme_self_cols)
      
      desc_cols <- names(milestone_data)[
        grepl("^(rep_|acgme_)(pc|mk|sbp|pbl|prof|ics)\\d+_desc", names(milestone_data))
      ]
    }
    
    message("Found ", length(score_cols), " score columns, ", 
            length(desc_cols), " description columns, ", 
            length(self_cols), " self-assessment columns")
    
    # ============================================================================
    # STEP 6: Convert milestone scores to numeric
    # ============================================================================
    
    # Convert score columns to numeric
    for (col in score_cols) {
      if (col %in% names(milestone_data)) {
        milestone_data[[col]] <- as.numeric(milestone_data[[col]])
      }
    }
    
    # Convert self-assessment columns to numeric  
    for (col in self_cols) {
      if (col %in% names(milestone_data)) {
        milestone_data[[col]] <- as.numeric(milestone_data[[col]])
      }
    }
    
    message("Converted milestone scores to numeric")
    
    # ============================================================================
    # STEP 7: Create separate datasets
    # ============================================================================
    
    # Dataset 1: CLEAN data for plotting (no description fields)
    plot_cols <- c("record_id", "name", "Level", "period", "period_name", 
                   "prog_mile_period", "prog_mile_date", score_cols, self_cols)
    
    # Only keep columns that actually exist
    plot_cols <- plot_cols[plot_cols %in% names(milestone_data)]
    
    clean_plot_data <- milestone_data %>%
      dplyr::select(dplyr::all_of(plot_cols)) %>%
      # Remove rows where all milestone scores are NA
      dplyr::filter(dplyr::if_any(dplyr::all_of(intersect(score_cols, names(.))), ~ !is.na(.)))
    
    message("Created clean plot data with ", nrow(clean_plot_data), " rows")
    
    # Dataset 2: FULL data with descriptions for tables
    desc_data <- milestone_data
    
    message("Created description data with ", nrow(desc_data), " rows")
    
    # ============================================================================
    # STEP 8: Separate program vs self assessments
    # ============================================================================
    
    # Program milestone data (clean, for plotting)
    p_miles <- clean_plot_data %>%
      dplyr::select(-dplyr::any_of(self_cols))  # Remove self columns
    
    # Self milestone data (clean, for plotting) 
    s_miles <- clean_plot_data %>%
      dplyr::select(record_id, name, Level, period, period_name, prog_mile_period, prog_mile_date,
                    dplyr::any_of(self_cols))
    
    # Program milestone data with descriptions (for tables)
    p_miles_descriptions <- desc_data %>%
      dplyr::select(-dplyr::any_of(self_cols))  # Remove self columns
    
    # Self milestone data with descriptions (for tables)
    s_miles_descriptions <- desc_data %>%
      dplyr::select(record_id, name, Level, period, period_name, prog_mile_period, prog_mile_date,
                    dplyr::any_of(self_cols), dplyr::any_of(desc_cols))
    
    message("Separated program and self assessment data")
    
    # ============================================================================
    # STEP 9: Calculate summary statistics
    # ============================================================================
    
    # Calculate medians for program assessments
    if (length(score_cols) > 0 && nrow(p_miles) > 0) {
      milestone_medians <- calculate_milestone_medians_simple(p_miles)
    } else {
      milestone_medians <- NULL
    }
    
    message("Calculated milestone medians")
    
    # ============================================================================
    # STEP 10: Return organized structure
    # ============================================================================
    
    result <- list(
      # Clean data for plotting (no descriptions)
      p_miles = p_miles,
      s_miles = s_miles,
      
      # Full data with descriptions for tables
      p_miles_descriptions = p_miles_descriptions,
      s_miles_descriptions = s_miles_descriptions,
      
      # Summary statistics
      milestone_medians = milestone_medians,
      
      # Metadata
      milestone_format = milestone_format,
      score_columns = score_cols,
      desc_columns = desc_cols,
      self_columns = self_cols,
      total_rows_processed = nrow(milestone_data)
    )
    
    message("=== Milestone workflow creation completed successfully ===")
    message("Milestone format: ", milestone_format)
    message("Program milestones: ", nrow(p_miles), " rows")
    message("Self milestones: ", nrow(s_miles), " rows") 
    message("Available score columns: ", paste(head(score_cols, 5), collapse = ", "))
    
    return(result)
    
  }, error = function(e) {
    message("ERROR in create_clean_milestone_workflow: ", e$message)
    message("Returning empty milestone workflow structure")
    return(create_empty_milestone_workflow())
  })
}

#' Detect Milestone Format
#' 
#' Auto-detects whether milestone data uses REP or ACGME format
#' @param milestone_data Raw milestone data
#' @return Character string: "rep", "acgme", or "mixed"
detect_milestone_format <- function(milestone_data) {
  
  if (is.null(milestone_data) || nrow(milestone_data) == 0) {
    return("rep")  # Default fallback
  }
  
  # Look for REP milestone columns
  rep_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", names(milestone_data), value = TRUE)
  
  # Look for ACGME milestone columns
  acgme_cols <- grep("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$", names(milestone_data), value = TRUE)
  
  if (length(rep_cols) > 0 && length(acgme_cols) > 0) {
    return("mixed")
  } else if (length(acgme_cols) > 0) {
    return("acgme")
  } else {
    return("rep")
  }
}

#' Create Empty Milestone Workflow Structure
#' 
#' Creates empty milestone workflow structure when no data is available
#' @return List with empty milestone data structure
create_empty_milestone_workflow <- function() {
  
  empty_df <- data.frame(
    record_id = character(0),
    name = character(0), 
    Level = character(0),
    period = character(0),
    period_name = character(0),
    prog_mile_period = character(0),
    prog_mile_date = character(0),
    stringsAsFactors = FALSE
  )
  
  return(list(
    p_miles = empty_df,
    s_miles = empty_df,
    p_miles_descriptions = empty_df,
    s_miles_descriptions = empty_df,
    milestone_medians = NULL,
    milestone_format = "unknown",
    score_columns = character(0),
    desc_columns = character(0),
    self_columns = character(0),
    total_rows_processed = 0
  ))
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

#' Create Milestone Spider Plot (RDM 2.0 Version)
#'
#' Creates a spider/radar plot for milestone data comparing individual resident scores
#' to cohort medians. Supports both REP and ACGME milestone formats.
#'
#' @param milestone_data Data frame containing milestone assessments
#' @param median_data Data frame containing calculated medians
#' @param resident_id Character string of resident record ID
#' @param period_text Character string of period (e.g., "End Intern")
#' @param milestone_type Type of milestone ("program", "self", "acgme", "acgme_self")
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
  
  if (length(milestone_cols) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No", milestone_type, "milestone columns found"), 
               x = 0.5, y = 0.5,
               showarrow = FALSE
             ))
  }
  
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
  
  # Clean milestone names for display (works for both REP and ACGME)
  milestone_labels <- gsub("^(rep_|acgme_)", "", milestone_cols[valid_indices])
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
                         "program" = "Program Milestones (REP)",
                         "self" = "Self-Assessment Milestones (REP)", 
                         "acgme" = "Program Milestones (ACGME)",
                         "acgme_self" = "Self-Assessment Milestones (ACGME)",
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

# ============================================================================
# COLUMN DETECTION FUNCTIONS (UPDATED FOR RDM 2.0)
# ============================================================================

#' Get Milestone Column Names (RDM 2.0 Version)
#' 
#' Extracts milestone column names for a specific instrument type.
#' Now supports both REP and ACGME milestone formats.
#' 
#' @param data Milestone data frame
#' @param type Type of milestone instrument ("program", "self", "acgme", "acgme_self")
#' @return Vector of milestone column names
#' @export
get_milestone_columns_simple <- function(data, type = "program") {
  
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }
  
  # Define patterns for each milestone type
  if (type == "program") {
    # Legacy REP program milestones: rep_pc1, rep_pc2, etc.
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$"
  } else if (type == "self") {
    # Legacy REP self milestones: rep_pc1_self, rep_pc2_self, etc.
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$"
  } else if (type == "acgme") {
    # New ACGME program milestones: acgme_pc1, acgme_pc2, etc.
    pattern <- "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"
  } else if (type == "acgme_self") {
    # New ACGME self milestones: acgme_pc1_self, acgme_pc2_self, etc.
    pattern <- "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+_self$"
  } else {
    stop("Unknown milestone type: ", type, ". Use: program, self, acgme, or acgme_self")
  }
  
  milestone_cols <- grep(pattern, names(data), value = TRUE)
  
  message("Found ", length(milestone_cols), " milestone columns for type '", type, "'")
  if (length(milestone_cols) > 0) {
    message("Sample columns: ", paste(head(milestone_cols, 5), collapse = ", "))
  }
  
  return(milestone_cols)
}

# ============================================================================
# DATA PROCESSING FUNCTIONS (UPDATED)
# ============================================================================

#' Process Milestone Data (RDM 2.0 Version)
#' 
#' Converts milestone data to numeric and adds period names.
#' Supports both REP and ACGME milestone formats.
#' 
#' @param milestone_data Raw milestone data
#' @param type Milestone type ("program", "self", "acgme", "acgme_self")
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

#' Calculate Milestone Medians (RDM 2.0 Version)
#' 
#' Calculates median milestone scores by period.
#' Supports both REP and ACGME milestone formats.
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
  
  # Get milestone columns (numeric ones only) - supports both REP and ACGME
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

# ============================================================================
# FIELD MAPPING FUNCTIONS (UPDATED FOR RDM 2.0)
# ============================================================================

#' Get Field Mappings for RDM 2.0 Milestone Forms
#' 
#' Returns the field mapping for writing milestone data to specific RDM 2.0 forms.
#' Now supports both REP and ACGME milestone formats.
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
                   "acgme_entry" = "acgme_",  # New ACGME form
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
#' @param milestone_format Format of milestones ("rep", "acgme", or "both")
#' @return Character vector of milestone fields with description support
#' @export
get_milestone_desc_fields <- function(milestone_format = "rep") {
  
  base_fields <- c(
    "PC_1", "PC_2", "PC_3", "PC_6",
    "PBLI_1", "PBLI_2",
    "PROF_1", "PROF_2", "PROF_3", "PROF_4",
    "ICS_1", "ICS_2", "ICS_3"
  )
  
  if (milestone_format == "rep") {
    return(paste0("rep_", tolower(gsub("_", "", base_fields)), "_desc"))
  } else if (milestone_format == "acgme") {
    return(paste0("acgme_", tolower(gsub("_", "", base_fields)), "_desc"))
  } else if (milestone_format == "both") {
    rep_fields <- paste0("rep_", tolower(gsub("_", "", base_fields)), "_desc")
    acgme_fields <- paste0("acgme_", tolower(gsub("_", "", base_fields)), "_desc")
    return(c(rep_fields, acgme_fields))
  } else {
    stop("Unknown milestone_format: ", milestone_format, ". Use 'rep', 'acgme', or 'both'")
  }
}

#' Milestone Definitions for UI Display (RDM 2.0 Version)
#'
#' Organized milestone definitions for use in UI components.
#' Supports both REP and ACGME milestone formats.
#'
#' @param milestone_format Format to return ("rep", "acgme", or "both")
#' @return List containing organized milestone definitions
#' @export
get_milestone_definitions <- function(milestone_format = "rep") {
  
  # Base milestone structure (same for both formats)
  base_structure <- list(
    pc_mk = list(
      title = "Patient Care & Medical Knowledge",
      items = c(
        "pc1" = "PC1: History",
        "pc2" = "PC2: Physical Examination", 
        "pc3" = "PC3: Clinical Reasoning",
        "pc4" = "PC4: Patient Management - Inpatient",
        "pc5" = "PC5: Patient Management - Outpatient",
        "pc6" = "PC6: Digital Health",
        "mk1" = "MK1: Applied Foundational Sciences",
        "mk2" = "MK2: Therapeutic Knowledge",
        "mk3" = "MK3: Knowledge of Diagnostic Testing"
      )
    ),
    sbp_pbl = list(
      title = "Systems-Based Practice & Practice-Based Learning",
      items = c(
        "sbp1" = "SBP1: Patient Safety and Quality Improvement",
        "sbp2" = "SBP2: System Navigation for Patient-Centered Care", 
        "sbp3" = "SBP3: Physician Role in Health Care Systems",
        "pbl1" = "PBLI1: Evidence-Based and Informed Practice",
        "pbl2" = "PBLI2: Reflective Practice and Commitment to Personal Growth"
      )
    ),
    prof_ics = list(
      title = "Professionalism & Interpersonal Communication",
      items = c(
        "prof1" = "PROF1: Professional Behavior",
        "prof2" = "PROF2: Ethical Principles",
        "prof3" = "PROF3: Accountability/Conscientiousness", 
        "prof4" = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",
        "ics1" = "ICS1: Patient- and Family-Centered Communication",
        "ics2" = "ICS2: Interprofessional and Team Communication",
        "ics3" = "ICS3: Communication within Health Care Systems"
      )
    )
  )
  
  if (milestone_format == "rep") {
    # Add rep_ prefix
    result <- base_structure
    for (section in names(result)) {
      names(result[[section]]$items) <- paste0("rep_", names(result[[section]]$items))
    }
    return(result)
    
  } else if (milestone_format == "acgme") {
    # Add acgme_ prefix
    result <- base_structure
    for (section in names(result)) {
      names(result[[section]]$items) <- paste0("acgme_", names(result[[section]]$items))
    }
    return(result)
    
  } else if (milestone_format == "both") {
    # Return both with prefixes
    rep_result <- base_structure
    acgme_result <- base_structure
    
    for (section in names(rep_result)) {
      names(rep_result[[section]]$items) <- paste0("rep_", names(rep_result[[section]]$items))
      names(acgme_result[[section]]$items) <- paste0("acgme_", names(acgme_result[[section]]$items))
    }
    
    return(list(
      rep = rep_result,
      acgme = acgme_result
    ))
    
  } else {
    stop("Unknown milestone_format: ", milestone_format, ". Use 'rep', 'acgme', or 'both'")
  }
}

# ============================================================================
# TRANSITION AND COMPATIBILITY FUNCTIONS
# ============================================================================

#' Convert REP Milestones to ACGME Format
#' 
#' Converts milestone data from REP format to ACGME format for transition period
#' @param rep_milestone_data Data frame with REP milestone data
#' @return Data frame with ACGME format milestone data
#' @export
convert_rep_to_acgme_format <- function(rep_milestone_data) {
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  if (is.null(rep_milestone_data) || nrow(rep_milestone_data) == 0) {
    return(rep_milestone_data)
  }
  
  message("Converting REP milestone data to ACGME format")
  
  # Get REP milestone columns
  rep_cols <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+", names(rep_milestone_data), value = TRUE)
  
  if (length(rep_cols) == 0) {
    message("No REP milestone columns found to convert")
    return(rep_milestone_data)
  }
  
  # Create conversion mapping
  acgme_data <- rep_milestone_data
  
  for (col in rep_cols) {
    # Convert rep_pc1 to acgme_pc1, etc.
    new_col <- gsub("^rep_", "acgme_", col)
    acgme_data[[new_col]] <- acgme_data[[col]]
  }
  
  message("Converted ", length(rep_cols), " REP columns to ACGME format")
  return(acgme_data)
}

#' Check Milestone Data Completeness
#' 
#' Checks completeness of milestone data for quality assurance
#' @param milestone_data Milestone data to check
#' @param milestone_format Format to check ("rep", "acgme", or "both")
#' @return List with completeness statistics
#' @export
check_milestone_completeness <- function(milestone_data, milestone_format = "auto") {
  
  if (is.null(milestone_data) || nrow(milestone_data) == 0) {
    return(list(
      total_records = 0,
      complete_records = 0,
      completion_rate = 0,
      missing_fields = character(0)
    ))
  }
  
  # Auto-detect format if needed
  if (milestone_format == "auto") {
    milestone_format <- detect_milestone_format(milestone_data)
  }
  
  # Get relevant milestone columns
  if (milestone_format == "rep") {
    milestone_cols <- get_milestone_columns_simple(milestone_data, "program")
  } else if (milestone_format == "acgme") {
    milestone_cols <- get_milestone_columns_simple(milestone_data, "acgme")
  } else {
    rep_cols <- get_milestone_columns_simple(milestone_data, "program")
    acgme_cols <- get_milestone_columns_simple(milestone_data, "acgme")
    milestone_cols <- c(rep_cols, acgme_cols)
  }
  
  if (length(milestone_cols) == 0) {
    return(list(
      total_records = nrow(milestone_data),
      complete_records = 0,
      completion_rate = 0,
      missing_fields = "No milestone columns found"
    ))
  }
  
  # Check completeness
  total_records <- nrow(milestone_data)
  
  # Count records with at least one milestone score
  complete_records <- milestone_data %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(milestone_cols), ~ !is.na(.))) %>%
    nrow()
  
  completion_rate <- if (total_records > 0) complete_records / total_records else 0
  
  # Find fields with high missing rates
  missing_rates <- sapply(milestone_cols, function(col) {
    if (col %in% names(milestone_data)) {
      sum(is.na(milestone_data[[col]])) / total_records
    } else {
      1  # Column doesn't exist
    }
  })
  
  high_missing_fields <- names(missing_rates)[missing_rates > 0.8]
  
  result <- list(
    total_records = total_records,
    complete_records = complete_records,
    completion_rate = round(completion_rate, 3),
    milestone_format = milestone_format,
    milestone_columns_found = length(milestone_cols),
    high_missing_fields = high_missing_fields
  )
  
  message("Milestone completeness check:")
  message("- Total records: ", total_records)
  message("- Complete records: ", complete_records)
  message("- Completion rate: ", round(completion_rate * 100, 1), "%")
  message("- Format: ", milestone_format)
  
  return(result)
}