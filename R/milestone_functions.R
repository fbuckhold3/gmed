#' @title Milestone Assessment Functions for GMED - RDM 2.0 Version
#' @description Functions for milestone visualization, assessment, and data processing
#' Supports both legacy REP milestones and new ACGME milestones in RDM 2.0
#' @name milestone_functions
NULL

# ============================================================================
# CORE DATA PROCESSING FUNCTIONS
# ============================================================================

# ============================================================================
# DATA DICTIONARY-DRIVEN MILESTONE WORKFLOW
# Uses redcap_repeat_instrument and data dictionary to auto-configure
# Simplified for your three actual formats: REP program, REP self, ACGME program
# ============================================================================
#' Extract Milestone Configuration from Data Dictionary
#'
#' Automatically detects milestone patterns from data dictionary based on
#' redcap_repeat_instrument and field patterns. Simplified for actual formats.
#'
#' @param data_dict Data dictionary from get_evaluation_dictionary()
#' @param verbose Logical. Print detection details
#' @return List of milestone configurations found
#' @export
extract_milestone_configs_from_dict <- function(data_dict, verbose = TRUE) {
  
  if (verbose) message("Extracting milestone configurations from data dictionary...")
  
  # Only the three patterns you actually have
  milestone_patterns <- c(
    "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$",           # REP program
    "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$",      # REP self
    "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"          # ACGME program
  )
  
  configs <- list()
  
  # Get unique form names that have milestone fields
  milestone_forms <- data_dict %>%
    dplyr::filter(grepl(paste(milestone_patterns, collapse = "|"), field_name)) %>%
    dplyr::pull(form_name) %>%
    unique() %>%
    na.omit()
  
  if (verbose) message("Found milestone fields in forms: ", paste(milestone_forms, collapse = ", "))
  
  # For each form, analyze its milestone pattern
  for (form_name in milestone_forms) {
    
    form_fields <- data_dict %>%
      dplyr::filter(form_name == !!form_name)
    
    # Extract milestone fields
    milestone_fields <- form_fields %>%
      dplyr::filter(grepl(paste(milestone_patterns, collapse = "|"), field_name)) %>%
      dplyr::pull(field_name)
    
    if (length(milestone_fields) == 0) next
    
    # Detect patterns - only the three formats you actually have
    rep_program <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", milestone_fields, value = TRUE)
    rep_self <- grep("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", milestone_fields, value = TRUE)
    acgme_program <- grep("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$", milestone_fields, value = TRUE)
    
    # Find period and date columns - universal approach
    period_cols <- form_fields %>%
      dplyr::filter(grepl("period", field_name, ignore.case = TRUE)) %>%
      dplyr::pull(field_name)
    
    date_cols <- form_fields %>%
      dplyr::filter(grepl("date", field_name, ignore.case = TRUE)) %>%
      dplyr::pull(field_name)
    
    # Create config for each detected pattern
    if (length(rep_program) > 0) {
      config_name <- paste0(form_name, "_rep_program")
      configs[[config_name]] <- list(
        form_name = form_name,
        milestone_type = "program",
        milestone_system = "rep",
        score_columns = rep_program,
        period_columns = period_cols,
        date_columns = date_cols,
        pattern = "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$"
      )
    }
    
    if (length(rep_self) > 0) {
      config_name <- paste0(form_name, "_rep_self")
      configs[[config_name]] <- list(
        form_name = form_name,
        milestone_type = "self", 
        milestone_system = "rep",
        score_columns = rep_self,
        period_columns = period_cols,
        date_columns = date_cols,
        pattern = "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$"
      )
    }
    
    if (length(acgme_program) > 0) {
      config_name <- paste0(form_name, "_acgme_program")
      configs[[config_name]] <- list(
        form_name = form_name,
        milestone_type = "program",
        milestone_system = "acgme", 
        score_columns = acgme_program,
        period_columns = period_cols,
        date_columns = date_cols,
        pattern = "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"
      )
    }
    
    if (verbose) {
      message("Form '", form_name, "':")
      message("  REP program: ", length(rep_program), " fields")
      message("  REP self: ", length(rep_self), " fields")
      message("  ACGME program: ", length(acgme_program), " fields")
      message("  Period columns: ", paste(period_cols, collapse = ", "))
      message("  Date columns: ", paste(date_cols, collapse = ", "))
    }
  }
  
  if (verbose) message("Extracted ", length(configs), " milestone configurations")
  
  return(configs)
}


#' Create Universal Period Mapping
#'
#' Creates a standardized period mapping that works across all milestone forms
#'
#' @return Data frame with period mappings
#' @export
create_universal_period_mapping <- function() {
  
  # Universal period structure - handles both raw period codes and "Period X" format
  period_mapping <- data.frame(
    period_code = c("1", "2", "3", "4", "5", "6", "7",
                    1, 2, 3, 4, 5, 6, 7,
                    "Mid Intern", "End Intern", "Mid PGY2", "End PGY2", 
                    "Mid PGY3", "Graduating", "Entering Residency",
                    "Period Mid Intern", "Period End Intern", "Period Mid PGY2", 
                    "Period End PGY2", "Period Mid PGY3", "Period Graduating", 
                    "Period Entering Residency"),
    period_name = c(rep(c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2", 
                          "Mid PGY3", "Graduating", "Entering Residency"), 4)),
    period_order = c(rep(c(1, 2, 3, 4, 5, 6, 0), 4)),
    stringsAsFactors = FALSE
  )
  
  return(period_mapping)
}

#' Data Dictionary-Driven Milestone Workflow
#'
#' Automatically processes milestone data using data dictionary configuration
#'
#' @param all_forms List of form data (from complete_data$all_forms)
#' @param data_dict Data dictionary from get_evaluation_dictionary()
#' @param resident_data Resident lookup data
#' @param forms_to_process Optional vector of specific forms to process
#' @param verbose Logical. Print processing details
#' @return List with processed milestone data by configuration
#' @export
create_milestone_workflow_from_dict <- function(all_forms, data_dict, resident_data = NULL, 
                                                forms_to_process = NULL, verbose = TRUE) {
  
  if (verbose) message("=== Data Dictionary-Driven Milestone Workflow ===")
  
  # Extract milestone configurations from data dictionary
  configs <- extract_milestone_configs_from_dict(data_dict, verbose)
  
  if (length(configs) == 0) {
    if (verbose) message("No milestone configurations found")
    return(list())
  }
  
  # Filter to specific forms if requested
  if (!is.null(forms_to_process)) {
    configs <- configs[grepl(paste(forms_to_process, collapse = "|"), names(configs))]
    if (verbose) message("Filtered to ", length(configs), " configurations for specified forms")
  }
  
  # Get universal period mapping
  period_mapping <- create_universal_period_mapping()
  
  # Process each configuration
  results <- list()
  
  for (config_name in names(configs)) {
    config <- configs[[config_name]]
    
    if (verbose) message("Processing: ", config_name)
    
    # Use all_forms data directly instead of complex filtering
    form_data <- all_forms[[config$form_name]]
    
    if (is.null(form_data) || nrow(form_data) == 0) {
      if (verbose) message("  No data found for ", config$form_name)
      next
    }
    
    # Standardize period column name
    primary_period_col <- config$period_columns[1]  # Use first period column found
    if (!primary_period_col %in% names(form_data)) {
      if (verbose) message("  Period column ", primary_period_col, " not found")
      next
    }
    
    # Process the data - keep original period column name!
    period_sym <- rlang::sym(primary_period_col)

    # DEBUG: Print what we're joining
    if (verbose) {
      message("  Primary period column: ", primary_period_col)
      message("  Form data columns before join: ", paste(head(names(form_data), 10), collapse = ", "))
    }

    processed_data <- form_data %>%
      # Add universal period mapping using the original period column name
      dplyr::left_join(period_mapping, by = setNames("period_code", primary_period_col)) %>%
      {
        if (verbose) {
          message("  Columns after join: ", paste(head(names(.), 15), collapse = ", "))
        }
        .
      } %>%

      # Convert milestone scores to numeric
      dplyr::mutate(dplyr::across(dplyr::all_of(config$score_columns), as.numeric)) %>%

      # Filter to rows with milestone data
      dplyr::filter(dplyr::if_any(dplyr::all_of(config$score_columns), ~ !is.na(.))) %>%

      # Ensure record_id is character
      dplyr::mutate(record_id = as.character(record_id))
    
    # Add resident information if provided
    if (!is.null(resident_data)) {
      processed_data <- processed_data %>%
        dplyr::left_join(
          resident_data %>%
            dplyr::select(record_id, name, Level) %>%
            dplyr::mutate(record_id = as.character(record_id)),
          by = "record_id"
        )
    }
    
    # Calculate medians using the original period column name
    if (nrow(processed_data) > 0) {
      medians <- processed_data %>%
        dplyr::group_by(!!period_sym, period_name) %>%
        dplyr::summarise(
          dplyr::across(dplyr::all_of(config$score_columns), ~ median(.x, na.rm = TRUE)),
          n_residents = dplyr::n(),
          .groups = "drop"
        )
    } else {
      medians <- NULL
    }
    
    # Store results
    results[[config_name]] <- list(
      config = config,
      data = processed_data,
      medians = medians,
      n_rows = nrow(processed_data)
    )
    
    if (verbose) {
      message("  Processed ", nrow(processed_data), " rows")
      if (!is.null(medians)) {
        message("  Calculated medians for ", nrow(medians), " periods")
      }
    }
  }
  
  if (verbose) message("Completed processing ", length(results), " milestone configurations")
  
  return(results)
}

#' Quick Access Functions for Processed Results
#'

#' Get milestone data by type and system
#' @param workflow_results Results from create_milestone_workflow_from_dict
#' @param milestone_type "program" or "self"
#' @param milestone_system "rep" or "acgme"
#' @return Processed milestone data
#' @export
get_milestone_data <- function(workflow_results, milestone_type = "program", milestone_system = "rep") {
  
  # Find matching configuration
  pattern <- paste0("_", milestone_system, "_", milestone_type, "$")
  matching_configs <- names(workflow_results)[grepl(pattern, names(workflow_results))]
  
  if (length(matching_configs) == 0) {
    message("No data found for ", milestone_system, " ", milestone_type)
    return(NULL)
  }
  
  # Use first match
  result <- workflow_results[[matching_configs[1]]]
  message("Using ", result$config$form_name, " (", result$n_rows, " rows)")
  
  return(result)
}

#' Test the new workflow with your data
#' @param complete_data Your loaded complete_data
#' @export
test_dict_driven_workflow <- function(complete_data) {
  
  message("Testing data dictionary-driven milestone workflow...")
  
  # Process all milestone data using data dictionary
  milestone_results <- create_milestone_workflow_from_dict(
    all_forms = complete_data$all_forms,
    data_dict = complete_data$data_dict,
    resident_data = complete_data$residents,
    verbose = TRUE
  )
  
  # Test getting specific data types
  rep_program <- get_milestone_data(milestone_results, "program", "rep")
  rep_self <- get_milestone_data(milestone_results, "self", "rep")
  acgme_program <- get_milestone_data(milestone_results, "program", "acgme")
  
  # Test spider plot if we have data
  if (!is.null(rep_program) && !is.null(rep_program$data) && nrow(rep_program$data) > 0) {
    sample_resident <- rep_program$data$record_id[1]
    sample_period <- rep_program$data$prog_mile_period[1]
    
    message("Testing spider plot...")
    spider_plot <- create_milestone_spider_plot_final(
      milestone_data = rep_program$data,
      median_data = rep_program$medians,
      resident_id = sample_resident,
      period_text = sample_period,
      milestone_type = "program",
      resident_data = complete_data$residents
    )
    
    return(list(
      milestone_results = milestone_results,
      spider_plot = spider_plot
    ))
  }
  
  return(milestone_results)
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

  # Map period text to number FIRST
  period_num <- switch(period_text,
                      "Mid Intern" = 1,
                      "End Intern" = 2,
                      "Mid PGY2" = 3,
                      "End PGY2" = 4,
                      "Mid PGY3" = 5,
                      "End PGY3" = 6,
                      "Graduating" = 6,
                      "Entering Residency" = 7,
                      as.numeric(period_text))

  
  # Debug
  cat("\n=== SPIDER PLOT FINAL DEBUG ===\n")
  cat("resident_id:", resident_id, "\n")
  cat("period_text:", period_text, "\n")
  cat("milestone_type:", milestone_type, "\n")
  
  # Get milestone columns
  milestone_cols <- get_milestone_columns_simple(milestone_data, milestone_type)
  
  cat("Found", length(milestone_cols), "milestone columns\n")
  
  if (length(milestone_cols) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No", milestone_type, "milestone columns found"), 
               x = 0.5, y = 0.5,
               showarrow = FALSE
             ))
  }
  
  # Map period text to number for filtering
  period_num <- switch(period_text,
                      "Mid Intern" = 1,
                      "End Intern" = 2,
                      "Mid PGY2" = 3,
                      "End PGY2" = 4,
                      "Mid PGY3" = 5,
                      "End PGY3" = 6,
                      "Entering Residency" = 7,
                      NULL)
  
  cat("Mapped period_text to period_num:", period_num, "\n")

  # Dynamically detect which period field exists in the data
  period_field <- NULL
  if ("acgme_mile_period" %in% names(milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(milestone_data)) {
    period_field <- "prog_mile_period"
  } else {
    return(plotly::plot_ly() %>%
             plotly::add_annotations(
               text = "No recognized period field found in data",
               x = 0.5, y = 0.5,
               showarrow = FALSE
             ))
  }

  cat("Using period field:", period_field, "\n")

  # Create a symbol for dynamic evaluation
  period_sym <- rlang::sym(period_field)

  # Filter individual resident's scores for the period using numeric period
  individual_data <- milestone_data %>%
    dplyr::filter(record_id == !!resident_id, !!period_sym == !!period_num)
  
  cat("Individual data rows:", nrow(individual_data), "\n")
  
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
  
  # Extract milestone scores - get the most recent instance
  if ("redcap_repeat_instance" %in% names(individual_data)) {
    individual_data <- individual_data %>%
      dplyr::arrange(desc(redcap_repeat_instance)) %>%
      dplyr::slice(1)
  }
  
  individual_scores <- individual_data %>%
    dplyr::select(dplyr::all_of(milestone_cols))
  
  cat("Individual scores:", as.numeric(individual_scores[1,]), "\n")
  
  # Filter median scores for the period using text period name
  median_scores <- median_data %>%
    dplyr::filter(period_name == !!period_text) %>%
    dplyr::select(dplyr::all_of(milestone_cols))
  
  cat("Median data rows:", nrow(median_scores), "\n")
  
  if (nrow(median_scores) == 0) {
    median_scores <- NULL
  } else {
    cat("Median scores:", as.numeric(median_scores[1,]), "\n")
  }
  
  # Create clean category labels
  categories <- gsub("^(rep_|acgme_)", "", milestone_cols)
  categories <- gsub("_self$", "", categories)
  
  # Create plotly figure
  fig <- plotly::plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself')
  
  # Add individual scores
  fig <- fig %>% plotly::add_trace(
    r = as.numeric(individual_scores[1,]),
    theta = categories,
    name = resident_name,
    line = list(color = '#1f77b4', width = 2),
    marker = list(size = 8, color = '#1f77b4'),
    fillcolor = 'rgba(31, 119, 180, 0.2)'
  )
  
  # Add median scores if available
  if (!is.null(median_scores)) {
    fig <- fig %>% plotly::add_trace(
      r = as.numeric(median_scores[1,]),
      theta = categories,
      name = "Program Median",
      line = list(color = '#ff7f0e', width = 2, dash = 'dash'),
      marker = list(size = 6, color = '#ff7f0e')
    )
  }
  
  # Layout
  fig <- fig %>% plotly::layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(1, 5),
        tickmode = "linear",
        tick0 = 1,
        dtick = 1
      )
    ),
    showlegend = TRUE,
    title = paste("Milestones -", period_text, "-", resident_name)
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
  
  # Auto-detect if data has ACGME or REP columns
  has_acgme <- any(grepl("^acgme_", names(data)))
  has_rep <- any(grepl("^rep_", names(data)))
  
  # Define patterns based on what's actually in the data
  if (type == "program") {
    if (has_acgme) {
      # ACGME program milestones
      pattern <- "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"
    } else {
      # REP program milestones
      pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$"
    }
  } else if (type == "self") {
    # REP self milestones (ACGME doesn't have self)
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$"
  } else if (type == "acgme") {
    # Explicitly ACGME program milestones
    pattern <- "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$"
  } else if (type == "acgme_self") {
    # ACGME self milestones (if they exist)
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

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required for data processing")
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

  # Dynamically detect which period field exists in the data
  # Support: prog_mile_period (REP program), prog_mile_period_self (REP self), acgme_mile_period (ACGME)
  period_field <- NULL
  if ("acgme_mile_period" %in% names(milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(milestone_data)) {
    period_field <- "prog_mile_period"
  } else {
    stop("No recognized period field found in data. Expected one of: prog_mile_period, prog_mile_period_self, acgme_mile_period")
  }

  message("Using period field: ", period_field)

  # Create a symbol for dynamic evaluation
  period_sym <- rlang::sym(period_field)

  # Process the data
  processed_data <- milestone_data %>%
    # Convert milestone scores to numeric
    dplyr::mutate(dplyr::across(dplyr::all_of(milestone_cols), as.numeric)) %>%
    # Add readable period names based on dynamically detected period field
    dplyr::mutate(
      period_name = dplyr::case_when(
        !!period_sym == 1 ~ "Mid Intern",
        !!period_sym == 2 ~ "End Intern",
        !!period_sym == 3 ~ "Mid PGY2",
        !!period_sym == 4 ~ "End PGY2",
        !!period_sym == 5 ~ "Mid PGY3",
        !!period_sym == 6 ~ "Graduation",
        !!period_sym == 7 ~ "Intern Intro",
        TRUE ~ paste("Period", !!period_sym)
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

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required for data processing")
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

  # Dynamically detect which period field exists in the data
  # Support: prog_mile_period (REP program), prog_mile_period_self (REP self), acgme_mile_period (ACGME)
  period_field <- NULL
  if ("acgme_mile_period" %in% names(processed_milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(processed_milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(processed_milestone_data)) {
    period_field <- "prog_mile_period"
  } else {
    stop("No recognized period field found in data. Expected one of: prog_mile_period, prog_mile_period_self, acgme_mile_period")
  }

  # Create a symbol for dynamic evaluation
  period_sym <- rlang::sym(period_field)

  medians <- processed_milestone_data %>%
    dplyr::group_by(!!period_sym, period_name) %>%
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


# ============================================================================
# MILESTONE PROGRESSION CHARTS
# Individual milestone progression over time with program and national benchmarks
# ============================================================================

#' Get Milestone Label from Column Name (Simple Pattern Matching)
#'
#' Converts milestone column names to proper labels using pattern matching
#' Works for any milestone column regardless of prefix or suffix
#' @param milestone_col Column name (e.g., "rep_pc1", "rep_pc1_self", "acgme_mk2")
#' @param milestone_format Format to use ("rep", "acgme") - not used in this implementation
#' @return Character string with proper label (e.g., "PC1: History")
#' @export
get_milestone_label <- function(milestone_col, milestone_format = "rep") {
  
  # Simple pattern matching - if it contains pc1, it's History, etc.
  # Patient Care
  if (grepl("pc1", milestone_col, ignore.case = TRUE)) return("PC1: History")
  if (grepl("pc2", milestone_col, ignore.case = TRUE)) return("PC2: Physical Examination")
  if (grepl("pc3", milestone_col, ignore.case = TRUE)) return("PC3: Clinical Reasoning")
  if (grepl("pc4", milestone_col, ignore.case = TRUE)) return("PC4: Patient Management - Inpatient")
  if (grepl("pc5", milestone_col, ignore.case = TRUE)) return("PC5: Patient Management - Outpatient")
  if (grepl("pc6", milestone_col, ignore.case = TRUE)) return("PC6: Digital Health")
  
  # Medical Knowledge
  if (grepl("mk1", milestone_col, ignore.case = TRUE)) return("MK1: Applied Foundational Sciences")
  if (grepl("mk2", milestone_col, ignore.case = TRUE)) return("MK2: Therapeutic Knowledge")
  if (grepl("mk3", milestone_col, ignore.case = TRUE)) return("MK3: Knowledge of Diagnostic Testing")
  
  # Systems-Based Practice
  if (grepl("sbp1", milestone_col, ignore.case = TRUE)) return("SBP1: Patient Safety and Quality Improvement")
  if (grepl("sbp2", milestone_col, ignore.case = TRUE)) return("SBP2: System Navigation for Patient-Centered Care")
  if (grepl("sbp3", milestone_col, ignore.case = TRUE)) return("SBP3: Physician Role in Health Care Systems")
  
  # Practice-Based Learning and Improvement
  if (grepl("pbl1", milestone_col, ignore.case = TRUE)) return("PBLI1: Evidence-Based and Informed Practice")
  if (grepl("pbl2", milestone_col, ignore.case = TRUE)) return("PBLI2: Reflective Practice and Commitment to Personal Growth")
  
  # Professionalism
  if (grepl("prof1", milestone_col, ignore.case = TRUE)) return("PROF1: Professional Behavior")
  if (grepl("prof2", milestone_col, ignore.case = TRUE)) return("PROF2: Ethical Principles")
  if (grepl("prof3", milestone_col, ignore.case = TRUE)) return("PROF3: Accountability/Conscientiousness")
  if (grepl("prof4", milestone_col, ignore.case = TRUE)) return("PROF4: Knowledge of Systemic and Individual Factors of Well-Being")
  
  # Interpersonal and Communication Skills
  if (grepl("ics1", milestone_col, ignore.case = TRUE)) return("ICS1: Patient- and Family-Centered Communication")
  if (grepl("ics2", milestone_col, ignore.case = TRUE)) return("ICS2: Interprofessional and Team Communication")
  if (grepl("ics3", milestone_col, ignore.case = TRUE)) return("ICS3: Communication within Health Care Systems")
  
  # Fallback: clean up the column name
  clean_name <- gsub("^(rep_|acgme_)", "", milestone_col)
  clean_name <- gsub("_self$", "", clean_name)
  return(toupper(clean_name))
}

#' Convert ACGME 5-Point Scale to Internal 9-Point Scale
#'
#' Converts ACGME national benchmark scores (1-5 scale) to internal reporting scale (1-9)
#' @param acgme_score Score on ACGME 1-5 scale
#' @return Score on internal 1-9 scale
convert_acgme_to_internal_scale <- function(acgme_score) {
  # Mapping: ACGME -> Internal (Your 9-point scale)
  # 1.0 -> 1, 1.5 -> 2, 2.0 -> 3, 2.5 -> 4, 3.0 -> 5, 3.5 -> 6, 4.0 -> 7, 4.5 -> 8, 5.0 -> 9
  # Formula: Internal = (ACGME - 1) * 2 + 1
  return((acgme_score - 1) * 2 + 1)
}

#' Get National Benchmark Data (Converted to Internal 9-Point Scale)
#'
#' Creates/retrieves national milestone benchmarks by period
#' Updated with actual ACGME 2024-2025 Internal Medicine data converted to 9-point scale
#'
#' @param milestone_format Format to return ("rep", "acgme")
#' @return Data frame with national benchmarks on 9-point scale
#' @export
get_national_milestone_benchmarks <- function(milestone_format = "rep") {
  
  if (milestone_format == "rep") {
    # REP format using ACGME 2024-2025 Internal Medicine medians
    # Converted from 5-point scale to 9-point scale
    national_data <- data.frame(
      period_name = c("End Intern", "End PGY2", "Graduation"),
      period_code = c("2", "4", "6"),
      
      # Patient Care (converted from ACGME 5-point to 9-point scale)
      rep_pc1 = convert_acgme_to_internal_scale(c(2.8, 3.7, 4.3)),  # PC1: History
      rep_pc2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.3)),  # PC2: Physical Examination
      rep_pc3 = convert_acgme_to_internal_scale(c(2.5, 3.4, 4.2)),  # PC3: Clinical Reasoning
      rep_pc4 = convert_acgme_to_internal_scale(c(2.7, 3.5, 4.2)),  # PC4: Patient Management - Inpatient
      rep_pc5 = convert_acgme_to_internal_scale(c(2.5, 3.3, 4.3)),  # PC5: Patient Management - Outpatient
      rep_pc6 = convert_acgme_to_internal_scale(c(2.8, 3.4, 4.3)),  # PC6: Digital Health
      
      # Medical Knowledge (converted from ACGME 5-point to 9-point scale)
      rep_mk1 = convert_acgme_to_internal_scale(c(2.7, 3.5, 4.3)),  # MK1: Applied Foundational Sciences
      rep_mk2 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.3)),  # MK2: Therapeutic Knowledge
      rep_mk3 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # MK3: Knowledge of Diagnostic Testing
      
      # Systems-Based Practice (converted from ACGME 5-point to 9-point scale)
      rep_sbp1 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # SBP1: Patient Safety and Quality Improvement
      rep_sbp2 = convert_acgme_to_internal_scale(c(2.6, 3.5, 4.3)),  # SBP2: System Navigation for Patient-Centered Care
      rep_sbp3 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.4)),  # SBP3: Physician Role in Health Care Systems
      
      # Practice-Based Learning (converted from ACGME 5-point to 9-point scale)
      rep_pbl1 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.3)),  # PBLI1: Evidence-Based and Informed Practice
      rep_pbl2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # PBLI2: Reflective Practice and Commitment to Personal Growth
      
      # Professionalism (converted from ACGME 5-point to 9-point scale)
      rep_prof1 = convert_acgme_to_internal_scale(c(2.9, 3.6, 4.4)),  # PROF1: Professional Behavior
      rep_prof2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # PROF2: Ethical Principles
      rep_prof3 = convert_acgme_to_internal_scale(c(2.8, 3.6, 4.4)),  # PROF3: Accountability/Conscientiousness
      rep_prof4 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # PROF4: Knowledge of Systemic and Individual Factors of Well-Being
      
      # Interpersonal Communication (converted from ACGME 5-point to 9-point scale)
      rep_ics1 = convert_acgme_to_internal_scale(c(2.9, 3.6, 4.4)),  # ICS1: Patient- and Family-Centered Communication
      rep_ics2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),  # ICS2: Interprofessional and Team Communication
      rep_ics3 = convert_acgme_to_internal_scale(c(2.8, 3.6, 4.4)),  # ICS3: Communication within Health Care Systems
      
      stringsAsFactors = FALSE
    )
  } else {
    # ACGME format - same data but with acgme_ prefix, converted to 9-point scale
    national_data <- data.frame(
      period_name = c("End Intern", "End PGY2", "Graduation"),
      period_code = c("2", "4", "6"),
      
      # Patient Care (converted from ACGME 5-point to 9-point scale)
      acgme_pc1 = convert_acgme_to_internal_scale(c(2.8, 3.7, 4.3)),
      acgme_pc2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.3)),
      acgme_pc3 = convert_acgme_to_internal_scale(c(2.5, 3.4, 4.2)),
      acgme_pc4 = convert_acgme_to_internal_scale(c(2.7, 3.5, 4.2)),
      acgme_pc5 = convert_acgme_to_internal_scale(c(2.5, 3.3, 4.3)),
      acgme_pc6 = convert_acgme_to_internal_scale(c(2.8, 3.4, 4.3)),
      
      # Medical Knowledge (converted from ACGME 5-point to 9-point scale)
      acgme_mk1 = convert_acgme_to_internal_scale(c(2.7, 3.5, 4.3)),
      acgme_mk2 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.3)),
      acgme_mk3 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      
      # Systems-Based Practice (converted from ACGME 5-point to 9-point scale)
      acgme_sbp1 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      acgme_sbp2 = convert_acgme_to_internal_scale(c(2.6, 3.5, 4.3)),
      acgme_sbp3 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.4)),
      
      # Practice-Based Learning (converted from ACGME 5-point to 9-point scale)
      acgme_pbl1 = convert_acgme_to_internal_scale(c(2.7, 3.4, 4.3)),
      acgme_pbl2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      
      # Professionalism (converted from ACGME 5-point to 9-point scale)
      acgme_prof1 = convert_acgme_to_internal_scale(c(2.9, 3.6, 4.4)),
      acgme_prof2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      acgme_prof3 = convert_acgme_to_internal_scale(c(2.8, 3.6, 4.4)),
      acgme_prof4 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      
      # Interpersonal Communication (converted from ACGME 5-point to 9-point scale)
      acgme_ics1 = convert_acgme_to_internal_scale(c(2.9, 3.6, 4.4)),
      acgme_ics2 = convert_acgme_to_internal_scale(c(2.8, 3.5, 4.4)),
      acgme_ics3 = convert_acgme_to_internal_scale(c(2.8, 3.6, 4.4)),
      
      stringsAsFactors = FALSE
    )
  }
  
  return(national_data)
}


#' Create Enhanced Milestone Spider Plot with Modern Styling
#'
#' Creates a visually stunning spider/radar plot with gradients, shadows, and modern styling
#'
#' @param milestone_data Data frame containing milestone assessments
#' @param median_data Data frame containing calculated medians
#' @param resident_id Character string of resident record ID
#' @param period_text Character string of period (e.g., "End Intern")
#' @param milestone_type Type of milestone ("program", "self", "acgme")
#' @param resident_data Optional data frame for resident name lookup
#'
#' @return Plotly object with enhanced interactive spider plot
#' @export
create_enhanced_milestone_spider_plot <- function(milestone_data, median_data, resident_id, period_text, 
                                                  milestone_type = "program", resident_data = NULL) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  # Map period text to number
  period_num <- switch(period_text,
                      "Mid Intern" = 1,
                      "End Intern" = 2,
                      "Mid PGY2" = 3,
                      "End PGY2" = 4,
                      "Mid PGY3" = 5,
                      "End PGY3" = 6,
                      "Graduating" = 6,
                      "Entering Residency" = 7,
                      as.numeric(period_text))

  # Get milestone columns
  milestone_cols <- get_milestone_columns_simple(milestone_data, milestone_type)
  
  if (length(milestone_cols) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No", milestone_type, "milestone columns found"), 
               x = 0.5, y = 0.5,
               showarrow = FALSE,
               font = list(size = 16, color = "red")
             ))
  }
  
  # Dynamically detect which period field exists in the data
  period_field <- NULL
  if ("acgme_mile_period" %in% names(milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(milestone_data)) {
    period_field <- "prog_mile_period"
  }

  # Create a symbol for dynamic evaluation
  period_sym <- if (!is.null(period_field)) rlang::sym(period_field) else NULL

  # Smart filtering: Try period_name first (text), then detected period field (numeric)
  individual_data <- milestone_data %>%
    dplyr::filter(record_id == !!resident_id)

  # Try filtering by period_name if it exists
  if ("period_name" %in% names(individual_data)) {
    individual_data <- individual_data %>%
      dplyr::filter(period_name == !!period_text)
  }

  # If no rows or period_name doesn't exist, try the detected period field
  if (nrow(individual_data) == 0 && !is.null(period_sym)) {
    individual_data <- milestone_data %>%
      dplyr::filter(record_id == !!resident_id) %>%
      dplyr::filter(
        !!period_sym == !!period_num |
        !!period_sym == !!as.character(period_num) |
        !!period_sym == !!period_text
      )
  }
  
  if (nrow(individual_data) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No data for resident", resident_id, "in period", period_text), 
               x = 0.5, y = 0.5,
               showarrow = FALSE,
               font = list(size = 16, color = "orange")
             ))
  }
  
  # Get resident name
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
  
  # Smart filtering for medians: Try period_name first, then detected period field
  if ("period_name" %in% names(median_data)) {
    median_scores <- median_data %>%
      dplyr::filter(period_name == !!period_text) %>%
      dplyr::select(dplyr::all_of(milestone_cols))
  } else if (!is.null(period_sym) && period_field %in% names(median_data)) {
    median_scores <- median_data %>%
      dplyr::filter(
        !!period_sym == !!period_num |
        !!period_sym == !!as.character(period_num) |
        !!period_sym == !!period_text
      ) %>%
      dplyr::select(dplyr::all_of(milestone_cols))
  } else {
    median_scores <- data.frame()
  }
  
  if (nrow(median_scores) == 0) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = "No median data available for this period", 
               x = 0.5, y = 0.5,
               showarrow = FALSE,
               font = list(size = 16, color = "orange")
             ))
  }
  
  # Prepare data for plotting
  individual_values <- as.numeric(individual_scores[1, ])
  median_values <- as.numeric(median_scores[1, ])
  
  # Remove any NA values and corresponding labels
  valid_indices <- !is.na(individual_values) & !is.na(median_values)
  individual_values <- individual_values[valid_indices]
  median_values <- median_values[valid_indices]
  
  # Enhanced milestone labels - keep short names but show full text on hover
  milestone_labels <- milestone_cols[valid_indices]
  milestone_system <- ifelse(grepl("^acgme_", milestone_labels[1]), "acgme", "rep")
  
  # Short labels for display (e.g., "PC1", "ICS3")
  short_labels <- sapply(milestone_labels, function(x) {
    clean_name <- gsub("^(rep_|acgme_)", "", x)
    clean_name <- gsub("_self$", "", clean_name)
    return(toupper(clean_name))
  })
  
  # Full labels for hover text
  full_labels <- sapply(milestone_labels, function(x) get_milestone_label(x, milestone_system))
  
  # Create enhanced hover text with full milestone descriptions
  individual_hover <- paste0(
    "<b style='color: #2E86AB; font-size: 16px;'>", full_labels, "</b><br>",
    "<span style='font-size: 14px;'><b>", resident_name, ":</b> ", individual_values, "</span><br>",
    "<span style='color: #A23B72;'>Program Median: ", median_values, "</span><br>",
    "<span style='color: #666;'>Period: ", period_text, "</span>",
    "<extra></extra>"
  )
  
  median_hover <- paste0(
    "<b style='color: #A23B72; font-size: 16px;'>", full_labels, "</b><br>",
    "<span style='font-size: 14px;'><b>Program Median:</b> ", median_values, "</span><br>",
    "<span style='color: #2E86AB;'>", resident_name, ": ", individual_values, "</span><br>",
    "<span style='color: #666;'>Period: ", period_text, "</span>",
    "<extra></extra>"
  )
  
  # Create the enhanced spider plot with modern colors and effects
  fig <- plotly::plot_ly(type = 'scatterpolar', fill = 'toself')
  
  # Add individual scores with gradient fill and glow effect
  fig <- fig %>% plotly::add_trace(
    r = individual_values,
    theta = short_labels,
    name = paste("🎯", resident_name),
    line = list(
      color = '#2E86AB', 
      width = 4,
      shape = 'spline'
    ),
    marker = list(
      color = '#2E86AB', 
      size = 10,
      line = list(color = 'white', width = 2),
      symbol = 'circle'
    ),
    fillcolor = 'rgba(46, 134, 171, 0.15)',
    hovertemplate = individual_hover
  )
  
  # Add median scores with contrasting style
  fig <- fig %>% plotly::add_trace(
    r = median_values,
    theta = short_labels,
    name = "📊 Program Median",
    line = list(
      color = '#A23B72', 
      width = 3, 
      dash = 'dash',
      shape = 'spline'
    ),
    marker = list(
      color = '#A23B72', 
      size = 8,
      symbol = 'diamond',
      line = list(color = 'white', width = 2)
    ),
    fillcolor = 'rgba(162, 59, 114, 0.08)',
    hovertemplate = median_hover
  )
  
  # Add invisible traces at outer edge for axis label hovering
  max_radius <- 9
  for (i in seq_along(short_labels)) {
    fig <- fig %>% plotly::add_trace(
      r = max_radius,
      theta = short_labels[i],
      mode = 'markers',
      marker = list(
        size = 1,
        color = 'rgba(0,0,0,0)',
        line = list(width = 0)
      ),
      showlegend = FALSE,
      hovertemplate = paste0(
        "<b style='color: #2c3e50; font-size: 14px;'>", full_labels[i], "</b>",
        "<extra></extra>"
      ),
      name = ""
    )
  }
  
  # Enhanced layout with modern design
  title_prefix <- switch(milestone_type,
                         "program" = "📋 Program Assessment",
                         "self" = "🔍 Self-Assessment", 
                         "acgme" = "📋 ACGME Assessment",
                         "📋 Milestone Assessment"
  )
  
  fig <- fig %>% plotly::layout(
    title = list(
      text = paste0("<span style='font-size: 12px; color: #7f8c8d;'>", period_text, "</span>"),
      font = list(family = "Arial, sans-serif"),
      x = 0.5
    ),
    
    polar = list(
      bgcolor = 'rgba(248, 249, 250, 0.8)',
      domain = list(
        x = c(0.05, 0.95),
        y = c(0.05, 0.95)
      ),
      radialaxis = list(
        visible = TRUE,
        range = c(0, 9),
        tickmode = 'array',
        tickvals = c(1, 3, 5, 7, 9),
        ticktext = c('<b>1</b><br>Novice', '<b>3</b><br>Adv. Beginner', '<b>5</b><br>Competent', '<b>7</b><br>Proficient', '<b>9</b><br>Expert'),
        tickfont = list(size = 11, family = "Arial, sans-serif", color = "#2c3e50"),
        gridcolor = 'rgba(52, 73, 94, 0.2)',
        linecolor = 'rgba(52, 73, 94, 0.3)'
      ),
      angularaxis = list(
        tickfont = list(size = 12, family = "Arial, sans-serif", color = "#2c3e50"),
        rotation = 90,
        direction = "clockwise",
        gridcolor = 'rgba(52, 73, 94, 0.15)',
        linecolor = 'rgba(52, 73, 94, 0.2)'
      )
    ),
    
    margin = list(
      l = 40,
      r = 40,  
      t = 60,
      b = 60
    ),
    
    paper_bgcolor = 'white',
    plot_bgcolor = 'rgba(248, 249, 250, 0.5)',
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      font = list(size = 13, family = "Arial, sans-serif", color = "#2c3e50"),
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(52, 73, 94, 0.2)',
      borderwidth = 1
    )
  ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
    )
  
  return(fig)
}

#' Create Enhanced Milestone Progression Chart with Modern Styling
#'
#' Creates a visually stunning line chart with gradients, better colors, and modern design
#'
#' @param milestone_results Results from create_milestone_workflow_from_dict()
#' @param resident_id Character string of resident record ID
#' @param milestone_col Column name of milestone to plot (e.g., "rep_pc1")
#' @param milestone_type Type of data to use ("program", "self")  
#' @param milestone_system System to use ("rep", "acgme")
#' @param resident_data Resident lookup data for name
#' @param show_national Logical. Include national benchmarks (default: TRUE)
#' @return Enhanced plotly object with modern styling
#' @export
create_enhanced_milestone_progression <- function(milestone_results, resident_id, milestone_col,
                                                  milestone_type = "program", milestone_system = "rep",
                                                  resident_data = NULL, show_national = TRUE) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive plots")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  # Get the appropriate milestone data
  milestone_data_obj <- get_milestone_data(milestone_results, milestone_type, milestone_system)
  
  if (is.null(milestone_data_obj)) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("No", milestone_type, milestone_system, "data available"), 
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 16, color = "red")
             ))
  }
  
  milestone_data <- milestone_data_obj$data
  median_data <- milestone_data_obj$medians
  
  # Get resident name
  resident_name <- "Unknown Resident"
  if (!is.null(resident_data)) {
    name_lookup <- resident_data %>%
      dplyr::filter(record_id == !!resident_id) %>%
      dplyr::pull(name)
    if (length(name_lookup) > 0 && !is.na(name_lookup[1])) {
      resident_name <- name_lookup[1]
    }
  }
  
  # Get milestone label
  milestone_label <- get_milestone_label(milestone_col, milestone_system)
  
  # Check if milestone column exists
  if (!milestone_col %in% names(milestone_data)) {
    return(plotly::plot_ly() %>% 
             plotly::add_annotations(
               text = paste("Milestone", milestone_col, "not found in data"), 
               x = 0.5, y = 0.5, showarrow = FALSE,
               font = list(size = 16, color = "orange")
             ))
  }
  
  # Dynamically detect which period field exists in the data
  period_field <- NULL
  if ("acgme_mile_period" %in% names(milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(milestone_data)) {
    period_field <- "prog_mile_period"
  } else {
    # Default to prog_mile_period for compatibility
    period_field <- "prog_mile_period"
  }

  # Create a symbol for dynamic evaluation
  period_sym <- rlang::sym(period_field)

  # Create complete period framework to ensure all periods are displayed
  all_periods <- data.frame(
    period_name = c("Entering Residency", "Mid Intern", "End Intern", "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating"),
    period_order = c(0, 1, 2, 3, 4, 5, 6),
    stringsAsFactors = FALSE
  )

  # Get individual resident data with proper ordering
  individual_data <- milestone_data %>%
    dplyr::filter(record_id == !!resident_id) %>%
    {if (period_field %in% names(.)) dplyr::select(., !!period_sym, period_name, !!milestone_col) else dplyr::select(., period_name, !!milestone_col)} %>%
    dplyr::mutate(
      period_order = dplyr::case_when(
        period_name == "Entering Residency" ~ 0,
        period_name == "Mid Intern" ~ 1,
        period_name == "End Intern" ~ 2,
        period_name == "Mid PGY2" ~ 3,
        period_name == "End PGY2" ~ 4,
        period_name == "Mid PGY3" ~ 5,
        period_name == "Graduating" ~ 6,
        # Handle numeric period field safely if it exists
        period_field %in% names(.) && !is.na(suppressWarnings(as.numeric(!!period_sym))) ~ as.numeric(!!period_sym),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::arrange(period_order) %>%
    dplyr::rename(individual_score = !!milestone_col) %>%
    # Ensure we have all periods, even if no data
    dplyr::right_join(all_periods, by = c("period_name", "period_order"))
  
  # Get program medians with proper ordering
  program_medians <- median_data %>%
    {if (period_field %in% names(.)) dplyr::select(., !!period_sym, period_name, !!milestone_col) else dplyr::select(., period_name, !!milestone_col)} %>%
    dplyr::mutate(
      period_order = dplyr::case_when(
        period_name == "Entering Residency" ~ 0,
        period_name == "Mid Intern" ~ 1,
        period_name == "End Intern" ~ 2,
        period_name == "Mid PGY2" ~ 3,
        period_name == "End PGY2" ~ 4,
        period_name == "Mid PGY3" ~ 5,
        period_name == "Graduating" ~ 6,
        # Handle numeric period field safely if it exists
        period_field %in% names(.) && !is.na(suppressWarnings(as.numeric(!!period_sym))) ~ as.numeric(!!period_sym),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::arrange(period_order) %>%
    dplyr::rename(program_median = !!milestone_col) %>%
    # Ensure we have all periods for program medians too
    dplyr::right_join(all_periods, by = c("period_name", "period_order"))
  
  # Combine individual and program data
  plot_data <- individual_data %>%
    dplyr::left_join(program_medians %>% dplyr::select(period_name, period_order, program_median), 
                     by = c("period_name", "period_order"))
  
  # Add national benchmarks if requested (only for key periods)
  if (show_national) {
    national_data <- get_national_milestone_benchmarks(milestone_system)
    if (milestone_col %in% names(national_data)) {
      national_benchmarks <- national_data %>%
        dplyr::select(period_name, !!milestone_col) %>%
        dplyr::mutate(
          period_order = dplyr::case_when(
            period_name == "End Intern" ~ 2,
            period_name == "End PGY2" ~ 4,
            period_name == "Graduating" ~ 6,
            TRUE ~ NA_real_
          )
        ) %>%
        dplyr::rename(national_benchmark = !!milestone_col)
      
      plot_data <- plot_data %>%
        dplyr::left_join(national_benchmarks, by = c("period_name", "period_order"))
    }
  }
  
  # Create ordered factor for proper x-axis ordering
  plot_data <- plot_data %>%
    dplyr::mutate(
      period_display = factor(period_name, 
                              levels = c("Entering Residency", "Mid Intern", "End Intern", 
                                         "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating"))
    )
  
  # Create the enhanced plot with modern styling
  fig <- plotly::plot_ly(data = plot_data, x = ~period_display)
  
  # Add individual scores line with modern styling
  fig <- fig %>% 
    plotly::add_trace(
      y = ~individual_score,
      name = paste("🎯", resident_name),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(
        color = '#2E86AB', 
        width = 4,
        shape = 'spline'
      ),
      marker = list(
        color = '#2E86AB', 
        size = 12,
        line = list(color = 'white', width = 2),
        symbol = 'circle'
      ),
      hovertemplate = paste0(
        "<b style='color: #2E86AB;'>", resident_name, "</b><br>",
        "<span style='font-size: 14px;'>Period: %{x}</span><br>",
        "<span style='font-size: 16px; font-weight: bold;'>Score: %{y}</span><br>",
        "<span style='color: #666;'>", milestone_label, "</span>",
        "<extra></extra>"
      )
    )
  
  # Add program median line with modern styling (only if we have median data)
  if ("program_median" %in% names(plot_data) && any(!is.na(plot_data$program_median))) {
    fig <- fig %>% 
      plotly::add_trace(
        y = ~program_median,
        name = "📊 Program Median",
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
          color = '#A23B72', 
          width = 3, 
          dash = 'dash',
          shape = 'spline'
        ),
        marker = list(
          color = '#A23B72', 
          size = 10,
          symbol = 'diamond',
          line = list(color = 'white', width = 2)
        ),
        connectgaps = FALSE,  # Don't connect across missing data points
        hovertemplate = paste0(
          "<b style='color: #A23B72;'>Program Median</b><br>",
          "<span style='font-size: 14px;'>Period: %{x}</span><br>",
          "<span style='font-size: 16px; font-weight: bold;'>Score: %{y}</span><br>",
          "<span style='color: #666;'>", milestone_label, "</span>",
          "<extra></extra>"
        )
      )
  }
  
  # Add national benchmarks if available
  if (show_national && "national_benchmark" %in% names(plot_data)) {
    fig <- fig %>% 
      plotly::add_trace(
        y = ~national_benchmark,
        name = "🏆 National Benchmark",
        type = 'scatter',
        mode = 'lines+markers',
        line = list(
          color = '#F18F01', 
          width = 4, 
          dash = 'longdash'
        ),
        marker = list(
          color = '#F18F01', 
          size = 14, 
          symbol = 'square',
          line = list(color = '#E85A00', width = 3)
        ),
        hovertemplate = paste0(
          "<b style='color: #F18F01;'>National Benchmark</b><br>",
          "<span style='font-size: 14px;'>Period: %{x}</span><br>",
          "<span style='font-size: 16px; font-weight: bold;'>Score: %{y}</span><br>",
          "<span style='color: #666;'>ACGME 2024-2025 Internal Medicine Data</span>",
          "<extra></extra>"
        )
      )
  }
  
  # Enhanced layout with modern design
  fig <- fig %>% 
    plotly::layout(
      title = list(
        text = paste0(
          "<b style='font-size: 22px; color: #2c3e50;'>📈 ", milestone_label, "</b><br>",
          "<span style='font-size: 16px; color: #34495e;'>", resident_name, " Progress Over Time</span>"
        ),
        font = list(family = "Arial, sans-serif"),
        x = 0.5
      ),
      xaxis = list(
        title = list(
          text = "<b>Training Period</b>",
          font = list(size = 16, color = "#2c3e50", family = "Arial, sans-serif")
        ),
        tickfont = list(size = 13, color = "#34495e", family = "Arial, sans-serif"),
        tickangle = -45,
        gridcolor = 'rgba(52, 73, 94, 0.1)',
        linecolor = 'rgba(52, 73, 94, 0.2)'
      ),
      yaxis = list(
        title = list(
          text = "<b>Milestone Score</b>",
          font = list(size = 16, color = "#2c3e50", family = "Arial, sans-serif")
        ),
        range = c(0.5, 9.5),
        tickmode = 'array',
        tickvals = c(1, 3, 5, 7, 9),
        ticktext = c('<b>1</b><br>Novice', '<b>3</b><br>Adv. Beginner', '<b>5</b><br>Competent', '<b>7</b><br>Proficient', '<b>9</b><br>Expert'),
        tickfont = list(size = 12, color = "#34495e", family = "Arial, sans-serif"),
        gridcolor = 'rgba(52, 73, 94, 0.15)',
        linecolor = 'rgba(52, 73, 94, 0.2)',
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.25,
        font = list(size = 13, family = "Arial, sans-serif", color = "#2c3e50"),
        bgcolor = 'rgba(255, 255, 255, 0.9)',
        bordercolor = 'rgba(52, 73, 94, 0.2)',
        borderwidth = 1
      ),
      paper_bgcolor = 'white',
      plot_bgcolor = 'rgba(248, 249, 250, 0.8)',
      margin = list(t = 120, b = 120, l = 80, r = 80),
      hovermode = 'x unified'
    ) %>%
    # Add config for better interactivity
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c('pan2d', 'select2d', 'lasso2d', 'autoScale2d')
    )
  
  return(fig)
}

#' Create Enhanced Milestone Overview Dashboard
#'
#' Creates a comprehensive dashboard showing multiple milestones for a resident
#'
#' @param milestone_results Results from create_milestone_workflow_from_dict()
#' @param resident_id Character string of resident record ID
#' @param period_text Period to display (e.g., "End Intern")
#' @param milestone_type Type of data to use ("program", "self")
#' @param milestone_system System to use ("rep", "acgme")
#' @param resident_data Resident lookup data
#' @return List with spider plot and summary statistics
#' @export
create_milestone_overview_dashboard <- function(milestone_results, resident_id, period_text,
                                                milestone_type = "program", milestone_system = "rep",
                                                resident_data = NULL) {
  
  # Get the appropriate milestone data
  milestone_data_obj <- get_milestone_data(milestone_results, milestone_type, milestone_system)
  
  if (is.null(milestone_data_obj)) {
    return(list(
      spider_plot = NULL,
      summary_stats = NULL,
      message = paste("No", milestone_type, milestone_system, "data available")
    ))
  }
  
  milestone_data <- milestone_data_obj$data
  median_data <- milestone_data_obj$medians
  
  # Create enhanced spider plot
  spider_plot <- create_enhanced_milestone_spider_plot(
    milestone_data = milestone_data,
    median_data = median_data,
    resident_id = resident_id,
    period_text = period_text,
    milestone_type = milestone_type,
    resident_data = resident_data
  )

  # Dynamically detect which period field exists in the data
  period_field <- NULL
  if ("acgme_mile_period" %in% names(milestone_data)) {
    period_field <- "acgme_mile_period"
  } else if ("prog_mile_period_self" %in% names(milestone_data)) {
    period_field <- "prog_mile_period_self"
  } else if ("prog_mile_period" %in% names(milestone_data)) {
    period_field <- "prog_mile_period"
  }

  # Calculate summary statistics
  # Try filtering by period_name first (text match), then by detected period field
  individual_data <- milestone_data %>%
    dplyr::filter(record_id == !!resident_id)

  if ("period_name" %in% names(individual_data)) {
    individual_data <- individual_data %>%
      dplyr::filter(period_name == !!period_text)
  } else if (!is.null(period_field) && period_field %in% names(individual_data)) {
    period_sym <- rlang::sym(period_field)
    individual_data <- individual_data %>%
      dplyr::filter(!!period_sym == !!period_text)
  }
  
  if (nrow(individual_data) > 0) {
    milestone_cols <- get_milestone_columns_simple(milestone_data, milestone_type)
    
    if (length(milestone_cols) > 0) {
      scores <- as.numeric(individual_data[1, milestone_cols])
      scores <- scores[!is.na(scores)]
      
      summary_stats <- list(
        n_milestones = length(scores),
        mean_score = round(mean(scores, na.rm = TRUE), 2),
        median_score = round(median(scores, na.rm = TRUE), 2),
        min_score = min(scores, na.rm = TRUE),
        max_score = max(scores, na.rm = TRUE),
        competent_or_above = sum(scores >= 5, na.rm = TRUE),  # Adjusted for 9-point scale
        proficient_or_above = sum(scores >= 7, na.rm = TRUE)  # Adjusted for 9-point scale
      )
    } else {
      summary_stats <- NULL
    }
  } else {
    summary_stats <- NULL
  }
  
  return(list(
    spider_plot = spider_plot,
    summary_stats = summary_stats,
    resident_id = resident_id,
    period_text = period_text,
    milestone_type = milestone_type,
    milestone_system = milestone_system
  ))
}


