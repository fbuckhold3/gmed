# milestone_data_workflow.R
# Complete milestone data workflow functions for gmed package

#' Calculate Milestone Medians for All Forms
#'
#' Processes milestone data and calculates medians for all available milestone forms.
#' Works with data loaded from load_data_by_forms().
#'
#' @param data_list List from load_data_by_forms() containing organized form data
#' @param verbose Boolean. Print progress messages (default: TRUE)
#'
#' @return List containing processed milestone data and medians for each form
#' @export
calculate_all_milestone_medians <- function(data_list, verbose = TRUE) {

  if (verbose) {
    message("=== CALCULATING MILESTONE MEDIANS FOR ALL FORMS ===")
  }

  # Check available milestone forms
  milestone_forms <- c("milestone_entry", "milestone_selfevaluation_c33c", "acgme_miles")
  available_forms <- intersect(milestone_forms, names(data_list$forms))

  if (verbose) {
    message("Available forms: ", paste(available_forms, collapse = ", "))
  }
  
  # Store results
  results <- list()
  
  for (form_name in available_forms) {
    if (verbose) {
      message("\n--- Processing ", form_name, " ---")
    }

    form_data <- data_list$forms[[form_name]]
    if (verbose) {
      message("Raw data rows: ", nrow(form_data))
    }

    if (nrow(form_data) == 0) {
      if (verbose) {
        message("No data - skipping")
      }
      next
    }
    
    # Find milestone columns for this form
    all_cols <- names(form_data)
    
    # Different column patterns for each form type
    rep_program_cols <- all_cols[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+$", all_cols)]
    rep_self_cols <- all_cols[grepl("^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", all_cols)]
    acgme_cols <- all_cols[grepl("^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$", all_cols)]
    
    # Determine form type and columns
    if (grepl("selfevaluation", form_name) && length(rep_self_cols) > 0) {
      milestone_cols <- rep_self_cols
      milestone_type <- "self"
    } else if (grepl("acgme", form_name) && length(acgme_cols) > 0) {
      milestone_cols <- acgme_cols  
      milestone_type <- "acgme"
    } else if (length(rep_program_cols) > 0) {
      milestone_cols <- rep_program_cols
      milestone_type <- "program"
    } else {
      if (verbose) {
        message("No milestone columns found - skipping")
      }
      next
    }

    if (verbose) {
      message("Using ", length(milestone_cols), " ", milestone_type, " milestone columns")
      message("Sample columns: ", paste(head(milestone_cols, 3), collapse = ", "))
    }
    
    # Process the data
    tryCatch({
      
      # Fix period column name for self-evaluations
      if (form_name == "milestone_selfevaluation_c33c") {
        if ("prog_mile_period_self" %in% names(form_data)) {
          form_data$prog_mile_period <- form_data$prog_mile_period_self
        }
      }
      
      # Process milestone data using gmed function
      processed_data <- process_milestone_data_simple(form_data, type = milestone_type)

      if (!is.null(processed_data)) {
        if (verbose) {
          message("Processed rows: ", nrow(processed_data))
        }

        # Calculate medians manually (works for all column patterns)
        medians <- processed_data %>%
          dplyr::group_by(prog_mile_period, period_name) %>%
          dplyr::summarise(
            dplyr::across(dplyr::all_of(milestone_cols), ~ median(.x, na.rm = TRUE)),
            n_residents = dplyr::n(),
            .groups = "drop"
          )

        if (nrow(medians) > 0) {
          if (verbose) {
            message("SUCCESS: Calculated medians for ", nrow(medians), " periods")
          }

          # Store results
          results[[form_name]] <- list(
            processed_data = processed_data,
            medians = medians,
            type = milestone_type,
            columns = milestone_cols,
            form_name = form_name
          )

        } else {
          if (verbose) {
            message("No medians calculated")
          }
        }

      } else {
        if (verbose) {
          message("Failed to process data")
        }
      }

    }, error = function(e) {
      if (verbose) {
        message("Error: ", e$message)
      }
    })
  }
  
  # Summary
  if (verbose) {
    message("\n=== MILESTONE MEDIANS SUMMARY ===")
    for (form_name in names(results)) {
      result <- results[[form_name]]
      message(form_name, ":")
      message("  Type: ", result$type)
      message("  Columns: ", length(result$columns))
      message("  Periods: ", nrow(result$medians))
      message("  \u2705 SUCCESS")
    }
  }

  return(results)
}

#' Complete Milestone Data Workflow
#'
#' Takes already-loaded data, calculates medians, filters archived residents, and prepares 
#' milestone data for app usage with separate dataframes for individuals and medians.
#'
#' @param data_list List from load_data_by_forms() containing organized form data
#' @param verbose Print progress messages
#'
#' @return List with organized milestone data ready for app usage
#' @export
prepare_milestone_app_data <- function(data_list, verbose = TRUE) {

  if (verbose) {
    message("=== PREPARING MILESTONE APP DATA ===")
  }

  # Step 1: Calculate milestone medians
  if (verbose) {
    message("1. Calculating milestone medians...")
  }
  milestone_results <- calculate_all_milestone_medians(data_list, verbose = verbose)

  # Step 2: Filter archived residents from individual data
  if (verbose) {
    message("2. Filtering archived residents...")
  }
  
  # Get clean resident list (non-archived)
  clean_residents <- data_list$resident_data
  archived_names <- character(0)
  
  if ("res_archive" %in% names(clean_residents)) {
    archived_names <- clean_residents %>%
      dplyr::filter(res_archive == "Yes") %>%
      dplyr::pull(name)

    if (length(archived_names) > 0) {
      if (verbose) {
        message("   Found ", length(archived_names), " archived residents to remove")
      }
      clean_residents <- clean_residents %>%
        dplyr::filter(!name %in% archived_names)
    }
  }

  # Step 3: Calculate resident levels
  if (verbose) {
    message("3. Calculating resident levels...")
  }
  clean_residents <- calculate_resident_level(clean_residents)
  
  # Step 4: Create cleaned milestone datasets (individuals only, no archived)
  cleaned_milestone_data <- list()
  
  for (form_name in names(milestone_results)) {
    result <- milestone_results[[form_name]]
    
    # Filter out archived residents from processed milestone data
    if ("name" %in% names(result$processed_data)) {
      clean_milestone_data <- result$processed_data %>%
        dplyr::filter(!name %in% archived_names)
    } else {
      # If no name column, filter by record_id
      clean_record_ids <- clean_residents$record_id
      clean_milestone_data <- result$processed_data %>%
        dplyr::filter(record_id %in% clean_record_ids)
    }
    
    cleaned_milestone_data[[form_name]] <- clean_milestone_data

    if (verbose) {
      message("   ", form_name, ": ", nrow(result$processed_data), " -> ",
          nrow(clean_milestone_data), " rows after archive filter")
    }
  }
  
  # Step 5: Organize final app data structure
  app_data <- list(
    # Individual milestone data (cleaned, ready for filtering/plotting)
    milestone_individual = cleaned_milestone_data,
    
    # Median data (for comparison lines/reference)
    milestone_medians = lapply(milestone_results, function(x) x$medians),
    
    # Resident data (cleaned, for demographics/filtering)
    residents = clean_residents,
    
    # Metadata
    milestone_metadata = lapply(milestone_results, function(x) {
      list(
        type = x$type,
        columns = x$columns,
        form_name = x$form_name
      )
    }),
    
    # Raw data (if needed for other purposes)
    raw_data = data_list
  )

  if (verbose) {
    message("4. Final app data structure:")
    message("   - milestone_individual: individual resident milestone data")
    message("   - milestone_medians: median reference data by period")
    message("   - residents: ", nrow(app_data$residents), " active residents")
    message("   - milestone_metadata: column info and form types")
  }

  return(app_data)
}

#' Join Individual Milestone Data with Medians for Plotting
#'
#' Helper function to join individual and median data when needed for visualization
#'
#' @param individual_data Individual milestone data for one form
#' @param median_data Median data for the same form  
#' @param milestone_cols Vector of milestone column names
#'
#' @return Data frame with individual and median values for plotting
#' @export
join_individual_with_medians <- function(individual_data, median_data, milestone_cols) {
  
  # Join individual data with corresponding medians
  joined_data <- individual_data %>%
    dplyr::left_join(
      median_data %>% 
        dplyr::select(prog_mile_period, dplyr::all_of(milestone_cols)) %>%
        dplyr::rename_with(~ paste0(.x, "_median"), dplyr::all_of(milestone_cols)),
      by = "prog_mile_period"
    )
  
  return(joined_data)
}