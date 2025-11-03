#' Display Career Planning Data
#'
#' Shows career planning goals and paths from current or previous periods
#'
#' @param rdm_data Data frame containing REDCap data
#' @param record_id The record ID to display data for
#' @param current_period Current period number (1-7)
#' @param data_dict Data dictionary for translating coded values
#'
#' @return A tagList containing the formatted display
#' @export
display_career_planning <- function(rdm_data, record_id, current_period, data_dict) {
  
  # Filter to this record
  record_data <- rdm_data %>%
    dplyr::filter(record_id == !!record_id)
  
  # Helper to get field name column
  get_field_name_col <- function() {
    if ("field_name" %in% names(data_dict)) return("field_name")
    if ("Variable / Field Name" %in% names(data_dict)) return("Variable / Field Name")
    if ("Variable...Field.Name" %in% names(data_dict)) return("Variable...Field.Name")
    stop("Cannot find field name column in data dictionary")
  }
  
  # Helper to get field label column
  get_field_label_col <- function() {
    if ("field_label" %in% names(data_dict)) return("field_label")
    if ("Field Label" %in% names(data_dict)) return("Field Label")
    if ("Field.Label" %in% names(data_dict)) return("Field.Label")
    stop("Cannot find field label column in data dictionary")
  }
  
  # Helper to get choices column
  get_choices_col <- function() {
    if ("select_choices_or_calculations" %in% names(data_dict)) return("select_choices_or_calculations")
    if ("Choices, Calculations, OR Slider Labels" %in% names(data_dict)) return("Choices, Calculations, OR Slider Labels")
    if ("Choices..Calculations..OR.Slider.Labels" %in% names(data_dict)) return("Choices..Calculations..OR.Slider.Labels")
    stop("Cannot find choices column in data dictionary")
  }
  
  field_name_col <- get_field_name_col()
  field_label_col <- get_field_label_col()
  choices_col <- get_choices_col()
  
  # Helper function to get field label from data dict
  get_field_label <- function(field_name) {
    label <- data_dict %>%
      dplyr::filter(!!rlang::sym(field_name_col) == field_name) %>%
      dplyr::pull(!!rlang::sym(field_label_col))
    if (length(label) == 0) return(field_name)
    label[1]
  }
  
  # Helper to parse choices from data dict
  parse_choices_from_dict <- function(field_name) {
    choices_raw <- data_dict %>%
      dplyr::filter(!!rlang::sym(field_name_col) == field_name) %>%
      dplyr::pull(!!rlang::sym(choices_col))
    
    if (length(choices_raw) == 0 || is.na(choices_raw[1])) return(NULL)
    
    # Parse choices manually
    items <- strsplit(choices_raw[1], "\\|")[[1]]
    items <- trimws(items)
    
    result <- list()
    for (item in items) {
      parts <- strsplit(item, ",", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        code <- trimws(parts[1])
        label <- trimws(paste(parts[-1], collapse = ","))
        result[[code]] <- label
      }
    }
    return(result)
  }
  
  # Helper to decode checkbox values
  decode_checkboxes <- function(data_row, field_base_name) {
    # Find all checkbox fields for this base field (field___1, field___2, etc.)
    checkbox_pattern <- paste0("^", field_base_name, "___")
    checkbox_cols <- grep(checkbox_pattern, names(data_row), value = TRUE)
    
    if (length(checkbox_cols) == 0) return(NULL)
    
    # Get the codes that are checked
    checked_codes <- character()
    for (col in checkbox_cols) {
      if (!is.na(data_row[[col]]) && data_row[[col]] == "1") {
        # Extract code from field name (e.g., "s_e_career_path___2" -> "2")
        code <- sub(paste0(field_base_name, "___"), "", col)
        checked_codes <- c(checked_codes, code)
      }
    }
    
    if (length(checked_codes) == 0) return(NULL)
    
    # Get the labels for these codes
    choices <- parse_choices_from_dict(field_base_name)
    if (is.null(choices)) return(paste(checked_codes, collapse = ", "))
    
    labels <- sapply(checked_codes, function(code) {
      choices[[code]] %||% code
    })
    
    return(paste(labels, collapse = ", "))
  }
  
  # Map period to number if it's text
  period_map <- c(
    "Entering Residency" = "7",
    "Mid Intern" = "1",
    "End Intern" = "2",
    "Mid PGY2" = "3",
    "End PGY2" = "4",
    "Mid PGY3" = "5",
    "Graduating" = "6"
  )
  
  current_period_num <- if (is.numeric(current_period)) {
    as.character(current_period)
  } else if (current_period %in% names(period_map)) {
    period_map[current_period]
  } else {
    as.character(current_period)
  }
  
  # Determine which period to show data FROM
  if (current_period_num == "1") {
    # Period 1: Show UME goals from period 7
    display_period <- "7"
    period_data <- record_data %>%
      dplyr::filter(
        redcap_repeat_instrument == "S Eval", 
        s_e_period == display_period
      )
    
    if (nrow(period_data) == 0) {
      return(shiny::tagList(
        shiny::h4("Career Planning Goals"),
        shiny::p("No UME goals data available yet.", class = "text-muted")
      ))
    }
    
    goal1 <- period_data$s_e_ume_goal1[1]
    goal2 <- period_data$s_e_ume_goal2[1]
    goal3 <- period_data$s_e_ume_goal3[1]
    
    return(shiny::tagList(
      shiny::h4("Your UME Goals"),
      shiny::p(class = "text-muted", "From your entry into residency (Period 7)"),
      if (!is.na(goal1) && goal1 != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_ume_goal1")),
          shiny::p(goal1)
        )
      },
      if (!is.na(goal2) && goal2 != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_ume_goal2")),
          shiny::p(goal2)
        )
      },
      if (!is.na(goal3) && goal3 != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_ume_goal3")),
          shiny::p(goal3)
        )
      }
    ))
    
  } else {
  # Period 2+: Show career planning from PREVIOUS period
  previous_period_num <- as.character(as.numeric(current_period_num) - 1)
  
  # Map to TEXT version of period (what's actually in the data)
  reverse_period_map <- c(
    "7" = "Entering Residency",
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduating"
  )
  
  previous_period_text <- reverse_period_map[previous_period_num]
  
  period_data <- record_data %>%
    dplyr::filter(
      redcap_repeat_instrument == "S Eval",
      s_e_period == previous_period_text  # CHANGED: Only check text version
    )
    
    if (nrow(period_data) == 0) {
      return(shiny::tagList(
        shiny::h4("Career Planning"),
        shiny::p(paste0("No career planning data from Period ", previous_period_num, " yet."), 
                class = "text-muted")
      ))
    }
    
    # Get single row
    period_row <- period_data[1, ]
    
    # Decode checkbox fields
    career_path_labels <- decode_checkboxes(period_row, "s_e_career_path")
    fellow_labels <- decode_checkboxes(period_row, "s_e_fellow")
    track_type_labels <- decode_checkboxes(period_row, "s_e_track_type")
    
    # Get text fields - FIXED: s_e_well not s_e_wellness
    career_oth <- period_row$s_e_career_oth[1]
    fellow_oth <- period_row$s_e_fellow_oth[1]
    wellness <- period_row$s_e_well[1]
    track <- period_row$s_e_track[1]
    
    return(shiny::tagList(
      shiny::h4(paste0("Career Planning (from Period ", previous_period_num, ")")),
      
      # Wellness
      if (!is.na(wellness) && wellness != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_well")),
          shiny::p(wellness)
        )
      },
      
      # Career path
      if (!is.null(career_path_labels) && career_path_labels != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_career_path")),
          shiny::p(
            career_path_labels,
            if (!is.na(career_oth) && career_oth != "") {
              shiny::tags$span(
                " (", shiny::em(career_oth), ")"
              )
            }
          )
        )
      },
      
      # Fellowship
      if (!is.null(fellow_labels) && fellow_labels != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_fellow")),
          shiny::p(
            fellow_labels,
            if (!is.na(fellow_oth) && fellow_oth != "") {
              shiny::tags$span(
                " (", shiny::em(fellow_oth), ")"
              )
            }
          )
        )
      },
      
      # Track
      if (!is.na(track) && track != "") {
        shiny::div(
          class = "mb-3",
          shiny::strong(get_field_label("s_e_track")),
          shiny::p(
            if (track == "1" || tolower(track) == "yes") {
              if (!is.null(track_type_labels) && track_type_labels != "") {
                paste("Yes:", track_type_labels)
              } else {
                "Yes"
              }
            } else {
              "No"
            }
          )
        )
      }
    ))
  }
}