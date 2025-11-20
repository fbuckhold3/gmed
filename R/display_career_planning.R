#' Display Previous Career Planning Data with Enhanced Styling
#'
#' Displays career planning data from previous period with badges, icons, and
#' improved visual hierarchy.
#'
#' @param rdm_data Data frame containing REDCap data OR data structure with $all_forms$s_eval
#' @param record_id Character or numeric record ID
#' @param current_period Current period number (1-7) or list with period_number, or character name
#' @param data_dict Data dictionary for field label lookups
#'
#' @return Shiny UI element (card with career planning display) or NULL
#'
#' @importFrom shiny div icon span h6 strong em p tagList
#' @importFrom dplyr filter
#' @importFrom rlang sym    # ← ADD THIS LINE
#' @export
display_career_planning <- function(rdm_data, record_id, current_period, data_dict) {

  # Handle different data structure formats
  if (!is.null(rdm_data$all_forms) && !is.null(rdm_data$all_forms$s_eval)) {
    s_eval_data <- rdm_data$all_forms$s_eval
  } else {
    s_eval_data <- rdm_data
  }

  # Helper to get field name column
  get_field_name_col <- function() {
    if ("field_name" %in% names(data_dict)) return("field_name")
    if ("Variable / Field Name" %in% names(data_dict)) return("Variable / Field Name")
    if ("Variable...Field.Name" %in% names(data_dict)) return("Variable...Field.Name")
    stop("Cannot find field name column in data dictionary")
  }

  # Helper to get choices column
  get_choices_col <- function() {
    if ("select_choices_or_calculations" %in% names(data_dict)) return("select_choices_or_calculations")
    if ("Choices, Calculations, OR Slider Labels" %in% names(data_dict)) return("Choices, Calculations, OR Slider Labels")
    if ("Choices..Calculations..OR.Slider.Labels" %in% names(data_dict)) return("Choices..Calculations..OR.Slider.Labels")
    stop("Cannot find choices column in data dictionary")
  }

  field_name_col <- get_field_name_col()
  choices_col <- get_choices_col()

  # Helper to get field info from data dict
  get_field_info <- function(field_name_val) {
    data_dict %>%
      dplyr::filter(!!rlang::sym(field_name_col) == !!field_name_val) %>%
      dplyr::slice(1)
  }

  # Helper to get choices string
  get_choices_string <- function(field_row) {
    field_row[[choices_col]]
  }

  # Helper to parse choices - returns named vector
  parse_choices <- function(choices_string) {
    if (is.na(choices_string) || choices_string == "") return(NULL)

    # Split by pipe
    items <- strsplit(choices_string, "\\|")[[1]]
    items <- trimws(items)

    # Parse each item: "code, label"
    codes <- character()
    labels <- character()

    for (item in items) {
      parts <- strsplit(item, ",", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        code <- trimws(parts[1])
        label <- trimws(paste(parts[-1], collapse = ","))
        codes <- c(codes, code)
        labels <- c(labels, label)
      }
    }

    # Return named vector: names = labels, values = codes
    setNames(codes, labels)
  }

  # Convert period to number - handle all types
  current_period_num <- if (is.numeric(current_period)) {
    current_period
  } else if (is.list(current_period) && "period_number" %in% names(current_period)) {
    current_period$period_number
  } else if (is.character(current_period)) {
    period_map <- c(
      "Entering Residency" = 7, "Mid Intern" = 1, "End Intern" = 2,
      "Mid PGY2" = 3, "End PGY2" = 4, "Mid PGY3" = 5, "Graduating" = 6
    )
    period_map[current_period]
  } else {
    as.numeric(current_period)
  }

  # Get previous period data
  prev_period <- current_period_num - 1
  if (prev_period < 1) return(NULL)

  # Filter to previous period's data
  prev_data <- s_eval_data %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == prev_period
    )

  if (nrow(prev_data) == 0) {
    return(NULL)  # Don't show anything if no previous data
  }

  # Decode career path checkboxes
  career_cols <- grep("^s_e_career_path___", names(prev_data), value = TRUE)
  career_selected <- character()
  if (length(career_cols) > 0) {
    career_field <- get_field_info("s_e_career_path")
    career_choices_map <- parse_choices(get_choices_string(career_field))

    for (col in career_cols) {
      if (!is.na(prev_data[[col]][1]) && prev_data[[col]][1] == "1") {
        code <- sub("s_e_career_path___", "", col)
        # Find label for this code
        label <- names(career_choices_map)[career_choices_map == code]
        if (length(label) > 0) {
          career_selected <- c(career_selected, label)
        }
      }
    }
  }

  # Decode fellowship checkboxes
  fellow_cols <- grep("^s_e_fellow___", names(prev_data), value = TRUE)
  fellow_selected <- character()
  if (length(fellow_cols) > 0) {
    fellow_field <- get_field_info("s_e_fellow")
    fellow_choices_map <- parse_choices(get_choices_string(fellow_field))

    for (col in fellow_cols) {
      if (!is.na(prev_data[[col]][1]) && prev_data[[col]][1] == "1") {
        code <- sub("s_e_fellow___", "", col)
        label <- names(fellow_choices_map)[fellow_choices_map == code]
        if (length(label) > 0) {
          fellow_selected <- c(fellow_selected, label)
        }
      }
    }
  }

  # Decode track type checkboxes
  track_cols <- grep("^s_e_track_type___", names(prev_data), value = TRUE)
  track_selected <- character()
  if (length(track_cols) > 0) {
    track_field <- get_field_info("s_e_track_type")
    track_choices_map <- parse_choices(get_choices_string(track_field))

    for (col in track_cols) {
      if (!is.na(prev_data[[col]][1]) && prev_data[[col]][1] == "1") {
        code <- sub("s_e_track_type___", "", col)
        label <- names(track_choices_map)[track_choices_map == code]
        if (length(label) > 0) {
          track_selected <- c(track_selected, label)
        }
      }
    }
  }

  # Build visual display with badges
  content_parts <- list()

  # Career Paths Section
  if (length(career_selected) > 0) {
    career_badges <- lapply(career_selected, function(path) {
      span(
        class = "badge badge-primary mr-1 mb-1",
        style = "font-size: 0.9em; padding: 0.4em 0.8em;",
        path
      )
    })

    content_parts <- c(content_parts, list(
      div(
        class = "mb-3",
        div(
          style = "display: flex; align-items: center; margin-bottom: 0.5rem;",
          icon("briefcase", class = "mr-2", style = "color: #1976d2;"),
          strong("Career Paths")
        ),
        div(
          style = "margin-left: 1.5rem;",
          career_badges
        ),
        if (!is.na(prev_data$s_e_career_oth[1]) && prev_data$s_e_career_oth[1] != "") {
          div(
            class = "text-muted mt-1",
            style = "margin-left: 1.5rem; font-size: 0.9em;",
            em("Other: ", prev_data$s_e_career_oth[1])
          )
        }
      )
    ))
  }

  # Fellowship Interests Section
  if (length(fellow_selected) > 0) {
    fellow_badges <- lapply(fellow_selected, function(fellow) {
      span(
        class = "badge badge-success mr-1 mb-1",
        style = "font-size: 0.9em; padding: 0.4em 0.8em;",
        fellow
      )
    })

    content_parts <- c(content_parts, list(
      div(
        class = "mb-3",
        div(
          style = "display: flex; align-items: center; margin-bottom: 0.5rem;",
          icon("graduation-cap", class = "mr-2", style = "color: #388e3c;"),
          strong("Fellowship Interests")
        ),
        div(
          style = "margin-left: 1.5rem;",
          fellow_badges
        ),
        if (!is.na(prev_data$s_e_fellow_oth[1]) && prev_data$s_e_fellow_oth[1] != "") {
          div(
            class = "text-muted mt-1",
            style = "margin-left: 1.5rem; font-size: 0.9em;",
            em("Other: ", prev_data$s_e_fellow_oth[1])
          )
        }
      )
    ))
  }

  # Track Information Section
  if (!is.na(prev_data$s_e_track[1])) {
    if (prev_data$s_e_track[1] == "1" && length(track_selected) > 0) {
      track_badges <- lapply(track_selected, function(track) {
        span(
          class = "badge badge-info mr-1 mb-1",
          style = "font-size: 0.9em; padding: 0.4em 0.8em;",
          track
        )
      })

      content_parts <- c(content_parts, list(
        div(
          class = "mb-2",
          div(
            style = "display: flex; align-items: center; margin-bottom: 0.5rem;",
            icon("road", class = "mr-2", style = "color: #0288d1;"),
            strong("Program Tracks")
          ),
          div(
            style = "margin-left: 1.5rem;",
            track_badges
          )
        )
      ))
    }
  }

  # Discussion Topics Section
  if (!is.na(prev_data$s_e_discussion[1]) && prev_data$s_e_discussion[1] != "") {
    content_parts <- c(content_parts, list(
      div(
        class = "mb-2",
        div(
          style = "display: flex; align-items: center; margin-bottom: 0.5rem;",
          icon("comments", class = "mr-2", style = "color: #7b1fa2;"),
          strong("Discussion Topics with Mentor")
        ),
        div(
          class = "text-muted",
          style = "margin-left: 1.5rem; font-size: 0.95em; font-style: italic;",
          prev_data$s_e_discussion[1]
        )
      )
    ))
  }

  if (length(content_parts) == 0) {
    return(NULL)  # Don't show anything if no data
  }

  return(
    div(
      class = "card mb-3",
      style = "background-color: #fafafa; border-left: 4px solid #1976d2;",
      div(
        class = "card-body",
        h6(
          class = "card-subtitle mb-3 text-muted",
          icon("history"),
          " Career Planning from Period ",
          prev_period
        ),
        content_parts
      )
    )
  )
}

#' Display Previous Wellness Data with Enhanced Styling
#'
#' Displays wellness data from previous period with subtle card styling
#' and history icon.
#'
#' @param rdm_data Data frame containing REDCap data OR data structure with $all_forms$s_eval
#' @param record_id Character or numeric record ID
#' @param current_period Current period number (1-7) or list with period_number, or character name
#'
#' @return Shiny UI element (card with wellness display) or NULL
#'
#' @importFrom shiny div icon h6 p
#' @importFrom dplyr filter
#' @export
display_wellness <- function(rdm_data, record_id, current_period) {

  # Handle different data structure formats
  if (!is.null(rdm_data$all_forms) && !is.null(rdm_data$all_forms$s_eval)) {
    s_eval_data <- rdm_data$all_forms$s_eval
  } else {
    s_eval_data <- rdm_data
  }

  # Convert period to number - handle all types
  current_period_num <- if (is.numeric(current_period)) {
    current_period
  } else if (is.list(current_period) && "period_number" %in% names(current_period)) {
    current_period$period_number
  } else if (is.character(current_period)) {
    period_map <- c(
      "Entering Residency" = 7, "Mid Intern" = 1, "End Intern" = 2,
      "Mid PGY2" = 3, "End PGY2" = 4, "Mid PGY3" = 5, "Graduating" = 6
    )
    period_map[current_period]
  } else {
    as.numeric(current_period)
  }

  # Get previous period data
  prev_period <- current_period_num - 1
  if (prev_period < 1) return(NULL)

  # Filter to previous period's data
  prev_data <- s_eval_data %>%
    dplyr::filter(
      record_id == !!record_id,
      redcap_repeat_instrument == "s_eval",
      redcap_repeat_instance == prev_period
    )

  if (nrow(prev_data) == 0 || is.na(prev_data$s_e_well[1]) || prev_data$s_e_well[1] == "") {
    return(NULL)  # Don't show anything if no previous data
  }

  return(
    div(
      class = "card mb-3",
      style = "background-color: #f5f5f5; border-left: 4px solid #9575cd;",
      div(
        class = "card-body",
        h6(class = "card-subtitle mb-2 text-muted",
           icon("history"), " Previous Wellness (Period ", prev_period, ")"),
        p(class = "card-text", style = "font-style: italic;", prev_data$s_e_well[1])
      )
    )
  )
}
