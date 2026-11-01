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

#' Get Human-Readable Assessment Labels
#'
#' Creates a named vector mapping field names to human-readable labels
#'
#' @param data_dict Data dictionary from load_rdm_complete()
#' @param form_name Form name (default: "assessment")
#'
#' @return Named character vector (names = field_names, values = labels)
get_assessment_labels <- function(data_dict, form_name = "assessment") {

  labels <- data_dict %>%
    dplyr::filter(form_name == !!form_name) %>%
    dplyr::select(field_name, field_label) %>%
    tibble::deframe()

  return(labels)
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
