#' Display Scholarship Summary with Safety Badges and Categorized Tables
#'
#' @param scholarship_data Data frame with scholarship records from REDCap
#' @param data_dict Data frame with REDCap data dictionary for field labels
#' @return A list with display elements (badges and categorized tables)
display_scholarship <- function(scholarship_data, data_dict = NULL) {

  # Initialize empty result
  result <- list(
    badges = list(ps = FALSE, rca = FALSE),
    qi_projects = data.frame(),
    research_projects = data.frame(),
    presentations = data.frame(),
    publications = data.frame(),
    committees = data.frame()
  )

  # Handle NULL or empty data
  if (is.null(scholarship_data)) {
    return(result)
  }

  if (!is.data.frame(scholarship_data)) {
    warning("scholarship_data must be a data frame")
    return(result)
  }

  if (nrow(scholarship_data) == 0) {
    return(result)
  }

  # Check completion badges - handle NA values properly
  result$badges <- list(
    ps = any(scholarship_data$schol_ps == "1", na.rm = TRUE),
    rca = any(scholarship_data$schol_rca == "1", na.rm = TRUE)
  )

  # Standardize data dictionary column names
  standardize_dict_names <- function(dict) {
    if (is.null(dict)) return(NULL)

    # Map various possible column names to standard names
    name_mapping <- list(
      field_name = c("field_name", "Variable / Field Name", "Variable...Field.Name"),
      field_label = c("field_label", "Field Label", "Field.Label"),
      choices = c("select_choices_or_calculations", "Choices, Calculations, OR Slider Labels",
                  "Choices..Calculations..OR.Slider.Labels")
    )

    for (standard_name in names(name_mapping)) {
      possible_names <- name_mapping[[standard_name]]
      for (poss_name in possible_names) {
        if (poss_name %in% names(dict)) {
          names(dict)[names(dict) == poss_name] <- standard_name
          break
        }
      }
    }

    return(dict)
  }

  # Standardize the data dictionary
  data_dict <- standardize_dict_names(data_dict)

  # Get choice label from data dictionary - VECTORIZED VERSION
  get_choice_label <- function(field_name, values) {
    # Return early if no data_dict
    if (is.null(data_dict) || !is.data.frame(data_dict)) {
      return(values)
    }

    # Check if standardized columns exist
    if (!"field_name" %in% names(data_dict) || !"choices" %in% names(data_dict)) {
      return(values)
    }

    # Get the choices for this field
    dict_row <- data_dict[data_dict$field_name == field_name, ]
    if (nrow(dict_row) == 0) return(values)

    choices_raw <- dict_row$choices[1]
    if (is.na(choices_raw) || choices_raw == "") return(values)

    # Build lookup table - handle both "1, Label" and "1,Label" formats
    choices <- strsplit(choices_raw, "\\|")[[1]]
    lookup <- list()
    for (choice in choices) {
      # Split on first comma only
      parts <- strsplit(choice, ",")[[1]]
      if (length(parts) >= 2) {
        value <- trimws(parts[1])
        label <- trimws(paste(parts[-1], collapse = ","))
        lookup[[value]] <- label
      }
    }

    # Vectorized lookup
    result <- sapply(values, function(val) {
      if (is.na(val)) return(NA_character_)
      val_str <- as.character(val)
      label <- lookup[[val_str]]
      if (is.null(label)) return(val_str)
      return(label)
    })

    return(result)
  }

  # Filter and process QI Projects (Type 1)
  if ("schol_type" %in% names(scholarship_data)) {
    qi_data <- scholarship_data %>% dplyr::filter(schol_type == "1")
    if (nrow(qi_data) > 0) {
      result$qi_projects <- qi_data %>%
        dplyr::mutate(
          Description = schol_qi,
          Division = get_choice_label("schol_div", schol_div),
          Status = get_choice_label("schol_res_status", schol_res_status),
          Mentor = schol_res_mentor
        ) %>%
        dplyr::select(Description, Division, Status, Mentor)
    }

    # Filter and process Research Projects (Type 3 and 6)
    research_data <- scholarship_data %>% dplyr::filter(schol_type %in% c("3", "6"))
    if (nrow(research_data) > 0) {
      result$research_projects <- research_data %>%
        dplyr::mutate(
          Type = get_choice_label("schol_type", schol_type),
          Description = schol_res,
          Division = get_choice_label("schol_div", schol_div),
          Status = get_choice_label("schol_res_status", schol_res_status),
          Mentor = schol_res_mentor
        ) %>%
        dplyr::select(Type, Description, Division, Status, Mentor)
    }

    # Filter and process Presentations (Type 4 or any with schol_pres == 1)
    pres_data <- scholarship_data %>%
      dplyr::filter(schol_type == "4" | (!is.na(schol_pres) & schol_pres == "1"))
    if (nrow(pres_data) > 0) {
      result$presentations <- pres_data %>%
        dplyr::mutate(
          Conference = schol_pres_conf,
          Location = get_choice_label("schol_pres_type", schol_pres_type)
        ) %>%
        dplyr::select(Conference, Location) %>%
        dplyr::filter(!is.na(Conference) & Conference != "")
    }

    # Filter and process Publications (Type 5 or any with schol_pub == 1)
    pub_data <- scholarship_data %>%
      dplyr::filter(schol_type == "5" | (!is.na(schol_pub) & schol_pub == "1"))
    if (nrow(pub_data) > 0) {
      result$publications <- pub_data %>%
        dplyr::mutate(
          Citation = schol_cit
        ) %>%
        dplyr::select(Citation) %>%
        dplyr::filter(!is.na(Citation) & Citation != "")
    }

    # Filter and process Committees (Type 7)
    comm_data <- scholarship_data %>% dplyr::filter(schol_type == "7")
    if (nrow(comm_data) > 0) {
      result$committees <- comm_data %>%
        dplyr::mutate(
          Committee = schol_comm,
          Type = get_choice_label("schol_comm_type", schol_comm_type)
        ) %>%
        dplyr::select(Committee, Type)
    }
  }

  return(result)
}
