#' Get milestone field information from data dictionary
#'
#' @param data_dict data.table of REDCap data dictionary
#' @param eval_type "self" for self-evaluation or "acgme" for ACGME evaluation
#' @export
get_milestone_fields_from_dict <- function(data_dict, eval_type = "self") {
  
  # Pattern for milestone fields
  if (eval_type == "self") {
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)[0-9]+_self$"
  } else {
    pattern <- "^rep_(pc|mk|sbp|pbl|prof|ics)[0-9]+$"
  }
  
  # Get rating fields
  rating_fields <- data_dict[grepl(pattern, `Variable / Field Name`)]
  
  # Extract competency info from field names
  rating_fields[, `:=`(
    competency_code = sub("^rep_([a-z]+)[0-9]+.*$", "\\1", `Variable / Field Name`),
    subcomp_num = as.integer(sub("^rep_[a-z]+([0-9]+).*$", "\\1", `Variable / Field Name`))
  )]
  
  # Map competency codes to full names
  comp_map <- c(
    "pc" = "Patient Care",
    "mk" = "Medical Knowledge",
    "sbp" = "Systems-Based Practice",
    "pbl" = "Practice-Based Learning and Improvement",
    "prof" = "Professionalism",
    "ics" = "Interpersonal and Communication Skills"
  )
  
  rating_fields[, competency := comp_map[competency_code]]
  
  # Get description fields for each rating field
  rating_fields[, desc_fields := purrr::map_chr(`Variable / Field Name`, function(field) {
    base_pattern <- sub("_self$", "_self_desc", field)
    desc_vars <- data_dict[grepl(paste0("^", base_pattern), `Variable / Field Name`), 
                           `Variable / Field Name`]
    if (length(desc_vars) == 0) return(NA_character_)
    paste(desc_vars, collapse = ",")
  })]
  
  # Create display order
  rating_fields[, display_order := .I]
  
  # Extract short label for subcompetency
  rating_fields[, subcompetency := sub("^<[^>]+>", "", `Field Label`)]
  rating_fields[, subcompetency := sub(":.*$", "", subcompetency)]
  rating_fields[, subcompetency := trimws(subcompetency)]
  
  # Select and rename columns
  result <- rating_fields[, .(
    rating_field = `Variable / Field Name`,
    desc_fields,
    competency,
    competency_code,
    subcomp_num,
    subcompetency,
    display_order,
    field_label = `Field Label`
  )]
  
  data.table::setorder(result, competency_code, subcomp_num)
  result[, display_order := .I]
  
  result
}

#' Create milestone competency short labels
#'
#' @export
milestone_short_labels <- function() {
  c(
    # Patient Care
    "PC1: History", "PC2: Physical Exam", "PC3: Clinical Reasoning",
    "PC4: Mgmt-Inpatient", "PC5: Mgmt-Outpatient", "PC6: Digital Health",
    # Medical Knowledge
    "MK1: Applied Sciences", "MK2: Therapeutics", "MK3: Diagnostics",
    # Systems-Based Practice
    "SBP1: Safety & QI", "SBP2: Navigation", "SBP3: Physician Role",
    # Practice-Based Learning
    "PBL1: Evidence-Based", "PBL2: Reflective",
    # Professionalism
    "PROF1: Behavior", "PROF2: Ethics", "PROF3: Accountability", "PROF4: Well-Being",
    # Interpersonal Communication
    "ICS1: Patient Comm", "ICS2: Team Comm", "ICS3: Documentation"
  )
}