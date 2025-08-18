#' @title Field Mapping Functions for GMED
#' @description Functions for mapping REDCap field codes to readable labels using data dictionary
#' @name field_mapping
NULL

#' Get Coach Name from Numeric Code
#'
#' @param coach_code Numeric or character code for coach
#' @param data_dict Data dictionary data frame (optional)
#'
#' @return Character string with coach name
#' @export
get_coach_name_from_code <- function(coach_code, data_dict = NULL) {
  if (is.na(coach_code) || coach_code == "" || is.null(coach_code)) {
    return(coach_code)
  }
  
  coach_code <- as.character(coach_code)
  
  # RDM 2.0 data dictionary mapping
  coach_mapping <- c(
    "1" = "Shieh", "3" = "Pollard", "4" = "Kunnath", "5" = "Mar", 
    "6" = "Purdy", "7" = "Kamel", "8" = "Freedle", "9" = "Cumming", 
    "10" = "Reid", "11" = "Can", "13" = "Walentik", "14" = "Ferguson", 
    "16" = "Buckhold", "18" = "Wheeler", "19" = "Morreale", "20" = "Bastin", 
    "21" = "Robin", "22" = "Fernelius", "23" = "Kent", "24" = "Karches"
  )
  
  result <- coach_mapping[coach_code]
  return(if(is.na(result)) coach_code else result)
}

#' Convert Coach Codes to Names in Dataset
#'
#' @param resident_data Data frame with coach codes
#' @param data_dict Data dictionary data frame (optional)
#'
#' @return Data frame with coach names instead of codes
#' @export
convert_coach_codes_to_names <- function(resident_data, data_dict = NULL) {
  if (is.null(resident_data) || nrow(resident_data) == 0) {
    return(resident_data)
  }
  
  message("Converting coach codes to names...")
  
  if ("coach" %in% names(resident_data)) {
    original_coaches <- unique(resident_data$coach[!is.na(resident_data$coach) & resident_data$coach != ""])
    if (length(original_coaches) > 0) {
      message("Original coach codes: ", paste(original_coaches, collapse = ", "))
    }
    
    resident_data$coach <- sapply(resident_data$coach, function(code) {
      get_coach_name_from_code(code, data_dict)
    })
    
    new_coaches <- unique(resident_data$coach[!is.na(resident_data$coach) & resident_data$coach != ""])
    if (length(new_coaches) > 0) {
      message("Converted coach names: ", paste(new_coaches, collapse = ", "))
    }
  }
  
  if ("second_rev" %in% names(resident_data)) {
    resident_data$second_rev <- sapply(resident_data$second_rev, function(code) {
      get_coach_name_from_code(code, data_dict)
    })
  }
  
  return(resident_data)
}
