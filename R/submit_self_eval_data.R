#' Submit Self-Evaluation Data to REDCap
#'
#' @param record_id Resident record ID
#' @param period Period number
#' @param data List of field data to submit
#'
#' @return List with success status and message
#' @export
submit_self_eval_data <- function(record_id, period, data) {
  
  tryCatch({
    # Get instance number for this period using gmed function
    instance_number <- gmed::get_redcap_instance(
      level = switch(as.character(period),
                     "7" = "Intern", "1" = "Intern", "2" = "Intern",
                     "3" = "PGY2", "4" = "PGY2",
                     "5" = "PGY3", "6" = "PGY3",
                     "Intern"),
      period = period,
      review_type = "scheduled",
      redcap_url = "https://redcapsurvey.slu.edu/api/",
      redcap_token = Sys.getenv("RDM_TOKEN"),
      record_id = record_id
    )
    
    # Prepare data for REDCap
    redcap_data <- list(
      record_id = as.character(record_id),
      redcap_repeat_instrument = "s_eval",
      redcap_repeat_instance = as.character(instance_number),
      s_e_period = as.character(period)
    )
    
    # Add all form fields
    redcap_data <- c(redcap_data, data)
    
    # Mark as complete
    redcap_data[["s_eval_complete"]] <- "2"
    
    # Convert to data frame for REDCap
    redcap_df <- as.data.frame(redcap_data, stringsAsFactors = FALSE)
    
    # Submit to REDCap
    response <- httr::POST(
      url = "https://redcapsurvey.slu.edu/api/",
      body = list(
        token = Sys.getenv("RDM_TOKEN"),
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "overwrite",
        data = jsonlite::toJSON(redcap_df, auto_unbox = TRUE),
        returnContent = "ids",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
    
    if (httr::status_code(response) == 200) {
      message("Self-evaluation data submitted successfully to instance ", instance_number)
      return(list(
        success = TRUE, 
        instance = instance_number,
        message = "Self-evaluation submitted successfully"
      ))
    } else {
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      return(list(
        success = FALSE, 
        message = paste("REDCap error:", error_text)
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE, 
      message = paste("Submission error:", e$message)
    ))
  })
}