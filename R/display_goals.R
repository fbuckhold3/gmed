#' Display Previous Goals from ILP Data
#'
#' Shows milestone-based goals set by resident in previous periods.
#' Designed to be flexible for use across multiple applications.
#'
#' @param ilp_data Data frame containing ILP instrument data from REDCap
#' @param record_id Character. The resident's record ID
#' @param period Numeric. Period number to display goals from (1-7)
#' @param domain Character. Which domain to display: "pcmk", "sbppbl", "profics", or "all"
#' @param return_format Character. "list" for structured data, "html" for formatted display
#'
#' @return Based on return_format: list of goal data or HTML formatted string
#' @export
#' @examples
#' \dontrun{
#' # Get all goals for a resident from period 2
#' goals <- display_goals(ilp_data, record_id = "1001", period = 2)
#' 
#' # Get just PC/MK goals formatted for HTML display
#' html <- display_goals(ilp_data, "1001", 2, domain = "pcmk", return_format = "html")
#' }
display_goals <- function(ilp_data, record_id, period, domain = "all", return_format = "list") {
  
  # Input validation
  if (is.null(ilp_data) || nrow(ilp_data) == 0) {
    return(NULL)
  }
  
  # Ensure period is numeric
  period_num <- as.numeric(period)
  
  # Filter to the specific record and period
  goals_data <- ilp_data[ilp_data$record_id == record_id & 
                          ilp_data$year_resident == period_num, ]
  
  if (nrow(goals_data) == 0) {
    return(NULL)
  }
  
  # Take first row if multiple instances
  goals_data <- goals_data[1, ]
  
  # Extract goals by domain
  extract_domain_goal <- function(data, domain_key) {
    
    # Field mappings for each domain
    domain_fields <- list(
      pcmk = list(
        prior = "prior_goal_pcmk",
        review = c("review_q_pcmk", "review_q2_pcmk"),
        goal = "goal_pcmk",
        level = "goal_level_pcmk",
        row = "goal_level_r_pcmk",
        how = "how_pcmk"
      ),
      sbppbl = list(
        prior = "prior_goal_sbppbl",
        review = c("review_q_sbppbl", "review_q2_sbppbl"),
        goal = "goal_sbppbl",
        level = "goal_level_sbppbl",
        row = "goal_r_sbppbl",
        how = "how_sbppbl"
      ),
      profics = list(
        prior = "prior_goal_profics",
        review = c("review_q_profics", "review_q2_profics"),
        goal = "goal_subcomp_profics",
        level = "goal_level_profics",
        row = "goal_r_profics",
        how = "how_profics"
      )
    )
    
    fields <- domain_fields[[domain_key]]
    if (is.null(fields)) return(NULL)
    
    # Extract data
    goal_info <- list(
      domain = domain_key,
      prior_goal_reached = data[[fields$prior]],
      review = NA,
      subcompetency = data[[fields$goal]],
      level = data[[fields$level]],
      row = data[[fields$row]],
      how_to_achieve = data[[fields$how]]
    )
    
    # Get the appropriate review field based on prior_goal_reached
    if (!is.na(goal_info$prior_goal_reached) && !is.null(goal_info$prior_goal_reached)) {
      if (goal_info$prior_goal_reached == "1") {
        goal_info$review <- data[[fields$review[2]]]  # review_q2
      } else {
        goal_info$review <- data[[fields$review[1]]]  # review_q
      }
    }
    
    # Check if this goal has any data
    has_data <- any(
      !is.na(unlist(goal_info[c("subcompetency", "level", "row", "how_to_achieve")])) &
      unlist(goal_info[c("subcompetency", "level", "row", "how_to_achieve")]) != ""
    )
    
    if (!has_data) return(NULL)
    
    return(goal_info)
  }
  
  # Extract based on domain request
  if (domain == "all") {
    result <- list(
      pcmk = extract_domain_goal(goals_data, "pcmk"),
      sbppbl = extract_domain_goal(goals_data, "sbppbl"),
      profics = extract_domain_goal(goals_data, "profics")
    )
    # Remove NULL entries
    result <- result[!sapply(result, is.null)]
  } else {
    result <- list()
    result[[domain]] <- extract_domain_goal(goals_data, domain)
    if (is.null(result[[domain]])) return(NULL)
  }
  
  # Return based on format
  if (return_format == "html") {
    return(format_goals_html(result))
  } else {
    return(result)
  }
}


#' Format Goals for HTML Display
#'
#' Internal helper to format goal data as HTML
#' @param goals_list List output from display_goals domain extraction
#' @return Character string with HTML formatted text
#' @keywords internal
format_goals_html <- function(goals_list) {
  
  if (is.null(goals_list) || length(goals_list) == 0) {
    return("<p class='text-muted'>No previous goals found.</p>")
  }
  
  # Domain name mappings
  domain_names <- c(
    pcmk = "Patient Care / Medical Knowledge",
    sbppbl = "Systems-Based Practice / Practice-Based Learning",
    profics = "Professionalism / Interpersonal and Communication Skills"
  )
  
  # Build formatted output
  output <- "<div class='previous-goals-display'>"
  
  for (domain in names(goals_list)) {
    goal <- goals_list[[domain]]
    if (is.null(goal)) next
    
    output <- paste0(output, "<div class='goal-domain' style='margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-left: 4px solid #0072B2; border-radius: 4px;'>")
    output <- paste0(output, "<h5 style='color: #0072B2; margin-top: 0;'>", domain_names[domain], "</h5>")
    
    if (!is.na(goal$subcompetency) && nzchar(as.character(goal$subcompetency))) {
      output <- paste0(output, "<p><strong>Subcompetency:</strong> ", goal$subcompetency, "</p>")
    }
    
    if (!is.na(goal$level) && !is.na(goal$row)) {
      output <- paste0(output, "<p><strong>Target:</strong> Level ", goal$level, ", Row ", goal$row, "</p>")
    }
    
    if (!is.na(goal$how_to_achieve) && nzchar(trimws(goal$how_to_achieve))) {
      output <- paste0(output, "<p><strong>Plan:</strong> ", htmltools::htmlEscape(goal$how_to_achieve), "</p>")
    }
    
    output <- paste0(output, "</div>")
  }
  
  output <- paste0(output, "</div>")
  
  return(output)
}