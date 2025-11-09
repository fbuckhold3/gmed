#' Visualize ITE Scores with Specialty Breakdown
#'
#' Creates an HTML table showing ITE percentile scores across training years
#' with specialty breakdowns
#'
#' @param resident_data Data frame containing resident ITE scores
#' @param data_dict Optional data dictionary for field labels
#' @return HTML table (shiny.tag object)
#' @export
visualize_ite_scores <- function(resident_data, data_dict = NULL) {
  
  # Specialty mapping
  specialty_map <- c(
    "total" = "Overall Total",
    "cards" = "Cardiology",
    "endo" = "Endocrinology",
    "gi" = "Gastroenterology",
    "gim" = "General Internal Medicine",
    "geri" = "Geriatrics",
    "hemonc" = "Hematology/Oncology",
    "id" = "Infectious Disease",
    "nephro" = "Nephrology",
    "neuro" = "Neurology",
    "pulm_ccm" = "Pulmonary/Critical Care",
    "rheum" = "Rheumatology",
    "hvc" = "High Value Care"
  )
  
  # Extract ITE data for all three years
  years <- c("pgy1", "pgy2", "pgy3")
  specialties <- names(specialty_map)
  
  # Build data structure
  ite_data <- purrr::map_dfr(years, function(year) {
    purrr::map_dfr(specialties, function(spec) {
      field_name <- paste0(year, "_", spec, "_ile")
      
      if (field_name %in% names(resident_data)) {
        value <- resident_data[[field_name]][1]
        if (!is.na(value) && value != "") {
          data.table::data.table(
            year = toupper(year),
            specialty = specialty_map[[spec]],
            percentile = as.numeric(value)
          )
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
  })
  
  if (nrow(ite_data) == 0) {
    return(shiny::p("No ITE scores available yet."))
  }
  
  # Reshape to wide format for table display
  ite_wide <- data.table::dcast(
    ite_data,
    specialty ~ year,
    value.var = "percentile"
  )
  
  # Ensure Overall Total is first row
  if ("Overall Total" %in% ite_wide$specialty) {
    total_row <- ite_wide[ite_wide$specialty == "Overall Total", ]
    other_rows <- ite_wide[ite_wide$specialty != "Overall Total", ]
    ite_wide <- rbind(total_row, other_rows)
  }
  
  # Create HTML table
  shiny::tagList(
    shiny::tags$table(
      class = "table table-striped table-hover",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Specialty"),
          shiny::tags$th("PGY1", style = "text-align: center;"),
          shiny::tags$th("PGY2", style = "text-align: center;"),
          shiny::tags$th("PGY3", style = "text-align: center;")
        )
      ),
      shiny::tags$tbody(
        purrr::pmap(ite_wide, function(specialty, PGY1, PGY2, PGY3) {
          # Style first row (Overall Total) differently
          row_class <- if (specialty == "Overall Total") "table-primary" else ""
          row_style <- if (specialty == "Overall Total") "font-weight: bold;" else ""
          
          shiny::tags$tr(
            class = row_class,
            shiny::tags$td(specialty, style = row_style),
            shiny::tags$td(
              if (!is.na(PGY1)) paste0(PGY1, "%") else "—",
              style = paste0("text-align: center;", row_style)
            ),
            shiny::tags$td(
              if (!is.na(PGY2)) paste0(PGY2, "%") else "—",
              style = paste0("text-align: center;", row_style)
            ),
            shiny::tags$td(
              if (!is.na(PGY3)) paste0(PGY3, "%") else "—",
              style = paste0("text-align: center;", row_style)
            )
          )
        })
      )
    )
  )
}

#' Assess ITE Performance Risk Level
#'
#' Determines risk level based on ITE trajectory and current performance
#'
#' @param resident_data Data frame containing resident ITE scores
#' @return List with risk_level, risk_class (Bootstrap alert class), and details
#' @export
assess_ite_risk <- function(resident_data) {
  
  # Extract total percentiles
  percentiles <- c(
    as.numeric(resident_data$pgy1_total_ile[1]),
    as.numeric(resident_data$pgy2_total_ile[1]),
    as.numeric(resident_data$pgy3_total_ile[1])
  )
  
  # Remove NAs
  percentiles <- percentiles[!is.na(percentiles)]
  
  if (length(percentiles) == 0) {
    return(list(
      risk_level = "No Data Available",
      risk_class = "alert-secondary",
      details = "No ITE scores available for risk assessment.",
      trajectory_type = "None",
      current_percentile = NA,
      change = NA
    ))
  }
  
  # Calculate risk metrics
  last_percentile <- percentiles[length(percentiles)]
  first_percentile <- percentiles[1]
  currently_below_50 <- last_percentile < 50
  currently_below_20 <- last_percentile < 20
  ever_below_50 <- any(percentiles < 50)
  ever_below_20 <- any(percentiles < 20)
  
  # Determine trajectory if multiple assessments
  trajectory_type <- "Single Assessment"
  change <- 0
  
  if (length(percentiles) > 1) {
    change <- last_percentile - first_percentile
    
    if (change > 10) {
      trajectory_type <- if (change > 20) "Strong Improvement" else "Modest Improvement"
    } else if (change < -10) {
      trajectory_type <- if (change < -20) "Declining" else "Modest Decline"
    } else {
      trajectory_type <- "Stable"
    }
  }
  
  # Risk level determination (from quick_ite_analysis.R logic)
  risk_level <- if (currently_below_20) {
    "Critical - Immediate Intervention"
  } else if (currently_below_50 && trajectory_type %in% c("Declining", "Modest Decline")) {
    "High Risk"
  } else if (currently_below_50 && trajectory_type == "Single Assessment") {
    "High Risk - Need More Data"
  } else if (currently_below_50 && trajectory_type == "Stable") {
    "Moderate Risk - Monitor"
  } else if (currently_below_50 && trajectory_type %in% c("Modest Improvement", "Strong Improvement")) {
    "Improving - Continue Support"
  } else if (!currently_below_50 && ever_below_50 && trajectory_type %in% c("Strong Improvement", "Modest Improvement")) {
    "Recovered - Monitor"
  } else {
    "Low Risk"
  }
  
  # Color coding for Bootstrap alerts
  alert_class <- if (grepl("Critical|High Risk", risk_level)) {
    "alert-danger"
  } else if (grepl("Moderate", risk_level)) {
    "alert-warning"
  } else if (grepl("Improving|Recovered", risk_level)) {
    "alert-info"
  } else {
    "alert-success"
  }
  
  # Details text
  details <- paste0(
    "Current Percentile: ", last_percentile, "% | ",
    "Trajectory: ", trajectory_type,
    if (length(percentiles) > 1) {
      paste0(" (", ifelse(change > 0, "+", ""), round(change), " point change)")
    } else {
      ""
    }
  )
  
  list(
    risk_level = risk_level,
    risk_class = alert_class,
    details = details,
    trajectory_type = trajectory_type,
    current_percentile = last_percentile,
    change = change,
    ever_below_50 = ever_below_50,
    ever_below_20 = ever_below_20
  )
}