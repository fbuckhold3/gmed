# assessment_viz_functions.R
# Assessment Visualization Functions for gmed package
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)

#' Create Combined Assessment Chart
#'
#' Creates a horizontal stacked bar chart showing assessment progress by level.
#' Handles both populated and empty ass_level fields.
#'
#' @param data Data frame containing assessment data with columns: record_id, source_form, ass_level
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will use "Resident [ID]"
#'
#' @return A plotly object showing assessment progress by level
#' @export
create_combined_assessment_chart <- function(data, record_id, resident_name = NULL) {
  
  # Use provided name or default to record_id
  if (is.null(resident_name)) {
    resident_name <- paste("Resident", record_id)
  }
  
  # Get colors
  colors <- ssm_colors()
  
  # Filter assessment data
  assessment_data <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "assessment",
      # Has either plus or delta content
      (!is.na(ass_plus) & nchar(trimws(ass_plus)) > 0) | 
      (!is.na(ass_delta) & nchar(trimws(ass_delta)) > 0)
    )
  
  # Check if we have level data
  has_level_data <- assessment_data %>%
    filter(!is.na(ass_level) & ass_level != "") %>%
    nrow() > 0
  
  if (!has_level_data) {
    # NO LEVEL DATA - Show single bar with total count
    total_assessments <- nrow(assessment_data)
    
    progress_data <- data.frame(
      level = "All Levels",
      actual = total_assessments,
      target = 25,
      progress_pct = round((total_assessments / 25) * 100, 1),
      status = case_when(
        total_assessments >= 25 ~ "Complete",
        total_assessments >= 19 ~ "On Track",
        total_assessments >= 13 ~ "Behind",
        TRUE ~ "Needs Attention"
      )
    )
    
  } else {
    # HAS LEVEL DATA - Break down by level
    assessment_counts <- assessment_data %>%
      mutate(
        level = case_when(
          ass_level == 1 | ass_level == "1" | ass_level == "Intern" ~ "Intern",
          ass_level == 2 | ass_level == "2" | ass_level == "PGY2" ~ "PGY2", 
          ass_level == 3 | ass_level == "3" | ass_level == "PGY3" ~ "PGY3",
          TRUE ~ "Unknown"
        )
      ) %>%
      count(level, name = "actual") %>%
      complete(level = c("Intern", "PGY2", "PGY3"), fill = list(actual = 0))
    
    progress_data <- assessment_counts %>%
      mutate(
        target = 25,
        progress_pct = round((actual / target) * 100, 1),
        status = case_when(
          actual >= target ~ "Complete",
          actual >= target * 0.75 ~ "On Track", 
          actual >= target * 0.5 ~ "Behind",
          TRUE ~ "Needs Attention"
        )
      ) %>%
      arrange(match(level, c("Intern", "PGY2", "PGY3")))
  }
  
  # Total assessments for display
  total_assessments <- sum(progress_data$actual)
  
  # Create the chart
  fig <- plot_ly()
  
  # Add progress bars (completed portion)
  fig <- fig %>%
    add_trace(
      data = progress_data,
      x = ~actual,
      y = ~level,
      type = "bar",
      orientation = "h",
      marker = list(
        color = case_when(
          progress_data$status == "Complete" ~ colors$success,
          progress_data$status == "On Track" ~ colors$info, 
          progress_data$status == "Behind" ~ colors$warning,
          TRUE ~ colors$danger
        )
      ),
      text = ~paste0(actual, "/", target),
      textposition = "inside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Completed: %{x} / 25<br>",
        "Progress: ", progress_data$progress_pct, "%<br>",
        "Status: ", progress_data$status, "<br>",
        "<extra></extra>"
      ),
      name = "Completed"
    )
  
  # Add remaining portion (gray)
  fig <- fig %>%
    add_trace(
      data = progress_data,
      x = ~(target - actual),
      y = ~level,
      type = "bar",
      orientation = "h",
      marker = list(color = "rgba(200, 200, 200, 0.3)"),
      hoverinfo = "skip",
      showlegend = FALSE,
      name = "Remaining"
    )
  
  # Layout
  fig <- fig %>%
    layout(
      title = paste("Feedback Assessments Completed:", total_assessments, "total -", resident_name),
      xaxis = list(title = "Progress to Goal (25 per year)", range = c(0, 30)),
      yaxis = list(title = if(has_level_data) "Level" else "", 
                   showticklabels = has_level_data),
      barmode = "stack",
      showlegend = FALSE,
      margin = list(l = 80, r = 40, t = 60, b = 40),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

#' Create Combined Faculty Evaluation Chart
#'
#' Creates a horizontal stacked bar chart showing faculty evaluation progress by level.
#' Handles both populated and empty fac_eval_level fields.
#'
#' @param data Data frame containing faculty evaluation data
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will use "Resident [ID]"
#'
#' @return A plotly object showing faculty evaluation progress by level
#' @export
create_combined_faculty_chart <- function(data, record_id, resident_name = NULL) {
  
  # Use provided name or default to record_id
  if (is.null(resident_name)) {
    resident_name <- paste("Resident", record_id)
  }
  
  # Check if faculty_evaluation data exists
  has_faculty <- "faculty_evaluation" %in% unique(data$source_form)
  
  if (!has_faculty) {
    # Return placeholder chart
    return(plot_ly() %>%
      layout(
        title = "Faculty Evaluations - Coming Soon",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            text = "Faculty evaluations not yet available",
            showarrow = FALSE,
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            font = list(size = 16, color = "gray")
          )
        )
      ))
  }
  
  # Get colors
  colors <- ssm_colors()
  
  # Filter faculty data
  faculty_data <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "faculty_evaluation"
    )
  
  # Check if we have level data
  has_level_data <- faculty_data %>%
    filter(!is.na(fac_eval_level) & fac_eval_level != "") %>%
    nrow() > 0
  
  if (!has_level_data) {
    # NO LEVEL DATA - Show single bar
    total_faculty <- nrow(faculty_data)
    
    progress_data <- data.frame(
      level = "All Levels",
      actual = total_faculty,
      target = 8,
      progress_pct = round((total_faculty / 8) * 100, 1),
      status = case_when(
        total_faculty >= 8 ~ "Complete",
        total_faculty >= 6 ~ "On Track",
        total_faculty >= 4 ~ "Behind",
        TRUE ~ "Needs Attention"
      )
    )
    
  } else {
    # HAS LEVEL DATA
    faculty_counts <- faculty_data %>%
      mutate(
        level = case_when(
          fac_eval_level == 1 | fac_eval_level == "1" | fac_eval_level == "Intern" ~ "Intern",
          fac_eval_level == 2 | fac_eval_level == "2" | fac_eval_level == "PGY2" ~ "PGY2", 
          fac_eval_level == 3 | fac_eval_level == "3" | fac_eval_level == "PGY3" ~ "PGY3",
          TRUE ~ "Unknown"
        )
      ) %>%
      count(level, name = "actual") %>%
      complete(level = c("Intern", "PGY2", "PGY3"), fill = list(actual = 0))
    
    progress_data <- faculty_counts %>%
      mutate(
        target = 8,
        progress_pct = round((actual / target) * 100, 1),
        status = case_when(
          actual >= target ~ "Complete",
          actual >= target * 0.75 ~ "On Track", 
          actual >= target * 0.5 ~ "Behind",
          TRUE ~ "Needs Attention"
        )
      ) %>%
      arrange(match(level, c("Intern", "PGY2", "PGY3")))
  }
  
  total_faculty <- sum(progress_data$actual)
  
  # Create the chart
  fig <- plot_ly()
  
  fig <- fig %>%
    add_trace(
      data = progress_data,
      x = ~actual,
      y = ~level,
      type = "bar",
      orientation = "h",
      marker = list(
        color = case_when(
          progress_data$status == "Complete" ~ colors$success,
          progress_data$status == "On Track" ~ colors$info, 
          progress_data$status == "Behind" ~ colors$warning,
          TRUE ~ colors$danger
        )
      ),
      text = ~paste0(actual, "/", target),
      textposition = "inside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Completed: %{x} / 8<br>",
        "Progress: ", progress_data$progress_pct, "%<br>",
        "Status: ", progress_data$status, "<br>",
        "<extra></extra>"
      ),
      name = "Completed"
    )
  
  fig <- fig %>%
    add_trace(
      data = progress_data,
      x = ~(target - actual),
      y = ~level,
      type = "bar",
      orientation = "h",
      marker = list(color = "rgba(200, 200, 200, 0.3)"),
      hoverinfo = "skip",
      showlegend = FALSE,
      name = "Remaining"
    )
  
  fig <- fig %>%
    layout(
      title = paste("Faculty Evaluations Completed:", total_faculty, "total -", resident_name),
      xaxis = list(title = "Progress to Goal (8 per year)", range = c(0, 10)),
      yaxis = list(title = if(has_level_data) "Level" else "",
                   showticklabels = has_level_data),
      barmode = "stack",
      showlegend = FALSE,
      margin = list(l = 80, r = 40, t = 60, b = 40),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  
  return(fig)
}

#' Calculate Weekly Questions Average
#'
#' @param data Data frame containing questions data
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name
#'
#' @return A list containing average, total_weeks, display_text, and detail_text
#' @export
create_weekly_questions_average <- function(data, record_id, resident_name = NULL) {
  
  if (is.null(resident_name)) {
    resident_name <- paste("Resident", record_id)
  }
  
  questions_data <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "questions",
      !is.na(q_date) & q_date != ""
    )
  
  if (nrow(questions_data) == 0) {
    return(list(
      average = 0,
      total_weeks = 0,
      display_text = "0 out of 4",
      detail_text = "No questions data"
    ))
  }
  
  questions_data <- questions_data %>%
    mutate(q_date = as.Date(q_date))
  
  weekly_summary <- questions_data %>%
    mutate(week_start = floor_date(q_date, "week")) %>%
    group_by(week_start) %>%
    summarise(questions_count = n(), .groups = "drop")
  
  overall_average <- round(mean(weekly_summary$questions_count), 1)
  total_weeks <- nrow(weekly_summary)
  
  return(list(
    average = overall_average,
    total_weeks = total_weeks,
    display_text = paste0(overall_average, " out of 4"),
    detail_text = paste0("Based on ", total_weeks, " weeks of data")
  ))
}

#' Calculate Recent Activity Summary (Last 4 Weeks)
#'
#' @param data Data frame containing assessment and evaluation data
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name
#'
#' @return A list containing recent_assessments, recent_faculty, and conference_avg
#' @export
create_recent_activity_summary <- function(data, record_id, resident_name = NULL) {
  
  four_weeks_ago <- Sys.Date() - 28
  
  # Try to use ass_date if available, otherwise just count all
  assessment_data <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "assessment",
      (!is.na(ass_plus) & nchar(trimws(ass_plus)) > 0) | 
      (!is.na(ass_delta) & nchar(trimws(ass_delta)) > 0)
    )
  
  # Check if we have dates
  has_dates <- assessment_data %>%
    filter(!is.na(ass_date) & ass_date != "") %>%
    nrow() > 0
  
  if (has_dates) {
    recent_assessments <- assessment_data %>%
      filter(!is.na(ass_date) & ass_date != "") %>%
      mutate(ass_date = as.Date(ass_date)) %>%
      filter(ass_date >= four_weeks_ago) %>%
      nrow()
  } else {
    # No dates, just use total
    recent_assessments <- nrow(assessment_data)
  }
  
  # Faculty evaluations
  recent_faculty <- 0
  if ("faculty_evaluation" %in% unique(data$source_form)) {
    faculty_data <- data %>%
      filter(
        record_id == !!record_id,
        source_form == "faculty_evaluation"
      )
    
    if (nrow(faculty_data) > 0) {
      has_fac_dates <- faculty_data %>%
        filter(!is.na(fac_eval_date) & fac_eval_date != "") %>%
        nrow() > 0
      
      if (has_fac_dates) {
        recent_faculty <- faculty_data %>%
          filter(!is.na(fac_eval_date) & fac_eval_date != "") %>%
          mutate(fac_eval_date = as.Date(fac_eval_date)) %>%
          filter(fac_eval_date >= four_weeks_ago) %>%
          nrow()
      } else {
        recent_faculty <- nrow(faculty_data)
      }
    }
  }
  
  questions_summary <- create_weekly_questions_average(data, record_id, resident_name)
  conference_avg <- questions_summary$average
  
  return(list(
    recent_assessments = recent_assessments,
    recent_faculty = recent_faculty,
    conference_avg = conference_avg,
    summary_text = paste0(
      if(has_dates) "In the last four weeks:\n" else "Total:\n",
      recent_assessments, " Assessment", if(recent_assessments != 1) "s" else "", "\n",
      recent_faculty, " Faculty Evaluation", if(recent_faculty != 1) "s" else "", "\n",
      "Avg questions: ", conference_avg, " out of 4"
    )
  ))
}