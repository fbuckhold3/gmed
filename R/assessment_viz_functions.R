# assessment_viz_functions.R
# Assessment Visualization Functions for gmed package
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)

#' Create Combined Assessment Chart
#'
#' Creates a horizontal stacked bar chart showing assessment progress by level.
#' Combines total count and progress toward annual goals in a single visualization.
#'
#' @param data Data frame containing assessment data with columns: record_id, source_form, ass_level
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will be extracted from data
#'
#' @return A plotly object showing assessment progress by level
#' @export
#'
#' @examples
#' \dontrun{
#' chart <- create_combined_assessment_chart(assessment_data, "88")
#' chart
#' }
create_combined_assessment_chart <- function(data, record_id, resident_name = NULL) {
  
  # Get resident name from data
  if (is.null(resident_name)) {
    name_from_data <- data %>%
      filter(record_id == !!record_id) %>%
      filter(!is.na(name) & name != "") %>%
      slice(1) %>%
      pull(name)
    
    resident_name <- if(length(name_from_data) > 0) {
      name_from_data
    } else {
      paste("Resident", record_id)
    }
  }
  
  # Get colors
  colors <- ssm_colors()
  
  # Count assessments by level - SIMPLIFIED VERSION
  assessment_counts <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "assessment"
    ) %>%
    mutate(
      # SIMPLIFIED: Only use ass_level, no fallback complexity
      level = case_when(
        ass_level == 1 | ass_level == "1" | ass_level == "Intern" ~ "Intern",
        ass_level == 2 | ass_level == "2" | ass_level == "PGY2" ~ "PGY2", 
        ass_level == 3 | ass_level == "3" | ass_level == "PGY3" ~ "PGY3",
        TRUE ~ "Unknown"
      )
    ) %>%
    count(level, name = "actual") %>%
    complete(level = c("Intern", "PGY2", "PGY3"), fill = list(actual = 0))
  
  # Add targets and calculate progress
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
  
  # Total assessments for display
  total_assessments <- sum(progress_data$actual)
  
  # Create the combined chart
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
      yaxis = list(title = "Level"),
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
#' Combines total count and progress toward annual goals in a single visualization.
#'
#' @param data Data frame containing faculty evaluation data with columns: record_id, source_form, fac_eval_level
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will be extracted from data
#'
#' @return A plotly object showing faculty evaluation progress by level
#' @export
#'
#' @examples
#' \dontrun{
#' chart <- create_combined_faculty_chart(faculty_data, "88")
#' chart
#' }
create_combined_faculty_chart <- function(data, record_id, resident_name = NULL) {
  
  # Get resident name from data
  if (is.null(resident_name)) {
    name_from_data <- data %>%
      filter(record_id == !!record_id) %>%
      filter(!is.na(name) & name != "") %>%
      slice(1) %>%
      pull(name)
    
    resident_name <- if(length(name_from_data) > 0) {
      name_from_data
    } else {
      paste("Resident", record_id)
    }
  }
  
  # Get colors
  colors <- ssm_colors()
  
  # Count faculty evaluations by level - SIMPLIFIED VERSION  
  faculty_counts <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "faculty_evaluation"
    ) %>%
    mutate(
      # SIMPLIFIED: Only use fac_eval_level, no fallback complexity
      level = case_when(
        fac_eval_level == 1 | fac_eval_level == "1" | fac_eval_level == "Intern" ~ "Intern",
        fac_eval_level == 2 | fac_eval_level == "2" | fac_eval_level == "PGY2" ~ "PGY2", 
        fac_eval_level == 3 | fac_eval_level == "3" | fac_eval_level == "PGY3" ~ "PGY3",
        TRUE ~ "Unknown"
      )
    ) %>%
    count(level, name = "actual") %>%
    complete(level = c("Intern", "PGY2", "PGY3"), fill = list(actual = 0))
  
  # Add targets and calculate progress
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
  
  # Total faculty evaluations for display
  total_faculty <- sum(progress_data$actual)
  
  # Create the combined chart
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
        "Completed: %{x} / 8<br>",
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
      title = paste("Faculty Evaluations Completed:", total_faculty, "total -", resident_name),
      xaxis = list(title = "Progress to Goal (8 per year)", range = c(0, 10)),
      yaxis = list(title = "Level"),
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
#' Calculates the weekly average number of questions completed by a resident.
#' Target is 4 questions per week. Returns summary statistics for display.
#'
#' @param data Data frame containing questions data with columns: record_id, source_form, q_date
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will be extracted from data
#'
#' @return A list containing average, total_weeks, display_text, and detail_text
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- create_weekly_questions_average(questions_data, "88")
#' summary$average  # 3.2
#' summary$display_text  # "3.2 out of 4"
#' }
create_weekly_questions_average <- function(data, record_id, resident_name = NULL) {
  
  # Get resident name from data
  if (is.null(resident_name)) {
    name_from_data <- data %>%
      filter(record_id == !!record_id) %>%
      filter(!is.na(name) & name != "") %>%
      slice(1) %>%
      pull(name)
    
    resident_name <- if(length(name_from_data) > 0) {
      name_from_data
    } else {
      paste("Resident", record_id)
    }
  }
  
  # Get questions data
  questions_data <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "questions"
    ) %>%
    filter(!is.na(q_date) & q_date != "") %>%
    mutate(q_date = as.Date(q_date))
  
  if (nrow(questions_data) == 0) {
    return(list(
      average = 0,
      total_weeks = 0,
      display_text = "0 out of 4",
      detail_text = "No questions data"
    ))
  }
  
  # Calculate weekly averages
  weekly_summary <- questions_data %>%
    mutate(
      week_start = floor_date(q_date, "week")
    ) %>%
    group_by(week_start) %>%
    summarise(
      questions_count = n(),
      .groups = "drop"
    )
  
  # Calculate overall average
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
#' Calculates assessment and faculty evaluation activity in the last 4 weeks,
#' plus conference attendance average for sidebar display.
#'
#' @param data Data frame containing assessment and evaluation data
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name
#'
#' @return A list containing recent_assessments, recent_faculty, and conference_avg
#' @export
create_recent_activity_summary <- function(data, record_id, resident_name = NULL) {
  
  # Calculate 4 weeks ago
  four_weeks_ago <- Sys.Date() - 28
  
  # Count recent assessments (using q_date as proxy since ass_date is NA)
  recent_assessments <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "assessment"
    ) %>%
    filter(!is.na(q_date) & q_date != "") %>%
    mutate(q_date = as.Date(q_date)) %>%
    filter(q_date >= four_weeks_ago) %>%
    nrow()
  
  # Count recent faculty evaluations
  recent_faculty <- data %>%
    filter(
      record_id == !!record_id,
      source_form == "faculty_evaluation"
    ) %>%
    filter(!is.na(fac_eval_date) & fac_eval_date != "") %>%
    mutate(fac_eval_date = as.Date(fac_eval_date)) %>%
    filter(fac_eval_date >= four_weeks_ago) %>%
    nrow()
  
  # Calculate conference attendance (from questions data)
  questions_summary <- create_weekly_questions_average(data, record_id, resident_name)
  conference_avg <- questions_summary$average
  
  return(list(
    recent_assessments = recent_assessments,
    recent_faculty = recent_faculty,
    conference_avg = conference_avg,
    summary_text = paste0(
      "In the last four weeks you have:\n",
      recent_assessments, " Resident Assessment", if(recent_assessments != 1) "s" else "", "\n",
      recent_faculty, " Faculty Evaluation", if(recent_faculty != 1) "s" else "", "\n",
      "Attended conference ", conference_avg, " out of 4 days on average"
    )
  ))
}

#' Create Questions Summary Display
#'
#' Creates a large number display showing weekly questions average.
#' Displays the result as a prominent number "out of 4" with supporting details.
#'
#' @param data Data frame containing questions data with columns: record_id, source_form, q_date
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will be extracted from data
#'
#' @return A plotly object displaying the questions average as a large number
#' @export
#'
#' @examples
#' \dontrun{
#' display <- create_questions_summary_display(questions_data, "88")
#' display
#' }
create_questions_summary_display <- function(data, record_id, resident_name = NULL) {
  
  # Get the summary data
  summary_data <- create_weekly_questions_average(data, record_id, resident_name)
  
  # Get resident name
  if (is.null(resident_name)) {
    name_from_data <- data %>%
      filter(record_id == !!record_id) %>%
      filter(!is.na(name) & name != "") %>%
      slice(1) %>%
      pull(name)
    
    resident_name <- if(length(name_from_data) > 0) {
      name_from_data
    } else {
      paste("Resident", record_id)
    }
  }
  
  # Handle no data case - show 0 
  if (summary_data$total_weeks == 0) {
    fig <- plot_ly(type = "scatter", mode = "markers") %>%
      add_annotations(
        x = 0.5, y = 0.7,
        text = "<b>0</b>",
        showarrow = FALSE,
        font = list(size = 80, color = ssm_colors()$accent_blue),
        xref = "paper", yref = "paper"
      ) %>%
      add_annotations(
        x = 0.5, y = 0.45,
        text = "out of 4",
        showarrow = FALSE,
        font = list(size = 24, color = "gray"),
        xref = "paper", yref = "paper"
      ) %>%
      add_annotations(
        x = 0.5, y = 0.25,
        text = "No questions data",
        showarrow = FALSE,
        font = list(size = 14, color = "gray"),
        xref = "paper", yref = "paper"
      ) %>%
      layout(
        title = paste("Weekly Questions Average -", resident_name),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        showlegend = FALSE,
        margin = list(t = 80, b = 40, l = 40, r = 40)
      )
    return(fig)
  }
  
  # Create display with data
  big_number <- as.character(summary_data$average)
  detail_text <- as.character(summary_data$detail_text)
  
  fig <- plot_ly(type = "scatter", mode = "markers") %>%
    add_annotations(
      x = 0.5, y = 0.7,
      text = paste0("<b>", big_number, "</b>"),
      showarrow = FALSE,
      font = list(size = 80, color = ssm_colors()$accent_blue),
      xref = "paper", yref = "paper"
    ) %>%
    add_annotations(
      x = 0.5, y = 0.45,
      text = "out of 4",
      showarrow = FALSE,
      font = list(size = 24, color = "gray"),
      xref = "paper", yref = "paper"
    ) %>%
    add_annotations(
      x = 0.5, y = 0.25,
      text = detail_text,
      showarrow = FALSE,
      font = list(size = 14, color = "gray"),
      xref = "paper", yref = "paper"
    ) %>%
    layout(
      title = paste("Weekly Questions Average -", resident_name),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      showlegend = FALSE,
      margin = list(t = 80, b = 40, l = 40, r = 40),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
  
  return(fig)
}