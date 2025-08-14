#' @title UI Component Functions for GMED
#' @description Consistent UI components with SSM SLUCare styling for GMED applications
#' @name ui_components
NULL

#' Create GMED Selector Container
#' 
#' Creates a styled container for dropdown selectors with GMED theming.
#'
#' @param label Character string for the label text
#' @param style Additional CSS styles to apply
#' @param class Additional CSS classes to apply
#' @param ... Additional UI elements to include in the container
#'
#' @return HTML div element with GMED selector styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_selector_container(
#'   label = "Select Your Name",
#'   selectizeInput("name", NULL, choices = names)
#' )
#' }
gmed_selector_container <- function(label = NULL, style = NULL, class = "", ...) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  shiny::div(
    class = paste("gmed-selector-container", class),
    style = style,
    
    # Label if provided
    if (!is.null(label)) {
      shiny::tags$label(
        label,
        class = "form-label text-center mb-3",
        style = "font-size: 1.25rem; font-weight: 600; color: var(--ssm-primary-blue); display: block;"
      )
    },
    
    # Container content
    ...
  )
}

#' Create GMED Resident Panel
#' 
#' Creates a styled panel for displaying resident information with GMED theming.
#'
#' @param resident_name Character string for resident name
#' @param level Character string for resident level (e.g., "PGY-1", "PGY-2")
#' @param period Character string for current period
#' @param coach Character string for coach name
#' @param access_code Character string for resident access code
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#'
#' @return HTML div element with resident information
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_resident_panel(
#'   resident_name = "John Doe",
#'   level = "PGY-2",
#'   period = "2024-2025 Q1",
#'   coach = "Dr. Smith"
#' )
#' }
gmed_resident_panel <- function(resident_name = NULL, 
                                level = NULL, 
                                period = NULL, 
                                coach = NULL, 
                                access_code = NULL,
                                class = "", 
                                style = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  shiny::div(
    class = paste("gmed-resident-panel", class),
    style = style,
    
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h4("Resident Information", class = "mb-3"),
        
        shiny::fluidRow(
          if (!is.null(resident_name)) {
            shiny::column(
              width = 3,
              shiny::tags$strong("Resident: "),
              shiny::span(resident_name)
            )
          },
          
          if (!is.null(level)) {
            shiny::column(
              width = 2,
              shiny::tags$strong("Level: "),
              shiny::span(level)
            )
          },
          
          if (!is.null(period)) {
            shiny::column(
              width = 3,
              shiny::tags$strong("Period: "),
              shiny::span(period)
            )
          },
          
          if (!is.null(coach)) {
            shiny::column(
              width = 3,
              shiny::tags$strong("Coach: "),
              shiny::span(coach)
            )
          },
          
          if (!is.null(access_code)) {
            shiny::column(
              width = 1,
              shiny::tags$strong("Code: "),
              shiny::span(access_code)
            )
          }
        )
      )
    )
  )
}

#' Create GMED Progress Bar
#' 
#' Creates a styled progress bar with GMED theming and optional animation.
#'
#' @param value Numeric value between 0 and 100 for progress percentage
#' @param animated Logical, whether to show animation (default: FALSE)
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#' @param id HTML id attribute for the progress bar
#'
#' @return HTML div element with progress bar styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_progress_bar(
#'   value = 60,
#'   animated = TRUE,
#'   id = "review-progress"
#' )
#' }
gmed_progress_bar <- function(value = 0, 
                              animated = FALSE, 
                              class = "", 
                              style = NULL, 
                              id = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  # Ensure value is between 0 and 100
  value <- max(0, min(100, value))
  
  # Build progress bar classes
  progress_classes <- "gmed-progress-bar"
  if (animated) {
    progress_classes <- paste(progress_classes, "animated")
  }
  
  shiny::div(
    class = paste("gmed-progress", class),
    style = style,
    
    shiny::div(
      class = progress_classes,
      id = id,
      style = paste0("width: ", value, "%;"),
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100
    )
  )
}

#' Create GMED Status Badge
#' 
#' Creates a styled status badge with GMED theming for different status types.
#'
#' @param text Character string for badge text
#' @param status Character string for status type: "complete", "incomplete", "in-progress", "pending"
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#'
#' @return HTML span element with status badge styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_status_badge("Complete", "complete")
#' gmed_status_badge("In Progress", "in-progress")
#' }
gmed_status_badge <- function(text, status = "pending", class = "", style = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  # Map status to CSS class
  status_class <- switch(status,
                         "complete" = "gmed-status-complete",
                         "incomplete" = "gmed-status-incomplete", 
                         "in-progress" = "gmed-status-in-progress",
                         "pending" = "gmed-status-pending",
                         "gmed-status-pending"  # default
  )
  
  shiny::span(
    text,
    class = paste("gmed-status-badge", status_class, class),
    style = style
  )
}

#' Create GMED Card
#' 
#' Creates a styled card container with GMED theming.
#'
#' @param title Character string for card title (optional)
#' @param ... Card content elements
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#' @param header_class Additional CSS classes for header
#' @param body_class Additional CSS classes for body
#'
#' @return HTML div element with card styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_card(
#'   title = "Patient Information",
#'   p("Card content goes here"),
#'   div("More content")
#' )
#' }
gmed_card <- function(title = NULL, ..., 
                      class = "", 
                      style = NULL, 
                      header_class = "", 
                      body_class = "") {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  shiny::div(
    class = paste("gmed-card card", class),
    style = style,
    
    # Card header if title provided
    if (!is.null(title)) {
      shiny::div(
        class = paste("gmed-card-header", header_class),
        shiny::h3(title, class = "gmed-card-title")
      )
    },
    
    # Card body
    shiny::div(
      class = paste("card-body", body_class),
      ...
    )
  )
}

#' Create GMED Plus/Delta Display
#' 
#' Creates a styled container for displaying Plus/Delta feedback with GMED theming.
#'
#' @param plus_text Character string for plus (strengths) feedback
#' @param delta_text Character string for delta (improvements) feedback
#' @param plus_title Character string for plus section title (default: "Strengths (Plus)")
#' @param delta_title Character string for delta section title (default: "Areas for Improvement (Delta)")
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#'
#' @return HTML div element with plus/delta styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_plus_delta_display(
#'   plus_text = "Great communication skills",
#'   delta_text = "Could improve time management"
#' )
#' }
gmed_plus_delta_display <- function(plus_text = "", 
                                    delta_text = "", 
                                    plus_title = "Strengths (Plus)", 
                                    delta_title = "Areas for Improvement (Delta)",
                                    class = "", 
                                    style = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  shiny::div(
    class = paste("gmed-plus-delta-container", class),
    style = style,
    
    # Plus section
    shiny::div(
      class = "gmed-plus-section",
      shiny::h5(plus_title),
      shiny::div(
        class = "feedback-text",
        if (nchar(plus_text) > 0) plus_text else "No strengths feedback provided"
      )
    ),
    
    # Delta section
    shiny::div(
      class = "gmed-delta-section", 
      shiny::h5(delta_title),
      shiny::div(
        class = "feedback-text",
        if (nchar(delta_text) > 0) delta_text else "No improvement feedback provided"
      )
    )
  )
}

#' Create GMED Step Indicator
#' 
#' Creates a styled step indicator for multi-step processes.
#'
#' @param current_step Integer for current step number
#' @param total_steps Integer for total number of steps
#' @param step_name Character string for current step name (optional)
#' @param class Additional CSS classes to apply
#' @param style Additional CSS styles to apply
#'
#' @return HTML div element with step indicator styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_step_indicator(
#'   current_step = 3,
#'   total_steps = 8,
#'   step_name = "Evaluations"
#' )
#' }
gmed_step_indicator <- function(current_step, 
                                total_steps, 
                                step_name = NULL, 
                                class = "", 
                                style = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  step_text <- if (!is.null(step_name)) {
    paste0("Step ", current_step, " of ", total_steps, ": ", step_name)
  } else {
    paste0("Step ", current_step, " of ", total_steps)
  }
  
  shiny::div(
    class = paste("gmed-step-indicator", class),
    style = style,
    step_text
  )
}