# =============================================================================
# CAREER GOALS MODULE
# R/mod_career_goals.R - for gmed package
# =============================================================================

#' Career Goals Module UI
#'
#' Display and edit career goals data with period comparison
#' @importFrom shiny icon
#' @param id Module namespace
#' @param mode Character: "edit", "view", or "compare"
#'
#' @export
mod_career_goals_ui <- function(id, mode = "edit") {
  ns <- NS(id)
  uiOutput(ns("career_goals_container"))
}

#' Career Goals Module Server
#'
#' @param id Module namespace
#' @param resident_info Reactive containing resident data (record_id, grad_yr, type)
#' @param app_data Reactive containing full app data structure
#' @param current_period Reactive or static: period number to display/edit (NULL = auto-calculate)
#' @param compare_periods Reactive or static: vector of period numbers to compare (for coach view)
#' @param mode Character: "edit", "view", or "compare"
#' @param redcap_token REDCap API token for saving
#'
#' @return Reactive list with save status and data
#'
#' @export
mod_career_goals_server <- function(id, 
                                    resident_info,
                                    app_data,
                                    current_period = NULL,
                                    compare_periods = NULL,
                                    mode = "edit",
                                    redcap_token = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =========================================================================
    # PERIOD CALCULATIONS
    # =========================================================================
    
    # Auto-calculate current period if not provided
    period_to_display <- reactive({
      if (!is.null(current_period)) {
        if (is.reactive(current_period)) current_period() else current_period
      } else {
        # Calculate from resident info
        req(resident_info())
        period_info <- gmed::calculate_pgy_and_period(
          grad_yr = resident_info()$grad_yr,
          type = resident_info()$type
        )
        period_info$period_number
      }
    })
    
    # Calculate previous period
    previous_period <- reactive({
      current <- period_to_display()
      switch(as.character(current),
             "7" = NA, "1" = NA,
             "2" = 1, "3" = 2, "4" = 3, "5" = 4, "6" = 5,
             NA)
    })
    
    # =========================================================================
    # DATA LOADING
    # =========================================================================
    
    # Load career data for specific period
    get_career_data_for_period <- function(period_num) {
      req(app_data(), resident_info())
      
      if (is.na(period_num)) return(NULL)
      
      se_data <- app_data()$all_forms$milestone_selfevaluation_c33c %||%
                 app_data()$all_forms$s_eval
      
      if (is.null(se_data)) return(NULL)
      
      data <- se_data %>%
        dplyr::filter(
          record_id == resident_info()$record_id,
          s_e_period == period_num
        ) %>%
        dplyr::arrange(desc(redcap_repeat_instance)) %>%
        dplyr::slice(1)
      
      if (nrow(data) == 0) return(NULL)
      
      list(
        career_path = data$s_e_career_path,
        career_oth = data$s_e_career_oth,
        fellow = data$s_e_fellow,
        fellow_oth = data$s_e_fellow_oth,
        track = data$s_e_track,
        track_type = data$s_e_track_type,
        period_number = period_num,
        period_name = gmed::get_period_label(period_num)
      )
    }
    
    # Previous and current data
    previous_data <- reactive({
      prev <- previous_period()
      get_career_data_for_period(prev)
    })
    
    current_data <- reactive({
      curr <- period_to_display()
      get_career_data_for_period(curr)
    })
    
    # =========================================================================
    # USER INTERACTION STATE
    # =========================================================================
    
    # Track user choice
    goals_same <- reactiveVal(NULL)
    
    observeEvent(input$career_same_yes, { goals_same(TRUE) })
    observeEvent(input$career_same_no, { goals_same(FALSE) })
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    
    # Get field definitions from data dictionary
    get_field_definitions <- function() {
      req(app_data())
      dict <- app_data()$data_dict
      
      list(
        career_path = dict %>% dplyr::filter(field_name == "s_e_career_path") %>% dplyr::slice(1),
        career_oth = dict %>% dplyr::filter(field_name == "s_e_career_oth") %>% dplyr::slice(1),
        fellow = dict %>% dplyr::filter(field_name == "s_e_fellow") %>% dplyr::slice(1),
        fellow_oth = dict %>% dplyr::filter(field_name == "s_e_fellow_oth") %>% dplyr::slice(1),
        track = dict %>% dplyr::filter(field_name == "s_e_track") %>% dplyr::slice(1),
        track_type = dict %>% dplyr::filter(field_name == "s_e_track_type") %>% dplyr::slice(1)
      )
    }
    
    # Parse data dictionary choices
    parse_dd_choices <- function(choices_string) {
      if (is.na(choices_string) || nchar(trimws(choices_string)) == 0) return(NULL)
      
      items <- strsplit(choices_string, "\\|")[[1]]
      items <- trimws(items)
      
      setNames(
        sapply(items, function(x) trimws(strsplit(x, ",")[[1]][1])),
        sapply(items, function(x) trimws(strsplit(x, ",")[[1]][2]))
      )
    }
    
    # Get choice labels from stored values
    get_choice_labels <- function(values_string, choices) {
      if (is.null(values_string) || is.na(values_string) || 
          nchar(trimws(values_string)) == 0) return(NULL)
      
      vals <- strsplit(as.character(values_string), ",")[[1]]
      vals <- trimws(vals)
      
      labels <- names(choices)[match(vals, choices)]
      labels[!is.na(labels)]
    }
    
    # =========================================================================
    # RENDER FUNCTIONS
    # =========================================================================
    
    # Main UI render
    output$career_goals_container <- renderUI({
      if (mode == "view") {
        render_view_mode()
      } else if (mode == "compare") {
        render_compare_mode()
      } else {
        render_edit_mode()
      }
    })
    
    # Edit mode UI (for resident)
    render_edit_mode <- function() {
      prev_data <- previous_data()
      field_defs <- get_field_definitions()
      
      tagList(
        # Show previous period data
        if (!is.null(prev_data)) {
          render_previous_data_display(prev_data, field_defs)
        },
        
        # Ask if same (only if previous data exists and not yet answered)
        if (!is.null(prev_data) && is.null(goals_same())) {
          div(
            class = "alert alert-info",
            p(class = "mb-3", strong("Are your career goals the same as last time?")),
            div(
              class = "btn-group",
              actionButton(ns("career_same_yes"), "Yes, same goals", 
                          icon = icon("check"), class = "btn-success"),
              actionButton(ns("career_same_no"), "No, update my goals", 
                          icon = icon("edit"), class = "btn-primary")
            )
          )
        },
        
        # Show input fields if needed
        if (is.null(prev_data) || (!is.null(goals_same()) && goals_same() == FALSE)) {
          render_input_fields(field_defs)
        },
        
        # Status message
        if (!is.null(goals_same()) && goals_same() == TRUE) {
          div(
            class = "alert alert-success mt-3",
            icon("check-circle", class = "me-2"),
            "Your previous career goals will be carried forward."
          )
        }
      )
    }
    
    # View mode UI (for coach)
    render_view_mode <- function() {
      curr_data <- current_data()
      
      if (is.null(curr_data)) {
        return(div(
          class = "alert alert-warning",
          icon("exclamation-triangle", class = "me-2"),
          "No career goals data available for this period."
        ))
      }
      
      field_defs <- get_field_definitions()
      render_previous_data_display(curr_data, field_defs)
    }
    
    # Compare mode UI (for coach - multiple periods)
    render_compare_mode <- function() {
      periods <- if (is.reactive(compare_periods)) compare_periods() else compare_periods
      
      if (is.null(periods)) {
        periods <- gmed::get_resident_periods(resident_info()$type)
      }
      
      # Load data for all periods
      all_data <- lapply(periods, get_career_data_for_period)
      all_data <- all_data[!sapply(all_data, is.null)]
      
      if (length(all_data) == 0) {
        return(div(
          class = "alert alert-warning",
          icon("exclamation-triangle", class = "me-2"),
          "No career goals data available."
        ))
      }
      
      field_defs <- get_field_definitions()
      
      # Create timeline view
      tagList(
        h4(icon("chart-line", class = "me-2"), "Career Goals Timeline"),
        lapply(all_data, function(period_data) {
          div(
            class = "card mb-3",
            div(
              class = "card-header bg-light",
              strong(period_data$period_name),
              tags$small(class = "text-muted ms-2", 
                        paste0("(Period ", period_data$period_number, ")"))
            ),
            div(
              class = "card-body",
              render_career_data_content(period_data, field_defs)
            )
          )
        })
      )
    }
    
    # Render previous data display (with alert wrapper)
    render_previous_data_display <- function(data, field_defs) {
      content <- render_career_data_content(data, field_defs)
      
      if (is.null(content)) return(NULL)
      
      div(
        class = "alert alert-light border-start border-primary border-4 mb-4",
        div(class = "d-flex align-items-center mb-3",
            icon("history", class = "text-primary me-2 fa-lg"),
            strong(paste0("From ", data$period_name, " evaluation:"))
        ),
        content
      )
    }
    
    # Render career data content (without wrapper - reusable)
    render_career_data_content <- function(data, field_defs) {
      display_items <- list()
      
      # Career path
      if (!is.null(data$career_path) && !is.na(data$career_path) && 
          nchar(trimws(data$career_path)) > 0) {
        career_choices <- parse_dd_choices(field_defs$career_path$select_choices_or_calculations)
        career_labels <- get_choice_labels(data$career_path, career_choices)
        
        if (length(career_labels) > 0) {
          career_text <- paste(career_labels, collapse = ", ")
          
          # Add "Other" specification if present
          if ("Other" %in% career_labels && !is.null(data$career_oth) && 
              nchar(trimws(data$career_oth)) > 0) {
            career_text <- paste0(career_text, " (", data$career_oth, ")")
          }
          
          display_items <- c(display_items, list(
            div(class = "mb-2",
                tags$strong("Career Path: "),
                tags$span(career_text))
          ))
        }
      }
      
      # Fellowship
      if (!is.null(data$fellow) && !is.na(data$fellow) && 
          nchar(trimws(data$fellow)) > 0) {
        fellow_choices <- parse_dd_choices(field_defs$fellow$select_choices_or_calculations)
        fellow_labels <- get_choice_labels(data$fellow, fellow_choices)
        
        if (length(fellow_labels) > 0) {
          fellow_text <- paste(fellow_labels, collapse = ", ")
          
          if ("Other" %in% fellow_labels && !is.null(data$fellow_oth) && 
              nchar(trimws(data$fellow_oth)) > 0) {
            fellow_text <- paste0(fellow_text, " (", data$fellow_oth, ")")
          }
          
          display_items <- c(display_items, list(
            div(class = "mb-2",
                tags$strong("Fellowship Interest: "),
                tags$span(fellow_text))
          ))
        }
      }
      
      # Track
      if (!is.null(data$track) && !is.na(data$track)) {
        track_text <- if (data$track == "1" || tolower(data$track) == "yes") {
          if (!is.null(data$track_type) && !is.na(data$track_type) && 
              nchar(trimws(data$track_type)) > 0) {
            track_choices <- parse_dd_choices(field_defs$track_type$select_choices_or_calculations)
            track_labels <- get_choice_labels(data$track_type, track_choices)
            if (length(track_labels) > 0) {
              paste("Yes:", paste(track_labels, collapse = ", "))
            } else "Yes"
          } else "Yes"
        } else "No"
        
        display_items <- c(display_items, list(
          div(class = "mb-2",
              tags$strong("Program Tracks: "),
              tags$span(track_text))
        ))
      }
      
      if (length(display_items) == 0) {
        return(div(class = "text-muted", icon("info-circle", class = "me-2"), 
                  "No career goals information entered."))
      }
      
      do.call(div, display_items)
    }
    
    # Render input fields
    render_input_fields <- function(field_defs) {
      career_choices <- parse_dd_choices(field_defs$career_path$select_choices_or_calculations)
      fellow_choices <- parse_dd_choices(field_defs$fellow$select_choices_or_calculations)
      track_choices <- parse_dd_choices(field_defs$track_type$select_choices_or_calculations)
      
      tagList(
        hr(),
        p(class = "text-muted mb-3", "Please update your career goals:"),
        
        # Career path checkboxes
        div(
          class = "mb-3",
          tags$label(class = "form-label", tags$strong(field_defs$career_path$field_label)),
          checkboxGroupInput(ns("s_e_career_path_new"), NULL, choices = career_choices)
        ),
        
        # Career path "Other" text
        conditionalPanel(
          condition = "input.s_e_career_path_new && input.s_e_career_path_new.includes('8')",
          ns = ns,
          textInput(ns("s_e_career_oth_new"), 
                   field_defs$career_oth$field_label,
                   placeholder = "Please specify")
        ),
        
        # Fellowship checkboxes
        div(
          class = "mb-3",
          tags$label(class = "form-label", tags$strong(field_defs$fellow$field_label)),
          checkboxGroupInput(ns("s_e_fellow_new"), NULL, choices = fellow_choices)
        ),
        
        # Fellowship "Other" text
        conditionalPanel(
          condition = "input.s_e_fellow_new && input.s_e_fellow_new.includes('12')",
          ns = ns,
          textInput(ns("s_e_fellow_oth_new"), 
                   field_defs$fellow_oth$field_label,
                   placeholder = "Please specify")
        ),
        
        # Track yes/no
        div(
          class = "mb-3",
          tags$label(class = "form-label", tags$strong(field_defs$track$field_label)),
          radioButtons(ns("s_e_track_new"), NULL, 
                      choices = c("Yes" = "1", "No" = "0"), 
                      inline = TRUE)
        ),
        
        # Track type checkboxes
        conditionalPanel(
          condition = "input.s_e_track_new == '1'",
          ns = ns,
          div(
            class = "mb-3",
            tags$label(class = "form-label", tags$strong(field_defs$track_type$field_label)),
            checkboxGroupInput(ns("s_e_track_type_new"), NULL, choices = track_choices)
          )
        )
      )
    }
    
    # =========================================================================
    # SAVE FUNCTION
    # =========================================================================
    
    save_career_data <- function() {
      req(resident_info())
      
      data_to_save <- data.frame(
        record_id = resident_info()$record_id,
        redcap_repeat_instrument = "s_eval",
        s_e_period = period_to_display(),
        stringsAsFactors = FALSE
      )
      
      # Determine what to save
      if (!is.null(goals_same()) && goals_same() == TRUE) {
        # Copy previous values
        prev <- previous_data()
        if (!is.null(prev)) {
          data_to_save$s_e_career_path <- prev$career_path
          data_to_save$s_e_career_oth <- prev$career_oth
          data_to_save$s_e_fellow <- prev$fellow
          data_to_save$s_e_fellow_oth <- prev$fellow_oth
          data_to_save$s_e_track <- prev$track
          data_to_save$s_e_track_type <- prev$track_type
        }
      } else if (!is.null(goals_same()) && goals_same() == FALSE) {
        # Use new values
        data_to_save$s_e_career_path <- if (!is.null(input$s_e_career_path_new)) {
          paste(input$s_e_career_path_new, collapse = ",")
        } else NA
        
        data_to_save$s_e_career_oth <- input$s_e_career_oth_new %||% NA
        
        data_to_save$s_e_fellow <- if (!is.null(input$s_e_fellow_new)) {
          paste(input$s_e_fellow_new, collapse = ",")
        } else NA
        
        data_to_save$s_e_fellow_oth <- input$s_e_fellow_oth_new %||% NA
        
        data_to_save$s_e_track <- input$s_e_track_new %||% NA
        
        data_to_save$s_e_track_type <- if (!is.null(input$s_e_track_type_new) && 
                                            input$s_e_track_new == "1") {
          paste(input$s_e_track_type_new, collapse = ",")
        } else NA
      } else {
        # No changes made
        return(list(success = FALSE, message = "No changes to save"))
      }
      
      tryCatch({
        # Write to REDCap using gmed function
        # TODO: Implement actual REDCap write
        # gmed::redcap_write_records(redcap_token, data_to_save)
        
        list(success = TRUE, data = data_to_save)
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })
    }
    
    # =========================================================================
    # RETURN INTERFACE
    # =========================================================================
    
    list(
      save = save_career_data,
      has_changes = reactive(!is.null(goals_same())),
      data_to_save = reactive({
        if (!is.null(goals_same())) {
          result <- save_career_data()
          if (result$success) result$data else NULL
        } else NULL
      }),
      current_period = period_to_display,
      previous_period = previous_period
    )
  })
}