#' Assessment Viewer Module - UI
#'
#' Display assessment data filtered by evaluation type from RDM 2.0
#'
#' @param id Module namespace ID
#' @export
mod_assessment_viewer_ui <- function(id) {
  ns <- NS(id)
  
  bslib::page_fluid(
    # Filters Row
    bslib::card(
      bslib::card_header("Assessment Filters"),
      bslib::layout_column_wrap(
        width = 1/3,
        selectInput(
          ns("resident"),
          "Select Resident:",
          choices = NULL,
          multiple = FALSE
        ),
        selectInput(
          ns("eval_type"),
          "Evaluation Type:",
          choices = c(
            "All Assessments" = "all",
            "Continuity Clinic" = "cc",
            "Single Clinic Days" = "day",
            "Consults" = "cons",
            "Intern Inpatient" = "int_ip",
            "Resident Inpatient" = "res_ip",
            "Bridge Clinic" = "bridge",
            "Observations" = "obs"
          ),
          selected = "all"
        ),
        # Conditional observation subtype selector
        conditionalPanel(
          condition = "input.eval_type == 'obs'",
          ns = ns,
          selectInput(
            ns("obs_subtype"),
            "Observation Type:",
            choices = c(
              "All Observations" = "all",
              "Clinical Decision Making" = "cdm",
              "Advanced Care Planning" = "acp",
              "Teaching" = "educat",
              "Physical Exam" = "pe",
              "Presentation" = "pres",
              "Written H&P" = "writehp",
              "Progress Note" = "daily",
              "Discharge Summary" = "dc",
              "Family Meeting" = "meet",
              "Supervision of Team" = "senior",
              "Procedure" = "proc",
              "Multi-Disciplinary Rounds" = "mdr",
              "Emergent Situation" = "emer",
              "Point of Care Ultrasound" = "pocus"
            ),
            selected = "all"
          )
        )
      ),
      bslib::layout_column_wrap(
        width = 1/2,
        dateRangeInput(
          ns("date_range"),
          "Date Range:",
          start = Sys.Date() - 365,
          end = Sys.Date()
        ),
        actionButton(ns("reset"), "Reset Filters", class = "btn-secondary mt-4")
      )
    ),
    
    # Summary Statistics
    bslib::card(
      bslib::card_header("Assessment Summary"),
      uiOutput(ns("summary_stats"))
    ),
    
    # Assessment Data Table
    bslib::card(
      bslib::card_header("Assessment Details"),
      DT::dataTableOutput(ns("assessment_table"))
    ),
    
    # Plus/Delta View
    bslib::card(
      bslib::card_header("Plus/Delta Feedback"),
      DT::dataTableOutput(ns("plusdelta_table"))
    )
  )
}

#' Assessment Viewer Module - Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing RDM 2.0 data from load_rdm_complete()
#' @export
mod_assessment_viewer_server <- function(id, rdm_data) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize resident choices
    observe({
      req(rdm_data())
      
      residents <- rdm_data()$residents
      
      resident_choices <- setNames(
        residents$name,
        paste0(residents$name, " (PGY-", residents$Level, ")")
      )
      
      updateSelectInput(
        session,
        "resident",
        choices = c("All Residents" = "all", resident_choices)
      )
    })
    
    # Get relevant field names based on evaluation type
    get_eval_fields <- function(eval_type, obs_subtype = NULL) {
      
      base_fields <- c("record_id", "ass_date", "ass_level", 
                      "ass_faculty", "ass_specialty", 
                      "ass_plus", "ass_delta")
      
      if (eval_type == "all") {
        # Return all assessment fields
        data_dict <- rdm_data()$data_dict
        all_ass_fields <- data_dict %>%
          filter(form_name == "assessment", 
                 grepl("^ass_", field_name),
                 !field_name %in% c("ass_date", "ass_level", "ass_faculty", 
                                   "ass_specialty", "ass_plus", "ass_delta")) %>%
          pull(field_name)
        
        return(c(base_fields, all_ass_fields))
      }
      
      if (eval_type == "obs" && !is.null(obs_subtype) && obs_subtype != "all") {
        # Specific observation subtype
        pattern <- paste0("^ass_obs_", obs_subtype, "_")
        specific_fields <- c("ass_obs_type", paste0("ass_obs_", obs_subtype))
      } else if (eval_type == "obs") {
        # All observation fields
        pattern <- "^ass_obs_"
        specific_fields <- NULL
      } else {
        # Other evaluation types
        pattern <- paste0("^ass_", eval_type, "_")
        specific_fields <- NULL
      }
      
      data_dict <- rdm_data()$data_dict
      type_fields <- data_dict %>%
        filter(form_name == "assessment",
               grepl(pattern, field_name)) %>%
        pull(field_name)
      
      return(c(base_fields, specific_fields, type_fields))
    }
    
    # Filter assessment data
    filtered_assessments <- reactive({
      req(rdm_data())
      
      assessment_data <- rdm_data()$assessment
      
      # Filter by resident
      if (input$resident != "all") {
        assessment_data <- assessment_data %>%
          filter(name == input$resident)
      }
      
      # Filter by date range
      if (!is.null(input$date_range)) {
        assessment_data <- assessment_data %>%
          filter(
            ass_date >= input$date_range[1],
            ass_date <= input$date_range[2]
          )
      }
      
      # Get relevant fields for this evaluation type
      eval_fields <- get_eval_fields(input$eval_type, input$obs_subtype)
      
      # Select only relevant fields and filter to rows with data
      available_fields <- intersect(eval_fields, names(assessment_data))
      
      result <- assessment_data %>%
        select(all_of(available_fields)) %>%
        # Keep only rows where at least one eval field has data
        filter(
          if_any(
            -c(record_id, ass_date, ass_level, ass_faculty, 
               ass_specialty, ass_plus, ass_delta),
            ~ !is.na(.) & . != ""
          )
        )
      
      return(result)
    })
    
    # Summary statistics
    output$summary_stats <- renderUI({
      data <- filtered_assessments()
      
      n_assessments <- nrow(data)
      n_residents <- length(unique(data$name))
      
      date_range <- if (n_assessments > 0) {
        paste(min(data$ass_date, na.rm = TRUE), "to", 
              max(data$ass_date, na.rm = TRUE))
      } else {
        "No data"
      }
      
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(
          title = "Total Assessments",
          value = n_assessments,
          theme = "primary"
        ),
        bslib::value_box(
          title = "Unique Residents",
          value = n_residents,
          theme = "success"
        ),
        bslib::value_box(
          title = "Date Range",
          value = date_range,
          theme = "info",
          showcase = fontawesome::fa("calendar")
        )
      )
    })
    
    # Create human-readable field labels
    get_field_labels <- function(data) {
      data_dict <- rdm_data()$data_dict
      
      field_mapping <- data_dict %>%
        filter(form_name == "assessment") %>%
        select(field_name, field_label) %>%
        deframe()
      
      return(field_mapping)
    }
    
    # Format assessment data for display
    format_assessment_display <- function(data) {
      if (nrow(data) == 0) return(data)
      
      field_labels <- get_field_labels(data)
      
      # Rename columns to human-readable labels
      display_data <- data
      
      for (col in names(display_data)) {
        if (col %in% names(field_labels)) {
          names(display_data)[names(display_data) == col] <- field_labels[col]
        }
      }
      
      # Move key columns to front
      key_cols <- c("Assessment date", "leval of res", "Faculty/Fellow", "Specialty")
      key_cols <- intersect(key_cols, names(display_data))
      other_cols <- setdiff(names(display_data), key_cols)
      
      display_data <- display_data %>%
        select(all_of(c(key_cols, other_cols)))
      
      return(display_data)
    }
    
    # Assessment data table
    output$assessment_table <- DT::renderDataTable({
      data <- filtered_assessments()
      
      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No assessments found with the selected filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Remove completely empty columns
      data <- data %>%
        select(where(~ !all(is.na(.) | . == "")))
      
      display_data <- format_assessment_display(data)
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(width = '150px', targets = 0:3)
          )
        ),
        filter = 'top',
        rownames = FALSE,
        class = 'cell-border stripe compact'
      ) %>%
        DT::formatStyle(
          columns = names(display_data),
          fontSize = '12px'
        )
    })
    
    # Plus/Delta table
    output$plusdelta_table <- DT::renderDataTable({
      data <- filtered_assessments()
      
      plusdelta_data <- data %>%
        filter(!is.na(ass_plus) | !is.na(ass_delta)) %>%
        select(ass_date, name, ass_level, ass_faculty, 
               ass_specialty, ass_plus, ass_delta) %>%
        arrange(desc(ass_date))
      
      if (nrow(plusdelta_data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No plus/delta feedback in selected assessments"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Rename for display
      names(plusdelta_data) <- c("Date", "Resident", "Level", "Faculty", 
                                  "Specialty", "Plus", "Delta")
      
      DT::datatable(
        plusdelta_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })
    
    # Reset filters
    observeEvent(input$reset, {
      updateSelectInput(session, "resident", selected = "all")
      updateSelectInput(session, "eval_type", selected = "all")
      updateSelectInput(session, "obs_subtype", selected = "all")
      updateDateRangeInput(
        session,
        "date_range",
        start = Sys.Date() - 365,
        end = Sys.Date()
      )
    })
  })
}