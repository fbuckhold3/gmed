#' Questions Visualization Module UI
#' 
#' Visualizes conference attendance by rotation with interactive charts
#' @param id Module namespace
#' @param title Title for the module
#' @export
mod_questions_viz_ui <- function(id, title = "Conference Attendance by Rotation") {
  ns <- NS(id)
  colors <- ssm_colors()
  
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        .questions-container {
          background: white;
          border-radius: 12px;
          padding: 1.5rem;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .questions-header {
          background: linear-gradient(135deg, ", colors$info, " 0%, ", colors$primary, " 100%);
          color: white;
          padding: 1.5rem;
          border-radius: 8px;
          margin-bottom: 1.5rem;
        }
        .summary-card {
          background: ", colors$light_gray, ";
          border-left: 4px solid ", colors$success, ";
          padding: 1rem;
          border-radius: 6px;
          margin-bottom: 1rem;
        }
        .summary-number {
          font-size: 2.5rem;
          font-weight: bold;
          color: ", colors$primary, ";
        }
        .rotation-selector {
          margin: 1rem 0;
        }
        .metric-card {
          background: white;
          border: 2px solid ", colors$light_gray, ";
          border-radius: 8px;
          padding: 1rem;
          margin: 0.5rem 0;
          transition: all 0.3s;
        }
        .metric-card:hover {
          border-color: ", colors$primary, ";
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }
        .metric-label {
          font-size: 0.9rem;
          color: #666;
          margin-bottom: 0.25rem;
        }
        .metric-value {
          font-size: 1.5rem;
          font-weight: bold;
          color: ", colors$primary, ";
        }
      ")))
    ),
    
    div(class = "questions-container",
        div(class = "questions-header",
            h3(icon("calendar-check"), title, style = "margin: 0;")
        ),
        
        # Overall summary
        fluidRow(
          column(4,
                 div(class = "summary-card",
                     div("Total Questions Answered"),
                     div(class = "summary-number", textOutput(ns("total_questions"), inline = TRUE))
                 )
          ),
          column(4,
                 div(class = "summary-card",
                     div("Average per Week"),
                     div(class = "summary-number", textOutput(ns("avg_per_week"), inline = TRUE))
                 )
          ),
          column(4,
                 div(class = "summary-card",
                     div("Weeks with Data"),
                     div(class = "summary-number", textOutput(ns("total_weeks"), inline = TRUE))
                 )
          )
        ),
        
        # Rotation selector
        div(class = "rotation-selector",
            h4("Filter by Rotation"),
            selectInput(
              ns("rotation_filter"),
              label = NULL,
              choices = NULL,  # Will populate dynamically
              width = "100%"
            )
        ),
        
        # Charts
        fluidRow(
          column(6,
                 h4("Questions by Rotation"),
                 plotlyOutput(ns("rotation_bar_chart"), height = "400px")
          ),
          column(6,
                 h4("Timeline"),
                 plotlyOutput(ns("timeline_chart"), height = "400px")
          )
        ),
        
        # Detailed metrics for selected rotation
        div(style = "margin-top: 2rem;",
            h4("Rotation Details"),
            uiOutput(ns("rotation_metrics"))
        ),
        
        # Data table
        div(style = "margin-top: 2rem;",
            h4("Question History"),
            DT::dataTableOutput(ns("questions_table"))
        )
    )
  )
}

#' Questions Visualization Module Server
#' @param id Module namespace
#' @param rdm_data Reactive returning full RDM data
#' @param record_id Reactive returning resident record_id
#' @param data_dict Reactive or static data dictionary
#' @export
mod_questions_viz_server <- function(id, rdm_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get rotation labels from data dictionary
    rotation_labels <- reactive({
      dict <- if(is.reactive(data_dict)) data_dict() else data_dict
      
      q_rot_row <- dict %>%
        dplyr::filter(field_name == "q_rotation")
      
      if (nrow(q_rot_row) == 0) return(NULL)
      
      parse_choices(q_rot_row$choices[1])
    })
    
    # Filter questions data for this resident
    questions_data <- reactive({
      req(rdm_data(), record_id())
      
      data <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          redcap_repeat_instrument == "questions"
        ) %>%
        dplyr::filter(!is.na(q_date) & q_date != "") %>%
        dplyr::mutate(
          q_date = as.Date(q_date, format = "%Y-%m-%d"),
          week = lubridate::floor_date(q_date, "week")
        )
      
      # Add rotation labels
      if (!is.null(rotation_labels())) {
        data <- data %>%
          dplyr::mutate(
            rotation_label = sapply(q_rotation, function(x) {
              if (is.na(x) || x == "") return("Unknown")
              label <- rotation_labels()[[as.character(x)]]
              if (is.null(label)) return(paste("Rotation", x))
              return(label)
            })
          )
      } else {
        data <- data %>%
          dplyr::mutate(rotation_label = paste("Rotation", q_rotation))
      }
      
      return(data)
    })
    
    # Update rotation filter choices
    observe({
      req(questions_data())
      
      rotations <- questions_data() %>%
        dplyr::count(rotation_label, sort = TRUE)
      
      choices <- c("All Rotations" = "all", 
                   setNames(rotations$rotation_label, rotations$rotation_label))
      
      updateSelectInput(session, "rotation_filter", choices = choices)
    })
    
    # Filtered data based on rotation selection
    filtered_questions <- reactive({
      req(questions_data(), input$rotation_filter)
      
      data <- questions_data()
      
      if (input$rotation_filter != "all") {
        data <- data %>%
          dplyr::filter(rotation_label == input$rotation_filter)
      }
      
      return(data)
    })
    
    # === SUMMARY METRICS ===
    
    output$total_questions <- renderText({
      req(questions_data())
      format(nrow(questions_data()), big.mark = ",")
    })
    
    output$avg_per_week <- renderText({
      req(questions_data())
      
      weekly_counts <- questions_data() %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      
      avg <- round(mean(weekly_counts$n), 1)
      as.character(avg)
    })
    
    output$total_weeks <- renderText({
      req(questions_data())
      
      n_weeks <- questions_data() %>%
        dplyr::pull(week) %>%
        unique() %>%
        length()
      
      as.character(n_weeks)
    })
    
    # === CHARTS ===
    
    output$rotation_bar_chart <- renderPlotly({
      req(questions_data())
      
      rotation_summary <- questions_data() %>%
        dplyr::count(rotation_label, sort = TRUE)
      
      colors <- ssm_colors()
      
      plot_ly(
        data = rotation_summary,
        x = ~n,
        y = ~reorder(rotation_label, n),
        type = "bar",
        orientation = "h",
        marker = list(
          color = colors$info,
          line = list(color = colors$primary, width = 1)
        ),
        text = ~n,
        textposition = "outside",
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Questions: %{x}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = "Total Questions by Rotation",
          xaxis = list(title = "Number of Questions"),
          yaxis = list(title = ""),
          margin = list(l = 150),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })
    
    output$timeline_chart <- renderPlotly({
      req(filtered_questions())
      
      weekly_data <- filtered_questions() %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
          questions = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(week)
      
      colors <- ssm_colors()
      
      plot_ly(
        data = weekly_data,
        x = ~week,
        y = ~questions,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = colors$success, width = 3),
        marker = list(
          color = colors$primary,
          size = 8,
          line = list(color = "white", width = 2)
        ),
        hovertemplate = paste0(
          "<b>Week of %{x|%b %d, %Y}</b><br>",
          "Questions: %{y}<br>",
          "<extra></extra>"
        )
      ) %>%
        add_trace(
          y = ~4,
          type = "scatter",
          mode = "lines",
          line = list(color = "red", width = 2, dash = "dash"),
          name = "Target (4/week)",
          hoverinfo = "skip"
        ) %>%
        layout(
          title = if(input$rotation_filter == "all") {
            "Questions Over Time (All Rotations)"
          } else {
            paste("Questions Over Time:", input$rotation_filter)
          },
          xaxis = list(title = "Week"),
          yaxis = list(title = "Questions per Week", rangemode = "tozero"),
          showlegend = TRUE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })
    
    # === ROTATION METRICS ===
    
    output$rotation_metrics <- renderUI({
      req(filtered_questions())
      
      data <- filtered_questions()
      
      if (nrow(data) == 0) {
        return(div(
          class = "alert alert-info",
          icon("info-circle"),
          " No data for selected rotation."
        ))
      }
      
      # Calculate metrics
      weekly_summary <- data %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      
      avg_per_week <- round(mean(weekly_summary$n), 2)
      median_per_week <- median(weekly_summary$n)
      weeks_at_target <- sum(weekly_summary$n >= 4)
      pct_at_target <- round(100 * weeks_at_target / nrow(weekly_summary), 1)
      
      fluidRow(
        column(3,
               div(class = "metric-card",
                   div(class = "metric-label", "Average per Week"),
                   div(class = "metric-value", avg_per_week)
               )
        ),
        column(3,
               div(class = "metric-card",
                   div(class = "metric-label", "Median per Week"),
                   div(class = "metric-value", median_per_week)
               )
        ),
        column(3,
               div(class = "metric-card",
                   div(class = "metric-label", "Weeks at Target"),
                   div(class = "metric-value", paste0(weeks_at_target, "/", nrow(weekly_summary)))
               )
        ),
        column(3,
               div(class = "metric-card",
                   div(class = "metric-label", "% Meeting Target"),
                   div(class = "metric-value", paste0(pct_at_target, "%"))
               )
        )
      )
    })
    
    # === DATA TABLE ===
    
    output$questions_table <- DT::renderDataTable({
      req(filtered_questions())
      
      table_data <- filtered_questions() %>%
        dplyr::select(q_date, rotation_label, q_answer, q_level) %>%
        dplyr::arrange(desc(q_date)) %>%
        dplyr::mutate(
          q_date = format(q_date, "%m/%d/%Y"),
          level_label = dplyr::case_when(
            q_level == "1" ~ "Intern",
            q_level == "2" ~ "PGY2",
            q_level == "3" ~ "PGY3",
            TRUE ~ as.character(q_level)
          )
        ) %>%
        dplyr::select(
          Date = q_date,
          Rotation = rotation_label,
          Answer = q_answer,
          Level = level_label
        )
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          order = list(list(0, 'desc'))
        ),
        rownames = FALSE,
        filter = "top"
      ) %>%
        DT::formatStyle(
          'Answer',
          fontWeight = 'bold',
          color = ssm_colors()$primary
        )
    })
  })
}

#' Parse REDCap Choices String
#' @param choices_raw Raw choices string from data dictionary
#' @return Named list of value -> label mappings
#' @keywords internal
parse_choices <- function(choices_raw) {
  # Handle NULL or NA input
  if (is.null(choices_raw) || length(choices_raw) == 0) return(NULL)
  if (is.na(choices_raw) || choices_raw == "") return(NULL)
  
  choices <- strsplit(choices_raw, "\\|")[[1]]
  lookup <- list()
  
  for (choice in choices) {
    parts <- trimws(strsplit(choice, ",")[[1]])
    if (length(parts) >= 2) {
      value <- parts[1]
      label <- paste(parts[-1], collapse = ",")
      lookup[[value]] <- label
    }
  }
  
  return(lookup)
}