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

        # Charts
        fluidRow(
          column(6,
                 h4("Questions by Rotation"),
                 plotlyOutput(ns("rotation_bar_chart"), height = "400px")
          ),
          column(6,
                 h4("Timeline (Weekly)"),
                 plotlyOutput(ns("timeline_chart"), height = "400px")
          )
        ),

        # Monthly aggregation chart
        fluidRow(
          column(12,
                 h4("Questions per Month"),
                 plotlyOutput(ns("monthly_chart"), height = "400px")
          )
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

      # Add rotation labels using the lookup
      rot_lookup <- rotation_labels()

      if (!is.null(rot_lookup) && length(rot_lookup) > 0) {
        data <- data %>%
          dplyr::mutate(
            rotation_label = dplyr::case_when(
              is.na(q_rotation) | q_rotation == "" ~ "Unknown",
              as.character(q_rotation) %in% names(rot_lookup) ~ rot_lookup[[as.character(q_rotation)]],
              TRUE ~ paste("Rotation", q_rotation)
            )
          )
      } else {
        # Fallback: use hardcoded mapping based on user's specification
        data <- data %>%
          dplyr::mutate(
            rotation_label = dplyr::case_when(
              is.na(q_rotation) | q_rotation == "" ~ "Unknown",
              q_rotation == "1" ~ "Red",
              q_rotation == "2" ~ "Green",
              q_rotation == "3" ~ "White",
              q_rotation == "4" ~ "Yellow",
              q_rotation == "5" ~ "Diamond",
              q_rotation == "6" ~ "Gold",
              q_rotation == "7" ~ "MICU",
              q_rotation == "8" ~ "Bronze",
              q_rotation == "9" ~ "Cardiology",
              q_rotation == "10" ~ "Bridge / Acute Care",
              q_rotation == "11" ~ "Consults -SLUH",
              q_rotation == "12" ~ "Elective / Clinics CSM",
              q_rotation == "13" ~ "VA A",
              q_rotation == "14" ~ "VA B",
              q_rotation == "15" ~ "VA C",
              q_rotation == "16" ~ "VA D",
              q_rotation == "17" ~ "VA Clinics or Consults",
              TRUE ~ paste("Rotation", q_rotation)
            )
          )
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
      req(questions_data())

      weekly_data <- questions_data() %>%
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
          title = "Questions Over Time (All Rotations)",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Questions per Week", rangemode = "tozero"),
          showlegend = TRUE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    output$monthly_chart <- renderPlotly({
      req(questions_data())

      monthly_data <- questions_data() %>%
        dplyr::mutate(
          month = lubridate::floor_date(q_date, "month")
        ) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          questions = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(month) %>%
        dplyr::mutate(
          month_label = format(month, "%b %Y")
        )

      colors <- ssm_colors()

      plot_ly(
        data = monthly_data,
        x = ~month_label,
        y = ~questions,
        type = "bar",
        marker = list(
          color = colors$accent_blue,
          line = list(color = colors$primary, width = 1)
        ),
        text = ~questions,
        textposition = "outside",
        hovertemplate = paste0(
          "<b>%{x}</b><br>",
          "Questions: %{y}<br>",
          "<extra></extra>"
        )
      ) %>%
        layout(
          title = "Monthly Question Totals (All Rotations)",
          xaxis = list(
            title = "Month",
            type = "category",
            categoryorder = "array",
            categoryarray = ~month_label
          ),
          yaxis = list(title = "Total Questions", rangemode = "tozero"),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          bargap = 0.2
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