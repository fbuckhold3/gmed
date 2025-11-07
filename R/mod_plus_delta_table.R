# ============================================================================
# PLUS/DELTA TABLE MODULE
# R/modules/mod_plus_delta_table.R
# ============================================================================

#' Plus/Delta Table Module UI
#'
#' Creates a user interface for displaying resident assessment plus/delta feedback
#' in a styled datatable. This module is designed to work with RDM 2.0 assessment
#' data structure and provides a consistent, professional display of evaluation
#' feedback filtered by resident record ID.
#'
#' The module creates a card-based interface with:
#' - Optional customizable title with icon
#' - Styled datatable with consistent gmed theming
#' - Summary statistics footer
#' - Responsive design that works on mobile and desktop
#'
#' @param id Character string. Module namespace ID used to create unique input/output IDs.
#'        This should be unique within your application.
#' @param title Character string. Optional title displayed in the card header.
#'        Defaults to "Plus/Delta Feedback". Use NULL to hide the header entirely.
#'
#' @return A \code{tagList} containing Shiny UI elements:
#'         - Card container with gmed styling classes
#'         - Header section (if title provided)
#'         - Datatable output container
#'         - Summary statistics footer
#'
#' @family plus_delta_modules
#' @seealso \code{\link{mod_plus_delta_table_server}} for the corresponding server function
#'
#' @examples
#' \dontrun{
#' # Basic usage in UI
#' fluidPage(
#'   mod_plus_delta_table_ui("resident_feedback")
#' )
#'
#' # With custom title
#' mod_plus_delta_table_ui("feedback_table", title = "Assessment Results")
#'
#' # Without header
#' mod_plus_delta_table_ui("minimal_table", title = NULL)
#' }
#'
#' @export
mod_plus_delta_table_ui <- function(id, title = "Plus/Delta Feedback") {
  ns <- NS(id)
  
  tagList(
    div(class = "gmed-card",
        if (!is.null(title)) {
          div(class = "gmed-card-header",
              h4(icon("comments"), title, class = "gmed-card-title")
          )
        },
        
        # Main table output
        div(class = "gmed-datatable-container",
            DT::dataTableOutput(ns("plus_delta_table"))
        ),
        
        # Summary statistics
        div(class = "gmed-table-footer",
            uiOutput(ns("table_summary"))
        )
    )
  )
}

#' Plus/Delta Table Module Server
#'
#' Server-side logic for the plus/delta table module. This function processes
#' RDM 2.0 assessment data to extract, clean, and display plus/delta feedback
#' for a selected resident. It handles data filtering, formatting, and provides
#' reactive summary statistics.
#'
#' The server function performs the following operations:
#' 1. Filters data by the provided record_id
#' 2. Extracts assessment records (redcap_repeat_instrument = "Assessment")
#' 3. Includes only records with non-empty plus or delta feedback
#' 4. Formats and cleans all display fields
#' 5. Renders a styled datatable with custom formatting
#' 6. Provides reactive summary statistics
#'
#' @param id Character string. Module namespace ID that must match the UI function.
#' @param rdm_data Reactive expression returning a data frame. The complete RDM 2.0
#'        dataset containing assessment records. Must include the following columns:
#'        \itemize{
#'          \item \code{record_id} - Unique resident identifier
#'          \item \code{redcap_repeat_instrument} - REDCap instrument type
#'          \item \code{redcap_repeat_instance} - REDCap repeat instance number
#'          \item \code{ass_date} - Assessment date
#'          \item \code{ass_level} - Assessment level (Intern, PGY2, etc.)
#'          \item \code{ass_plus} - Positive feedback text
#'          \item \code{ass_delta} - Areas for improvement text
#'          \item \code{ass_faculty} - Evaluating faculty name
#'          \item \code{ass_specialty} - Department/specialty
#'        }
#' @param record_id Reactive expression returning a character string. The record_id
#'        of the resident whose assessments should be displayed. When this changes,
#'        the table will automatically update to show the new resident's data.
#'
#' @return A named list containing reactive expressions:
#'         \describe{
#'           \item{\code{data}}{Reactive data frame with processed plus/delta records}
#'           \item{\code{summary}}{Reactive list with summary statistics:
#'             \itemize{
#'               \item \code{total_entries} - Number of assessment records
#'               \item \code{plus_count} - Number of records with plus feedback
#'               \item \code{delta_count} - Number of records with delta feedback
#'               \item \code{faculty_count} - Number of unique faculty members
#'             }
#'           }
#'         }
#'
#' @details
#' The module expects assessment data to follow the RDM 2.0 structure where:
#' - Each resident has a unique \code{record_id}
#' - Assessment records are marked with \code{redcap_repeat_instrument = "Assessment"}
#' - Plus and delta feedback are stored in separate fields
#' - Missing or empty values are handled gracefully with "Not specified" or "Not provided" defaults
#'
#' The rendered datatable includes:
#' - Date formatting (MM/DD/YYYY)
#' - Color-coded columns (green for Plus, orange for Delta, purple for Level)
#' - Sortable columns with date sorting (newest first by default)
#' - Responsive design with horizontal scrolling when needed
#' - Consistent gmed package styling
#'
#' @family plus_delta_modules
#' @seealso \code{\link{mod_plus_delta_table_ui}} for the corresponding UI function
#' @seealso \code{\link{create_gmed_datatable}} for the underlying table creation function
#'
#' @examples
#' \dontrun{
#' # Basic server usage
#' server <- function(input, output, session) {
#'   # Your data loading logic
#'   assessment_data <- reactive({
#'     # Load your RDM 2.0 data
#'   })
#'   
#'   # Selected resident
#'   selected_resident_id <- reactive(input$resident_selector)
#'   
#'   # Call the module
#'   feedback_results <- mod_plus_delta_table_server(
#'     "resident_feedback",
#'     rdm_data = assessment_data,
#'     record_id = selected_resident_id
#'   )
#'   
#'   # Access summary statistics
#'   observe({
#'     summary <- feedback_results$summary()
#'     cat("Total assessments:", summary$total_entries, "\n")
#'   })
#' }
#' }
#'
#' @export
mod_plus_delta_table_server <- function(id, rdm_data, record_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ========================================================================
    # REACTIVE DATA PROCESSING
    # ========================================================================
    
    # Filter and process plus/delta data
    plus_delta_data <- reactive({
      req(rdm_data(), record_id())
      
      cat("=== PLUS/DELTA DATA PROCESSING ===\n")
      cat("Record ID:", record_id(), "\n")
      
      # Get the raw data
      data <- rdm_data()
      
      # Filter by record_id and assessment fields
      filtered_data <- data %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::filter(!is.na(redcap_repeat_instrument) & 
                        redcap_repeat_instrument == "assessment") %>%
        dplyr::select(
          record_id,
          redcap_repeat_instance,
          ass_date,
          ass_level,
          ass_plus,
          ass_delta, 
          ass_faculty,
          ass_specialty
        ) %>%
        # Only keep rows that have either plus or delta content
        dplyr::filter(
          (!is.na(ass_plus) & nzchar(trimws(ass_plus))) | 
            (!is.na(ass_delta) & nzchar(trimws(ass_delta)))
        )
      
      cat("Found", nrow(filtered_data), "plus/delta records\n")
      
      if (nrow(filtered_data) == 0) {
        return(data.frame(
          Date = character(0),
          Level = character(0),
          Faculty = character(0),
          Specialty = character(0),
          Plus = character(0),
          Delta = character(0)
        ))
      }
      
      # In the plus_delta_data reactive, replace the select() call with:
      
      # In your gmed package R/mod_plus_delta_table.R
      # In the plus_delta_data reactive, replace the select() call with:
      
      result <- filtered_data %>%
        # FIXED: Use ass_level instead of level
        dplyr::select(record_id, ass_date, ass_level, ass_faculty, ass_specialty, ass_plus, ass_delta) %>%
        dplyr::filter(
          (!is.na(ass_plus) & nzchar(trimws(ass_plus))) | 
            (!is.na(ass_delta) & nzchar(trimws(ass_delta)))
        ) %>%
        dplyr::mutate(
          # Create a sortable date column FIRST
          sort_date = dplyr::case_when(
            !is.na(ass_date) ~ {
              if(inherits(ass_date, "Date")) {
                ass_date
              } else {
                tryCatch({
                  lubridate::as_date(ass_date)
                }, error = function(e) as.Date(NA))
              }
            },
            TRUE ~ as.Date(NA)
          ),
          
          # Format display date
          Date = dplyr::case_when(
            !is.na(sort_date) ~ format(sort_date, "%m/%d/%Y"),
            TRUE ~ "No Date"
          ),
          
          # FIXED: Handle ass_level directly (no fallback to missing level column)
          Level = dplyr::case_when(
            !is.na(ass_level) ~ case_when(
              ass_level == 1 | ass_level == "1" | ass_level == "Intern" ~ "Intern",
              ass_level == 2 | ass_level == "2" | ass_level == "PGY2" ~ "PGY2", 
              ass_level == 3 | ass_level == "3" | ass_level == "PGY3" ~ "PGY3",
              TRUE ~ as.character(ass_level)
            ),
            TRUE ~ "Unknown"
          ),
          
          # Format faculty and specialty
          Faculty = dplyr::case_when(
            !is.na(ass_faculty) & nzchar(trimws(ass_faculty)) ~ trimws(ass_faculty),
            TRUE ~ "Not specified"
          ),
          
          Specialty = dplyr::case_when(
            !is.na(ass_specialty) & nzchar(trimws(ass_specialty)) ~ trimws(ass_specialty),
            TRUE ~ "Not specified"
          ),
          
          # Format plus feedback
          Plus = dplyr::case_when(
            !is.na(ass_plus) & nzchar(trimws(ass_plus)) ~ trimws(ass_plus),
            TRUE ~ "Not provided"
          ),
          
          # Format delta feedback
          Delta = dplyr::case_when(
            !is.na(ass_delta) & nzchar(trimws(ass_delta)) ~ trimws(ass_delta),
            TRUE ~ "Not provided"
          )
        ) %>%
        # FIXED: Sort by the proper date column (newest first)
        dplyr::arrange(dplyr::desc(sort_date)) %>%
        # Remove the helper column
        dplyr::select(Date, Level, Faculty, Specialty, Plus, Delta)
      
      return(result)
    })
    
    # ========================================================================
    # RENDER DATATABLE
    # ========================================================================
    
    output$plus_delta_table <- DT::renderDataTable({
      data <- plus_delta_data()
      
      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No plus/delta feedback available for this resident"),
          options = list(dom = 't', ordering = FALSE, searching = FALSE),
          rownames = FALSE,
          class = 'table table-striped'
        ))
      }
      
      # Create the datatable with gmed styling
      dt <- create_gmed_datatable(
        data,
        caption = paste("Plus/Delta Feedback (", nrow(data), "entries )"),
        page_length = 10,
        scrollX = TRUE,
        highlight_columns = c("Plus", "Delta")
      )
      
      # Additional custom styling for plus/delta
      dt <- dt %>%
        DT::formatStyle(
          'Plus',
          backgroundColor = '#e8f5e9',  # Light green
          borderLeft = '3px solid #4caf50'  # Green border
        ) %>%
        DT::formatStyle(
          'Delta',
          backgroundColor = '#fff3e0',  # Light orange  
          borderLeft = '3px solid #ff9800'  # Orange border
        ) %>%
        DT::formatStyle(
          'Date',
          fontWeight = 'bold',
          color = '#1976d2'
        ) %>%
        DT::formatStyle(
          'Level',
          fontWeight = 'bold',
          backgroundColor = '#f3e5f5',  # Light purple
          color = '#7b1fa2'
        )
      
      return(dt)
    })
    
    # ========================================================================
    # RENDER SUMMARY STATISTICS
    # ========================================================================
    
    output$table_summary <- renderUI({
      data <- plus_delta_data()
      
      if (nrow(data) == 0) {
        return(NULL)
      }
      
      plus_count <- sum(data$Plus != "Not provided")
      delta_count <- sum(data$Delta != "Not provided") 
      faculty_count <- length(unique(data$Faculty[data$Faculty != "Not specified"]))
      
      div(class = "gmed-summary-stats",
          p(
            icon("chart-bar"), 
            strong("Summary: "),
            span(paste(plus_count, "plus items,"), style = "color: #4caf50;"),
            span(paste(delta_count, "delta items"), style = "color: #ff9800;"),
            paste0("from ", faculty_count, " faculty member", if(faculty_count != 1) "s" else "")
          )
      )
    })
    
    # ========================================================================
    # RETURN MODULE INTERFACE
    # ========================================================================
    
    return(list(
      data = plus_delta_data,
      summary = reactive({
        data <- plus_delta_data()
        list(
          total_entries = nrow(data),
          plus_count = sum(data$Plus != "Not provided"),
          delta_count = sum(data$Delta != "Not provided"),
          faculty_count = length(unique(data$Faculty[data$Faculty != "Not specified"]))
        )
      })
    ))
  })
}

#' Create Questions Summary Display
#'
#' Creates a large number display showing weekly questions average.
#' Displays the result as a prominent number "out of 4" with supporting details.
#'
#' @param data Data frame containing questions data with columns: record_id, source_form, q_date
#' @param record_id Character or numeric ID of the resident to analyze
#' @param resident_name Optional character string with resident name. If NULL, will use "Resident [ID]"
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
  
  # Use provided name or default
  if (is.null(resident_name)) {
    resident_name <- paste("Resident", record_id)
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