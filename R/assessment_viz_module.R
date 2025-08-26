# assessment_viz_module.R
# Assessment Visualization Module for GMED Package

# Source the functions
# source("assessment_viz_functions.R")

library(shiny)
library(bslib)
library(plotly)

#' Assessment Visualization Module UI
#'
#' Creates a comprehensive assessment dashboard UI with combined charts and questions display.
#' Layout: 3/4 width for assessment and faculty charts, 1/4 width for questions summary.
#'
#' @param id Character string, module namespace ID
#' @param title Character string, title to display at top of module
#'
#' @return Shiny UI tagList for the assessment visualization module
#' @export
#'
#' @examples
#' \dontrun{
#' # In Shiny UI
#' assessment_viz_ui("main_assess", "Student Assessment Dashboard")
#' }
assessment_viz_ui <- function(id, title = "Assessment Dashboard") {
  ns <- NS(id)
  colors <- ssm_colors()
  
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        .assessment-viz-container {
          background: linear-gradient(135deg, ", colors$light_gray, " 0%, #e9ecef 100%);
          border-radius: 16px;
          padding: 1.5rem;
          animation: containerFadeIn 0.6s ease-out;
        }
        
        @keyframes containerFadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        .viz-card {
          background: white;
          border: none;
          border-radius: 12px;
          box-shadow: 0 4px 16px rgba(0, 61, 92, 0.08);
          margin-bottom: 1rem;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          transform-origin: center;
          animation: cardSlideIn 0.8s ease-out forwards;
          opacity: 0;
        }
        
        @keyframes cardSlideIn {
          from { opacity: 0; transform: translateX(-30px); }
          to { opacity: 1; transform: translateX(0); }
        }
        
        .viz-card:nth-child(2) { animation-delay: 0.2s; }
        .viz-card:nth-child(3) { animation-delay: 0.4s; }
        
        .viz-card:hover {
          transform: translateY(-4px) scale(1.01);
          box-shadow: 0 12px 32px rgba(0, 61, 92, 0.15);
        }
        
        .viz-card-header {
          background: linear-gradient(135deg, ", colors$primary, " 0%, ", colors$secondary, " 100%);
          color: white;
          padding: 1rem 1.5rem;
          border-radius: 12px 12px 0 0;
          font-weight: 600;
          font-size: 1.1rem;
          position: relative;
          overflow: hidden;
        }
        
        .viz-card-header::before {
          content: '';
          position: absolute;
          top: -50%;
          left: -100%;
          width: 200%;
          height: 200%;
          background: linear-gradient(45deg, transparent, rgba(255,255,255,0.1), transparent);
          transform: rotate(45deg);
          animation: headerShine 3s linear infinite;
        }
        
        @keyframes headerShine {
          0% { transform: translateX(-100%) rotate(45deg); }
          100% { transform: translateX(100%) rotate(45deg); }
        }
        
        .questions-display {
          background: linear-gradient(135deg, ", colors$accent_blue, " 0%, ", colors$light_blue, " 100%);
          color: white;
          border-radius: 12px;
          padding: 2rem;
          text-align: center;
          height: 100%;
          display: flex;
          flex-direction: column;
          justify-content: flex-start;
          min-height: 500px;
          animation: questionsSlideIn 1s ease-out 0.6s forwards, questionsPulse 3s ease-in-out 2s infinite;
          opacity: 0;
          transform: translateX(30px);
          box-shadow: 0 8px 24px rgba(33, 150, 243, 0.2);
          transition: all 0.3s ease;
        }
        
        .recent-activity-section {
          margin-bottom: 2rem;
          padding-bottom: 2rem;
          border-bottom: 2px solid rgba(255,255,255,0.3);
        }
        
        .activity-title {
          font-size: 1.4rem;
          font-weight: 600;
          margin-bottom: 1.5rem;
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 0.5rem;
          color: white !important;
        }
        
        .activity-item {
          margin-bottom: 1.5rem;
          color: white !important;
        }
        
        .activity-label {
          font-size: 1.1rem;
          opacity: 0.9;
          margin-bottom: 0.8rem;
          font-weight: 500;
          color: white !important;
        }
        
        .activity-big-number {
          font-size: 4.5rem;
          font-weight: 700;
          line-height: 1;
          margin-bottom: 0.3rem;
          color: white !important;
          text-shadow: 0 2px 4px rgba(0,0,0,0.2);
        }
        
        .activity-subtitle {
          font-size: 1.2rem;
          opacity: 0.8;
          margin-bottom: 0.5rem;
          color: white !important;
        }
        
        .conference-section {
          text-align: center;
        }
        
        .conference-explanation {
          font-size: 1.2rem;
          opacity: 0.9;
          margin-bottom: 1rem;
          line-height: 1.4;
          font-weight: 500;
          color: white !important;
        }
        
        @keyframes questionsSlideIn {
          to { opacity: 1; transform: translateX(0); }
        }
        
        @keyframes questionsPulse {
          0%, 100% { transform: scale(1); box-shadow: 0 8px 24px rgba(33, 150, 243, 0.2); }
          50% { transform: scale(1.02); box-shadow: 0 12px 32px rgba(33, 150, 243, 0.3); }
        }
        
        .questions-display:hover {
          transform: scale(1.03);
          box-shadow: 0 16px 40px rgba(33, 150, 243, 0.4);
        }
        
        .questions-big-number {
          font-size: 5rem;
          font-weight: 700;
          line-height: 1;
          margin-bottom: 0.5rem;
          animation: numberCountUp 1.5s ease-out 1s;
        }
        
        @keyframes numberCountUp {
          from { transform: scale(0); opacity: 0; }
          50% { transform: scale(1.2); opacity: 0.8; }
          to { transform: scale(1); opacity: 1; }
        }
        
        .questions-subtitle {
          font-size: 1.5rem;
          opacity: 0.9;
          margin-bottom: 1rem;
          animation: fadeInUp 0.8s ease-out 1.2s forwards;
          opacity: 0;
        }
        
        .questions-detail {
          font-size: 1rem;
          opacity: 0.8;
          animation: fadeInUp 0.8s ease-out 1.4s forwards;
          opacity: 0;
        }
        
        @keyframes fadeInUp {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 0.9; transform: translateY(0); }
        }
        
        /* Plotly chart enhancements */
        .js-plotly-plot {
          transition: all 0.3s ease;
          border-radius: 8px;
          overflow: hidden;
        }
        
        .js-plotly-plot:hover {
          transform: scale(1.01);
          box-shadow: 0 4px 16px rgba(0, 61, 92, 0.1);
        }
        
        /* Loading shimmer for charts */
        .chart-loading {
          background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
          background-size: 200% 100%;
          animation: loading 1.5s infinite;
          border-radius: 8px;
          height: 300px;
        }
        
        @keyframes loading {
          0% { background-position: 200% 0; }
          100% { background-position: -200% 0; }
        }
        
        /* Responsive animations */
        @media (max-width: 768px) {
          .viz-card:hover {
            transform: translateY(-2px) scale(1.005);
          }
          .questions-display:hover {
            transform: scale(1.01);
          }
        }
      ")))
    ),
    
    div(class = "assessment-viz-container",
        h3(title, style = paste0("color: ", colors$primary, "; font-weight: 600; margin-bottom: 1.5rem; animation: titleSlide 0.8s ease-out;")),
        
        fluidRow(
          # Left column: Charts (3/4 width)
          column(9,
                 # Assessment section
                 div(class = "viz-card",
                     div(class = "viz-card-header", 
                         icon("clipboard-check"), " Feedback Assessments Completed"),
                     div(style = "padding: 1.5rem;",
                         plotlyOutput(ns("assessment_combined"), height = "300px")
                     )
                 ),
                 
                 # Faculty evaluation section  
                 div(class = "viz-card",
                     div(class = "viz-card-header",
                         icon("user-tie"), " Faculty Evaluations Completed"),
                     div(style = "padding: 1.5rem;",
                         plotlyOutput(ns("faculty_combined"), height = "300px")
                     )
                 )
          ),
          
          # Right column: Combined sidebar in blue box (1/4 width)
          column(3,
                 div(class = "questions-display",
                     # Recent activity section
                     div(class = "recent-activity-section",
                         div(class = "activity-title",
                             icon("clock"),
                             "Last 4 Weeks"
                         ),
                         # Assessment activity
                         div(class = "activity-item",
                             div(class = "activity-label", "Resident Assessments"),
                             div(class = "activity-big-number", textOutput(ns("recent_assessments"), inline = TRUE)),
                             div(class = "activity-subtitle", "completed")
                         ),
                         # Faculty activity  
                         div(class = "activity-item",
                             div(class = "activity-label", "Faculty Evaluations"),
                             div(class = "activity-big-number", textOutput(ns("recent_faculty"), inline = TRUE)),
                             div(class = "activity-subtitle", "completed")
                         )
                     ),
                     
                     # Conference attendance section
                     div(class = "conference-section",
                         div(class = "conference-explanation",
                             "Conference Attendance",
                             br(),
                             "Average days you attended noon conference"
                         ),
                         div(class = "questions-big-number", textOutput(ns("questions_number"))),
                         div(class = "questions-subtitle", "out of 4"),
                         div(class = "questions-detail", textOutput(ns("questions_detail")))
                     )
                 )
          )
        )
    ),
    
    # JavaScript for enhanced chart interactions
    tags$script(HTML(paste0("
      $(document).ready(function() {
        // Add loading states for plotly charts
        $(document).on('shiny:busy', function() {
          $('#", ns(""), " .js-plotly-plot').parent().addClass('chart-loading');
        });
        
        $(document).on('shiny:idle', function() {
          $('#", ns(""), " .js-plotly-plot').parent().removeClass('chart-loading');
        });
        
        // Enhanced plotly hover effects
        $(document).on('plotly_hover', '#", ns(""), " .js-plotly-plot', function() {
          $(this).css({
            'transform': 'scale(1.02)',
            'z-index': '10'
          });
        });
        
        $(document).on('plotly_unhover', '#", ns(""), " .js-plotly-plot', function() {
          $(this).css({
            'transform': 'scale(1)',
            'z-index': '1'
          });
        });
        
        // Stagger chart loading animations
        setTimeout(function() {
          $('#", ns(""), " .js-plotly-plot').each(function(index) {
            $(this).delay(index * 200).animate({ opacity: 1 }, 600);
          });
        }, 1000);
      });
    ")))
  )
}

#' Assessment Visualization Module Server
#'
#' Server function for the assessment visualization module. Renders combined assessment
#' and faculty evaluation charts plus weekly questions summary display.
#'
#' @param id Character string, module namespace ID (must match UI)
#' @param data Reactive expression returning data frame with assessment/evaluation data
#' @param record_id Reactive expression returning the resident record ID to display
#' @param resident_name Reactive expression returning resident name (optional, will extract from data if NULL)
#'
#' @return Server function for use with moduleServer()
#' @export
#'
#' @examples
#' \dontrun{
#' # In Shiny server
#' assessment_viz_server(
#'   "main_assess",
#'   data = reactive(my_data),
#'   record_id = reactive(input$selected_resident),
#'   resident_name = reactive(input$resident_name)
#' )
#' }
assessment_viz_server <- function(id, data, record_id, resident_name = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Calculate recent activity summary
    recent_activity <- reactive({
      req(data(), record_id())
      create_recent_activity_summary(data(), record_id(), resident_name())
    })
    
    # Render assessment chart
    output$assessment_combined <- renderPlotly({
      req(data(), record_id())
      create_combined_assessment_chart(data(), record_id(), resident_name())
    })
    
    # Render faculty chart  
    output$faculty_combined <- renderPlotly({
      req(data(), record_id())
      create_combined_faculty_chart(data(), record_id(), resident_name())
    })
    
    # Render recent activity numbers
    output$recent_assessments <- renderText({
      req(recent_activity())
      as.character(recent_activity()$recent_assessments)
    })
    
    output$recent_faculty <- renderText({
      req(recent_activity())
      as.character(recent_activity()$recent_faculty)
    })
    
    # Render questions/conference data
    output$questions_number <- renderText({
      req(recent_activity())
      as.character(recent_activity()$conference_avg)
    })
    
    output$questions_detail <- renderText({
      req(data(), record_id())
      summary <- create_weekly_questions_average(data(), record_id(), resident_name())
      summary$detail_text
    })
  })
}
