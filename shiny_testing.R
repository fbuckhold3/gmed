library(shiny)
library(plotly)
library(dplyr)
library(shinyjs)

rdm_token <- '76AA0C37298CDA3B6D8B97F26D36A752'
devtools::load_all()

complete_data$residents$access_code
complete_data <- load_rdm_complete(rdm_token = rdm_token, verbose = TRUE)

# Demo UI
ui <- fluidPage(
  
  # Enable shinyjs
  useShinyjs(),
  
  # Include milestone dashboard CSS
  milestone_dashboard_css(),
  
  # App title
  titlePanel("Milestone Dashboard Demo"),
  
  # Sidebar for controls
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Demo Controls"),
      
      # Mock controls - replace with your actual data loading
      selectInput("resident_select", 
                  "Select Resident:",
                  choices = c("Brian Abboud (86)" = "86",
                              "Sample Resident 2" = "12", 
                              "Sample Resident 3" = "45"),
                  selected = "86"),
      
      selectInput("period_select",
                  "Select Period:", 
                  choices = c("Mid Intern" = "Mid Intern",
                              "End Intern" = "End Intern",
                              "Mid PGY2" = "Mid PGY2", 
                              "End PGY2" = "End PGY2",
                              "Mid PGY3" = "Mid PGY3",
                              "Graduating" = "Graduating"),
                  selected = "End PGY2"),
      
      hr(),
      
      h5("Layout Options:"),
      radioButtons("layout_option",
                   "Choose Layout:",
                   choices = c("Single Dashboard" = "single",
                               "Two Column (Program + Self)" = "two_col", 
                               "Three Column (All Types)" = "three_col"),
                   selected = "two_col"),
      
      hr(),
      
      # Instructions
      div(
        h5("Instructions:"),
        p("1. Load your milestone data first", style = "font-size: 12px;"),
        p("2. Select a resident and period", style = "font-size: 12px;"),
        p("3. View spider plots and select individual milestones", style = "font-size: 12px;"),
        p("4. Compare across different assessment types", style = "font-size: 12px;")
      )
    ),
    
    # Main panel with milestone dashboards
    mainPanel(
      width = 9,
      
      # Status/loading message
      div(id = "status_message",
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            div(class = "alert alert-info",
                icon("spinner", class = "fa-spin"), 
                " Loading milestone data... This may take a moment.")
          )
      ),
      
      # Instructions for setting up data
      conditionalPanel(
        condition = "!output.data_loaded",
        div(class = "alert alert-warning",
            h4("Setup Required"),
            p("To use this demo, you need to load your milestone data first:"),
            code("rdm_token <- 'YOUR_TOKEN_HERE'"),
            br(),
            code("devtools::load_all()"),
            br(), 
            code("complete_data <- load_rdm_complete(rdm_token = rdm_token)"),
            br(),
            code("# Then run this demo app")
        )
      ),
      
      # Dynamic milestone dashboard layout
      uiOutput("milestone_layout")
    )
  )
)

# Demo Server
server <- function(input, output, session) {
  
  # Mock data loading - replace with your actual data
  milestone_results <- reactive({
    # Check if user has loaded milestone data
    if (exists("complete_data", envir = .GlobalEnv)) {
      tryCatch({
        create_milestone_workflow_from_dict(
          all_forms = complete_data$all_forms,
          data_dict = complete_data$data_dict,
          resident_data = complete_data$residents,
          verbose = FALSE
        )
      }, error = function(e) {
        message("Error loading milestone data: ", e$message)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  })
  
  resident_data <- reactive({
    if (exists("complete_data", envir = .GlobalEnv)) {
      return(complete_data$residents)
    } else {
      # Mock resident data for demo
      return(data.frame(
        record_id = c("86", "12", "45"),
        name = c("Brian Abboud", "Sample Resident 2", "Sample Resident 3"),
        Level = c("PGY2", "Intern", "PGY3"),
        stringsAsFactors = FALSE
      ))
    }
  })
  
  # Check if data is loaded
  output$data_loaded <- reactive({
    !is.null(milestone_results())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Dynamic milestone layout based on user selection
  output$milestone_layout <- renderUI({
    req(input$layout_option)
    
    if (input$layout_option == "single") {
      # Single dashboard
      fluidRow(
        column(12, 
               milestone_dashboard_ui("single_dash", milestone_type = "program", height = "700px")
        )
      )
      
    } else if (input$layout_option == "two_col") {
      # Two column layout
      fluidRow(
        column(6, 
               milestone_dashboard_ui("prog_dash", milestone_type = "program", height = "600px")
        ),
        column(6,
               milestone_dashboard_ui("self_dash", milestone_type = "self", height = "600px")
        )
      )
      
    } else if (input$layout_option == "three_col") {
      # Three column layout
      fluidRow(
        column(4,
               milestone_dashboard_ui("prog_dash_3", milestone_type = "program", height = "550px")
        ),
        column(4,
               milestone_dashboard_ui("self_dash_3", milestone_type = "self", height = "550px")
        ),
        column(4,
               milestone_dashboard_ui("acgme_dash_3", milestone_type = "acgme", height = "550px")
        )
      )
    }
  })
  
  # Initialize milestone dashboard modules based on layout
  observe({
    req(input$layout_option, milestone_results())
    
    if (input$layout_option == "single") {
      milestone_dashboard_server("single_dash", milestone_results, 
                                 reactive(input$resident_select), reactive(input$period_select),
                                 "program", resident_data)
      
    } else if (input$layout_option == "two_col") {
      milestone_dashboard_server("prog_dash", milestone_results,
                                 reactive(input$resident_select), reactive(input$period_select), 
                                 "program", resident_data)
      
      milestone_dashboard_server("self_dash", milestone_results,
                                 reactive(input$resident_select), reactive(input$period_select),
                                 "self", resident_data)
      
    } else if (input$layout_option == "three_col") {
      milestone_dashboard_server("prog_dash_3", milestone_results,
                                 reactive(input$resident_select), reactive(input$period_select),
                                 "program", resident_data)
      
      milestone_dashboard_server("self_dash_3", milestone_results, 
                                 reactive(input$resident_select), reactive(input$period_select),
                                 "self", resident_data)
      
      milestone_dashboard_server("acgme_dash_3", milestone_results,
                                 reactive(input$resident_select), reactive(input$period_select), 
                                 "acgme", resident_data)
    }
  })
}

# Run the demo app
if (interactive()) {
  
  # Check if milestone data is available
  if (exists("complete_data", envir = .GlobalEnv)) {
    message("Found complete_data - running demo with real data")
  } else {
    message("complete_data not found - demo will show data loading instructions")
    message("To use with real data, run:")
    message("rdm_token <- 'YOUR_TOKEN'")
    message("devtools::load_all()")  
    message("complete_data <- load_rdm_complete(rdm_token = rdm_token)")
  }
  
  shinyApp(ui = ui, server = server)
}
