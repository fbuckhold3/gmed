# Quick test for Milestone Rating Module
# Tests the removal of Next button on final milestone (ics3)

library(shiny)
library(shinyjs)

# Load the package
devtools::load_all()

# Simple test UI
ui <- fluidPage(
  useShinyjs(),

  titlePanel("Milestone Rating Module Test"),

  div(
    class = "alert alert-info",
    h4("Testing Instructions:"),
    tags$ul(
      tags$li("Select a period from the dropdown"),
      tags$li("Navigate through all milestones using the Next button"),
      tags$li("When you reach the FINAL milestone (ICS3: Communication within Health Care Systems):"),
      tags$ul(
        tags$li(strong("Expected behavior:"), " The Next button should be hidden"),
        tags$li(strong("You should see:"), " Only the Previous button (full width)"),
        tags$li(strong("You should see:"), " The Submit Milestones button becomes enabled after completing all ratings")
      )
    )
  ),

  hr(),

  # Period selector
  selectInput("period",
              "Select Period:",
              choices = c("", "Entering Residency", "Mid Intern", "End Intern",
                         "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating"),
              width = "300px"),

  hr(),

  # Milestone rating module
  mod_miles_rating_ui("test_milestone")
)

# Server logic
server <- function(input, output, session) {

  # Call the milestone rating module server
  milestone_results <- mod_miles_rating_server(
    "test_milestone",
    period = reactive(input$period)
  )

  # Show results when done is clicked
  observeEvent(milestone_results$done(), {
    showModal(modalDialog(
      title = "Milestone Ratings Submitted!",
      div(
        h4("Scores:"),
        verbatimTextOutput("scores_output"),
        h4("Explanations:"),
        verbatimTextOutput("descriptions_output")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  output$scores_output <- renderPrint({
    milestone_results$scores()
  })

  output$descriptions_output <- renderPrint({
    milestone_results$desc()
  })
}

# Run the app
message("====================================")
message("Testing Milestone Rating Module")
message("====================================")
message("")
message("To test the Next button removal:")
message("1. Select a period")
message("2. Navigate through all milestones")
message("3. Verify Next button is hidden on ICS3 (final milestone)")
message("")
message("There are 21 total milestones:")
message("  - Patient Care (PC): 6 items")
message("  - Medical Knowledge (MK): 3 items")
message("  - Systems-Based Practice (SBP): 3 items")
message("  - Practice-Based Learning (PBLI): 2 items")
message("  - Professionalism (PROF): 4 items")
message("  - Interpersonal & Comm Skills (ICS): 3 items")
message("")
message("ICS3 is milestone #21 (the final one)")
message("====================================")
message("")

shinyApp(ui, server)
