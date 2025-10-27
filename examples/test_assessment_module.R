# examples/test_assessment_module.R

library(gmed)
library(shiny)

# Load test data
rdm_data <- reactive({
  load_rdm_complete(
    rdm_token = Sys.getenv("RDM_TOKEN"),
    redcap_url = "https://redcapsurvey.slu.edu/api/"
  )
})

# Create minimal UI
ui <- fluidPage(
  mod_assessment_viewer_ui("test")
)

server <- function(input, output, session) {
  mod_assessment_viewer_server("test", rdm_data)
}

shinyApp(ui, server)