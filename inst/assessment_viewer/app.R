# Assessment Viewer Standalone App
# Save as: inst/assessment_viewer/app.R

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(gmed)  # Your package with the module

# UI ----
ui <- bslib::page_navbar(
  title = "Assessment Data Viewer",
  theme = bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#005EB8",  # SLUCare blue
    secondary = "#6c757d",
    base_font = bslib::font_google("Open Sans"),
    heading_font = bslib::font_google("Montserrat")
  ),
  
  # Main assessment viewer panel
  bslib::nav_panel(
    title = "Assessment Explorer",
    icon = fontawesome::fa("clipboard-list"),
    mod_assessment_viewer_ui("assessment")
  ),
  
  # About/Help panel
  bslib::nav_panel(
    title = "About",
    icon = fontawesome::fa("circle-info"),
    bslib::card(
      bslib::card_header("Assessment Data Viewer"),
      shiny::HTML("
<h3>Overview</h3>
<p>This tool allows you to explore assessment data from RDM 2.0 organized by evaluation type.</p>

<h3>Evaluation Types</h3>

<p><strong>Continuity Clinic (CC)</strong>: Multiple assessment types for continuity clinic evaluations</p>
<ul>
  <li>Documentation review</li>
  <li>Inbasket management</li>
  <li>Patient interactions</li>
  <li>Quarterly evaluations</li>
</ul>

<p><strong>Single Clinic Days</strong>: Single-day clinic evaluations focusing on clinical reasoning and care plans</p>

<p><strong>Consults</strong>: Consultation evaluations including care planning and communication</p>

<p><strong>Intern Inpatient</strong>: Inpatient evaluations specific to intern-level competencies</p>

<p><strong>Resident Inpatient</strong>: Inpatient evaluations for upper-level residents</p>

<p><strong>Bridge Clinic</strong>: Bridge clinic specific assessments</p>

<p><strong>Observations</strong>: Direct observation assessments including:</p>
<ul>
  <li>Clinical Decision Making (CDM)</li>
  <li>Advanced Care Planning (ACP)</li>
  <li>Teaching Sessions</li>
  <li>Physical Exam</li>
  <li>Presentations</li>
  <li>Written H&P</li>
  <li>Progress Notes</li>
  <li>Discharge Summaries</li>
  <li>Family Meetings</li>
  <li>Team Supervision</li>
  <li>Procedures</li>
  <li>Multi-Disciplinary Rounds</li>
  <li>Emergent Situations</li>
  <li>Point of Care Ultrasound (POCUS) - <em>coming soon</em></li>
</ul>

<h3>Features</h3>
<ul>
  <li><strong>Filter by resident</strong>: View assessments for specific residents or all residents</li>
  <li><strong>Filter by type</strong>: Focus on specific evaluation types</li>
  <li><strong>Date range filtering</strong>: Limit results to specific time periods</li>
  <li><strong>Export capability</strong>: Download filtered data as CSV or Excel</li>
  <li><strong>Plus/Delta view</strong>: Quickly review narrative feedback</li>
</ul>

<h3>Data Source</h3>
<p>Data is pulled directly from the RDM 2.0 REDCap database using the assessment form.</p>
      ")
    )
  ),
  
  # Add footer
  bslib::nav_spacer(),
  bslib::nav_item(
    tags$span(
      style = "color: gray; font-size: 0.9em;",
      "RDM 2.0 | Internal Medicine Residency"
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Load RDM data on startup
  rdm_data <- reactive({
    
    # Show loading notification
    showNotification(
      "Loading RDM 2.0 data...",
      id = "loading",
      duration = NULL,
      type = "message"
    )
    
    # Load data using gmed function
    data <- tryCatch({
      load_rdm_complete(
        rdm_token = Sys.getenv("RDM_TOKEN"),
        redcap_url = "https://redcapsurvey.slu.edu/api/",
        verbose = TRUE
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
    
    # Remove loading notification
    removeNotification("loading")
    
    if (!is.null(data)) {
      showNotification(
        paste("Loaded", nrow(data$assessment), "assessment records"),
        type = "message",
        duration = 3
      )
    }
    
    return(data)
  })
  
  # Call the assessment viewer module
  mod_assessment_viewer_server("assessment", rdm_data)
}

# Run the app ----
shinyApp(ui, server)