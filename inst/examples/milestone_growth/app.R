# Minimal example app for the shared milestone growth module.
#
# Runs on synthetic data by default (no REDCap access needed):
#   shiny::runApp(system.file("examples/milestone_growth", package = "gmed"))
#
# To try it on the RDM test project instead, set RDM_TOKEN_TEST and
# MG_USE_REDCAP=1. Never point this at prod until it has been checked
# against the test project.
library(shiny)
if (!"gmed" %in% loadedNamespaces()) library(gmed)

use_redcap <- identical(Sys.getenv("MG_USE_REDCAP"), "1") && nzchar(Sys.getenv("RDM_TOKEN_TEST"))

if (use_redcap) {
  # Unfiltered load: graduated (archived) residents are the cohort history
  rdm <- load_data_by_forms(rdm_token = Sys.getenv("RDM_TOKEN_TEST"),
                            filter_archived = FALSE, calculate_levels = FALSE,
                            raw_or_label = "raw")
  all_forms <- rdm$forms
  residents <- rdm$resident_data
  if (!"name" %in% names(residents)) residents$name <- residents$record_id
} else {
  sim <- simulate_milestone_cohort(n_per_class = 15, seed = 42)
  all_forms <- sim$all_forms
  residents <- sim$residents
}

# Build the long data once at startup (cheap) and fit once. In the real apps
# the fit comes from load_cached_milestone_growth(), written by the
# data-refresh job; fitting here only stands in for that cache.
long <- build_milestone_long(all_forms, residents)
fit <- fit_milestone_growth(long)

has_data <- unique(long$record_id)
choices <- residents[residents$record_id %in% has_data, ]
choices <- stats::setNames(choices$record_id, paste0(choices$name, " (", choices$grad_yr, ")"))

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),
  tags$h3("Milestone growth: example"),
  fluidRow(
    column(4, selectInput("rid", "Resident", choices = choices,
                          selected = choices[grepl("2027", names(choices))][1])),
    column(3, selectInput("period", "Current period", selected = 4,
                          choices = stats::setNames(1:6, milestone_periods()$period_name)))
  ),
  mod_milestone_growth_ui("mg")
)

server <- function(input, output, session) {
  mod_milestone_growth_server(
    "mg",
    milestone_data = long,
    resident_id    = reactive(input$rid),
    period         = reactive(input$period),
    ilp_data       = all_forms$ilp,
    fit            = fit
  )
}

shinyApp(ui, server)
