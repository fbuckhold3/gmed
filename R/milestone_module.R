#' Module UI for Milestone Rating (your working version with fixes)
#' @param id module id
#' @export
mod_miles_rating_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("moduleContainer"),
        uiOutput(ns("progressSection")),
        uiOutput(ns("mainContent")),
        # Placeholder for the inline explanation box - moved above navigation buttons
        uiOutput(ns("explanationUI")),
        uiOutput(ns("navigationButtons")),
        # Final submit - now rendered conditionally via server
        uiOutput(ns("submitButtonUI"))
    )
  )
}

#' Module Server for Milestone Rating (your working version with fixes)
#'
#' @param id module id
#' @param period reactive returning the selected period
#' @export
mod_miles_rating_server <- function(id, period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # thresholds for when to require explanation
    thresholds <- list(
      "Entering Residency" = 3,
      "Mid Intern"          = 4,
      "End Intern"          = 5,
      "Mid PGY2"            = 6,
      "End PGY2"            = 7,
      "Mid PGY3"            = 8,
      "Graduating"          = 9
    )
    
    # image sets (same as before)
    imageSets <- list(
      PC = list(
        title = "Patient Care",
        images = paste0("pc", 1:6, ".png"),
        imageTitles = c("History","Physical Examination","Clinical Reasoning",
                        "Patient Management - Inpatient","Patient Management - Outpatient","Digital Health")
      ),
      MK = list(
        title = "Medical Knowledge",
        images = paste0("mk", 1:3, ".png"),
        imageTitles = c("Applied Foundational Sciences","Therapeutic Knowledge","Knowledge of Diagnostic Testing")
      ),
      SBP = list(
        title = "Systems-Based Practice",
        images = paste0("sbp", 1:3, ".png"),
        imageTitles = c("Patient Safety and Quality Improvement",
                        "System Navigation for Patient-Centered Care",
                        "Physician Role in Health Care Systems")
      ),
      PBLI = list(
        title = "Practice-Based Learning and Improvement",
        images = c("pbli1.png","pbli2.png"),
        imageTitles = c("Evidence-Based and Informed Practice","Reflective Practice and Commitment to Personal Growth")
      ),
      PROF = list(
        title = "Professionalism",
        images = paste0("prof", 1:4, ".png"),
        imageTitles = c("Professional Behavior","Ethical Principles","Accountability/Conscientiousness",
                        "Knowledge of Systemic and Individual Factors of Well-Being")
      ),
      ICS = list(
        title = "Interpersonal and Communication Skills",
        images = paste0("ics", 1:3, ".png"),
        imageTitles = c("Patient- and Family-Centered Communication",
                        "Interprofessional and Team Communication",
                        "Communication within Health Care Systems")
      )
    )
    
    # state
    state <- reactiveValues(
      currentSetIndex   = 1,
      currentImageIndex = 1,
      selections        = list(),
      descriptions      = list(),
      pendingSelection  = list(key = NULL, value = NULL)
    )
    
    # helpers
    currentSetName   <- reactive(names(imageSets)[ state$currentSetIndex ])
    currentSet       <- reactive(imageSets[[ state$currentSetIndex ]])
    currentImageFile <- reactive(currentSet()$images[ state$currentImageIndex ])
    selectionKey     <- reactive(paste0(currentSetName(), "_", state$currentImageIndex))
    
    # Fixed imagesDone reactive to avoid the "non-function" error
    totalImages <- reactive(sum(sapply(imageSets, function(z) length(z$images))))
    imagesDone <- reactive({
      base <- if(state$currentSetIndex > 1) {
        sum(sapply(imageSets[1:(state$currentSetIndex-1)], function(z) length(z$images)))
      } else {
        0
      }
      return(base + state$currentImageIndex - 1)
    })
    
    # Check if all milestone ratings are complete
    allComplete <- reactive({
      expectedKeys <- unlist(lapply(seq_along(imageSets), function(i) {
        setName <- names(imageSets)[i]
        imageCount <- length(imageSets[[i]]$images)
        paste0(setName, "_", seq_len(imageCount))
      }))
      
      # Check if all expected keys have selections
      allSelected <- all(expectedKeys %in% names(state$selections))
      
      # Check if all required explanations are provided
      if(allSelected) {
        periodVal <- period()
        if(!is.null(periodVal) && periodVal != "" && periodVal != "Interim Review") {
          threshold <- thresholds[[periodVal]]
          if(!is.null(threshold)) {
            needsExplanation <- sapply(names(state$selections), function(key) {
              state$selections[[key]] >= threshold
            })
            
            if(any(needsExplanation)) {
              keysNeedingExplanation <- names(state$selections)[needsExplanation]
              allExplanations <- all(keysNeedingExplanation %in% names(state$descriptions))
              return(allExplanations)
            }
          }
        }
        return(TRUE)
      }
      return(FALSE)
    })
    
    uiState <- reactive({
      if (is.null(period()) || period()=="") "none" else "active"
    })
    
    # progress UI
    output$progressSection <- renderUI({
      req(uiState()=="active")
      pct <- round(100 * imagesDone()/totalImages(), 1)
      tagList(
        div(class="d-flex justify-content-between mb-2",
            lapply(seq_along(imageSets), function(i){
              cls <- if (i< state$currentSetIndex) "text-success"
              else if (i==state$currentSetIndex) "text-primary fw-bold"
              else "text-muted"
              span(class=cls, imageSets[[i]]$title)
            })
        ),
        div(class="text-center mb-2", paste0(imagesDone()," of ", totalImages()," (",pct,"%)")),
        div(class="progress mb-3",
            div(class="progress-bar", role="progressbar",
                style=paste0("width:",pct,"%"),
                `aria-valuenow`=pct, `aria-valuemin`=0, `aria-valuemax`=100
            )
        )
      )
    })
    
    # Conditional submit button UI
    output$submitButtonUI <- renderUI({
      req(uiState() == "active")
      if(allComplete()) {
        div(
          class = "mt-4 text-center",
          actionButton(ns("done"), "Submit Milestones",
                       class = "btn-success btn-lg",
                       style = "width: 50%;")
        )
      } else {
        div(
          class = "mt-4 text-center text-muted",
          "Complete all milestone ratings to enable submission"
        )
      }
    })
    
    # main image + score buttons (FIXED: Updated for 1-9 scale and local images)
    output$mainContent <- renderUI({
      if (uiState()=="none") return(div("Please select a period to begin."))
      key <- selectionKey()
      sel <- state$selections[[key]]
      tagList(
        div(class="card",
            div(class="card-header",
                paste0(currentSet()$title," – ", currentSetName(),
                       " ", state$currentImageIndex," of ",length(currentSet()$images))
            ),
            div(style="position: relative;",
                imageOutput(ns("mainImage"), height="auto"),
                # buttons 1–9 (FIXED: Now shows 1-9 instead of blank buttons)
                div(style="position: relative; height:40px; width:1140px; margin-top:10px;",
                    lapply(1:9, function(i) {
                      left <- 100 + (i-1)*120
                      bg <- if (!is.null(sel) && sel==i) "#4CAF50" else "#f0f0f0"
                      clr <- if (!is.null(sel) && sel==i) "white" else "black"
                      div(style=paste0("position:absolute; left:",left,"px; top:0;"),
                          tags$button(id=ns(paste0("box_",i)),
                                      class="action-button",
                                      style=paste0("width:30px;height:30px;padding:0;
                                               background:",bg,";color:",clr,";
                                               border:1px solid #ccc;
                                               font-weight:bold;"),
                                      i  # FIXED: Show the number instead of HTML("&nbsp;")
                          )
                      )
                    })
                )
            )
        )
      )
    })
    
    # FIXED: Image loading - try multiple paths
    observe({
      req(uiState()=="active", currentImageFile())
      output$mainImage <- renderImage({
        image_file <- currentImageFile()
        
        # Try multiple possible image paths
        possible_paths <- c(
          # Local www folder (for standalone testing)
          file.path("www", "milestones", image_file),
          # Relative path
          file.path("milestones", image_file),
          # System file from imres package (fallback)
          system.file("www", image_file, package="imres"),
          # GitHub fallback
          paste0("https://raw.githubusercontent.com/fbuckhold3/imres.ccc.dashboard/main/www/milestones/", image_file)
        )
        
        # Find first existing path
        image_src <- NULL
        for (path in possible_paths) {
          if (file.exists(path) || grepl("^https://", path)) {
            image_src <- path
            break
          }
        }
        
        # Fallback to a placeholder if no image found
        if (is.null(image_src)) {
          # Create a simple placeholder
          image_src <- "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMTIwMCIgaGVpZ2h0PSI2MDAiIHZpZXdCb3g9IjAgMCAxMjAwIDYwMCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHJlY3Qgd2lkdGg9IjEyMDAiIGhlaWdodD0iNjAwIiBmaWxsPSIjZjBmMGYwIi8+Cjx0ZXh0IHg9IjYwMCIgeT0iMzAwIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LWZhbWlseT0iQXJpYWwiIGZvbnQtc2l6ZT0iMjQiIGZpbGw9IiM2NjYiPkltYWdlIE5vdCBGb3VuZDogPC90ZXh0Pgo8dGV4dCB4PSI2MDAiIHk9IjMzMCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1mYW1pbHk9IkFyaWFsIiBmb250LXNpemU9IjE4IiBmaWxsPSIjNjY2Ij5NSUxFU1RPTkVTPC90ZXh0Pgo8L3N2Zz4="
        }
        
        list(src = image_src,
             width = "1200px",
             height = "auto",
             alt = paste("Milestone:", image_file))
      }, deleteFile=FALSE)
    })
    
    # Handle explanation UI updates
    observe({
      req(uiState() == "active")
      key <- selectionKey()
      sel <- state$selections[[key]]  # Get current selection
      
      # Only proceed with explanation UI if we have a selection
      if (!is.null(sel)) {
        period_val <- period()
        needsExplanation <- !is.null(thresholds[[period_val]]) &&
          sel >= thresholds[[period_val]] &&
          period_val != "Interim Review"
        
        if (needsExplanation) {
          # Load existing explanation for this specific key if it exists
          existingExplanation <- if(key %in% names(state$descriptions)) state$descriptions[[key]] else NULL
          
          output$explanationUI <- renderUI({
            textAreaInput(ns("explanation"),
                          "This rating is a bit higher than expected for your level of training (which may be deserving). Please take a moment to justify this rating.",
                          value = existingExplanation,
                          rows = 3,
                          width = "100%")
          })
        } else {
          output$explanationUI <- renderUI(NULL)
        }
      } else {
        # No selection yet, so no explanation UI
        output$explanationUI <- renderUI(NULL)
      }
    })
    
    # navigation buttons
    output$navigationButtons <- renderUI({
      req(uiState()=="active")
      key <- selectionKey()
      sel <- state$selections[[key]]
      period_val <- period()

      # Determine if we need explanation
      needsExplanation <- !is.null(sel) &&
        !is.null(thresholds[[period_val]]) &&
        sel >= thresholds[[period_val]] &&
        period_val != "Interim Review"

      # Determine if Next should be disabled
      disableNext <- is.null(sel) ||
        (needsExplanation && (is.null(input$explanation) ||
                                trimws(input$explanation) == ""))

      # Check if we're at the final milestone (ics3)
      isAtFinalMilestone <- state$currentSetIndex == length(imageSets) &&
        state$currentImageIndex == length(currentSet()$images)

      div(class="card",
          div(class="card-body",
              if (isAtFinalMilestone) {
                # Only show Previous button at final milestone
                fluidRow(
                  column(12, actionButton(ns("prev"), "Previous", class="btn-primary", width="100%"))
                )
              } else {
                # Show both buttons for all other milestones
                fluidRow(
                  column(6, actionButton(ns("prev"), "Previous", class="btn-primary", width="100%")),
                  column(6, actionButton(ns("next"), "Next",
                                         class=if(disableNext) "btn-primary disabled" else "btn-primary",
                                         width="100%"))
                )
              }
          )
      )
    })
    
    # Score click handler
    observe({
      req(uiState()=="active")
      for (i in 1:9) {
        local({
          ii <- i
          observeEvent(input[[paste0("box_",ii)]], {
            key    <- selectionKey()
            period_val <- period()
            overTh <- !is.null(thresholds[[period_val]]) &&
              ii >= thresholds[[period_val]] &&
              period_val != "Interim Review"
            
            # Save the numeric score
            state$selections[[key]] <- ii
            
            # Update pending selection if explanation needed
            if (overTh) {
              state$pendingSelection <- list(key=key, value=ii)
            } else {
              state$pendingSelection <- list(key=NULL, value=NULL)
              state$descriptions[[key]] <- NULL  # Clear any existing description if no longer needed
            }
          }, ignoreInit=TRUE)
        })
      }
    })
    
    # Next button handler
    observeEvent(input[["next"]], {
      key <- selectionKey()
      
      if (is.null(state$selections[[key]])) {
        showNotification("Pick a score first", type="error")
        return()
      }
      
      # Check if explanation is needed
      period_val <- period()
      sel <- state$selections[[key]]
      needsExplanation <- !is.null(sel) &&
        !is.null(thresholds[[period_val]]) &&
        sel >= thresholds[[period_val]] &&
        period_val != "Interim Review"
      
      if (needsExplanation) {
        # Save current explanation if it exists
        if (!is.null(input$explanation) && nzchar(trimws(input$explanation))) {
          state$descriptions[[key]] <- input$explanation
        }
        
        # Check if explanation is required but missing
        if (is.null(state$descriptions[[key]]) ||
            trimws(state$descriptions[[key]]) == "") {
          showNotification("Please provide an explanation for this rating", type="error")
          return()
        }
      }
      
      # If we get here, we can proceed with navigation
      if (state$currentImageIndex < length(currentSet()$images)) {
        state$currentImageIndex <- state$currentImageIndex + 1
      } else if (state$currentSetIndex < length(imageSets)) {
        state$currentSetIndex <- state$currentSetIndex + 1
        state$currentImageIndex <- 1
      }
      
      # Reset pending selection after successful navigation
      state$pendingSelection <- list(key = NULL, value = NULL)
    })
    
    # Previous button handler
    observeEvent(input$prev, {
      key <- selectionKey()
      
      # Save current explanation if needed
      if (!is.null(input$explanation) && nzchar(trimws(input$explanation))) {
        state$descriptions[[key]] <- input$explanation
      }
      
      # Navigate
      if (state$currentImageIndex > 1) {
        state$currentImageIndex <- state$currentImageIndex - 1
      } else if (state$currentSetIndex > 1) {
        state$currentSetIndex <- state$currentSetIndex - 1
        state$currentImageIndex <- length(imageSets[[state$currentSetIndex]]$images)
      }
      
      # Reset pending selection after navigation
      state$pendingSelection <- list(key = NULL, value = NULL)
    })
    
    # Handle the done button
    observeEvent(input$done, {
      # Just trigger the event without any subscript calls that might cause issues
    })
    
    # final return
    list(
      done   = reactive(input$done),
      scores = reactive(state$selections),
      desc   = reactive(state$descriptions)
    )
  })
}

#' Milestone Dashboard Module UI
#'
#' Creates a self-contained milestone dashboard with spider plot and progression chart
#'
#' @param id Module namespace ID
#' @param milestone_type Type of milestones ("program", "self", "acgme")
#' @param height Height of the dashboard (default: "600px")
#' @return Shiny UI tagList
#' @export
milestone_dashboard_ui <- function(id, milestone_type = "program", height = "600px") {
  
  ns <- NS(id)
  
  # Module title based on type
  module_title <- switch(milestone_type,
                         "program" = "Program Assessment",
                         "self" = "Self-Assessment", 
                         "acgme" = "ACGME Assessment",
                         "Milestone Assessment")
  
  tagList(
    div(class = "milestone-dashboard-container",
        style = paste0("height: ", height, "; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin: 5px;"),
        
        # Module header
        div(class = "dashboard-header", 
            style = "text-align: center; margin-bottom: 15px; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;",
            h4(module_title, style = "color: #2c3e50; margin: 0; font-weight: bold;")
        ),
        
        # Loading indicator
        conditionalPanel(
          condition = paste0("$('html').hasClass('shiny-busy') && $('#", ns("loading"), "').is(':visible')"),
          div(id = ns("loading"), 
              style = "text-align: center; padding: 20px;",
              icon("spinner", class = "fa-spin"), " Loading milestone data..."
          )
        ),
        
        # Main content
        div(id = ns("main_content"),
            
            # Spider plot section (top half)
            div(class = "spider-section",
                style = "height: 60%; margin-bottom: 15px;",  # Increased from 45% to 60%
                plotlyOutput(ns("spider_plot"), height = "100%")
            ),
            
            # Controls section
            div(class = "controls-section",
                style = "height: 8%; margin-bottom: 10px; padding: 5px;",
                fluidRow(
                  column(12,
                         selectInput(
                           ns("selected_milestone"),
                           label = "Select Milestone for Progression View:",
                           choices = list("Loading milestones..." = ""),  # <- Provide initial choice
                           width = "100%"
                         )
                  )
                )
            ),
            
            # Progression chart section (bottom half)
            div(class = "progression-section",
                style = "height: 35%;",  # Reduced from 42% to 35%
                plotlyOutput(ns("progression_plot"), height = "100%")
            )
        ),
        
        # Error/no data message
        div(id = ns("error_message"),
            style = "display: none; text-align: center; padding: 50px; color: #e74c3c;",
            icon("exclamation-triangle"), 
            p("No milestone data available for the selected criteria.")
        )
    )
  )
}

#' Milestone Dashboard Module Server
#'
#' Server logic for the milestone dashboard module
#'
#' @param id Module namespace ID  
#' @param milestone_results Reactive containing processed milestone results
#' @param record_id Reactive containing selected resident record ID
#' @param period Reactive containing selected period
#' @param milestone_type Type of milestones ("program", "self", "acgme")
#' @param resident_data Reactive containing resident lookup data
#' @return Server function
#' @export
milestone_dashboard_server <- function(id, milestone_results, record_id, period, 
                                       milestone_type = "program", resident_data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive to get milestone data for this type
    milestone_data_obj <- reactive({
      req(milestone_results())
      
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      
      get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = if (milestone_type == "acgme") "program" else milestone_type,
        milestone_system = milestone_system
      )
    })
    
    # Reactive to get available milestones for the selector
    available_milestones <- reactive({
      req(milestone_data_obj())
      
      milestone_data <- milestone_data_obj()$data
      if (is.null(milestone_data) || nrow(milestone_data) == 0) return(NULL)
      
      # Get milestone columns for this type
      milestone_cols <- get_milestone_columns_simple(milestone_data, 
                                                     if (milestone_type == "acgme") "acgme" else milestone_type)
      
      if (length(milestone_cols) == 0) return(NULL)
      
      # Create named vector for selectInput (label = value)
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      milestone_labels <- sapply(milestone_cols, function(x) get_milestone_label(x, milestone_system))
      
      choices <- setNames(milestone_cols, milestone_labels)
      return(choices)
    })
    
    # Update milestone selector choices
    observe({
      choices <- available_milestones()
      
      if (is.null(choices)) {
        updateSelectInput(session, "selected_milestone", 
                          choices = list("No milestones available" = ""))
      } else {
        updateSelectInput(session, "selected_milestone", choices = choices)
      }
    })
    
    # Show/hide content based on data availability
    observe({
      if (is.null(milestone_data_obj()) || is.null(available_milestones())) {
        shinyjs::hide("main_content")
        shinyjs::show("error_message")
      } else {
        shinyjs::show("main_content")
        shinyjs::hide("error_message")
      }
    })
    
    # Spider plot
    output$spider_plot <- renderPlotly({
      req(milestone_data_obj(), record_id(), period())
      
      milestone_data <- milestone_data_obj()$data
      median_data <- milestone_data_obj()$medians
      
      if (is.null(milestone_data) || is.null(median_data)) {
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "No milestone data available", 
                   x = 0.5, y = 0.5, showarrow = FALSE
                 ))
      }
      
      create_enhanced_milestone_spider_plot(
        milestone_data = milestone_data,
        median_data = median_data,
        resident_id = record_id(),
        period_text = period(),
        milestone_type = milestone_type,
        resident_data = resident_data()
      )
    })
    
    # Progression plot
    output$progression_plot <- renderPlotly({
      req(milestone_data_obj(), record_id(), input$selected_milestone)
      
      # Skip if no milestone selected or invalid selection
      if (is.null(input$selected_milestone) || input$selected_milestone == "") {
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "Select a milestone above to view progression", 
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "#666")
                 ))
      }
      
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      
      create_enhanced_milestone_progression(
        milestone_results = milestone_results(),
        resident_id = record_id(),
        milestone_col = input$selected_milestone,
        milestone_type = if (milestone_type == "acgme") "program" else milestone_type,
        milestone_system = milestone_system,
        resident_data = resident_data(),
        show_national = TRUE
      )
    })
    
    # Return reactive values for parent app if needed
return(list(
  selected_milestone = reactive(input$selected_milestone),
  data_available = reactive(!is.null(milestone_data_obj())),
  current_data = reactive({
    # Return the milestone results the user just entered
    milestone_results()
  }),
  milestone_data_obj = reactive({
    # Return the full milestone data object
    milestone_data_obj()
  })
))
  })
}

#' Helper Function to Create Multiple Dashboard Columns
#'
#' Creates a layout with multiple milestone dashboard modules side by side
#'
#' @param milestone_results Reactive containing processed milestone results
#' @param record_id Reactive containing selected resident record ID  
#' @param period Reactive containing selected period
#' @param resident_data Reactive containing resident lookup data
#' @param milestone_types Vector of milestone types to display (e.g., c("program", "self"))
#' @param column_width Bootstrap column width (e.g., 4 for 3 columns, 6 for 2 columns)
#' @return Shiny UI fluidRow
#' @export
create_milestone_dashboard_layout <- function(milestone_results, record_id, period, resident_data,
                                              milestone_types = c("program", "self"), 
                                              column_width = 6) {
  
  # Create columns for each milestone type
  columns <- lapply(seq_along(milestone_types), function(i) {
    milestone_type <- milestone_types[i]
    module_id <- paste0("milestone_dash_", i)
    
    column(column_width,
           milestone_dashboard_ui(module_id, milestone_type = milestone_type),
           # Call server function in the parent server
           # milestone_dashboard_server(module_id, milestone_results, record_id, period, milestone_type, resident_data)
    )
  })
  
  do.call(fluidRow, columns)
}

#' Example Usage in App Server
#' 
#' @examples
#' # In your app server function:
#' 
#' # Create multiple dashboard modules
#' milestone_dashboard_server("milestone_dash_1", milestone_results, record_id, period, "program", resident_data)
#' milestone_dashboard_server("milestone_dash_2", milestone_results, record_id, period, "self", resident_data)
#' 
#' # In your app UI:
#' fluidRow(
#'   column(6, milestone_dashboard_ui("milestone_dash_1", milestone_type = "program")),
#'   column(6, milestone_dashboard_ui("milestone_dash_2", milestone_type = "self"))
#' )
#' 
#' # Or use the helper function:
#' output$milestone_layout <- renderUI({
#'   create_milestone_dashboard_layout(
#'     milestone_results = reactive(milestone_results),
#'     record_id = reactive(input$resident_select),
#'     period = reactive(input$period_select), 
#'     resident_data = reactive(complete_data$residents),
#'     milestone_types = c("program", "self"),
#'     column_width = 6
#'   )
#' })
#' @export
example_milestone_dashboard_usage <- function() {
  # This is just documentation - see examples above
}

#' Add Required CSS for Milestone Dashboard
#'
#' Adds CSS styling for the milestone dashboard modules
#'
#' @return HTML tags with CSS
#' @export
milestone_dashboard_css <- function() {
  tags$head(
    tags$style(HTML("
      .milestone-dashboard-container {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
      }
      
      .milestone-dashboard-container:hover {
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        transform: translateY(-2px);
      }
      
      .dashboard-header h4 {
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      .spider-section, .progression-section {
        background: white;
        border-radius: 6px;
        box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);
      }
      
      .controls-section {
        padding: 10px;
        background: rgba(255,255,255,0.7);
        border-radius: 6px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .milestone-dashboard-container {
          height: auto !important;
          min-height: 500px;
        }
      }
    "))
  )
}
