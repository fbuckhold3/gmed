#' Assessment Visualization Wrapper Module UI
#'
#' Combines plus_delta table, assessment charts, detail viz, and questions into single module
#'
#' @param id Module namespace ID
#' @param title Title for the visualization section
#' @param include_questions Logical, include questions module (default TRUE)
#' @export
mod_assessment_viz_wrapper_ui <- function(id, title = "Assessment Review", include_questions = TRUE) {
  ns <- NS(id)
  
  tagList(
    # Plus/Delta feedback table
    mod_plus_delta_table_ui(ns("plus_delta"), title = "Recent Feedback"),
    
    # Assessment charts and metrics
    assessment_viz_ui(ns("charts"), title = "Assessment Progress"),
    
    # Detailed assessment breakdown
    mod_assessment_detail_viz_ui(ns("details")),
    
    # Questions/conference attendance
    if (include_questions) {
      mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation")
    }
  )
}

#' Assessment Visualization Wrapper Module Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive returning full RDM data (must include assessment and questions forms)
#' @param record_id Reactive returning current resident record_id
#' @param data_dict Reactive or static data dictionary
#' @param include_questions Logical, include questions module (default TRUE)
#' @param resident_name Reactive returning resident name (optional)
#' @param rdm_data_raw Reactive returning raw assessment data for plus/delta (optional)
#' @export
mod_assessment_viz_wrapper_server <- function(id, rdm_data, record_id, data_dict, 
                                              include_questions = TRUE, 
                                              resident_name = NULL,
                                              rdm_data_raw = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Use provided resident_name or create a fallback
    res_name <- if (!is.null(resident_name)) {
      resident_name
    } else {
      reactive({
        paste("Resident", record_id())
      })
    }
    
    # Plus/Delta table - use raw data if provided, otherwise use combined
    plus_delta_input <- if (!is.null(rdm_data_raw)) {
      rdm_data_raw
    } else {
      rdm_data
    }
    
    # Plus/Delta table
    mod_plus_delta_table_server(
      "plus_delta",
      rdm_data = plus_delta_input,
      record_id = record_id
    )
    
    # Assessment visualizations
    assessment_viz_server(
      "charts",
      data = rdm_data,
      record_id = record_id,
      resident_name = res_name
    )
    
    # Detailed assessment breakdown
    mod_assessment_detail_viz_server(
      "details",
      rdm_data = rdm_data,
      record_id = record_id,
      data_dict = data_dict
    )
    
    # Questions module
    if (include_questions) {
      mod_questions_viz_server(
        "questions",
        rdm_data = rdm_data,
        record_id = record_id,
        data_dict = data_dict
      )
    }
  })
}