# ============================================================================
# UPDATED mod_assessment_viz_wrapper.R for gmed package
# Copy this to: /Users/fredbuckhold/Documents/GitHub/gmed/R/mod_assessment_viz_wrapper.R
# ============================================================================

#' Assessment Visualization Wrapper UI
#'
#' Wrapper for all assessment visualizations including plus/delta, charts,
#' CC completion, detailed breakdown, and questions
#'
#' @param id Module namespace
#' @param title Section title (default: "Assessment Review")
#' @param include_questions Include questions/conference module (default: TRUE)
#' @param include_cc_completion Include CC completion status (default: TRUE)
#' @export
mod_assessment_viz_wrapper_ui <- function(id,
                                          title = "Assessment Review",
                                          include_questions = TRUE,
                                          include_cc_completion = TRUE) {
  ns <- NS(id)

  tagList(
    # Plus/Delta feedback table
    mod_plus_delta_table_ui(ns("plus_delta"), title = "Recent Feedback"),

    # Assessment progress charts
    assessment_viz_ui(ns("charts"), title = "Assessment Progress"),

    # CC Completion Status (NEW)
    if (include_cc_completion) {
      mod_cc_completion_ui(ns("cc_completion"))
    },

    # Detailed assessment breakdown
    mod_assessment_detail_viz_ui(ns("details")),

    # Questions/conference attendance
    if (include_questions) {
      mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation")
    }
  )
}

#' Assessment Visualization Wrapper Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive containing full RDM data (must include assessment and questions)
#' @param record_id Reactive returning resident record_id
#' @param data_dict Data dictionary (reactive or static)
#' @param include_questions Include questions module (default: TRUE)
#' @param include_cc_completion Include CC completion (default: TRUE)
#' @param resident_name Optional reactive returning resident name
#' @param resident_data Optional reactive returning resident info (for CC completion)
#' @param rdm_data_raw Optional raw assessment data (for plus_delta if needed)
#' @export
mod_assessment_viz_wrapper_server <- function(id,
                                              rdm_data,
                                              record_id,
                                              data_dict,
                                              include_questions = TRUE,
                                              include_cc_completion = TRUE,
                                              resident_name = NULL,
                                              resident_data = NULL,
                                              rdm_data_raw = NULL) {
  moduleServer(id, function(input, output, session) {

    # Resident name fallback
    res_name <- if (!is.null(resident_name)) {
      resident_name
    } else {
      reactive({
        paste("Resident", record_id())
      })
    }

    # Choose data source for plus_delta
    plus_delta_input <- if (!is.null(rdm_data_raw)) {
      rdm_data_raw
    } else {
      rdm_data
    }

    # Plus/Delta table
    mod_plus_delta_table_server("plus_delta",
                               rdm_data = plus_delta_input,
                               record_id = record_id)

    # Assessment charts
    assessment_viz_server("charts",
                         data = rdm_data,
                         record_id = record_id,
                         resident_name = res_name)

    # CC Completion Status (NEW)
    if (include_cc_completion) {
      mod_cc_completion_server("cc_completion",
                              rdm_data = rdm_data,
                              record_id = record_id,
                              resident_data = resident_data)
    }

    # Detailed assessment breakdown
    mod_assessment_detail_viz_server("details",
                                    rdm_data = rdm_data,
                                    record_id = record_id,
                                    data_dict = data_dict)

    # Questions visualization
    if (include_questions) {
      mod_questions_viz_server("questions",
                              rdm_data = rdm_data,
                              record_id = record_id,
                              data_dict = data_dict)
    }
  })
}
