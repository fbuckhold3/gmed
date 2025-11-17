# ============================================================================
# CONTINUITY CLINIC COMPLETION STATUS MODULE
# For gmed package - reusable across apps
# ============================================================================

#' CC Completion Status UI
#'
#' Display continuity clinic assessment completion status
#' @param id Module namespace
#' @export
mod_cc_completion_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "cc-completion-container",

    # Section header
    div(
      class = "card mb-4",
      div(
        class = "card-header bg-info text-white",
        h4(class = "mb-0",
           icon("calendar-check", class = "me-2"),
           "Continuity Clinic Assessment Completion")
      ),
      div(
        class = "card-body",
        p(class = "text-muted mb-3",
          "Track your continuity clinic assessments. Four assessments are expected per academic year."),

        # Year tabs or accordion
        uiOutput(ns("completion_display"))
      )
    ),

    # CSS styling
    tags$head(
      tags$style(HTML("
        .quarter-card {
          border: 2px solid #dee2e6;
          border-radius: 8px;
          padding: 1rem;
          margin-bottom: 1rem;
          transition: all 0.3s ease;
        }
        .quarter-card.completed {
          background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%);
          border-color: #28a745;
        }
        .quarter-card.pending {
          background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
          border-color: #ffc107;
        }
        .quarter-header {
          display: flex;
          align-items: center;
          font-weight: 600;
          font-size: 1.1rem;
          margin-bottom: 0.5rem;
        }
        .quarter-icon {
          font-size: 1.5rem;
          margin-right: 0.75rem;
        }
        .quarter-details {
          font-size: 0.9rem;
          color: #6c757d;
        }
      "))
    )
  )
}

#' CC Completion Status Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive containing assessment data with ass_cc_quart field
#' @param record_id Reactive containing resident record_id
#' @param resident_data Reactive containing resident info (for grad_yr, type)
#' @export
mod_cc_completion_server <- function(id, rdm_data, record_id, resident_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Calculate completion status
    completion_data <- reactive({
      req(rdm_data(), record_id())

      # Filter for this resident's CC assessments
      cc_assessments <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          redcap_repeat_instrument == "assessment",
          !is.na(ass_cc_quart),
          ass_cc_quart != ""
        ) %>%
        dplyr::select(record_id, redcap_repeat_instance,
                     ass_cc_quart, ass_date, ass_faculty, ass_level)

      # Determine academic years to show
      if (!is.null(resident_data) && !is.null(resident_data())) {
        res_info <- resident_data()

        # Calculate which years based on grad year and type
        # For now, show last 3 academic years
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        current_month <- as.numeric(format(Sys.Date(), "%m"))

        if (current_month >= 7) {
          current_academic_year <- current_year
        } else {
          current_academic_year <- current_year - 1
        }

        academic_years <- c(
          current_academic_year - 2,
          current_academic_year - 1,
          current_academic_year
        )
      } else {
        # Default: show last 3 years
        current_year <- as.numeric(format(Sys.Date(), "%Y"))
        current_month <- as.numeric(format(Sys.Date(), "%m"))

        if (current_month >= 7) {
          current_academic_year <- current_year
        } else {
          current_academic_year <- current_year - 1
        }

        academic_years <- c(
          current_academic_year - 2,
          current_academic_year - 1,
          current_academic_year
        )
      }

      # Build completion structure for each year
      result <- lapply(academic_years, function(year) {
        year_start <- as.Date(paste0(year, "-07-01"))
        year_end <- as.Date(paste0(year + 1, "-06-30"))

        # Filter assessments for this academic year
        year_assessments <- cc_assessments %>%
          dplyr::filter(
            !is.na(ass_date),
            ass_date >= year_start,
            ass_date <= year_end
          )

        # Check each quarter
        quarters <- lapply(1:4, function(q) {
          quarter_data <- year_assessments %>%
            dplyr::filter(ass_cc_quart == as.character(q)) %>%
            dplyr::slice(1)  # Take most recent if multiple

          if (nrow(quarter_data) > 0) {
            list(
              quarter = q,
              completed = TRUE,
              faculty = quarter_data$ass_faculty[1],
              date = quarter_data$ass_date[1],
              level = quarter_data$ass_level[1]
            )
          } else {
            list(
              quarter = q,
              completed = FALSE,
              faculty = NA,
              date = NA,
              level = NA
            )
          }
        })

        list(
          academic_year = year,
          year_label = paste0(year, "-", year + 1),
          quarters = quarters
        )
      })

      return(result)
    })

    # Render completion display
    output$completion_display <- renderUI({
      req(completion_data())

      years_data <- completion_data()

      # Create accordion for each year
      accordion_items <- lapply(seq_along(years_data), function(i) {
        year_info <- years_data[[i]]

        # Count completed quarters
        completed_count <- sum(sapply(year_info$quarters, function(q) q$completed))

        # Create quarter cards
        quarter_cards <- lapply(year_info$quarters, function(quarter) {
          card_class <- if (quarter$completed) "quarter-card completed" else "quarter-card pending"
          icon_html <- if (quarter$completed) {
            icon("check-circle", class = "quarter-icon text-success")
          } else {
            icon("circle", class = "quarter-icon text-warning")
          }

          status_text <- if (quarter$completed) {
            paste0("Completed by ", quarter$faculty, " on ",
                  format(as.Date(quarter$date), "%b %d, %Y"))
          } else {
            "Not yet completed"
          }

          div(
            class = card_class,
            div(
              class = "quarter-header",
              icon_html,
              paste("Quarter", quarter$quarter)
            ),
            div(
              class = "quarter-details",
              status_text
            )
          )
        })

        # Year accordion item
        div(
          class = "accordion-item",
          div(
            class = "accordion-header",
            tags$button(
              class = if (i == length(years_data)) "accordion-button" else "accordion-button collapsed",
              type = "button",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = paste0("#collapse-year-", i),
              `aria-expanded` = if (i == length(years_data)) "true" else "false",
              `aria-controls` = paste0("collapse-year-", i),
              paste0("Academic Year ", year_info$year_label, " (",
                    completed_count, " of 4 completed)")
            )
          ),
          div(
            id = paste0("collapse-year-", i),
            class = if (i == length(years_data)) "accordion-collapse collapse show" else "accordion-collapse collapse",
            `data-bs-parent` = "#ccAccordion",
            div(
              class = "accordion-body",
              quarter_cards
            )
          )
        )
      })

      div(
        class = "accordion",
        id = "ccAccordion",
        accordion_items
      )
    })
  })
}
