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
          "Track your continuity clinic assessments. Four per academic year are expected.",
          tags$small(class = "d-block mt-1", style = "color:#6c757d;",
                     "Click a completed quarter to see details.")),

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
        .quarter-card-clickable {
          cursor: pointer;
          transition: box-shadow 0.2s, transform 0.15s;
        }
        .quarter-card-clickable:hover {
          box-shadow: 0 4px 12px rgba(0,102,161,0.18);
          transform: translateY(-2px);
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
    ns <- session$ns

    # Track which quarter card was last clicked
    selected_quarter <- reactiveVal(NULL)

    # Calculate completion status
    completion_data <- reactive({
      req(rdm_data(), record_id())

      # Filter for this resident's CC assessments
      cc_assessments <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(trimws(redcap_repeat_instrument)) == "assessment",
          !is.na(ass_cc_quart),
          ass_cc_quart != ""
        ) %>%
        dplyr::select(record_id, redcap_repeat_instance,
                     ass_cc_quart, ass_date, ass_faculty, ass_level)

      # ── Number of years to show based on PGY level ──────────────────────────
      current_year  <- as.integer(format(Sys.Date(), "%Y"))
      current_month <- as.integer(format(Sys.Date(), "%m"))
      current_acy   <- if (current_month >= 7L) current_year else current_year - 1L

      n_years <- 2L  # sensible default
      if (!is.null(resident_data) && !is.null(resident_data())) {
        lvl <- resident_data()$Level
        if (length(lvl) > 0 && !is.na(lvl[1])) {
          n_years <- switch(as.character(lvl[1]),
            "Intern"     = 1L,
            "PGY1"       = 1L,
            "PGY2"       = 2L,
            "PGY3"       = 3L,
            "Graduating" = 3L,
            2L   # default
          )
        }
      }
      academic_years <- seq.int(current_acy - n_years + 1L, current_acy)

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

        # Create quarter cards — completed ones expand inline on click
        quarter_cards <- lapply(year_info$quarters, function(quarter) {
          card_class <- if (quarter$completed) "quarter-card completed" else "quarter-card pending"
          icon_html  <- if (quarter$completed)
            icon("check-circle", class = "quarter-icon text-success")
          else
            icon("circle", class = "quarter-icon text-warning")

          collapse_id <- paste0("qd-", year_info$academic_year, "-", quarter$quarter)

          level_label <- if (quarter$completed) {
            switch(as.character(quarter$level),
              "1" = "Intern", "Intern" = "Intern",
              "2" = "PGY2",   "PGY2"  = "PGY2",
              "3" = "PGY3",   "PGY3"  = "PGY3",
              as.character(quarter$level))
          } else NULL

          if (quarter$completed) {
            div(
              class = paste(card_class, "quarter-card-clickable p-0"),
              style = "overflow:hidden;",
              # ── Header row (always visible, toggles detail) ──
              tags$button(
                class            = "btn w-100 text-start p-3",
                style            = "background:none; border:none;",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = paste0("#", collapse_id),
                `aria-expanded`  = "false",
                div(class = "quarter-header mb-1", icon_html,
                    paste("Quarter", quarter$quarter)),
                div(class = "quarter-details",
                    paste0(quarter$faculty, " \u00b7 ",
                           format(as.Date(quarter$date), "%b %d, %Y"))),
                tags$small(style = "color:#0066a1; font-size:0.72rem;",
                           "View details \u203a")
              ),
              # ── Collapsible detail ───────────────────────────
              div(
                id    = collapse_id,
                class = "collapse",
                div(
                  style = "background:#f0f6fb; border-top:1px solid #c8dff0; padding:10px 14px;",
                  div(class = "row g-2",
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block", "DATE"),
                      tags$span(style = "font-size:0.85rem; font-weight:600;",
                                format(as.Date(quarter$date), "%B %d, %Y"))
                    ),
                    div(class = "col-6",
                      tags$small(class = "text-muted d-block", "FACULTY"),
                      tags$span(style = "font-size:0.85rem; font-weight:600;",
                                quarter$faculty)
                    ),
                    div(class = "col-6 mt-2",
                      tags$small(class = "text-muted d-block", "LEVEL"),
                      tags$span(style = "font-size:0.85rem; font-weight:600;",
                                level_label)
                    )
                  )
                )
              )
            )
          } else {
            div(
              class = card_class,
              div(class = "quarter-header", icon_html,
                  paste("Quarter", quarter$quarter)),
              div(class = "quarter-details", "Not yet completed")
            )
          }
        })

        # Year accordion item — most recent year open by default
        div(
          class = "accordion-item",
          div(
            class = "accordion-header",
            tags$button(
              class = if (i == length(years_data)) "accordion-button" else "accordion-button collapsed",
              type = "button",
              `data-bs-toggle` = "collapse",
              `data-bs-target` = paste0("#collapse-year-", i),
              `aria-expanded`  = if (i == length(years_data)) "true" else "false",
              `aria-controls`  = paste0("collapse-year-", i),
              paste0("Academic Year ", year_info$year_label,
                     " (", completed_count, " of 4 completed)")
            )
          ),
          div(
            id    = paste0("collapse-year-", i),
            class = if (i == length(years_data))
              "accordion-collapse collapse show" else "accordion-collapse collapse",
            `data-bs-parent` = "#ccAccordion",
            div(class = "accordion-body",
                div(style = "display:grid; grid-template-columns: repeat(4,1fr); gap:12px;",
                    quarter_cards))
          )
        )
      })

      div(class = "accordion", id = "ccAccordion", accordion_items)
    })

    # Detail is now handled inline via Bootstrap collapse — no server output needed

  })
}
