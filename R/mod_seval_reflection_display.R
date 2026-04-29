# ============================================================================
# SELF-EVAL REFLECTION DISPLAY MODULE
# R/mod_seval_reflection_display.R
#
# Read-only display of the resident's self-reflection block from the s_eval
# form for one period. Mirrors the field set rendered in
# imslu.ind.dash mod_self_eval.R "reflection" section so coach.dash and other
# consumers can show the same data without re-implementing the layout.
#
# Fields rendered (all read-only, with optional previous-period reference):
#   * s_e_plus        — what went well
#   * s_e_delta       — what would you do differently
#   * s_e_well        — mental / emotional / physical wellness
#   * s_e_discussion  — topics for next mentor meeting (gated; hidden if empty)
#   * s_e_prog_assist — things the program can do to help (gated; hidden if empty)
# ============================================================================

#' Self-Eval Reflection Display UI
#'
#' Read-only display of the resident's self-reflection block (`s_e_plus`,
#' `s_e_delta`, `s_e_well`, `s_e_discussion`, `s_e_prog_assist`) for a single
#' self-evaluation period, with an optional previous-period reference panel
#' above each field.
#'
#' Caller-side coach-entry textareas (or any wrapping chrome) should be added
#' around `mod_seval_reflection_display_ui()`, not inside it.
#'
#' @param id Character. Module namespace id.
#' @param title Character or NULL. Optional header rendered above the panel.
#' @param fields Character vector of field keys to render, in display order.
#'   Defaults to all five. Allowed values: `"plus"`, `"delta"`, `"well"`,
#'   `"discussion"`, `"prog_assist"`.
#'
#' @return A `shiny::tagList` with one `uiOutput()` per requested field.
#' @family seval_display
#' @seealso [mod_seval_reflection_display_server()]
#' @export
mod_seval_reflection_display_ui <- function(id,
                                            title  = "Self-Reflection",
                                            fields = c("plus", "delta", "well",
                                                       "discussion", "prog_assist")) {
  ns <- shiny::NS(id)

  meta <- .seval_reflection_meta()
  fields <- intersect(fields, names(meta))

  blocks <- lapply(fields, function(f) {
    m <- meta[[f]]
    shiny::div(
      class = "mb-3",
      shiny::tags$label(
        class = "form-label fw-semibold",
        shiny::tags$i(class = paste0("bi ", m$icon, " me-1"), style = m$icon_style),
        m$label
      ),
      shiny::uiOutput(ns(f))
    )
  })

  shiny::tagList(
    if (!is.null(title)) shiny::tags$h5(title, class = "mb-2") else NULL,
    shiny::div(class = "seval-reflection-display", blocks)
  )
}

#' Self-Eval Reflection Display Server
#'
#' Server-side renderer for [mod_seval_reflection_display_ui()]. Pulls the
#' relevant `s_e_*` columns from the supplied current-period row and, if
#' provided, surfaces the previous-period text in a styled reference panel
#' above each block.
#'
#' Mirrors ind.dash's gating for optional fields: `s_e_discussion` and
#' `s_e_prog_assist` are completely suppressed when both periods are empty
#' (rather than showing a placeholder). `s_e_plus` / `s_e_delta` / `s_e_well`
#' always render — they show an italic placeholder when the resident hasn't
#' answered yet.
#'
#' @param id Character. Module namespace id (must match the UI call).
#' @param current_row A reactive returning a 1-row data frame (or NULL/empty)
#'   containing any of `s_e_plus`, `s_e_delta`, `s_e_well`, `s_e_discussion`,
#'   `s_e_prog_assist`. Typically the resident's `s_eval` row for the period
#'   being reviewed.
#' @param previous_row Optional reactive returning a 1-row data frame for
#'   the previous period. Pass NULL to suppress the prior-period reference panel.
#' @param fields Character vector matching the UI call (so the server only
#'   wires up the outputs the UI actually rendered).
#'
#' @return Invisibly, NULL.
#' @family seval_display
#' @seealso [mod_seval_reflection_display_ui()]
#' @export
mod_seval_reflection_display_server <- function(id,
                                                current_row,
                                                previous_row = NULL,
                                                fields = c("plus", "delta", "well",
                                                           "discussion", "prog_assist")) {
  shiny::moduleServer(id, function(input, output, session) {

    meta <- .seval_reflection_meta()
    fields <- intersect(fields, names(meta))

    .pull_row <- function(r) {
      if (is.null(r)) return(NULL)
      v <- if (is.function(r)) r() else r
      if (is.null(v) || !is.data.frame(v) || nrow(v) == 0) NULL else v
    }

    .field <- function(row, fld) {
      if (is.null(row) || !fld %in% names(row)) return("")
      v <- row[[fld]][1]
      if (is.null(v) || is.na(v)) "" else as.character(v)
    }

    .ref_box <- function(value, label) {
      if (!nzchar(trimws(value))) return(NULL)
      shiny::div(
        class = "mb-2 py-2 px-3",
        style = paste(
          "background:#f6f7fb; border-left:3px solid #adb5bd;",
          "border-radius:4px; font-size:0.82rem; color:#495057;"
        ),
        shiny::tags$span(
          style = paste(
            "font-size:0.7rem; text-transform:uppercase; letter-spacing:.06em;",
            "font-weight:700; color:#6c757d; display:block; margin-bottom:4px;"
          ),
          shiny::tags$i(class = "bi bi-clock-history me-1"), label
        ),
        shiny::tags$div(style = "white-space:pre-wrap;", value)
      )
    }

    .current_box <- function(value, empty_msg, suppress_when_empty = FALSE) {
      if (!nzchar(trimws(value))) {
        if (suppress_when_empty) return(NULL)
        return(shiny::tags$p(
          class = "text-muted fst-italic mb-0",
          empty_msg
        ))
      }
      shiny::div(
        style = paste(
          "padding:10px 12px; background:#ffffff;",
          "border:1px solid #dee2e6; border-radius:4px;",
          "white-space:pre-wrap;"
        ),
        value
      )
    }

    for (f in fields) local({
      key <- f
      m   <- meta[[key]]
      output[[key]] <- shiny::renderUI({
        cur  <- .pull_row(current_row)
        prev <- .pull_row(previous_row)

        cur_val  <- .field(cur,  m$field)
        prev_val <- .field(prev, m$field)

        # Optional fields collapse entirely when both periods are empty.
        if (m$optional && !nzchar(trimws(cur_val)) && !nzchar(trimws(prev_val))) {
          return(shiny::tags$p(
            class = "text-muted fst-italic small mb-0",
            m$absent
          ))
        }

        shiny::tagList(
          .ref_box(prev_val, m$prev_label),
          .current_box(cur_val, m$empty_msg, suppress_when_empty = m$optional)
        )
      })
    })

    invisible(NULL)
  })
}

# Internal: per-field display metadata.
.seval_reflection_meta <- function() {
  list(
    plus = list(
      field      = "s_e_plus",
      label      = "What went well",
      icon       = "bi-plus-circle",
      icon_style = "color:#198754;",
      prev_label = "Previous period \u2014 what went well",
      empty_msg  = "Resident has not yet entered a reflection on what went well.",
      absent     = "",
      optional   = FALSE
    ),
    delta = list(
      field      = "s_e_delta",
      label      = "What would you do differently",
      icon       = "bi-arrow-repeat",
      icon_style = "color:#fd7e14;",
      prev_label = "Previous period \u2014 areas to develop",
      empty_msg  = "Resident has not yet entered a reflection on what to do differently.",
      absent     = "",
      optional   = FALSE
    ),
    well = list(
      field      = "s_e_well",
      label      = "How are you doing \u2014 mentally, emotionally, physically",
      icon       = "bi-heart-pulse",
      icon_style = "color:#d63384;",
      prev_label = "Previous period \u2014 wellness",
      empty_msg  = "Resident did not share a wellness reflection this period.",
      absent     = "",
      optional   = FALSE
    ),
    discussion = list(
      field      = "s_e_discussion",
      label      = "Topics resident wants to discuss with their mentor",
      icon       = "bi-chat-square-text",
      icon_style = "color:#0d6efd;",
      prev_label = "Previous period \u2014 mentor agenda",
      empty_msg  = "",
      absent     = "Resident did not flag specific topics to discuss.",
      optional   = TRUE
    ),
    prog_assist = list(
      field      = "s_e_prog_assist",
      label      = "Things the program can do to help",
      icon       = "bi-life-preserver",
      icon_style = "color:#6f42c1;",
      prev_label = "Previous period \u2014 program assistance requested",
      empty_msg  = "",
      absent     = "Resident did not request program assistance.",
      optional   = TRUE
    )
  )
}
