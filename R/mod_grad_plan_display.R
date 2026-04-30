# ============================================================================
# GRADUATION PLAN DISPLAY MODULE
# R/mod_grad_plan_display.R
#
# Read-only display of the resident's Period 6 graduation submission. Mirrors
# the alumni / graduation block rendered in imslu.ind.dash mod_self_eval.R so
# coach.dash (and other consumers) can show the same data without
# re-implementing the layout.
#
# Sources:
#   * s_eval form (period 6 row): s_e_grad_next  — career path
#   * resident_data row (non-repeating):
#       chief, grad_email, grad_phone, grad_spec,
#       res_alumni_position, res_alumni_academic
# ============================================================================

#' Graduation Plan Display UI
#'
#' Single read-only card summarizing a graduating resident's post-residency
#' plans, contact info, and chief-resident status. Caller-side coach-entry
#' textareas should be added around the module, not inside it.
#'
#' @param id Character. Module namespace id.
#' @param title Character or NULL. Optional header rendered above the panel.
#'
#' @return A `shiny::tagList` with a single `uiOutput()`.
#' @family seval_display
#' @seealso [mod_grad_plan_display_server()]
#' @export
mod_grad_plan_display_ui <- function(id, title = "Graduation Plan & Alumni Info") {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (!is.null(title)) shiny::tags$h5(title, class = "mb-2") else NULL,
    shiny::div(class = "grad-plan-display", shiny::uiOutput(ns("plan")))
  )
}

#' Graduation Plan Display Server
#'
#' Server-side renderer for [mod_grad_plan_display_ui()].
#'
#' @param id Character. Module namespace id (must match the UI call).
#' @param seval_row Reactive returning a 1-row data frame (or NULL/empty)
#'   containing `s_e_grad_next` from the resident's period-6 s_eval row.
#' @param resident_row Reactive returning a 1-row data frame from
#'   `resident_data` containing `chief`, `grad_email`, `grad_phone`,
#'   `grad_spec`, `res_alumni_position`, `res_alumni_academic`.
#' @param data_dict Optional reactive (or static data frame) of the REDCap
#'   data dictionary. When supplied, raw codes for `grad_spec` are translated
#'   to their human-readable labels. When NULL, raw codes are shown.
#'
#' @return Invisibly, NULL.
#' @family seval_display
#' @seealso [mod_grad_plan_display_ui()]
#' @export
mod_grad_plan_display_server <- function(id,
                                         seval_row,
                                         resident_row,
                                         data_dict = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

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

    .yn_label <- function(v) {
      v <- trimws(v)
      if (!nzchar(v)) return(NA_character_)
      if (v %in% c("1", "yes", "Yes", "YES", "TRUE")) "Yes"
      else if (v %in% c("0", "no", "No", "NO", "FALSE")) "No"
      else v
    }

    .grad_next_label <- function(v) {
      v <- trimws(v)
      switch(v,
             "1" = "Primary Care",
             "2" = "Hospitalist",
             "3" = "Fellowship",
             "4" = "Other",
             if (nzchar(v)) v else NA_character_)
    }

    .grad_spec_label <- function(v, dd) {
      v <- trimws(v)
      if (!nzchar(v)) return(NA_character_)
      lbl <- NULL
      tryCatch({
        ddv <- if (is.function(dd)) dd() else dd
        if (!is.null(ddv) && is.data.frame(ddv)) {
          ch <- .grad_dd_choices(ddv, "grad_spec")
          if (!is.null(ch) && v %in% names(ch)) lbl <- unname(ch[[v]])
        }
      }, error = function(e) NULL)
      if (is.null(lbl)) v else lbl
    }

    .row <- function(label, value, icon = NULL, icon_style = NULL) {
      if (is.null(value) || is.na(value) || !nzchar(trimws(value))) return(NULL)
      shiny::div(
        class = "d-flex align-items-start mb-2",
        if (!is.null(icon)) shiny::tags$i(
          class = paste0("bi ", icon, " me-2 mt-1"),
          style = if (is.null(icon_style)) "color:#6c757d;" else icon_style
        ),
        shiny::tags$div(
          shiny::tags$div(
            style = paste(
              "font-size:0.72rem; text-transform:uppercase; letter-spacing:.06em;",
              "font-weight:700; color:#6c757d;"),
            label
          ),
          shiny::tags$div(style = "color:#212529;", value)
        )
      )
    }

    # Yes/No row: render the value as a colored pill instead of plain text.
    .yn_row <- function(label, yn_value, icon = NULL) {
      if (is.null(yn_value) || is.na(yn_value) || !nzchar(trimws(yn_value))) return(NULL)
      pill_style <- if (identical(yn_value, "Yes")) {
        "background:#d1fae5; color:#065f46; border:1px solid #6ee7b7;"
      } else if (identical(yn_value, "No")) {
        "background:#f1f5f9; color:#475569; border:1px solid #cbd5e1;"
      } else {
        "background:#e2e8f0; color:#1e293b; border:1px solid #94a3b8;"
      }
      pill <- shiny::tags$span(
        style = paste("display:inline-block; padding:2px 10px; border-radius:999px;",
                      "font-size:0.78rem; font-weight:600;", pill_style),
        yn_value
      )
      shiny::div(
        class = "d-flex align-items-center justify-content-between mb-2",
        shiny::div(
          class = "d-flex align-items-center",
          if (!is.null(icon)) shiny::tags$i(
            class = paste0("bi ", icon, " me-2"),
            style = "color:#6c757d;"
          ),
          shiny::tags$span(
            style = "font-size:0.85rem; color:#334155;",
            label
          )
        ),
        pill
      )
    }

    .section <- function(title, ..., border = "#cbd5e1") {
      kids <- list(...); kids <- kids[!vapply(kids, is.null, logical(1))]
      if (!length(kids)) return(NULL)
      shiny::div(
        style = paste0(
          "background:#ffffff; border:1px solid ", border, "; border-radius:8px;",
          " padding:14px 16px; margin-bottom:12px;"),
        shiny::tags$div(
          style = paste(
            "font-size:0.7rem; text-transform:uppercase; letter-spacing:.08em;",
            "font-weight:700; color:#64748b; margin-bottom:10px;"),
          title
        ),
        kids
      )
    }

    output$plan <- shiny::renderUI({
      sev  <- .pull_row(seval_row)
      res  <- .pull_row(resident_row)

      grad_next_raw <- .field(sev, "s_e_grad_next")
      chief_raw     <- .field(res, "chief")
      email         <- .field(res, "grad_email")
      phone         <- .field(res, "grad_phone")
      spec_raw      <- .field(res, "grad_spec")
      alumni_pos    <- .field(res, "res_alumni_position")
      alumni_acad   <- .field(res, "res_alumni_academic")
      ssm_raw       <- .field(res, "ssm")
      mo_prac_raw   <- .field(res, "mo_prac")
      rural_raw     <- .field(res, "rural")
      und_urban_raw <- .field(res, "und_urban")

      if (!nzchar(grad_next_raw) && !nzchar(chief_raw) &&
          !nzchar(email) && !nzchar(phone) && !nzchar(spec_raw) &&
          !nzchar(alumni_pos)) {
        return(shiny::tags$p(
          class = "text-muted fst-italic mb-0",
          "Resident has not yet submitted graduation plans for this period."
        ))
      }

      path_lbl   <- .grad_next_label(grad_next_raw)
      chief_lbl  <- .yn_label(chief_raw)
      acad_lbl   <- .yn_label(alumni_acad)
      spec_lbl   <- .grad_spec_label(spec_raw, data_dict)
      ssm_lbl    <- .yn_label(ssm_raw)
      mo_lbl     <- .yn_label(mo_prac_raw)
      rural_lbl  <- .yn_label(rural_raw)
      urban_lbl  <- .yn_label(und_urban_raw)

      # Hero strip: post-residency path as a big colored band, with chief
      # status as a pill on the right when applicable.
      hero <- shiny::div(
        style = paste(
          "background:linear-gradient(135deg,#6610f2 0%,#4338ca 100%);",
          "color:#ffffff; border-radius:10px; padding:18px 20px;",
          "margin-bottom:14px; box-shadow:0 2px 6px rgba(102,16,242,0.18);",
          "display:flex; align-items:center; justify-content:space-between;",
          "flex-wrap:wrap; gap:12px;"),
        shiny::div(
          shiny::div(
            style = paste(
              "font-size:0.7rem; text-transform:uppercase; letter-spacing:.1em;",
              "font-weight:700; color:#e0e7ff; margin-bottom:4px;"),
            shiny::tags$i(class = "bi bi-signpost-split me-1"),
            "Post-residency path"
          ),
          shiny::tags$div(
            style = "font-size:1.4rem; font-weight:700; line-height:1.2;",
            if (!is.null(path_lbl) && !is.na(path_lbl) && nzchar(path_lbl))
              path_lbl
            else shiny::tags$em(style = "color:#e0e7ff; font-weight:500;",
                                "Not specified")
          )
        ),
        if (!is.null(chief_lbl) && !is.na(chief_lbl) && nzchar(chief_lbl))
          shiny::div(
            style = paste(
              "background:rgba(255,255,255,0.18); border:1px solid rgba(255,255,255,0.35);",
              "color:#fff; padding:6px 14px; border-radius:999px;",
              "font-size:0.82rem; font-weight:600;"),
            shiny::tags$i(class = "bi bi-star-fill me-1",
                          style = "color:#fde68a;"),
            "Chief Resident: ", chief_lbl
          )
      )

      # Section: Plans (fellowship spec)
      plans_block <- .section(
        "Plans",
        .row("Fellowship specialty", spec_lbl,
             icon = "bi-mortarboard", icon_style = "color:#6610f2;"),
        .row("Position / Practice", alumni_pos,
             icon = "bi-briefcase", icon_style = "color:#0d6efd;"),
        border = "#e9d5ff"
      )

      # Section: Practice details (yes/no pills)
      practice_block <- .section(
        "Practice details",
        .yn_row("Academic medicine",          acad_lbl,  icon = "bi-building"),
        .yn_row("Within SSM Health",          ssm_lbl,   icon = "bi-hospital"),
        .yn_row("Practicing in Missouri",     mo_lbl,    icon = "bi-geo-alt"),
        .yn_row("Rural practice setting",     rural_lbl, icon = "bi-tree"),
        .yn_row("Underserved / urban",        urban_lbl, icon = "bi-buildings"),
        border = "#bae6fd"
      )

      # Section: Contact
      contact_block <- .section(
        "Contact",
        .row("Email", email,
             icon = "bi-envelope", icon_style = "color:#0d6efd;"),
        .row("Phone", phone,
             icon = "bi-telephone", icon_style = "color:#0d6efd;"),
        border = "#bbf7d0"
      )

      shiny::div(
        hero,
        # Two-column grid; collapses on narrow screens.
        shiny::div(
          style = paste(
            "display:grid; grid-template-columns:repeat(auto-fit,minmax(280px,1fr));",
            "gap:12px;"),
          plans_block,
          practice_block,
          contact_block
        )
      )
    })

    invisible(NULL)
  })
}

# Internal: parse a "1, A | 2, B" choice string from the data dictionary into
# a named character vector mapping raw code -> label.
.grad_dd_choices <- function(dd, field) {
  col_var <- intersect(c("Variable / Field Name", "field_name"), names(dd))[1]
  col_ch  <- intersect(
    c("Choices, Calculations, OR Slider Labels",
      "select_choices_or_calculations"),
    names(dd))[1]
  if (is.na(col_var) || is.na(col_ch)) return(NULL)
  s <- dd[[col_ch]][dd[[col_var]] == field]
  if (length(s) == 0 || is.na(s[1]) || !nzchar(s[1])) return(NULL)
  parts <- strsplit(s[1], "\\|")[[1]]
  out <- vapply(parts, function(p) {
    kv <- strsplit(trimws(p), ",", fixed = FALSE)[[1]]
    if (length(kv) < 2) return(NA_character_)
    paste(trimws(kv[-1]), collapse = ",")
  }, character(1))
  keys <- vapply(parts, function(p) {
    kv <- strsplit(trimws(p), ",", fixed = FALSE)[[1]]
    trimws(kv[1])
  }, character(1))
  setNames(out, keys)
}
