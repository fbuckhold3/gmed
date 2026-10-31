# ============================================================================
# BOARDS / STEP 3 DISPLAY MODULE
# R/mod_seval_boards_display.R
#
# Read-only board-prep summary for coach review:
#   1. Step 3 status (program-confirmed first; resident self-report fallback)
#   2. ITE-based ABIM board pass-probability estimate (iteviz card) with the
#      iteviz plotly nomogram: McDonald 2020 PGY-1/2/3 curves, risk-tier bands
#      and the resident's actual score points overlaid.
#   3. Convenience link to MKSAP tracker (configurable) + self-reported
#      MKSAP completion (translated via the REDCap data dictionary).
#
# The estimate card, nomogram plot and tiers (High <50%, Intermediate <90%,
# Low >=90%) come from iteviz, which mirrors im-ite-analyzer's nomogram.py.
# ============================================================================

#' Boards / Step 3 Display UI
#'
#' Read-only summary of a resident's board-preparation status for coach review.
#' Renders three cards stacked vertically:
#'   1. Step 3 status (passed / scheduled / not yet)
#'   2. ABIM pass-probability prediction from in-training-exam (ITE) percent
#'      correct, including a nomogram plot of the McDonald 2020 curves.
#'   3. A "Review MKSAP Tracking" external link plus the resident's
#'      self-reported MKSAP completion category.
#'
#' Resident-entered board concerns / Step 3 details (`s_e_board_concern`,
#' `s_e_board_help`, `s_e_board_discu`, `s_e_step3*`, `s_e_mksap_comp`) are
#' surfaced inline beneath the relevant cards when present.
#'
#' @param id Character. Module namespace id.
#' @param mksap_url Character. URL the "Review MKSAP Tracking" link opens.
#'   Defaults to the IM MKSAP tracker.
#'
#' @return A `shiny::tagList`.
#' @family seval_display
#' @seealso [mod_seval_boards_display_server()]
#' @export
mod_seval_boards_display_ui <- function(id,
                                        mksap_url = "https://mksap.acponline.org/login?forward=%2Ftracker#/") {
  ns <- shiny::NS(id)

  # Loaded here (not just relying on the host app's page shell) - same
  # reasoning as gmed's other roundsui-migrated modules: ccc.dashboard/
  # coach.dash still call gmed::gmed_page(), not roundsui::roundsui_page().
  # Safe to load repeatedly.
  styles <- roundsui::load_roundsui_styles()

  section_card <- function(icon, label, ...) {
    shiny::div(
      class = "roundsui-card mb-4",
      shiny::div(
        class = "roundsui-card__header",
        shiny::tags$i(class = paste0("bi ", icon, " me-2")),
        shiny::tags$span(class = "roundsui-card__title", style = "display:inline;", label)
      ),
      shiny::div(class = "roundsui-card__body", ...)
    )
  }

  shiny::tagList(
    styles,
    section_card("bi-patch-check-fill", "Step 3",
      shiny::uiOutput(ns("step3"))
    ),
    section_card("bi-graph-up-arrow", "ABIM Board Prediction (from ITE)",
      shiny::uiOutput(ns("prediction_summary")),
      shiny::div(class = "mt-2",
        plotly::plotlyOutput(ns("nomogram_plot"), height = "340px")
      ),
      shiny::uiOutput(ns("board_concerns"))
    ),
    section_card("bi-journal-bookmark-fill", "MKSAP",
      shiny::tags$p(
        class = "mb-2",
        style = "font-size:0.95rem;",
        shiny::tags$a(
          href = mksap_url,
          target = "_blank",
          rel = "noopener",
          class = "fw-semibold",
          shiny::tags$i(class = "bi bi-box-arrow-up-right me-1"),
          "Review MKSAP Tracking"
        ),
        shiny::tags$span(style = "color:var(--roundsui-ink-faint); margin-left:8px; font-size:0.85rem;",
                         "(opens in new tab)")
      ),
      shiny::uiOutput(ns("mksap"))
    )
  )
}

#' Boards / Step 3 Display Server
#'
#' Server side of [mod_seval_boards_display_ui()].
#'
#' @param id Character. Module namespace id (must match the UI call).
#' @param current_s_eval Reactive returning a 1-row data frame from the
#'   resident's `s_eval` form for the period being reviewed (or NULL/empty).
#' @param resident_info Reactive returning a 1-row data frame from the
#'   `resident_data` form. Used for program-confirmed Step 3 (`step3`,
#'   `usmle_step3_score`, `comlex_step3_score`).
#' @param test_data Reactive returning a 0+ row data frame from the `test_data`
#'   form (most recent ITE row is used for the prediction; all PGY rows are
#'   plotted on the nomogram if present).
#' @param pgy Reactive returning the resident's current PGY level (1, 2, 3).
#' @param data_dict Optional reactive returning the REDCap data dictionary
#'   data frame. When supplied, used to translate the `s_e_mksap_comp` raw
#'   code to its human-readable label.
#'
#' @return Invisibly NULL.
#' @family seval_display
#' @seealso [mod_seval_boards_display_ui()]
#' @export
mod_seval_boards_display_server <- function(id,
                                            current_s_eval,
                                            resident_info = NULL,
                                            test_data     = NULL,
                                            pgy           = NULL,
                                            data_dict     = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    .pull <- function(r) {
      if (is.null(r)) return(NULL)
      v <- if (is.function(r)) r() else r
      if (is.null(v)) return(NULL)
      v
    }

    .field <- function(row, fld) {
      if (is.null(row) || !is.data.frame(row) || nrow(row) == 0) return("")
      if (!fld %in% names(row)) return("")
      v <- row[[fld]][1]
      if (is.null(v) || is.na(v)) "" else as.character(v)
    }

    .yesno <- function(v) {
      if (!nzchar(v)) return(NA)
      v <- trimws(tolower(v))
      if (v %in% c("1", "yes", "y", "true", "t")) TRUE
      else if (v %in% c("0", "no",  "n", "false","f")) FALSE
      else NA
    }

    # PGY pct vector for the nomogram plot.
    .pct_vec <- function(td) {
      if (is.null(td) || !is.data.frame(td) || nrow(td) == 0) return(numeric(0))
      pick <- function(fld) {
        if (!fld %in% names(td)) return(NA_real_)
        for (v in td[[fld]]) {
          if (!is.na(v) && nzchar(as.character(v))) {
            n <- suppressWarnings(as.numeric(v))
            if (!is.na(n)) return(n)
          }
        }
        NA_real_
      }
      v <- c("1" = pick("pgy1_tot_correct"),
             "2" = pick("pgy2_tot_correct"),
             "3" = pick("pgy3_tot_correct"))
      v[!is.na(v)]
    }

    # ------------------------------------------------------------- Step 3 ---
    output$step3 <- shiny::renderUI({
      ri <- .pull(resident_info)
      sr <- .pull(current_s_eval)

      program_step3   <- .yesno(.field(ri, "step3"))
      usmle_score     <- .field(ri, "usmle_step3_score")
      comlex_score    <- .field(ri, "comlex_step3_score")

      self_done       <- .yesno(.field(sr, "s_e_step3"))
      self_contact    <- .yesno(.field(sr, "s_e_step3_contact"))
      self_date_set   <- .yesno(.field(sr, "s_e_step3_date_set"))
      self_date       <- .field(sr, "s_e_step3_date")

      rc <- roundsui::roundsui_colors()

      .card <- function(border, icon, color, title, body) {
        shiny::div(
          class = "p-3",
          style = sprintf("background:%s; border:1px solid %s; border-left:5px solid %s; border-radius:8px; font-size:0.95rem;",
                          rc$surface, rc$border, border),
          shiny::div(class = "d-flex align-items-center mb-2",
            shiny::tags$i(class = paste0("bi ", icon, " me-2"),
                          style = sprintf("font-size:1.5rem; color:%s;", color)),
            shiny::tags$span(style = sprintf("font-weight:700; color:%s; font-size:1.05rem;", color), title)
          ),
          body
        )
      }

      # Status -> roundsui token mapping: passed = success, scheduled =
      # warning, not-done = danger, not-reported = neutral. "Reportedly
      # complete (program record pending)" doesn't fit success/warning/
      # danger - it's informational, not alarming - so it uses roundsui's
      # accent rather than the original's Bootstrap blue.
      if (isTRUE(program_step3)) {
        score_bits <- c(
          if (nzchar(usmle_score))  paste("USMLE:", usmle_score) else NULL,
          if (nzchar(comlex_score)) paste("COMLEX:", comlex_score) else NULL
        )
        score_line <- if (length(score_bits))
          shiny::tags$div(class = "text-muted",
                          paste(score_bits, collapse = "  •  "))
        else
          shiny::tags$div(class = "fst-italic text-muted",
                          "Score not yet on file in resident_data.")
        return(.card(rc$success, "bi-check-circle-fill", rc$success,
                     "Step 3 passed (program-confirmed)", score_line))
      }

      if (isTRUE(self_done)) {
        bits <- list(shiny::tags$div("Resident reports Step 3 is complete."))
        if (isTRUE(self_contact))
          bits[[length(bits)+1]] <- shiny::tags$div(class = "text-muted",
            shiny::tags$i(class = "bi bi-envelope-check me-1"),
            "Reports score was emailed to the program.")
        else if (identical(self_contact, FALSE))
          bits[[length(bits)+1]] <- shiny::tags$div(style = sprintf("color:%s;", rc$warning),
            shiny::tags$i(class = "bi bi-envelope-exclamation me-1"),
            "Score has not yet been emailed to the program.")
        return(.card(rc$accent, "bi-info-circle-fill", rc$accent,
                     "Step 3 reportedly complete (program record pending)",
                     do.call(shiny::tagList, bits)))
      }

      # Not yet done — show scheduling status
      if (isTRUE(self_date_set)) {
        msg <- if (nzchar(self_date)) paste("Scheduled date:", self_date)
               else "Resident reports a date is set (not provided)."
        return(.card(rc$warning, "bi-calendar-event", rc$warning,
                     "Step 3 not yet taken \u2014 date scheduled",
                     shiny::tags$div(msg)))
      }
      if (identical(self_done, FALSE)) {
        return(.card(rc$danger, "bi-exclamation-triangle-fill", rc$danger,
                     "Step 3 not yet taken",
                     shiny::tags$div(class = "text-muted",
                                     "Resident has not yet reported a scheduled date.")))
      }
      .card(rc$border_strong, "bi-question-circle", rc$ink_faint,
            "Step 3 status not reported",
            shiny::tags$div(class = "fst-italic text-muted",
                            "Neither program record nor resident self-report on file for this period."))
    })

    # --------------------------------------------------- Prediction summary ---
    output$prediction_summary <- shiny::renderUI({
      td  <- .pull(test_data)
      pp  <- .pull(pgy)

      pgy_int <- suppressWarnings(as.integer(pp))
      if (is.na(pgy_int) || pgy_int < 1) pgy_int <- 1
      if (pgy_int > 3) pgy_int <- 3

      ite_pct <- NA_real_
      if (!is.null(td) && is.data.frame(td) && nrow(td) > 0) {
        if ("redcap_repeat_instance" %in% names(td))
          td <- td[order(suppressWarnings(as.integer(td$redcap_repeat_instance)),
                         decreasing = TRUE, na.last = TRUE), , drop = FALSE]
        fld <- paste0("pgy", pgy_int, "_tot_correct")
        if (fld %in% names(td)) {
          for (v in td[[fld]]) {
            if (!is.na(v) && nzchar(as.character(v))) {
              n <- suppressWarnings(as.numeric(v))
              if (!is.na(n)) { ite_pct <- n; break }
            }
          }
        }
      }

      rc <- roundsui::roundsui_colors()

      if (is.na(ite_pct)) {
        return(shiny::div(
          class = "p-3",
          style = sprintf("background:%s; border:1px solid %s; border-left:5px solid %s; border-radius:8px; font-size:0.95rem;",
                          rc$surface, rc$border, rc$border_strong),
          shiny::tags$div(style = sprintf("font-weight:600; color:%s;", rc$ink_muted),
            shiny::tags$i(class = "bi bi-info-circle me-2"),
            sprintf("No PGY%d ITE percent-correct on file yet.", pgy_int)),
          shiny::tags$div(class = "mt-1", style = sprintf("color:%s;", rc$ink_faint),
            "Once an ITE result is recorded, the ABIM pass-probability prediction will appear here.")
        ))
      }

      iteviz::ite_estimate_card(ite_pct, pgy_int)
    })

    # --------------------------------------------------- Nomogram plot ---
    output$nomogram_plot <- plotly::renderPlotly({
      td <- .pull(test_data)
      if (is.null(td)) td <- data.frame()
      pcts <- .pct_vec(td)
      iteviz::ite_nomogram_plot(if (length(pcts)) pcts else NULL)
    })

    # --------------------------------------------------- Board concerns ---
    output$board_concerns <- shiny::renderUI({
      sr <- .pull(current_s_eval)
      board_concern <- .yesno(.field(sr, "s_e_board_concern"))
      board_discu   <- .field(sr, "s_e_board_discu")

      if (!isTRUE(board_concern) && !nzchar(trimws(board_discu))) return(NULL)

      rc <- roundsui::roundsui_colors()
      shiny::div(
        class = "mt-3 p-3",
        style = sprintf("background:%s; border-left:4px solid %s; border-radius:6px; font-size:0.95rem;",
                        rc$warning_tint, rc$warning),
        shiny::tags$div(class = "fw-bold mb-1", style = sprintf("color:%s;", rc$warning),
          shiny::tags$i(class = "bi bi-exclamation-circle-fill me-1"),
          "Resident flagged board-prep concerns"),
        if (nzchar(trimws(board_discu)))
          shiny::tags$div(style = sprintf("white-space:pre-wrap; color:%s;", rc$ink),
                          board_discu)
      )
    })

    # ----------------------------------------------------------- MKSAP ---
    output$mksap <- shiny::renderUI({
      sr <- .pull(current_s_eval)
      mksap_comp_raw <- .field(sr, "s_e_mksap_comp")

      if (!nzchar(mksap_comp_raw))
        return(shiny::tags$div(class = "fst-italic text-muted",
                               style = "font-size:0.95rem;",
                               "Resident has not reported MKSAP completion this period."))

      # Translate raw code to label via the data dictionary if available.
      label <- mksap_comp_raw
      dd <- if (!is.null(data_dict))
              tryCatch(if (is.function(data_dict)) data_dict() else data_dict,
                       error = function(e) NULL)
            else NULL
      if (!is.null(dd) && is.data.frame(dd) && nrow(dd) > 0) {
        fn_col <- intersect(c("field_name", "Variable / Field Name",
                              "Variable...Field.Name"), names(dd))
        ch_col <- intersect(c("select_choices_or_calculations",
                              "Choices, Calculations, OR Slider Labels",
                              "Choices..Calculations..OR.Slider.Labels"),
                            names(dd))
        if (length(fn_col) && length(ch_col)) {
          row <- dd[dd[[fn_col[1]]] == "s_e_mksap_comp", , drop = FALSE]
          if (nrow(row) > 0) {
            choices_str <- as.character(row[[ch_col[1]]][1])
            cmap <- parse_redcap_choices(choices_str)
            if (length(cmap) > 0 && mksap_comp_raw %in% names(cmap))
              label <- unname(cmap[mksap_comp_raw])
          }
        }
      }

      rc <- roundsui::roundsui_colors()
      shiny::div(
        class = "d-flex align-items-baseline flex-wrap gap-2",
        style = "font-size:0.95rem;",
        shiny::tags$span(class = "fw-semibold",
                         style = sprintf("color:%s;", rc$ink_muted),
                         "MKSAP completion (self-reported):"),
        shiny::tags$span(style = sprintf("font-weight:700; color:%s;", rc$ink), label)
      )
    })

    invisible(NULL)
  })
}
