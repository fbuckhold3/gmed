# ============================================================================
# BOARDS / STEP 3 DISPLAY MODULE
# R/mod_seval_boards_display.R
#
# Read-only board-prep summary for coach review:
#   1. Step 3 status (program-confirmed first; resident self-report fallback)
#   2. ITE-based ABIM board pass-probability prediction (from gmed::pass_prob)
#      with a plotly nomogram showing the McDonald 2020 PGY-1/2/3 curves and
#      the resident's actual score points overlaid.
#   3. Convenience link to MKSAP tracker (configurable) + self-reported
#      MKSAP completion (translated via the REDCap data dictionary).
#
# Distilled from imslu.ind.dash mod_self_eval.R `.ite_board_section_ui` plus
# imslu.ind.dash mod_learning.R `.nomogram_curves` / `ite_nomogram_plot` so
# coach.dash and other consumers can render the same prediction card without
# duplicating the nomogram lookup or plot code.
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

  section_h <- function(icon, label) {
    shiny::tags$h6(
      class = "fw-bold mb-2",
      style = "color:#2c3e50; font-size:1rem; text-transform:none; letter-spacing:0;",
      shiny::tags$i(class = paste0("bi ", icon, " me-2")),
      label
    )
  }

  shiny::tagList(
    shiny::div(class = "mb-4",
      section_h("bi-patch-check-fill", "Step 3"),
      shiny::uiOutput(ns("step3"))
    ),
    shiny::div(class = "mb-4",
      section_h("bi-graph-up-arrow", "ABIM Board Prediction (from ITE)"),
      shiny::uiOutput(ns("prediction_summary")),
      shiny::div(class = "mt-2",
        plotly::plotlyOutput(ns("nomogram_plot"), height = "340px")
      ),
      shiny::uiOutput(ns("board_concerns"))
    ),
    shiny::div(class = "mb-2",
      section_h("bi-journal-bookmark-fill", "MKSAP"),
      shiny::div(
        class = "p-3",
        style = "background:#f8fafc; border:1px solid #e9ecef; border-radius:8px;",
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
          shiny::tags$span(class = "text-muted ms-2",
                           style = "font-size:0.85rem;",
                           "(opens in new tab)")
        ),
        shiny::uiOutput(ns("mksap"))
      )
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

    # Build the nomogram reference curves (PGY-1/2/3) using gmed::pass_prob.
    .build_curves <- function() {
      do.call(rbind, lapply(1:3, function(p) {
        x <- seq(20, 100, by = 0.5)
        data.frame(
          ITE_Pct   = x,
          PGY       = p,
          Pass_Prob = vapply(x, function(v) gmed::pass_prob(v, p), numeric(1))
        )
      }))
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

      .card <- function(border, icon, color, title, body) {
        shiny::div(
          class = "p-3",
          style = sprintf("background:#fff; border:1px solid #e9ecef; border-left:5px solid %s; border-radius:8px; font-size:0.95rem;", border),
          shiny::div(class = "d-flex align-items-center mb-2",
            shiny::tags$i(class = paste0("bi ", icon, " me-2"),
                          style = sprintf("font-size:1.5rem; color:%s;", color)),
            shiny::tags$span(style = sprintf("font-weight:700; color:%s; font-size:1.05rem;", color), title)
          ),
          body
        )
      }

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
        return(.card("#198754", "bi-check-circle-fill", "#0f5132",
                     "Step 3 passed (program-confirmed)", score_line))
      }

      if (isTRUE(self_done)) {
        bits <- list(shiny::tags$div("Resident reports Step 3 is complete."))
        if (isTRUE(self_contact))
          bits[[length(bits)+1]] <- shiny::tags$div(class = "text-muted",
            shiny::tags$i(class = "bi bi-envelope-check me-1"),
            "Reports score was emailed to the program.")
        else if (identical(self_contact, FALSE))
          bits[[length(bits)+1]] <- shiny::tags$div(class = "text-warning",
            shiny::tags$i(class = "bi bi-envelope-exclamation me-1"),
            "Score has not yet been emailed to the program.")
        return(.card("#0d6efd", "bi-info-circle-fill", "#055160",
                     "Step 3 reportedly complete (program record pending)",
                     do.call(shiny::tagList, bits)))
      }

      # Not yet done — show scheduling status
      if (isTRUE(self_date_set)) {
        msg <- if (nzchar(self_date)) paste("Scheduled date:", self_date)
               else "Resident reports a date is set (not provided)."
        return(.card("#fd7e14", "bi-calendar-event", "#7a4500",
                     "Step 3 not yet taken \u2014 date scheduled",
                     shiny::tags$div(msg)))
      }
      if (identical(self_done, FALSE)) {
        return(.card("#dc3545", "bi-exclamation-triangle-fill", "#842029",
                     "Step 3 not yet taken",
                     shiny::tags$div(class = "text-muted",
                                     "Resident has not yet reported a scheduled date.")))
      }
      .card("#adb5bd", "bi-question-circle", "#495057",
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

      if (is.na(ite_pct)) {
        return(shiny::div(
          class = "p-3",
          style = "background:#fff; border:1px solid #e9ecef; border-left:5px solid #adb5bd; border-radius:8px; font-size:0.95rem;",
          shiny::tags$div(class = "fw-semibold", style = "color:#495057;",
            shiny::tags$i(class = "bi bi-info-circle me-2"),
            sprintf("No PGY%d ITE percent-correct on file yet.", pgy_int)),
          shiny::tags$div(class = "text-muted mt-1",
            "Once an ITE result is recorded, the ABIM pass-probability prediction will appear here.")
        ))
      }

      prob <- gmed::pass_prob(ite_pct, pgy_int)
      risk <- gmed::risk_from_prob(prob)

      shiny::div(
        class = "p-3 d-flex align-items-center gap-3",
        style = sprintf("background:#fff; border:1px solid #e9ecef; border-left:5px solid %s; border-radius:8px; font-size:0.95rem;",
                        risk$color),
        shiny::div(
          style = "min-width:96px; text-align:center;",
          shiny::tags$div(style = sprintf("font-size:2rem; font-weight:700; color:%s; line-height:1;", risk$color),
                          paste0(round(ite_pct, 1), "%")),
          shiny::tags$div(class = "text-muted", style = "font-size:0.85rem;", "ITE % correct")
        ),
        shiny::div(
          style = "flex:1;",
          shiny::tags$div(class = "fw-bold mb-1", style = "color:#003d5c; font-size:1.05rem;",
                          sprintf("PGY%d ACP ITE Score", pgy_int)),
          if (!is.na(prob))
            shiny::tags$div(
              shiny::tags$span(style = "color:#212529;",
                               paste0(round(prob * 100, 0), "% predicted ABIM pass probability")),
              shiny::tags$span(style = sprintf("margin-left:8px; font-weight:700; color:%s;", risk$color),
                               paste0("\u2014 ", risk$level))
            )
          else
            shiny::tags$div(class = "text-muted", risk$level)
        )
      )
    })

    # --------------------------------------------------- Nomogram plot ---
    output$nomogram_plot <- plotly::renderPlotly({
      td <- .pull(test_data)
      if (is.null(td)) td <- data.frame()
      pcts <- .pct_vec(td)

      curves <- .build_curves()
      curve_cols   <- c("1"="#4e79a7", "2"="#59a14f", "3"="#f28e2b")
      curve_dashes <- c("1"="solid",   "2"="dash",    "3"="dot")

      fig <- plotly::plot_ly()

      # Risk zone fills
      fig <- fig %>%
        plotly::add_ribbons(x = c(20, 100), ymin = 0, ymax = 0.5,
                            fillcolor = "rgba(211,47,47,0.06)",
                            line = list(width = 0),
                            hoverinfo = "none", showlegend = FALSE) %>%
        plotly::add_ribbons(x = c(20, 100), ymin = 0.5, ymax = 0.75,
                            fillcolor = "rgba(230,81,0,0.06)",
                            line = list(width = 0),
                            hoverinfo = "none", showlegend = FALSE)

      # Reference threshold lines
      fig <- fig %>%
        plotly::add_segments(x = 20, xend = 100, y = 0.75, yend = 0.75,
                             line = list(color = "#e65100", dash = "dash", width = 1),
                             name = "75% threshold", showlegend = TRUE) %>%
        plotly::add_segments(x = 20, xend = 100, y = 0.50, yend = 0.50,
                             line = list(color = "#d32f2f", dash = "dash", width = 1),
                             name = "50% threshold", showlegend = TRUE)

      # PGY-1/2/3 nomogram curves
      for (p in 1:3) {
        cd <- curves[curves$PGY == p, ]
        fig <- fig %>%
          plotly::add_trace(data = cd, x = ~ITE_Pct, y = ~Pass_Prob,
                            type = "scatter", mode = "lines",
                            line = list(color = curve_cols[[as.character(p)]],
                                        dash  = curve_dashes[[as.character(p)]],
                                        width = 2),
                            name = paste0("PGY-", p, " curve"),
                            hovertemplate = paste0("PGY-", p, " | %{x:.0f}% correct<br>",
                                                   "P(pass) = %{y:.1%}<extra></extra>"))
      }

      # Resident's actual score points
      for (y in names(pcts)) {
        pct_v  <- pcts[[y]]
        prob_v <- gmed::pass_prob(pct_v, as.integer(y))
        risk   <- gmed::risk_from_prob(prob_v)
        fig <- fig %>%
          plotly::add_trace(x = pct_v, y = prob_v,
                            type = "scatter", mode = "markers",
                            marker = list(size = 14, color = risk$color,
                                          line = list(color = "white", width = 2),
                                          symbol = "diamond"),
                            name = paste0("Resident PGY-", y, " score"),
                            hovertemplate = paste0("PGY-", y, "<br>",
                                                   round(pct_v, 1), "% correct<br>",
                                                   "P(pass ABIM) = ", round(prob_v * 100, 1), "%<br>",
                                                   risk$level, "<extra></extra>"))
      }

      fig %>% plotly::layout(
        xaxis = list(title = "ITE % Correct", range = c(20, 100),
                     showgrid = TRUE, gridcolor = "#f0f0f0", dtick = 10),
        yaxis = list(title = "Probability of Passing ABIM",
                     range = c(0, 1), tickformat = ".0%",
                     showgrid = TRUE, gridcolor = "#f0f0f0"),
        legend = list(orientation = "h", x = 0, y = -0.25),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
        margin = list(t = 10, b = 60)
      ) %>% plotly::config(displayModeBar = FALSE)
    })

    # --------------------------------------------------- Board concerns ---
    output$board_concerns <- shiny::renderUI({
      sr <- .pull(current_s_eval)
      board_concern <- .yesno(.field(sr, "s_e_board_concern"))
      board_discu   <- .field(sr, "s_e_board_discu")

      if (!isTRUE(board_concern) && !nzchar(trimws(board_discu))) return(NULL)

      shiny::div(
        class = "mt-3 p-3",
        style = "background:#fff8e1; border-left:4px solid #ffb300; border-radius:6px; font-size:0.95rem;",
        shiny::tags$div(class = "fw-bold mb-1", style = "color:#7a4500;",
          shiny::tags$i(class = "bi bi-exclamation-circle-fill me-1"),
          "Resident flagged board-prep concerns"),
        if (nzchar(trimws(board_discu)))
          shiny::tags$div(style = "white-space:pre-wrap; color:#212529;",
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

      shiny::div(
        class = "d-flex align-items-baseline flex-wrap gap-2",
        style = "font-size:0.95rem;",
        shiny::tags$span(class = "fw-semibold",
                         style = "color:#495057;",
                         "MKSAP completion (self-reported):"),
        shiny::tags$span(style = "font-weight:700; color:#2c3e50;", label)
      )
    })

    invisible(NULL)
  })
}
