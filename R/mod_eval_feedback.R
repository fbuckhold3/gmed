# =============================================================================
# mod_eval_feedback.R — Evaluation Feedback Module
#
# Reusable resident-facing module that shows:
#   1. Assessment type breakdown — count buttons for every possible obs type
#   2. Written feedback table   — plus/delta rows with PGY + type filters
#   3. Score detail panel       — coloured badges on row click
#
# Parameters
#   assessment_data  reactive  Full assessment data frame, already filtered to
#                              redcap_repeat_instrument == "assessment" (casing
#                              normalised).  May contain all residents.
#   record_id        reactive  record_id of the resident to display.
#   data_dict        reactive  REDCap data-dictionary data frame.
# =============================================================================

#' Evaluation Feedback UI
#'
#' @param id Module namespace
#' @export
mod_eval_feedback_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # ── 1. Assessment Breakdown ──────────────────────────────────────────────
    div(
      class = "card border-0 shadow-sm mb-4",
      style = "border-radius:8px; overflow:hidden;",
      div(
        class = "card-header d-flex align-items-center justify-content-between",
        style = "background:#003d5c; color:white; padding:10px 18px;",
        div(
          class = "d-flex align-items-center",
          tags$i(class = "bi bi-grid-1x2-fill me-2"),
          tags$span(style = "font-weight:700;", "Assessment Breakdown")
        ),
        tags$span(
          style = "font-size:0.72rem; opacity:0.75;",
          "All types defined for this programme \u2014 grey = none completed yet"
        )
      ),
      div(
        class = "card-body p-3",
        tags$p(
          class = "text-muted mb-3",
          style = "font-size:0.82rem;",
          tags$i(class = "bi bi-info-circle me-1"),
          "Each tile shows one type of direct observation or assessment. ",
          "The number is how many you have received. ",
          "Coloured tiles are complete; grey tiles have not yet been done."
        ),
        uiOutput(ns("type_buttons"))
      )
    ),

    # ── 2 & 3. Written Feedback + Score Detail ────────────────────────────────
    div(
      class = "card border-0 shadow-sm",
      style = "border-radius:8px; overflow:hidden;",
      div(
        class = "card-header",
        style = "background:#003d5c; color:white; padding:10px 18px;",
        div(
          class = "d-flex align-items-center",
          tags$i(class = "bi bi-chat-left-text-fill me-2"),
          tags$span(style = "font-weight:700;", "Written Feedback & Scores"),
          tags$span(
            style = "font-size:0.75rem; opacity:0.7; margin-left:10px;",
            "Click a row to expand the full assessment scores"
          )
        )
      ),
      div(
        class = "card-body p-3",

        # How-to banner
        div(
          class = "alert alert-info d-flex align-items-start gap-2 py-2 mb-3",
          style = "font-size:0.82rem; border-radius:6px;",
          tags$i(class = "bi bi-lightbulb-fill mt-1 flex-shrink-0"),
          tags$div(
            tags$strong("How to use: "),
            "Use the filters below to narrow evaluations by training level or assessment type. ",
            "The table shows every evaluation that includes written feedback. ",
            tags$strong("Plus "), "(\u2795) highlights what went well; ",
            tags$strong("Delta "), "(\u25b3) notes areas to develop. ",
            "Select any row to reveal the full scored items for that evaluation."
          )
        ),

        # PGY filter
        uiOutput(ns("pgy_chips")),

        # Type filter
        uiOutput(ns("type_chips")),

        # Table
        DT::dataTableOutput(ns("pd_table")),

        # Score detail
        uiOutput(ns("score_detail"))
      )
    )
  )
}


#' Evaluation Feedback Server
#'
#' @param id             Module namespace
#' @param assessment_data Reactive — full assessment data frame (normalised casing)
#' @param record_id      Reactive — resident record_id
#' @param data_dict      Reactive — REDCap data dictionary data frame
#' @export
mod_eval_feedback_server <- function(id, assessment_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Palette constants ─────────────────────────────────────────────────────
    TYPE_PAL  <- c("#003d5c","#0066a1","#1a6b3a","#6c3483",
                   "#c0392b","#e67e22","#17a589","#2471a3")
    LEVEL_COL <- c("Intern" = "#6c3483", "PGY2" = "#0066a1", "PGY3" = "#003d5c")

    # ── obs_type choice map ───────────────────────────────────────────────────
    obs_type_map_r <- reactive({
      dd <- data_dict()
      if (is.null(dd) || nrow(dd) == 0) return(character(0))
      row <- dd[dd$field_name == "ass_obs_type", ]
      if (nrow(row) == 0) return(character(0))
      parse_redcap_choices(row$select_choices_or_calculations[1])
    })

    # ── Resident's assessments ────────────────────────────────────────────────
    my_evals <- reactive({
      req(assessment_data(), record_id())
      assessment_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          tolower(trimws(redcap_repeat_instrument)) == "assessment"
        )
    })

    # ── Decode ass_obs_type → label ───────────────────────────────────────────
    .decode_type <- function(raw_type, cc_quart, obs_map) {
      out     <- rep(NA_character_, length(raw_type))
      raw_chr <- as.character(raw_type)
      if (length(obs_map) > 0) {
        decoded <- obs_map[raw_chr]
        out     <- ifelse(!is.na(decoded), decoded, out)
      }
      is_cc       <- is.na(out) & !is.na(cc_quart) &
                     nzchar(trimws(as.character(cc_quart)))
      out[is_cc]  <- "Continuity Clinic"
      out[is.na(out)] <- "General Assessment"
      out
    }

    # ── 1. Assessment type count buttons ─────────────────────────────────────
    output$type_buttons <- renderUI({
      req(my_evals(), data_dict())
      df      <- my_evals()
      obs_map <- obs_type_map_r()

      all_types <- c(
        if (length(obs_map) > 0) unname(obs_map) else character(0),
        "Continuity Clinic"
      )
      cc_quart <- if ("ass_cc_quart" %in% names(df)) df$ass_cc_quart
                  else rep(NA_character_, nrow(df))
      type_vec <- .decode_type(df$ass_obs_type, cc_quart, obs_map)
      actual   <- data.frame(Type = type_vec, stringsAsFactors = FALSE) %>%
        dplyr::count(Type)
      if ("General Assessment" %in% actual$Type)
        all_types <- c(all_types, "General Assessment")

      counts <- data.frame(Type = all_types, stringsAsFactors = FALSE) %>%
        dplyr::left_join(actual, by = "Type") %>%
        dplyr::mutate(n = ifelse(is.na(n), 0L, as.integer(n))) %>%
        dplyr::arrange(dplyr::desc(n > 0), dplyr::desc(n), Type)

      done_rows <- which(counts$n > 0)
      buttons <- lapply(seq_len(nrow(counts)), function(i) {
        tp   <- counts$Type[i]
        cnt  <- counts$n[i]
        done <- cnt > 0
        col  <- if (done)
          TYPE_PAL[((match(i, done_rows) - 1L) %% length(TYPE_PAL)) + 1L]
        else "#c8d3dd"
        div(
          class = "col-6 col-sm-4 col-md-3 col-lg-2 mb-2",
          div(
            style = paste0(
              "border:2px solid ", col, "; border-radius:8px;",
              "padding:10px 8px; text-align:center; height:100%;",
              if (done) paste0("background:", col, "18;") else "background:#f7f9fb;"
            ),
            tags$div(
              style = paste0(
                "font-size:1.6rem; font-weight:800; line-height:1; margin-bottom:4px;",
                "color:", if (done) col else "#b0bec5", ";"
              ),
              cnt
            ),
            tags$div(
              style = paste0(
                "font-size:0.70rem; font-weight:600; line-height:1.3;",
                "color:", if (done) "#2c3e50" else "#9aa5b0", ";"
              ),
              tp
            )
          )
        )
      })
      div(class = "row g-2 px-1", buttons)
    })

    # ── pd_data_all (unfiltered) ──────────────────────────────────────────────
    pd_data_all <- reactive({
      req(my_evals())
      df      <- my_evals()
      obs_map <- obs_type_map_r()
      cc_quart <- if ("ass_cc_quart" %in% names(df)) df$ass_cc_quart
                  else rep(NA_character_, nrow(df))
      df %>%
        dplyr::mutate(.Type = .decode_type(ass_obs_type, cc_quart, obs_map)) %>%
        dplyr::filter(
          (!is.na(ass_plus)  & nzchar(trimws(ass_plus)))  |
          (!is.na(ass_delta) & nzchar(trimws(ass_delta)))
        ) %>%
        dplyr::mutate(
          Date    = format(as.Date(ass_date), "%b %d, %Y"),
          Type    = .Type,
          Level   = dplyr::case_when(
            as.character(ass_level) %in% c("1","Intern") ~ "Intern",
            as.character(ass_level) %in% c("2","PGY2")   ~ "PGY2",
            as.character(ass_level) %in% c("3","PGY3")   ~ "PGY3",
            TRUE ~ as.character(ass_level)
          ),
          Faculty = dplyr::coalesce(ass_faculty, "\u2014"),
          Plus    = dplyr::if_else(!is.na(ass_plus)  & nzchar(trimws(ass_plus)),
                                   trimws(ass_plus),  "Not provided"),
          Delta   = dplyr::if_else(!is.na(ass_delta) & nzchar(trimws(ass_delta)),
                                   trimws(ass_delta), "Not provided")
        ) %>%
        dplyr::arrange(dplyr::desc(ass_date))
    })

    # ── Filter state ──────────────────────────────────────────────────────────
    selected_type  <- reactiveVal("")
    selected_level <- reactiveVal("")

    observeEvent(input$type_chip_click, {
      clicked <- input$type_chip_click
      selected_type(if (!is.null(clicked) && clicked == selected_type()) "" else clicked)
    })
    observeEvent(input$pgy_chip_click, {
      clicked <- input$pgy_chip_click
      selected_level(if (!is.null(clicked) && clicked == selected_level()) "" else clicked)
    })

    observe({
      selected_type(); selected_level()
      DT::selectRows(DT::dataTableProxy("pd_table"), NULL)
    })

    pd_data <- reactive({
      df <- pd_data_all()
      if (nzchar(selected_type()))  df <- df %>% dplyr::filter(Type  == selected_type())
      if (nzchar(selected_level())) df <- df %>% dplyr::filter(Level == selected_level())
      df
    })

    # ── 2a. PGY filter chips ──────────────────────────────────────────────────
    output$pgy_chips <- renderUI({
      df  <- pd_data_all()
      sel <- selected_level()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      level_order <- c("Intern", "PGY2", "PGY3")
      counts <- df %>%
        dplyr::count(Level) %>%
        dplyr::filter(Level %in% level_order) %>%
        dplyr::mutate(Level = factor(Level, levels = level_order)) %>%
        dplyr::arrange(Level)
      if (nrow(counts) == 0) return(NULL)

      .chip <- function(label, onclick_val, col, active, count = NULL) {
        tags$span(
          style = paste0(
            "display:inline-flex; align-items:center; gap:4px;",
            "background:", if (active) col else "#eee", ";",
            "color:", if (active) "white" else col, ";",
            "border-radius:20px; padding:4px 14px; font-size:0.78rem;",
            "font-weight:600; margin:2px 3px; cursor:pointer;",
            "border:2px solid ", col, ";"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            ns("pgy_chip_click"), onclick_val
          ),
          label,
          if (!is.null(count)) tags$span(
            style = paste0(
              "display:inline-flex; align-items:center; justify-content:center;",
              "background:", if (active) "rgba(255,255,255,.3)" else col, ";",
              "color:", if (active) col else "white", ";",
              "border-radius:50%; width:19px; height:19px; font-size:0.7rem;"
            ),
            count
          )
        )
      }

      all_active <- !nzchar(sel)
      div(
        style = "margin-bottom:10px;",
        div(
          style = "font-size:0.7rem; font-weight:700; text-transform:uppercase;
                   letter-spacing:.08em; color:#6c757d; margin-bottom:5px;",
          tags$i(class = "bi bi-mortarboard me-1"),
          "Filter by Training Level"
        ),
        div(
          style = "display:flex; flex-wrap:wrap; align-items:center;",
          .chip("All levels", "", "#555", all_active, count = nrow(df)),
          lapply(seq_len(nrow(counts)), function(i) {
            lvl <- as.character(counts$Level[i])
            .chip(lvl, lvl, LEVEL_COL[[lvl]],
                  nzchar(sel) && sel == lvl,
                  count = counts$n[i])
          })
        )
      )
    })

    # ── 2b. Type filter chips ─────────────────────────────────────────────────
    output$type_chips <- renderUI({
      df  <- pd_data_all()
      sel <- selected_type()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      counts <- df %>%
        dplyr::count(Type, sort = TRUE) %>%
        dplyr::filter(!is.na(Type))
      if (nrow(counts) == 0) return(NULL)

      all_active <- !nzchar(sel)
      all_chip <- tags$span(
        style = paste0(
          "display:inline-flex; align-items:center; gap:5px;",
          "background:", if (all_active) "#003d5c" else "#e8edf2", ";",
          "color:", if (all_active) "white" else "#003d5c", ";",
          "border-radius:20px; padding:5px 14px; font-size:0.78rem;",
          "font-weight:600; margin:3px; cursor:pointer;",
          "border:2px solid #003d5c;"
        ),
        onclick = sprintf(
          "Shiny.setInputValue('%s', '', {priority:'event'})",
          ns("type_chip_click")
        ),
        tags$i(class = "bi bi-grid-3x3-gap me-1"),
        paste0("All  ", nrow(df))
      )

      type_chips <- lapply(seq_len(nrow(counts)), function(i) {
        tp     <- counts$Type[i]
        cnt    <- counts$n[i]
        col    <- TYPE_PAL[((i - 1L) %% length(TYPE_PAL)) + 1L]
        active <- nzchar(sel) && sel == tp
        tags$span(
          style = paste0(
            "display:inline-flex; align-items:center; gap:5px;",
            "background:", if (active) col else "#f0f4f8", ";",
            "color:", if (active) "white" else col, ";",
            "border-radius:20px; padding:5px 14px; font-size:0.78rem;",
            "font-weight:600; margin:3px; cursor:pointer;",
            "border:2px solid ", col, ";"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority:'event'})",
            ns("type_chip_click"), gsub("'", "\\\\'", tp)
          ),
          tags$span(
            style = paste0(
              "display:inline-flex; align-items:center; justify-content:center;",
              "background:", if (active) "rgba(255,255,255,.3)" else col, ";",
              "color:", if (active) col else "white", ";",
              "border-radius:50%; width:20px; height:20px;",
              "font-size:0.72rem; font-weight:700;"
            ),
            cnt
          ),
          tp
        )
      })

      div(
        style = "margin-bottom:14px;",
        div(
          style = "font-size:0.7rem; font-weight:700; text-transform:uppercase;
                   letter-spacing:.08em; color:#6c757d; margin-bottom:6px;",
          tags$i(class = "bi bi-funnel me-1"), "Filter by Assessment Type"
        ),
        div(style = "display:flex; flex-wrap:wrap; align-items:center;",
            all_chip, type_chips)
      )
    })

    # ── 2c. Plus / delta table ────────────────────────────────────────────────
    output$pd_table <- DT::renderDataTable({
      req(pd_data())
      df <- pd_data()
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No written feedback matches the current filters."),
          options  = list(dom = "t"), rownames = FALSE
        ))
      }
      display <- df %>% dplyr::select(Date, Type, Level, Faculty, Plus, Delta)
      DT::datatable(
        display,
        rownames  = FALSE,
        selection = "single",
        options   = list(
          pageLength = 10, scrollX = TRUE,
          dom = "tp", ordering = FALSE,
          columnDefs = list(
            list(className = "dt-center", targets = c(0, 1, 2, 3)),
            list(width = "28%", targets = c(4, 5))
          )
        ),
        class = "table table-sm table-hover"
      ) %>%
        DT::formatStyle("Plus",
          backgroundColor = "#e8f5e9", borderLeft = "3px solid #27ae60") %>%
        DT::formatStyle("Delta",
          backgroundColor = "#fff3e0", borderLeft = "3px solid #e67e22") %>%
        DT::formatStyle("Date",
          fontWeight = "bold", color = "#003d5c") %>%
        DT::formatStyle("Type",
          fontWeight = "600", color = "#0066a1") %>%
        DT::formatStyle("Level",
          fontWeight = "600", color = "#6c3483")
    })

    # ── 2d. Score detail on row click ─────────────────────────────────────────
    output$score_detail <- renderUI({
      row_idx <- input$pd_table_rows_selected
      if (is.null(row_idx) || length(row_idx) == 0) {
        return(tags$p(
          style = "font-size:0.78rem; color:#0066a1; margin-top:10px; font-style:italic;",
          tags$i(class = "bi bi-hand-index me-1"),
          "Select a row above to see the full assessment scores for that evaluation."
        ))
      }
      sel_row <- pd_data() %>% dplyr::slice(row_idx)
      if (nrow(sel_row) == 0) return(NULL)

      inst_id  <- sel_row$redcap_repeat_instance[1]
      eval_row <- assessment_data() %>%
        dplyr::filter(redcap_repeat_instance == inst_id,
                      record_id == !!record_id())
      if (nrow(eval_row) == 0) return(NULL)

      dd <- data_dict()
      if (is.null(dd) || nrow(dd) == 0) return(NULL)

      meta_skip <- c("record_id","redcap_repeat_instrument","redcap_repeat_instance",
                     "ass_date","ass_level","ass_plus","ass_delta","ass_faculty",
                     "ass_specialty","ass_rotator","ass_cc_quart","ass_obs_type",
                     "fac_eval_level","q_level","source_form")

      item_fields <- dd %>%
        dplyr::filter(
          grepl("^ass_", field_name),
          !field_name %in% meta_skip,
          field_type %in% c("radio", "dropdown")
        )

      scored <- item_fields %>%
        dplyr::filter(field_name %in% names(eval_row)) %>%
        dplyr::rowwise() %>%
        dplyr::filter({
          v <- eval_row[[field_name]][1]
          !is.na(v) && as.character(v) != "" && as.character(v) != "0"
        }) %>%
        dplyr::ungroup()

      if (nrow(scored) == 0) {
        return(div(
          style = "margin-top:10px; padding:10px; background:#f8f9fa;
                   border-radius:6px; font-size:0.83rem; color:#6c757d;",
          tags$i(class = "bi bi-dash-circle me-1"),
          "No scored items recorded for this evaluation."
        ))
      }

      badges <- lapply(seq_len(nrow(scored)), function(i) {
        fn    <- scored$field_name[i]
        label <- scored$field_label[i]
        val   <- as.character(eval_row[[fn]][1])
        cm    <- parse_redcap_choices(scored$select_choices_or_calculations[i])
        val_label <- if (length(cm) > 0 && val %in% names(cm)) cm[[val]] else val
        n_choices <- length(cm)
        pos       <- if (length(cm) > 0) which(names(cm) == val) else NA_integer_
        bg_col <- if (!is.na(pos) && n_choices > 0) {
          pal <- grDevices::colorRampPalette(
            c("#c0392b","#e67e22","#f9e79f","#27ae60","#1a6b3a")
          )(n_choices)
          pal[pos]
        } else "#aaa"
        fg_col <- if (!is.na(pos) && pos == ceiling(n_choices / 2) && n_choices >= 3)
          "#555" else "#fff"
        div(
          style = "display:inline-block; margin:4px;",
          div(
            style = paste0(
              "background:", bg_col, "; color:", fg_col, ";",
              "border-radius:6px; padding:6px 12px;",
              "font-size:0.78rem; font-weight:600;",
              "max-width:220px; text-align:center;"
            ),
            tags$div(
              style = "opacity:0.8; font-size:0.68rem; font-weight:400;
                       white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
              label
            ),
            tags$div(val_label)
          )
        )
      })

      div(
        style = paste0("margin-top:14px; padding:14px 16px;",
                       "background:#f8fbff; border-radius:6px;",
                       "border:1px solid #dde5ed;"),
        div(
          style = "font-size:0.72rem; font-weight:700; text-transform:uppercase;
                   letter-spacing:.08em; color:#6c757d; margin-bottom:6px;",
          tags$i(class = "bi bi-award me-1"),
          paste0("Scores \u2014 ", sel_row$Date[1],
                 " \u00b7 ", sel_row$Type[1],
                 " \u00b7 ", sel_row$Faculty[1])
        ),
        tags$p(
          style = "font-size:0.75rem; color:#6c757d; margin-bottom:8px;",
          "Each badge shows one assessed item. Colour runs ",
          tags$span(style = "color:#c0392b; font-weight:600;", "red"),
          " (below expectations) \u2192 ",
          tags$span(style = "color:#27ae60; font-weight:600;", "green"),
          " (attending level)."
        ),
        div(style = "display:flex; flex-wrap:wrap;", badges),
        tags$button(
          style   = "margin-top:8px; font-size:0.72rem;",
          class   = "btn btn-sm btn-outline-secondary py-0",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
            ns("close_scores")
          ),
          "\u00d7 Close"
        )
      )
    })

    observeEvent(input$close_scores, {
      DT::selectRows(DT::dataTableProxy("pd_table"), NULL)
    })

  })
}

