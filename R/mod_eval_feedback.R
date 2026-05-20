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
          tags$i(class = "bi bi-table me-2"),
          tags$span(style = "font-weight:700;", "Assessment History & Scores"),
          tags$span(
            style = "font-size:0.75rem; opacity:0.7; margin-left:10px;",
            "Click a row to expand rubric scores for that evaluation"
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
            "Use the filters below to narrow by training level or assessment type. ",
            "The table shows every assessment. ",
            "The ", tags$strong("Scores"), " column shows how many rubric items were rated \u2014 click any row to expand them. ",
            tags$strong("Plus "), "(\u2795) and ", tags$strong("Delta "), "(\u25b3) show written feedback when provided."
          )
        ),

        # PGY filter
        uiOutput(ns("pgy_chips")),

        # Type filter
        uiOutput(ns("type_chips")),

        # Table
        DT::dataTableOutput(ns("pd_table")),

        # Score detail header / prompt
        uiOutput(ns("score_detail")),

        # Score chart — plotly bar chart rendered on row click
        plotly::plotlyOutput(ns("score_chart"), height = "auto")
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

    # ── Detect assessment type from scored field-name prefixes ────────────────
    #
    # REDCap stores assessment sub-type in the PREFIX of scored-item columns:
    #   ass_obs_type set          → Direct Observation (sub-type from obs_map)
    #   ass_int_ip_*  scored      → Intern Inpatient
    #   ass_res_ip_*  scored      → Resident Inpatient
    #   ass_bridge_*  scored      → Bridge Clinic
    #   ass_cons_*    scored      → Consult
    #   ass_day_*     scored      → Single Clinic Day
    #   ass_cc_*      scored / ass_cc_quart set → Continuity Clinic
    #
    # Returns a character vector of generic type labels (no specialty prefix).
    .detect_assessment_type <- function(df, obs_map) {
      n  <- nrow(df)
      if (n == 0) return(character(0))
      out <- rep(NA_character_, n)
      cn  <- names(df)

      # Which rows have >= 1 non-NA, non-empty, non-"0" value in columns
      # whose names start with `prefix`.
      scored_in <- function(prefix) {
        cols <- cn[startsWith(cn, prefix)]
        if (length(cols) == 0) return(rep(FALSE, n))
        apply(df[, cols, drop = FALSE], 1, function(r) {
          vals <- trimws(as.character(r))
          any(!is.na(r) & nzchar(vals) & vals != "0")
        })
      }

      # 1. Direct observation: ass_obs_type set → decode via data-dict map
      if ("ass_obs_type" %in% cn) {
        ot      <- trimws(as.character(df$ass_obs_type))
        has_ot  <- !is.na(df$ass_obs_type) & nzchar(ot)
        dec_ot  <- dplyr::if_else(
          has_ot & ot %in% names(obs_map),
          obs_map[ot],
          dplyr::if_else(has_ot, "Direct Observation", NA_character_)
        )
        out[is.na(out) & has_ot] <- dec_ot[is.na(out) & has_ot]
      }

      # 2. Inpatient forms
      out[is.na(out) & scored_in("ass_int_ip_")] <- "Intern Inpatient"
      out[is.na(out) & scored_in("ass_res_ip_")] <- "Resident Inpatient"

      # 3. Other rotation types
      out[is.na(out) & scored_in("ass_bridge_")]  <- "Bridge Clinic"
      out[is.na(out) & scored_in("ass_cons_")]    <- "Consult"
      out[is.na(out) & scored_in("ass_day_")]     <- "Single Clinic Day"

      # 4. Continuity Clinic (scored CC items or cc_quart set)
      has_cc <- scored_in("ass_cc_")
      if ("ass_cc_quart" %in% cn) {
        has_cc <- has_cc | (!is.na(df$ass_cc_quart) &
                              nzchar(trimws(as.character(df$ass_cc_quart))))
      }
      out[is.na(out) & has_cc] <- "Continuity Clinic"

      # 5. Fallback: use ass_specialty as the type label.
      #    Many assessments only have narrative plus/delta without rubric scores;
      #    the specialty field (e.g. "Hospitalist", "GI", "Nephrology") gives
      #    meaningful context where rubric-based detection cannot fire.
      if ("ass_specialty" %in% cn) {
        spec_raw <- trimws(as.character(ifelse(is.na(df$ass_specialty), "", df$ass_specialty)))
        spec_tc  <- tools::toTitleCase(tolower(spec_raw))
        still_na <- is.na(out)
        out[still_na & nzchar(spec_tc)] <- spec_tc[still_na & nzchar(spec_tc)]
      }

      out[is.na(out)] <- "General Assessment"
      out
    }

    # ── Combine type with specialty for the plus/delta table ──────────────────
    # For structural rotation types (those detected via rubric scores), prepend
    # the specialty: "Intern Inpatient" + "GI" → "GI Intern Inpatient".
    # For types that are ALREADY the specialty (fallback path above), or for
    # Continuity Clinic / obs sub-types, leave the label unchanged.
    STRUCTURAL_ROTATION_TYPES <- c(
      "Intern Inpatient", "Resident Inpatient",
      "Bridge Clinic", "Consult", "Single Clinic Day"
    )
    .apply_specialty <- function(type_vec, specialty_vec) {
      if (is.null(specialty_vec)) return(type_vec)
      spec <- trimws(as.character(ifelse(is.na(specialty_vec), "", specialty_vec)))
      spec <- tools::toTitleCase(tolower(spec))
      is_structural <- type_vec %in% STRUCTURAL_ROTATION_TYPES
      dplyr::if_else(is_structural & nzchar(spec), paste(spec, type_vec), type_vec)
    }

    # ── 1. Assessment type count buttons ─────────────────────────────────────
    output$type_buttons <- renderUI({
      req(my_evals(), data_dict())
      df      <- my_evals()
      obs_map <- obs_type_map_r()

      # Generic types detected from field prefixes (no specialty prefix on tiles —
      # keeps the grid compact and comparable across residents)
      type_vec <- .detect_assessment_type(df, obs_map)

      # Seed the grid with all known obs-type labels + structural rotation types
      # so tiles with 0 count appear in grey (programme expectation view).
      STRUCTURAL_TYPES <- c(
        "Intern Inpatient", "Resident Inpatient",
        "Bridge Clinic", "Consult", "Single Clinic Day", "Continuity Clinic"
      )
      all_types <- unique(c(
        if (length(obs_map) > 0) unname(obs_map) else character(0),
        STRUCTURAL_TYPES
      ))

      actual <- data.frame(Type = type_vec, stringsAsFactors = FALSE) %>%
        dplyr::count(Type)
      # Add any types from data not already in all_types (e.g. "General Assessment")
      extra <- setdiff(actual$Type, all_types)
      if (length(extra) > 0) all_types <- c(all_types, extra)

      counts <- data.frame(Type = all_types, stringsAsFactors = FALSE) %>%
        dplyr::left_join(actual, by = "Type") %>%
        dplyr::mutate(n = ifelse(is.na(n), 0L, as.integer(n))) %>%
        dplyr::arrange(dplyr::desc(n > 0), dplyr::desc(n), Type)

      done_rows <- which(counts$n > 0)
      sel_type  <- selected_type()
      buttons <- lapply(seq_len(nrow(counts)), function(i) {
        tp      <- counts$Type[i]
        cnt     <- counts$n[i]
        done    <- cnt > 0
        is_sel  <- nzchar(sel_type) && sel_type == tp
        col     <- if (done)
          TYPE_PAL[((match(i, done_rows) - 1L) %% length(TYPE_PAL)) + 1L]
        else "#c8d3dd"
        # Clickable only when there is data; selected tile gets ring highlight
        onclick_js <- if (done)
          sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                  ns("type_chip_click"), gsub("'", "\\'", tp))
        else NULL
        div(
          class = "col-6 col-sm-4 col-md-3 col-lg-2 mb-2",
          div(
            style = paste0(
              "border:2px solid ", col, "; border-radius:8px;",
              "padding:10px 8px; text-align:center; height:100%;",
              if (done) paste0("background:", col, "18;") else "background:#f7f9fb;",
              if (is_sel) paste0("box-shadow:0 0 0 3px ", col, ";") else "",
              if (done) "cursor:pointer;" else ""
            ),
            onclick = onclick_js,
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

    # ── pd_data_all (all assessments — not filtered to narrative-only) ────────
    pd_data_all <- reactive({
      req(my_evals())
      df      <- my_evals()
      obs_map <- obs_type_map_r()
      dd      <- data_dict()

      # Detect type from field prefixes, then prepend specialty for context
      spec_vec <- if ("ass_specialty" %in% names(df)) df$ass_specialty else NULL
      type_vec <- .apply_specialty(.detect_assessment_type(df, obs_map), spec_vec)

      df %>%
        dplyr::mutate(.Type = type_vec) %>%
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
                                   trimws(ass_plus),  "\u2014"),
          Delta   = dplyr::if_else(!is.na(ass_delta) & nzchar(trimws(ass_delta)),
                                   trimws(ass_delta), "\u2014")
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

    # ── 2c. Assessment table ──────────────────────────────────────────────────
    output$pd_table <- DT::renderDataTable({
      req(pd_data())
      df <- pd_data()
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No assessments match the current filters."),
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
            list(width = "25%", targets = c(4, 5))
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

    # ── Helper: build scored-items data frame for the selected row ────────────
    .build_scored_df <- function(row_idx) {
      if (is.null(row_idx) || length(row_idx) == 0) return(NULL)
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
        ) %>%
        dplyr::filter(field_name %in% names(eval_row))

      rows <- lapply(seq_len(nrow(item_fields)), function(i) {
        fn  <- item_fields$field_name[i]
        lbl <- item_fields$field_label[i]
        val <- as.character(eval_row[[fn]][1])
        if (is.na(val) || !nzchar(val) || val == "0") return(NULL)
        cm  <- parse_redcap_choices(item_fields$select_choices_or_calculations[i])
        n   <- length(cm)
        pos <- if (n > 0 && val %in% names(cm)) which(names(cm) == val) else NA_integer_
        val_lbl <- if (n > 0 && val %in% names(cm)) unname(cm[val]) else val
        # Mark "unable to evaluate" type responses for separate colouring
        is_na_resp <- grepl("unable|cannot|n/a|not observed|not applicable",
                            val_lbl, ignore.case = TRUE)
        # Shorten label: strip trailing parenthetical qualifiers
        short_lbl <- gsub("\\s*\\(.*?\\)", "", lbl)
        short_lbl <- trimws(gsub("\\s+", " ", short_lbl))
        data.frame(
          field_name = fn,
          label      = short_lbl,
          full_label = lbl,
          val_label  = val_lbl,
          pos        = pos,
          n_choices  = n,
          norm_pos   = if (!is.na(pos) && n > 1) (pos - 1) / (n - 1) else 0,
          is_na_resp = is_na_resp,
          stringsAsFactors = FALSE
        )
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(NULL)
      df <- do.call(rbind, rows)
      df[!is.na(df$pos), ]
    }

    # ── 2d. Score detail header (prompt / metadata above chart) ───────────────
    output$score_detail <- renderUI({
      row_idx <- input$pd_table_rows_selected
      if (is.null(row_idx) || length(row_idx) == 0) {
        return(tags$p(
          style = "font-size:0.78rem; color:#0066a1; margin-top:10px; font-style:italic;",
          tags$i(class = "bi bi-hand-index me-1"),
          "Select a row above to see the scored items for that evaluation."
        ))
      }
      sel_row <- pd_data() %>% dplyr::slice(row_idx)
      if (nrow(sel_row) == 0) return(NULL)

      scored_df <- .build_scored_df(row_idx)
      n_scored  <- if (!is.null(scored_df)) nrow(scored_df) else 0

      div(
        style = paste0("margin-top:14px; padding:12px 16px;",
                       "background:#f8fbff; border-radius:6px 6px 0 0;",
                       "border:1px solid #dde5ed; border-bottom:none;"),
        div(
          class = "d-flex justify-content-between align-items-start",
          div(
            div(
              style = "font-size:0.72rem; font-weight:700; text-transform:uppercase;
                       letter-spacing:.08em; color:#6c757d; margin-bottom:3px;",
              tags$i(class = "bi bi-bar-chart-fill me-1"),
              paste0(sel_row$Date[1], " \u00b7 ", sel_row$Type[1],
                     " \u00b7 ", sel_row$Faculty[1])
            ),
            if (n_scored == 0) {
              tags$span(
                style = "font-size:0.78rem; color:#6c757d; font-style:italic;",
                tags$i(class = "bi bi-dash-circle me-1"),
                "No rubric items recorded \u2014 narrative feedback only."
              )
            } else {
              tags$span(
                style = "font-size:0.78rem; color:#1a6b3a;",
                tags$i(class = "bi bi-check-circle me-1"),
                paste0(n_scored, " rated item", if (n_scored != 1) "s" else "",
                       ". Bars run red \u2192 green (low \u2192 high)."),
                " Hover for exact choice."
              )
            }
          ),
          tags$button(
            style   = "font-size:0.72rem; margin-left:12px; flex-shrink:0;",
            class   = "btn btn-sm btn-outline-secondary py-0",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("close_scores")
            ),
            "\u00d7 Close"
          )
        )
      )
    })

    # ── 2e. Score chart: type-aggregate or per-instance ──────────────────────
    #
    # Priority:
    #   1. Row clicked       → per-instance bar chart
    #   2. Type selected     → aggregate bar chart (avg across all instances)
    #   3. Neither           → empty
    output$score_chart <- plotly::renderPlotly({
      row_idx  <- input$pd_table_rows_selected
      type_sel <- selected_type()

      dd <- data_dict()
      empty_plot <- plotly::plotly_empty() %>% plotly::config(displayModeBar = FALSE)
      if (is.null(dd) || nrow(dd) == 0) return(empty_plot)

      meta_skip <- c("record_id","redcap_repeat_instrument","redcap_repeat_instance",
                     "ass_date","ass_level","ass_plus","ass_delta","ass_faculty",
                     "ass_specialty","ass_rotator","ass_cc_quart","ass_obs_type",
                     "fac_eval_level","q_level","source_form")

      # ── Inner helper: extract scored items from one eval_row ──────────────
      .get_items <- function(eval_row) {
        item_fields <- dd %>%
          dplyr::filter(grepl("^ass_", field_name),
                        !field_name %in% meta_skip,
                        field_type %in% c("radio", "dropdown")) %>%
          dplyr::filter(field_name %in% names(eval_row))
        rows <- lapply(seq_len(nrow(item_fields)), function(j) {
          fn  <- item_fields$field_name[j]
          lbl <- item_fields$field_label[j]
          val <- as.character(eval_row[[fn]][1])
          if (is.na(val) || !nzchar(val) || val == "0") return(NULL)
          cm  <- parse_redcap_choices(item_fields$select_choices_or_calculations[j])
          n   <- length(cm)
          pos <- if (n > 0 && val %in% names(cm)) which(names(cm) == val) else NA_integer_
          if (is.na(pos)) return(NULL)
          val_lbl  <- unname(cm[val])
          is_na_r  <- grepl("unable|cannot|n/a|not observed|not applicable",
                            val_lbl, ignore.case = TRUE)
          short_lbl <- trimws(gsub("\\s+", " ", gsub("\\s*\\(.*?\\)", "", lbl)))
          data.frame(field_name  = fn,
                     label       = short_lbl,
                     val_label   = val_lbl,
                     choices_str = item_fields$select_choices_or_calculations[j],
                     pos         = pos,
                     n_choices   = n,
                     norm_pos    = if (n > 1) (pos - 1) / (n - 1) else 0,
                     is_na_resp  = is_na_r,
                     stringsAsFactors = FALSE)
        })
        rows <- rows[!sapply(rows, is.null)]
        if (length(rows) == 0) return(NULL)
        do.call(rbind, rows)
      }

      # ── Build the chart data frame (per-instance OR aggregate) ────────────
      if (!is.null(row_idx) && length(row_idx) > 0) {
        # ── PER-INSTANCE ──────────────────────────────────────────────────
        sel_row  <- pd_data() %>% dplyr::slice(row_idx)
        inst_id  <- sel_row$redcap_repeat_instance[1]
        eval_row <- assessment_data() %>%
          dplyr::filter(redcap_repeat_instance == inst_id,
                        record_id == !!record_id())
        if (nrow(eval_row) == 0) return(empty_plot)
        chart_df <- .get_items(eval_row)
        if (is.null(chart_df) || nrow(chart_df) == 0) return(empty_plot)
        chart_df$hover <- paste0(
          "<b>", chart_df$label, "</b><br>",
          chart_df$val_label, "<br>",
          "<span style=\'color:#999\'>", chart_df$pos, " of ",
          chart_df$n_choices, "</span>"
        )
        subtitle <- paste0(sel_row$Date[1], " \u00b7 ", sel_row$Faculty[1])

      } else if (nzchar(type_sel)) {
        # ── TYPE AGGREGATE ────────────────────────────────────────────────
        df_type <- pd_data_all() %>% dplyr::filter(Type == type_sel)
        if (nrow(df_type) == 0) return(empty_plot)

        all_items <- lapply(seq_len(nrow(df_type)), function(i) {
          inst_id  <- df_type$redcap_repeat_instance[i]
          eval_row <- assessment_data() %>%
            dplyr::filter(redcap_repeat_instance == inst_id,
                          record_id == !!record_id())
          if (nrow(eval_row) == 0) return(NULL)
          .get_items(eval_row)
        })
        all_items <- all_items[!sapply(all_items, is.null)]
        if (length(all_items) == 0) return(empty_plot)
        all_df <- do.call(rbind, all_items)

        chart_df <- all_df %>%
          dplyr::group_by(field_name, label) %>%
          dplyr::summarise(
            norm_pos    = mean(norm_pos,  na.rm = TRUE),
            n_choices   = max(n_choices),
            choices_str = dplyr::first(choices_str),
            n_obs       = dplyr::n(),
            is_na_resp  = all(is_na_resp),
            .groups     = "drop"
          ) %>%
          dplyr::mutate(
            pos    = round(norm_pos * (n_choices - 1)) + 1,
            hover  = paste0(
              "<b>", label, "</b><br>",
              round(norm_pos * 100), "% of scale<br>",
              "<span style=\'color:#999\'>", n_obs, " assessment",
              dplyr::if_else(n_obs > 1, "s averaged", ""), "</span>"
            )
          ) %>%
          as.data.frame()
        n_assessed <- nrow(df_type)
        subtitle <- paste0(
          n_assessed, " assessment", if (n_assessed != 1) "s" else "",
          " \u2014 bars show average score"
        )

      } else {
        return(empty_plot)
      }

      # ── Build axis ticks from the dominant scale in chart_df ─────────────
      # Pick the most common n_choices; use its choice labels as tick text.
      modal_n <- as.integer(
        names(sort(table(chart_df$n_choices), decreasing = TRUE))[1]
      )
      rep_str <- chart_df$choices_str[
        !is.na(chart_df$choices_str) & chart_df$n_choices == modal_n
      ][1]
      cm_ticks   <- if (!is.na(rep_str)) parse_redcap_choices(rep_str) else list()
      n_ticks    <- length(cm_ticks)
      tick_vals  <- if (n_ticks > 1) seq(0, 1, length.out = n_ticks) else c(0, 1)
      # Truncate long labels for the axis (full label still in hover)
      trunc_lbl  <- function(s, n = 20)
        ifelse(nchar(s) > n, paste0(substr(s, 1, n - 1), "…"), s)
      tick_text  <- if (n_ticks > 0) trunc_lbl(unname(cm_ticks)) else c("", "")

      # ── Render the chart ──────────────────────────────────────────────────
      pal_fn <- grDevices::colorRampPalette(
        c("#c0392b","#e67e22","#f1c40f","#27ae60","#1a6b3a")
      )
      chart_df$bar_color <- ifelse(
        chart_df$is_na_resp, "#b0bec5",
        pal_fn(100)[pmin(100, pmax(1, round(chart_df$norm_pos * 99) + 1))]
      )

      # Sort ascending so lowest-scoring items appear at bottom
      chart_df <- chart_df[order(chart_df$norm_pos), ]
      chart_df$label <- factor(chart_df$label, levels = chart_df$label)

      n_items <- nrow(chart_df)
      height  <- max(180, n_items * 38 + 90)

      plotly::plot_ly(
        data          = chart_df,
        x             = ~norm_pos,
        y             = ~label,
        type          = "bar",
        orientation   = "h",
        marker        = list(
          color = ~bar_color,
          line  = list(color = "rgba(255,255,255,0.5)", width = 1)
        ),
        hovertemplate = ~paste0(hover, "<extra></extra>"),
        showlegend    = FALSE
      ) %>%
        plotly::layout(
          title  = list(
            text     = subtitle,
            font     = list(size = 11, color = "#6c757d"),
            x        = 0,
            xanchor  = "left",
            pad      = list(l = 8)
          ),
          xaxis  = list(
            title      = "",
            range      = c(-0.02, 1.05),
            tickvals   = tick_vals,
            ticktext   = tick_text,
            tickangle  = -30,
            showgrid   = TRUE,
            gridcolor  = "#eee",
            zeroline   = FALSE,
            fixedrange = TRUE
          ),
          yaxis  = list(title = "", automargin = TRUE, fixedrange = TRUE),
          margin = list(l = 8, r = 20, t = 30, b = 80),
          paper_bgcolor = "rgba(248,251,255,1)",
          plot_bgcolor  = "rgba(248,251,255,1)",
          height = height,
          shapes = list(
            list(type = "line", x0 = 0.5, x1 = 0.5,
                 y0 = -0.5, y1 = n_items - 0.5,
                 line = list(color = "#ccc", dash = "dot", width = 1))
          )
        ) %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    observeEvent(input$close_scores, {
      DT::selectRows(DT::dataTableProxy("pd_table"), NULL)
    })

  })
}

