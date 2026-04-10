# ============================================================================
# EVALUATION TABLE MODULE
# Resident-facing table: one row per evaluation, dynamic item columns,
# click-to-expand written feedback (plus / delta).
# ============================================================================

# Metadata fields that are never shown as item columns
.eval_table_meta <- c(
  "record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
  "ass_date", "ass_level", "ass_plus", "ass_delta",
  "ass_faculty", "ass_specialty", "ass_rotator", "ass_cc_quart",
  "ass_obs_type", "fac_eval_level", "q_level", "source_form",
  "ass_grade", "ass_levelnum"
)

#' Evaluation Table UI
#' @param id Module namespace
#' @export
mod_eval_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .eval-feedback-panel {
        background: #fff;
        border-left: 4px solid #0066a1;
        border-radius: 0 6px 6px 0;
        padding: 16px 20px;
        margin-top: 12px;
        animation: evFadeIn .18s ease;
      }
      @keyframes evFadeIn {
        from { opacity:0; transform:translateY(-4px); }
        to   { opacity:1; transform:translateY(0); }
      }
      .fb-section-label {
        font-size: 0.68rem; font-weight: 700;
        text-transform: uppercase; letter-spacing: .08em;
        color: #6c757d; margin-bottom: 5px;
      }
      .fb-plus  { color: #1a6b3a; font-size: 0.88rem; line-height: 1.55; }
      .fb-delta { color: #c0392b; font-size: 0.88rem; line-height: 1.55; }
      .eval-select-hint {
        font-size: 0.78rem; color: #0066a1; margin-top: 8px; font-style: italic;
      }
      /* Keep selected row highlighted even after focus moves */
      table.dataTable tbody tr.selected td { background: #e8f0f7 !important; }
    ")),
    DT::dataTableOutput(ns("tbl")),
    uiOutput(ns("feedback"))
  )
}

#' Evaluation Table Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive — full assessment data frame (already filtered to
#'   \code{redcap_repeat_instrument == "assessment"})
#' @param record_id Reactive — logged-in resident's record_id
#' @param data_dict Plain data frame from the REDCap data dictionary
#' @export
mod_eval_table_server <- function(id, rdm_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Identify scale item fields from data dict ─────────────────────────────
    item_meta <- (function() {
      if (is.null(data_dict) || nrow(data_dict) == 0) return(data.frame())
      data_dict %>%
        dplyr::filter(
          grepl("^ass_", field_name),
          !field_name %in% .eval_table_meta,
          field_type %in% c("radio", "dropdown")
        )
    })()

    # Build choice map per field: field_name -> c("1"="Label 1", ...)
    choice_maps <- (function() {
      if (nrow(item_meta) == 0) return(list())
      maps <- lapply(seq_len(nrow(item_meta)), function(i) {
        parse_redcap_choices_internal(
          item_meta$select_choices_or_calculations[i]
        )
      })
      setNames(maps, item_meta$field_name)
    })()

    # Build label lookup: field_name -> readable label
    field_labels <- if (nrow(item_meta) > 0)
      setNames(item_meta$field_label, item_meta$field_name)
    else
      character(0)

    # ── Filtered assessments for this resident ────────────────────────────────
    my_evals <- reactive({
      req(rdm_data(), record_id())
      rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(trimws(redcap_repeat_instrument)) == "assessment"
        ) %>%
        dplyr::arrange(dplyr::desc(ass_date))
    })

    # ── Build display table ───────────────────────────────────────────────────
    wide_tbl <- reactive({
      req(my_evals())
      df <- my_evals()
      if (nrow(df) == 0) return(NULL)

      # Which item fields exist AND are non-trivially filled?
      candidate_fields <- intersect(item_meta$field_name, names(df))
      filled_fields <- candidate_fields[sapply(candidate_fields, function(f) {
        vals <- df[[f]]
        any(!is.na(vals) & as.character(vals) != "" & as.character(vals) != "0")
      })]

      # Map numeric codes → labels for each filled field
      df_mapped <- df
      for (fn in filled_fields) {
        cm <- choice_maps[[fn]]
        if (length(cm) > 0) {
          raw_vals <- as.character(df_mapped[[fn]])
          df_mapped[[fn]] <- dplyr::coalesce(cm[raw_vals], raw_vals)
        }
      }

      # Rename fields to readable labels
      col_rename <- setNames(
        filled_fields,
        ifelse(filled_fields %in% names(field_labels),
               field_labels[filled_fields],
               filled_fields)
      )
      if (length(col_rename) > 0) {
        df_mapped <- dplyr::rename(df_mapped, !!!col_rename)
      }
      item_display_names <- names(col_rename)

      # Level label
      level_label <- dplyr::case_when(
        as.character(df_mapped$ass_level) %in% c("1", "Intern") ~ "Intern",
        as.character(df_mapped$ass_level) %in% c("2", "PGY2")   ~ "PGY2",
        as.character(df_mapped$ass_level) %in% c("3", "PGY3")   ~ "PGY3",
        TRUE ~ as.character(df_mapped$ass_level)
      )

      df_display <- df_mapped %>%
        dplyr::mutate(
          .row_idx  = dplyr::row_number(),
          Date      = format(as.Date(ass_date), "%b %d, %Y"),
          Level     = level_label,
          Rotation  = dplyr::coalesce(
            if ("ass_rotator"  %in% names(.)) ass_rotator  else NA_character_,
            if ("ass_specialty" %in% names(.)) ass_specialty else NA_character_,
            "\u2014"
          ),
          Faculty   = dplyr::coalesce(ass_faculty, "\u2014")
        )

      display_cols <- c("Date", "Level", "Rotation", "Faculty",
                        item_display_names)
      display_cols <- display_cols[display_cols %in% names(df_display)]

      list(
        display      = df_display %>% dplyr::select(all_of(display_cols)),
        full         = df_display,         # includes ass_plus, ass_delta, .row_idx
        item_cols    = item_display_names,
        choice_maps  = choice_maps,
        field_labels = field_labels
      )
    })

    # ── Render table ──────────────────────────────────────────────────────────
    output$tbl <- DT::renderDataTable({
      wt <- wide_tbl()
      if (is.null(wt)) {
        return(DT::datatable(
          data.frame(Message = "No assessments found."),
          options  = list(dom = "t"),
          rownames = FALSE
        ))
      }

      dt <- DT::datatable(
        wt$display,
        rownames  = FALSE,
        selection = "single",
        options   = list(
          pageLength  = 10,
          scrollX     = TRUE,
          dom         = "tp",
          ordering    = FALSE,
          columnDefs  = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "table table-sm table-hover"
      ) %>%
        DT::formatStyle("Date",
          fontWeight = "bold", color = "#003d5c") %>%
        DT::formatStyle("Level",
          fontWeight = "600", color = "#6c3483") %>%
        DT::formatStyle("Rotation",
          fontWeight = "600", color = "#0066a1") %>%
        DT::formatStyle("Faculty",
          color = "#555")

      # Per-column color styling for item columns
      for (col in wt$item_cols) {
        if (!col %in% names(wt$display)) next
        # Build label→color map for this column
        unique_vals <- unique(na.omit(wt$display[[col]]))
        unique_vals <- unique_vals[unique_vals != ""]
        if (length(unique_vals) == 0) next

        # Find the choice map for this column (reverse-lookup via field_labels)
        fn <- names(wt$field_labels)[wt$field_labels == col]
        cm <- if (length(fn) > 0 && fn[1] %in% names(wt$choice_maps))
          wt$choice_maps[[fn[1]]] else character(0)

        if (length(cm) > 0) {
          # Order by numeric code position
          ordered_labels <- unname(cm)
          n <- length(ordered_labels)
          bg_pal <- grDevices::colorRampPalette(
            c("#c0392b", "#e67e22", "#f9e79f", "#27ae60", "#1a6b3a")
          )(n)
          fg_pal <- ifelse(
            seq_len(n) == ceiling(n / 2) & n >= 3, "#555555", "#ffffff"
          )
          present <- ordered_labels[ordered_labels %in% unique_vals]
          if (length(present) > 0) {
            bg_map <- setNames(bg_pal, ordered_labels)
            fg_map <- setNames(fg_pal, ordered_labels)
            dt <- dt %>%
              DT::formatStyle(
                col,
                fontWeight      = "600",
                fontSize        = "0.78rem",
                backgroundColor = DT::styleEqual(present, bg_map[present]),
                color           = DT::styleEqual(present, fg_map[present])
              )
          }
        }
      }

      dt
    })

    # ── Feedback panel ────────────────────────────────────────────────────────
    output$feedback <- renderUI({
      row_idx <- input$tbl_rows_selected
      wt      <- wide_tbl()

      if (is.null(row_idx) || is.null(wt)) {
        return(tags$p(
          class = "eval-select-hint",
          tags$i(class = "bi bi-hand-index me-1"),
          "Select a row to see written feedback."
        ))
      }

      sel <- wt$full %>% dplyr::slice(row_idx)
      if (nrow(sel) == 0) return(NULL)

      plus_text  <- if ("ass_plus"  %in% names(sel)) sel$ass_plus[1]  else NA
      delta_text <- if ("ass_delta" %in% names(sel)) sel$ass_delta[1] else NA
      has_plus   <- !is.na(plus_text)  && nzchar(trimws(plus_text))
      has_delta  <- !is.na(delta_text) && nzchar(trimws(delta_text))

      if (!has_plus && !has_delta) {
        return(div(
          class = "eval-feedback-panel",
          tags$p(class = "text-muted mb-0 fst-italic",
                 "No written feedback recorded for this evaluation.")
        ))
      }

      div(
        class = "eval-feedback-panel",
        # Header
        div(
          class = "d-flex justify-content-between align-items-start mb-3",
          tags$span(
            style = "font-weight:700; color:#003d5c; font-size:0.88rem;",
            if ("Date" %in% names(sel)) sel$Date[1] else "",
            if ("Rotation" %in% names(sel) && !is.na(sel$Rotation[1]))
              paste0(" \u00b7 ", sel$Rotation[1]),
            if ("Faculty" %in% names(sel) && !is.na(sel$Faculty[1]))
              paste0(" \u00b7 ", sel$Faculty[1])
          ),
          tags$button(
            class   = "btn btn-sm btn-outline-secondary py-0 px-2",
            style   = "font-size:0.72rem;",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("close_feedback")
            ),
            "\u00d7 Close"
          )
        ),
        # Plus / Delta
        div(
          class = "row g-3",
          div(
            class = "col-md-6",
            div(class = "fb-section-label",
                tags$i(class = "bi bi-plus-circle-fill me-1",
                       style = "color:#27ae60;"),
                "Strengths"),
            if (has_plus)
              tags$p(class = "fb-plus mb-0", plus_text)
            else
              tags$p(class = "text-muted fst-italic mb-0 small",
                     "None recorded")
          ),
          div(
            class = "col-md-6",
            div(class = "fb-section-label",
                tags$i(class = "bi bi-arrow-up-circle-fill me-1",
                       style = "color:#e67e22;"),
                "Areas to Develop"),
            if (has_delta)
              tags$p(class = "fb-delta mb-0", delta_text)
            else
              tags$p(class = "text-muted fst-italic mb-0 small",
                     "None recorded")
          )
        )
      )
    })

    observeEvent(input$close_feedback, {
      DT::selectRows(DT::dataTableProxy("tbl"), NULL)
    })

  })
}

# Internal choice parser (avoids dependency on export order)
parse_redcap_choices_internal <- function(choices_str) {
  if (is.null(choices_str) || is.na(choices_str) || !nzchar(trimws(choices_str)))
    return(character(0))
  parts <- trimws(strsplit(choices_str, "\\|")[[1]])
  result <- character(0)
  for (part in parts) {
    kv <- strsplit(part, ",\\s*", perl = TRUE)[[1]]
    if (length(kv) >= 2) {
      code  <- trimws(kv[1])
      label <- trimws(paste(kv[-1], collapse = ", "))
      result[code] <- label
    }
  }
  result
}
