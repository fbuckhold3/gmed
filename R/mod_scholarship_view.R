# mod_scholarship_view.R ── Scholarship & Teaching Portfolio (view)
#
# Promoted from imslu.ind.dash (2026-11) so the coach dashboard shows the
# same list the resident sees. Changes from the ind.dash original:
#   - `actions = FALSE` gives a read-only list (coach / CCC); TRUE keeps the
#     resident's Edit / Delete / Recategorize buttons.
#   - Type 6 "Ongoing project (no output yet)" renders as its own group.
#   - Rows whose main text matches another row for the same resident get a
#     "Possible duplicate" badge (the 2026 migration and the old module both
#     left repeated rows).
#   - Older-format rows fall back through more legacy fields for their text,
#     so fewer show as "(legacy entry)".
#   - Loads Bootstrap Icons itself rather than relying on the host app.
#
# Takes a reactive returning one resident's already-filtered scholarship
# rows. Rows with schol_work_type render grouped by type (or collection);
# rows with only legacy fields go under "Older entries".

#' @importFrom shiny NS uiOutput renderUI reactiveVal observeEvent moduleServer reactive div tags tagList singleton
NULL

.SCHOL_TYPE_LABELS <- c("1" = "Journal Articles", "2" = "Book Chapters",
                        "3" = "Journal Abstracts", "4" = "Oral Presentations",
                        "5" = "Poster Presentations", "6" = "Ongoing Projects")
.SCHOL_PROJ_KIND   <- c("1" = "Research", "2" = "Quality improvement", "3" = "Patient safety",
                        "4" = "Case report", "5" = "Medical education", "6" = "Other")
.SCHOL_PROJ_STATUS <- c("1" = "Planning / IRB", "2" = "Data collection", "3" = "Analysis",
                        "4" = "Writing up", "5" = "On hold", "6" = "Stopped")
# Fields compared for duplicates: substantive text only, no venue names.
.SCHOL_DUP_TEXT <- c("schol_title", "schol_proj_desc", "schol_cit", "schol_citation",
                     "schol_res", "schol_qi", "schol_comm")
# Legacy fields that may hold an older entry's text, in display priority.
.SCHOL_LEGACY_TEXT <- c("schol_title", "schol_cit", "schol_citation", "schol_res",
                        "schol_qi", "schol_pres_conf", "schol_venue", "schol_comm",
                        "schol_comm_other")

#' Main text of a scholarship row (title, else citation, else legacy text)
#' @keywords internal
.schol_row_text <- function(row, fields = c("schol_title", "schol_proj_desc", .SCHOL_LEGACY_TEXT)) {
  for (f in fields) {
    if (f %in% names(row)) {
      v <- row[[f]][1]
      if (!is.na(v) && nzchar(trimws(v))) return(trimws(as.character(v)))
    }
  }
  ""
}

#' Flag rows whose main text repeats within one resident's scholarship rows
#'
#' Compares lower-cased, letters-and-digits-only text (first 80 characters)
#' of the title, citation or project description. Conference / venue names
#' alone are not compared (two posters at one meeting aren't duplicates).
#' Rows with fewer than 12 characters of such text are never flagged.
#' @param df One resident's scholarship rows.
#' @return Logical vector, TRUE where another row has the same text.
#' @export
scholarship_duplicate_flags <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(logical(0))
  key <- vapply(seq_len(nrow(df)), function(i) {
    k <- gsub("[^a-z0-9]", "", tolower(.schol_row_text(df[i, , drop = FALSE], .SCHOL_DUP_TEXT)))
    substr(k, 1, 80)
  }, character(1))
  ok <- nchar(key) >= 12
  ok & (duplicated(key) | duplicated(key, fromLast = TRUE))
}

#' Scholarship view UI
#' @param id Module id.
#' @export
mod_scholarship_view_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(tags$head(tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"))),
    shiny::uiOutput(ns("entries_panel")))
}

#' Scholarship view server
#'
#' @param id Module id.
#' @param schol_data reactive() -> one resident's scholarship rows.
#' @param title Card title.
#' @param show_intro Show the resident-facing how-to text.
#' @param actions TRUE shows Edit / Delete / Recategorize buttons; FALSE is
#'   read-only (coach, CCC).
#' @param inline_form Optional function() returning UI (e.g.
#'   `mod_scholarship_entry_form_ui()`), rendered directly under the entry
#'   whose instance equals `active_instance()`; with it, clicking an entry
#'   opens it for editing in place.
#' @param active_instance reactive() -> repeat instance currently being
#'   edited, or NULL.
#' @return reactive() -> last row action, list(action, instance, record_id,
#'   nonce), or NULL. Always NULL when `actions = FALSE`.
#' @export
mod_scholarship_view_server <- function(id, schol_data,
                                        title = "Scholarship & Teaching Portfolio",
                                        show_intro = TRUE,
                                        actions = TRUE,
                                        inline_form = NULL,
                                        active_instance = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .STATUS_LABELS <- c("1" = "Submitted", "2" = "Accepted", "3" = "Published")
    .STATUS_BADGE  <- c("1" = "background:#e2e3e5; color:#41464b;",
                        "2" = "background:#cfe2ff; color:#084298;",
                        "3" = "background:#198754; color:#ffffff;")

    gv <- function(row, f) if (f %in% names(row)) { v <- row[[f]][1]; if (is.na(v)) "" else as.character(v) } else ""
    trunc <- function(s, n) if (nchar(s) > n) paste0(substr(s, 1, n - 3), "...") else s
    badge <- function(style, ...) tags$span(class = "badge",
      style = paste0(style, " font-size:0.72rem; font-weight:600; margin-left:6px;"), ...)
    dup_badge <- function() badge("background:#fde2e1; color:#842029;",
      tags$i(class = "bi bi-files me-1"), "Possible duplicate")

    # Per-row action buttons emit {action, instance, record_id, nonce} on the
    # ns'd `row_action` input; the orchestrator listens on the returned reactive.
    action_val <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$row_action, action_val(input$row_action))
    act_js <- function(action, inst, rid) sprintf(
      "Shiny.setInputValue('%s',{action:'%s',instance:'%s',record_id:'%s',nonce:Date.now()},{priority:'event'})",
      ns("row_action"), action, inst, rid)
    # Clicking anywhere on a row (not its buttons) opens it for editing.
    row_click <- function(action, inst, rid) {
      if (!isTRUE(actions) || is.null(inline_form)) return(NULL)
      paste0("if(event.target.closest('button,a,input,select,textarea,label,.selectize-control,.schol-inline'))return;",
             act_js(action, inst, rid))
    }
    is_active <- function(inst) {
      a <- tryCatch(active_instance(), error = function(e) NULL)
      !is.null(inline_form) && !is.null(a) && identical(as.character(a), as.character(inst))
    }
    inline_slot <- function(inst)
      if (is_active(inst)) div(class = "my-2 schol-inline", style = "cursor:auto;", inline_form()) else NULL
    act_btn <- function(action, inst, rid, icon, tip, color) {
      if (!isTRUE(actions)) return(NULL)
      tags$button(class = "btn btn-sm p-1 border-0",
        style = paste0("color:", color, "; background:transparent; line-height:1;"), title = tip,
        onclick = sprintf(
          "Shiny.setInputValue('%s',{action:'%s',instance:'%s',record_id:'%s',nonce:Date.now()},{priority:'event'})",
          ns("row_action"), action, inst, rid),
        tags$i(class = paste0("bi ", icon)))
    }

    # Render one categorized entry as a list item.
    render_entry <- function(row) {
      inst   <- gv(row, "redcap_repeat_instance"); rid <- gv(row, "record_id")
      type   <- gv(row, "schol_work_type")
      title_v<- gv(row, "schol_title"); if (!nzchar(title_v)) title_v <- "(untitled)"
      authors<- if (type == "6") "" else trunc(gv(row, "schol_authors"), 110)
      star   <- identical(gv(row, "schol_meaningful"), "1")
      first  <- type != "6" && identical(gv(row, "schol_first_author"), "1")
      coll   <- gv(row, "schol_collection")
      dup    <- isTRUE(row$.dup[1])

      # Type-specific meta line
      meta <- if (type %in% c("1", "3")) {
        src <- gv(row, "schol_source_name")
        vol <- gv(row, "schol_volume"); iss <- gv(row, "schol_issue"); pg <- gv(row, "schol_pages")
        vip <- paste0(vol, if (nzchar(iss)) paste0("(", iss, ")"), if (nzchar(pg)) paste0(":", pg))
        dt  <- gv(row, "schol_pub_date")
        paste(c(src, if (nzchar(vip)) vip, dt)[nzchar(c(src, if (nzchar(vip)) vip, dt))], collapse = " · ")
      } else if (type == "2") {
        gv(row, "schol_source_name")
      } else if (type == "6") {
        kd <- unname(.SCHOL_PROJ_KIND[gv(row, "schol_proj_kind")])
        mn <- gv(row, "schol_proj_mentor")
        parts <- c(if (!is.na(kd)) kd, if (nzchar(mn)) paste("Mentor:", mn))
        paste(parts, collapse = " · ")
      } else {
        ev  <- gv(row, "schol_event_name")
        loc <- paste(c(gv(row,"schol_event_city"), gv(row,"schol_event_state"),
                       gv(row,"schol_event_country"))[nzchar(c(gv(row,"schol_event_city"),
                       gv(row,"schol_event_state"), gv(row,"schol_event_country")))], collapse = ", ")
        dt  <- gv(row, "schol_event_date")
        paste(c(ev, loc, dt)[nzchar(c(ev, loc, dt))], collapse = " · ")
      }
      desc <- if (type == "6") trunc(gv(row, "schol_proj_desc"), 220) else ""

      status_badge <- if (type %in% c("1","3")) {
        sv <- gv(row, "schol_pub_status")
        if (nzchar(sv)) {
          sty <- unname(.STATUS_BADGE[sv]);  if (is.na(sty)) sty <- "background:#e9ecef;color:var(--roundsui-ink-muted, #495057);"
          slb <- unname(.STATUS_LABELS[sv]); if (is.na(slb)) slb <- sv
          badge(sty, slb)
        } else NULL
      } else if (type == "6") {
        slb <- unname(.SCHOL_PROJ_STATUS[gv(row, "schol_proj_status")])
        if (!is.na(slb)) badge("background:#e8f4f8; color:#0c4a5e;", slb) else NULL
      } else NULL

      div(class = "py-2",
          style = paste0("border-bottom:1px solid var(--roundsui-border, #f0f2f5);",
                         if (!is.null(row_click("edit", inst, rid))) " cursor:pointer;"),
          onclick = row_click("edit", inst, rid),
        div(class = "d-flex align-items-start gap-2",
          if (star) tags$i(class = "bi bi-star-fill", style = "color:#f0ad4e; font-size:0.85rem; margin-top:2px;"),
          div(style = "flex:1;",
            div(tags$span(style = "font-weight:600; color:var(--roundsui-ink, #13161F); font-size:0.96rem;", title_v),
                status_badge,
                if (first) badge("background:#eef2ff; color:#3730a3;", "First author"),
                if (dup) dup_badge()),
            if (nzchar(authors)) div(style = "font-size:0.87rem; color:var(--roundsui-ink-muted, #6c757d);", authors),
            if (nzchar(meta)) div(style = "font-size:0.87rem; color:var(--roundsui-ink-muted, #495057); font-style:italic;", meta),
            if (nzchar(desc)) div(style = "font-size:0.87rem; color:var(--roundsui-ink-muted, #495057); white-space:pre-wrap;", desc),
            if (nzchar(coll)) tags$span(class = "badge",
              style = "background:#f8f4ff; color:#4a1d96; font-size:0.72rem; margin-top:3px;",
              tags$i(class = "bi bi-collection me-1"), coll)),
          div(class = "d-flex align-items-start gap-1",
            act_btn("edit",   inst, rid, "bi-pencil", "Edit",   "#0d6efd"),
            act_btn("delete", inst, rid, "bi-trash",  "Delete", "var(--roundsui-danger, #D33A2C)"))),
        inline_slot(inst))
    }

    output$entries_panel <- shiny::renderUI({
      df <- schol_data()
      gb <- input$group_by %||% "type"
      has_rows <- !(is.null(df) || nrow(df) == 0)
      if (has_rows) df$.dup <- scholarship_duplicate_flags(df)

      is_cat <- if (has_rows) nzchar(vapply(seq_len(nrow(df)), function(i) gv(df[i, ], "schol_work_type"), character(1))) else logical(0)
      legacy_content <- if (has_rows) vapply(seq_len(nrow(df)), function(i) {
        r <- df[i, ]
        any(nzchar(vapply(c("schol_type", "schol_output_type", .SCHOL_LEGACY_TEXT),
                          function(f) gv(r, f), character(1))))
      }, logical(1)) else logical(0)
      cat_df    <- if (has_rows) df[is_cat, , drop = FALSE] else data.frame()
      legacy_df <- if (has_rows) df[!is_cat & legacy_content, , drop = FALSE] else data.frame()

      # Most-meaningful entries float to the top of any group.
      sort_starred <- function(rows) {
        if (!"schol_meaningful" %in% names(rows) || nrow(rows) == 0) return(rows)
        rows[order(as.character(rows$schol_meaningful) != "1"), , drop = FALSE]
      }
      render_group <- function(rows, heading, upper = TRUE) {
        rows <- sort_starred(rows)
        div(class = "mb-3",
          div(style = paste0("font-weight:700; color:var(--roundsui-accent, #3355D8); font-size:0.86rem; ",
                      if (upper) "text-transform:uppercase; letter-spacing:0.03em; " else "", "margin-bottom:4px;"),
              heading,
              tags$span(style = "color:var(--roundsui-ink-faint, #adb5bd); font-weight:500;", paste0(" (", nrow(rows), ")"))),
          lapply(seq_len(nrow(rows)), function(i) render_entry(rows[i, ])))
      }

      cat_sections <- if (nrow(cat_df) == 0) NULL
        else if (gb == "collection") {
          cvec <- if ("schol_collection" %in% names(cat_df)) as.character(cat_df$schol_collection) else rep("", nrow(cat_df))
          cvec[is.na(cvec)] <- ""
          colls <- sort(unique(cvec[nzchar(cvec)]))
          secs <- lapply(colls, function(cl)
            render_group(cat_df[cvec == cl, , drop = FALSE],
                         tagList(tags$i(class = "bi bi-collection me-1"), cl), upper = FALSE))
          ung <- cat_df[!nzchar(cvec), , drop = FALSE]
          if (nrow(ung) > 0) secs <- c(secs, list(render_group(ung, "Not in a collection", upper = FALSE)))
          secs
        } else {
          # Ongoing projects first: they're what a coach asks about.
          lapply(c("6", "1", "2", "3", "4", "5"), function(tp) {
            rows <- cat_df[as.character(cat_df$schol_work_type) == tp, , drop = FALSE]
            if (nrow(rows) == 0) return(NULL)
            render_group(rows, unname(.SCHOL_TYPE_LABELS[tp]))
          })
        }

      legacy_section <- if (nrow(legacy_df) > 0) {
        n_dup <- sum(legacy_df$.dup)
        items <- lapply(seq_len(nrow(legacy_df)), function(i) {
          r <- legacy_df[i, , drop = FALSE]
          li <- gv(r, "redcap_repeat_instance"); lrid <- gv(r, "record_id")
          desc <- .schol_row_text(r); if (!nzchar(desc)) desc <- "(legacy entry)"
          clk <- row_click("recategorize", li, lrid)
          div(style = paste0("border-bottom:1px solid var(--roundsui-border, #f0f2f5);",
                             if (!is.null(clk)) " cursor:pointer;"),
              onclick = clk, title = if (!is.null(clk)) "Click to review and update this entry",
            div(class = "py-2 d-flex align-items-center justify-content-between gap-2",
              div(style = "font-size:0.88rem; color:var(--roundsui-ink-muted, #495057);", trunc(desc, 110)),
              div(class = "d-flex align-items-center gap-2",
                if (isTRUE(r$.dup[1])) dup_badge(),
                if (isTRUE(actions)) badge("background:#fff3cd; color:#664d03;",
                  tags$i(class = "bi bi-exclamation-triangle me-1"), "Needs review"),
                act_btn("recategorize", li, lrid, "bi-arrow-repeat", "Recategorize", "#6f42c1"),
                act_btn("delete",       li, lrid, "bi-trash",        "Delete",       "var(--roundsui-danger, #D33A2C)"))),
            inline_slot(li))
        })
        div(class = "mt-3 pt-2", style = "border-top:1px dashed var(--roundsui-border, #dee2e6);",
          div(style = "font-weight:700; color:#8a6d3b; font-size:0.85rem; margin-bottom:4px;",
              tags$i(class = "bi bi-clock-history me-1"),
              if (isTRUE(actions)) paste0("Older entries to recategorize (", nrow(legacy_df), ")")
              else paste0("Older-format entries (", nrow(legacy_df), ")")),
          if (isTRUE(actions))
            tags$p(style = "font-size:0.8rem; margin-bottom:6px; color:var(--roundsui-ink-muted, #6c757d);",
                   "These predate the ERAS format. ",
                   if (!is.null(inline_form)) "Click an entry to update it into the new format"
                   else tagList("Use ", tags$i(class = "bi bi-arrow-repeat"),
                                " to recategorize each into the new format"),
                   if (n_dup > 0) tagList(", and ", tags$i(class = "bi bi-trash"),
                                          " to delete repeats marked ", tags$em("Possible duplicate")),
                   ".")
          else if (n_dup > 0)
            tags$p(style = "font-size:0.8rem; margin-bottom:6px; color:var(--roundsui-ink-muted, #6c757d);",
                   n_dup, " entries look like repeats; the resident can delete them in their dashboard."),
          items)
      } else NULL

      body <- if (is.null(cat_sections) && is.null(legacy_section))
          div(class = "text-muted fst-italic", style = "font-size:0.85rem; padding:16px 0;",
              tags$i(class = "bi bi-inbox me-2"),
              if (isTRUE(actions)) "No scholarly work yet — click below to add your first entry."
              else "No scholarship entries recorded.")
        else tagList(cat_sections, legacy_section)

      # Group-by toggle — shown once there is more than one categorized entry.
      seg_btn <- function(val, label) tags$button(type = "button",
        class = paste0("btn ", if (identical(gb, val)) "btn-primary" else "btn-outline-secondary"),
        style = "font-size:0.75rem; padding:2px 10px;",
        onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", ns("group_by"), val), label)
      group_toggle <- if (nrow(cat_df) > 1)
        div(class = "d-flex justify-content-end align-items-center gap-2 mb-2",
          tags$span(class = "text-muted", style = "font-size:0.8rem;", "Group by:"),
          div(class = "btn-group btn-group-sm", role = "group",
            seg_btn("type", "Type"), seg_btn("collection", "Collection"))) else NULL

      div(class = "card border-0 shadow-sm mb-3",
          style = "border-radius:8px; background:var(--roundsui-surface, #ffffff); color:var(--roundsui-ink, #13161F);",
        div(class = "card-header border-0 d-flex align-items-center gap-2",
            style = "background:var(--roundsui-surface-2, #f8fafc); border-radius:8px 8px 0 0; padding:12px 18px;",
          tags$i(class = "bi bi-award-fill", style = "color:var(--roundsui-accent, #3355D8); font-size:1rem;"),
          tags$span(style = "font-weight:700; color:var(--roundsui-accent, #3355D8); font-size:0.95rem;", title)),
        div(class = "card-body",
          if (isTRUE(show_intro))
            div(class = "mb-3 px-3 py-2",
                style = "background:var(--roundsui-surface-2, #f8fafc); border-left:3px solid var(--roundsui-accent, #3355D8); border-radius:4px;",
              tags$p(class = "mb-2", style = "font-size:0.85rem; color:var(--roundsui-ink-muted, #495057);",
                "Log your scholarly ", tags$strong("outputs"), " here — grouped by the ERAS ",
                "categories so entries copy straight into your application — plus any ",
                tags$strong("ongoing projects"), " that don't have a poster or paper yet. Use ",
                tags$strong("Download CV (Word)"), " for a formatted copy. Patient-safety reviews ",
                "and committee work live in your Self Evaluation, not here."),
              tags$ul(class = "mb-0 ps-3", style = "font-size:0.83rem; color:var(--roundsui-ink-muted, #495057);",
                tags$li(class = "mb-1",
                  tags$i(class = "bi bi-star-fill", style = "color:#f0ad4e;"),
                  tags$strong(" Most meaningful"),
                  " — star up to 3 works you'd most want highlighted (as ERAS allows). ",
                  "They pin to the top of each group and are flagged in your CV."),
                tags$li(class = "mb-1",
                  tags$i(class = "bi bi-collection", style = "color:#4a1d96;"),
                  tags$strong(" Collections"),
                  " — tag related works with a project name (for example, an ongoing project and ",
                  "the poster that came out of it), then switch ",
                  tags$em("Group by"), " to ", tags$em("Collection"), " to see them together."),
                tags$li(
                  tags$i(class = "bi bi-pencil", style = "color:#0d6efd;"), tags$strong(" Edit"),
                  " and ",
                  tags$i(class = "bi bi-trash", style = "color:var(--roundsui-danger, #D33A2C);"), tags$strong(" Delete"),
                  " sit on each entry — deleting is permanent. ",
                  tags$i(class = "bi bi-arrow-repeat", style = "color:#6f42c1;"),
                  tags$strong(" Recategorize"),
                  " converts an older entry into the new format, pre-filled from what you'd entered before. ",
                  tags$i(class = "bi bi-files", style = "color:#842029;"),
                  tags$strong(" Possible duplicate"), " marks entries whose text matches another one."))),
          group_toggle,
          body))
    })

    # Expose per-row actions to the orchestrator.
    shiny::reactive(if (isTRUE(actions)) action_val() else NULL)

  }) # end moduleServer
}
