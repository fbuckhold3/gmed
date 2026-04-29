# ============================================================================
# ILP DISPLAY MODULE
# R/mod_ilp_display.R
#
# Read-only display of a resident's Individual Learning Plan (ILP) data for
# coach review. Renders three blocks:
#   1. Previous-period coach ILP summary (coach_mile_goal, coach_ilp_final)
#   2. Previous-period goals + resident's achievement reflection
#      (goal_*, prior_goal_*, review_q_*, review_q2_*)
#   3. Current-period goals (goal_*, goal_level_*, how_*)
#
# Extracted from imslu.coach.dash mod_goals.R + imslu.ind.dash mod_self_eval.R
# so coach.dash and other consumers can render the same data without
# re-implementing the layout. Coach-entry textareas stay in the consumer.
# ============================================================================

#' ILP Display UI
#'
#' Read-only display of a resident's ILP across two periods, structured for
#' coach review. Renders (in order): previous-period coach ILP summary,
#' previous-period goals with the resident's achievement reflection, and the
#' current-period goals the resident has set.
#'
#' Coach-entry textareas (`coach_mile_goal`, `coach_ilp_final`) are NOT part
#' of this module — the consumer adds them around `mod_ilp_display_ui()`.
#'
#' @param id Character. Module namespace id.
#' @param title Character or NULL. Optional header rendered above the panel.
#' @param sections Character vector of sections to render, in order. Defaults
#'   to `c("prev_coach", "compare")`. Allowed values:
#'   * `"prev_coach"` — prior coach ILP summary text (`coach_mile_goal`,
#'     `coach_ilp_final` from the previous-period `coach_rev`).
#'   * `"compare"` — per-domain side-by-side: previous goal + achievement +
#'     reflection on the left, current goal on the right (3 domain rows).
#'   * `"prev_goals"` / `"current"` — legacy stacked layout (kept for
#'     back-compat; not in the default).
#'
#' @return A `shiny::tagList` with one `uiOutput()` per requested section.
#' @family ilp_display
#' @seealso [mod_ilp_display_server()]
#' @export
mod_ilp_display_ui <- function(id,
                               title    = NULL,
                               sections = c("prev_coach", "compare")) {
  ns <- shiny::NS(id)

  meta <- .ilp_section_meta()
  sections <- intersect(sections, names(meta))

  blocks <- lapply(sections, function(s) {
    m <- meta[[s]]
    shiny::div(
      class = "mb-4",
      shiny::tags$h5(
        m$header,
        style = "color:#34495e; margin-top:10px;"
      ),
      if (nzchar(m$subhead))
        shiny::tags$p(m$subhead, style = "color:#7f8c8d; font-size:0.9rem;") else NULL,
      shiny::wellPanel(
        style = m$panel_style,
        shiny::uiOutput(ns(s))
      )
    )
  })

  shiny::tagList(
    if (!is.null(title)) shiny::tags$h4(title, class = "mb-2") else NULL,
    blocks
  )
}

#' ILP Display Server
#'
#' Server side of [mod_ilp_display_ui()]. Reads three reactive data frames
#' (current-period ilp, previous-period ilp, previous-period coach_rev) and
#' renders the requested display sections.
#'
#' @param id Character. Module namespace id (must match the UI call).
#' @param current_ilp Reactive returning a 1-row data frame from the resident's
#'   `ilp` form for the period being reviewed (or NULL/empty).
#' @param previous_ilp Reactive returning a 1-row data frame from the prior
#'   period's `ilp` form (or NULL/empty). May be NULL.
#' @param previous_coach_rev Reactive returning a 1-row data frame from the
#'   prior period's `coach_rev` form (or NULL/empty). May be NULL.
#' @param data_dict Optional reactive returning the REDCap data dictionary
#'   data frame. When supplied, used to resolve milestone-level descriptors;
#'   otherwise, levels are shown as bare numbers.
#' @param sections Character vector matching the UI call.
#'
#' @return Invisibly NULL.
#' @family ilp_display
#' @seealso [mod_ilp_display_ui()]
#' @export
mod_ilp_display_server <- function(id,
                                   current_ilp,
                                   previous_ilp        = NULL,
                                   previous_coach_rev  = NULL,
                                   data_dict           = NULL,
                                   sections = c("prev_coach", "compare")) {
  shiny::moduleServer(id, function(input, output, session) {

    meta <- .ilp_section_meta()
    sections <- intersect(sections, names(meta))

    .pull <- function(r) {
      if (is.null(r)) return(NULL)
      v <- if (is.function(r)) r() else r
      if (is.null(v) || !is.data.frame(v) || nrow(v) == 0) NULL else v
    }

    .field <- function(row, fld) {
      if (is.null(row) || !fld %in% names(row)) return("")
      v <- row[[fld]][1]
      if (is.null(v) || is.na(v)) "" else as.character(v)
    }

    if ("prev_coach" %in% sections) {
      output$prev_coach <- shiny::renderUI({
        prev <- .pull(previous_coach_rev)
        if (is.null(prev)) return(.ilp_empty("No previous coach review on file."))

        mile_goal <- .field(prev, "coach_mile_goal")
        ilp_final <- .field(prev, "coach_ilp_final")

        if (!nzchar(trimws(mile_goal)) && !nzchar(trimws(ilp_final)))
          return(.ilp_empty("No previous coach ILP comments."))

        shiny::tagList(
          if (nzchar(trimws(mile_goal)))
            .ilp_text_block("Goal & Milestone Comments", mile_goal),
          if (nzchar(trimws(ilp_final)))
            .ilp_text_block("ILP Summary", ilp_final)
        )
      })
    }

    if ("compare" %in% sections) {
      output$compare <- shiny::renderUI({
        prev <- .pull(previous_ilp)
        cur  <- .pull(current_ilp)
        if (is.null(prev) && is.null(cur))
          return(.ilp_empty("No ILP data on file for either period."))
        dd <- if (!is.null(data_dict))
                tryCatch(if (is.function(data_dict)) data_dict() else data_dict,
                         error = function(e) NULL)
              else NULL

        domains <- list(
          list("pcmk",    "Patient Care / Medical Knowledge"),
          list("sbppbl",  "Systems-Based / Practice-Based Learning"),
          list("profics", "Professionalism / Interpersonal Communication")
        )

        rows <- lapply(domains, function(d) {
          .ilp_compare_row(domain      = d[[1]],
                           domain_name = d[[2]],
                           prev_data   = prev,
                           cur_data    = cur,
                           data_dict   = dd)
        })
        do.call(shiny::tagList, rows)
      })
    }

    if ("prev_goals" %in% sections) {
      output$prev_goals <- shiny::renderUI({
        prev <- .pull(previous_ilp)
        cur  <- .pull(current_ilp)
        if (is.null(prev)) return(.ilp_empty("No previous-period goals on file."))
        dd <- if (!is.null(data_dict))
                tryCatch(if (is.function(data_dict)) data_dict() else data_dict,
                         error = function(e) NULL)
              else NULL

        # Achievement (prior_goal_*) + reflection (review_q*) live in the
        # CURRENT period's ilp row — the resident enters them at the start
        # of period N when reviewing period N-1's goals.
        blocks <- list(
          .ilp_prev_goal_block(
            domain      = "pcmk",
            domain_name = "Patient Care / Medical Knowledge Goal",
            prev_data   = prev, cur_data = cur, data_dict = dd
          ),
          .ilp_prev_goal_block(
            domain      = "sbppbl",
            domain_name = "Systems-Based / Practice-Based Learning Goal",
            prev_data   = prev, cur_data = cur, data_dict = dd
          ),
          .ilp_prev_goal_block(
            domain      = "profics",
            domain_name = "Professionalism / Interpersonal Communication Goal",
            prev_data   = prev, cur_data = cur, data_dict = dd
          )
        )
        blocks <- Filter(Negate(is.null), blocks)
        if (length(blocks) == 0) return(.ilp_empty("No previous goals recorded."))
        do.call(shiny::tagList, blocks)
      })
    }

    if ("current" %in% sections) {
      output$current <- shiny::renderUI({
        cur <- .pull(current_ilp)
        if (is.null(cur)) return(.ilp_empty("Resident has not yet set goals for this period."))
        dd <- if (!is.null(data_dict))
                tryCatch(if (is.function(data_dict)) data_dict() else data_dict,
                         error = function(e) NULL)
              else NULL

        blocks <- list(
          .ilp_current_goal_block(
            domain      = "pcmk",
            domain_name = "Patient Care / Medical Knowledge Goal",
            cur_data    = cur,
            data_dict   = dd
          ),
          .ilp_current_goal_block(
            domain      = "sbppbl",
            domain_name = "Systems-Based / Practice-Based Learning Goal",
            cur_data    = cur,
            data_dict   = dd
          ),
          .ilp_current_goal_block(
            domain      = "profics",
            domain_name = "Professionalism / Interpersonal Communication Goal",
            cur_data    = cur,
            data_dict   = dd
          )
        )
        blocks <- Filter(Negate(is.null), blocks)
        if (length(blocks) == 0) return(.ilp_empty("Resident has not yet set goals for this period."))
        do.call(shiny::tagList, blocks)
      })
    }

    invisible(NULL)
  })
}

# ----------------------------------------------------------------------------
# Section metadata
# ----------------------------------------------------------------------------

.ilp_section_meta <- function() {
  list(
    prev_coach = list(
      header = "Previous Coach ILP Summary",
      subhead = "Comments and ILP summary from the prior coaching review.",
      panel_style = "background-color:#e8f4f8; border-left:4px solid #3498db;"
    ),
    compare = list(
      header = "Goals: Last Period vs This Period",
      subhead = "Per-domain comparison: prior goal and whether it was met on the left, current goal on the right.",
      panel_style = "background-color:#f8f9fa; border-left:4px solid #3498db;"
    ),
    prev_goals = list(
      header = "Previous Period Goals & Achievement",
      subhead = "Goals the resident set last period and their reflection on whether they met them.",
      panel_style = "background-color:#f8f9fa; border-left:4px solid #27ae60;"
    ),
    current = list(
      header = "Current Period Goals",
      subhead = "Goals the resident has set for this period.",
      panel_style = "background-color:#fff8e1; border-left:4px solid #f39c12;"
    )
  )
}

# ----------------------------------------------------------------------------
# Domain field maps
# ----------------------------------------------------------------------------
# Note: profics intentionally uses goal_subcomp_profics for the subcomp code
# (REDCap historical asymmetry).
.ilp_domain_fields <- function(domain) {
  switch(domain,
    pcmk = list(
      goal       = "goal_pcmk",
      level      = "goal_level_pcmk",
      how        = "how_pcmk",
      prior      = "prior_goal_pcmk",
      review_no  = "review_q_pcmk",
      review_yes = "review_q2_pcmk",
      color      = "#0d6efd"
    ),
    sbppbl = list(
      goal       = "goal_sbppbl",
      level      = "goal_level_sbppbl",
      how        = "how_sbppbl",
      prior      = "prior_goal_sbppbl",
      review_no  = "review_q_sbppbl",
      review_yes = "review_q2_sbppbl",
      color      = "#198754"
    ),
    profics = list(
      goal       = "goal_subcomp_profics",
      level      = "goal_level_profics",
      how        = "how_profics",
      prior      = "prior_goal_profics",
      review_no  = "review_q_profics",
      review_yes = "review_q2_profics",
      color      = "#6f42c1"
    ),
    NULL
  )
}

# Domain → subcompetency-code → label map. Mirrors mod_goals.R.
.ilp_subcomp_label <- function(domain, code) {
  if (!nzchar(code)) return("")
  pcmk <- c("1"="PC1: History", "2"="PC2: Physical Exam", "3"="PC3: Clinical Reasoning",
            "4"="PC4: Mgmt-Inpatient", "5"="PC5: Mgmt-Outpatient", "6"="PC6: Digital Health",
            "7"="MK1: Applied Sciences", "8"="MK2: Therapeutics", "9"="MK3: Diagnostics")
  sbppbl <- c("1"="SBP1: Safety & QI", "2"="SBP2: Navigation", "3"="SBP3: Physician Role",
              "4"="PBL1: Evidence-Based", "5"="PBL2: Reflective")
  profics <- c("1"="PROF1: Behavior", "2"="PROF2: Ethics", "3"="PROF3: Accountability",
               "4"="PROF4: Well-Being", "5"="ICS1: Patient Comm",
               "6"="ICS2: Team Comm", "7"="ICS3: Documentation")
  tbl <- switch(domain, pcmk = pcmk, sbppbl = sbppbl, profics = profics, NULL)
  if (is.null(tbl)) return(paste("Code", code))
  v <- tbl[as.character(code)]
  if (is.na(v)) paste("Code", code) else unname(v)
}

# Optional: resolve full milestone-level descriptor from data dict if available.
# Falls back to "Level N" text. Mirrors mod_goals.R::get_milestone_level_text.
.ilp_level_text <- function(domain, code, level, dd) {
  if (!nzchar(code) || !nzchar(level)) return("Level not specified")
  bare <- paste("Level", level)
  if (is.null(dd) || !is.data.frame(dd)) return(bare)

  subcomp <- .ilp_subcomp_code(domain, code)
  if (is.null(subcomp)) return(bare)

  fld <- paste0(subcomp, "_r1")
  fn_col <- intersect(c("field_name", "Variable / Field Name", "Variable...Field.Name"),
                      names(dd))
  ch_col <- intersect(c("select_choices_or_calculations",
                        "Choices, Calculations, OR Slider Labels",
                        "Choices..Calculations..OR.Slider.Labels"),
                      names(dd))
  if (!length(fn_col) || !length(ch_col)) return(bare)
  row <- dd[dd[[fn_col[1]]] == fld, , drop = FALSE]
  if (!nrow(row)) return(bare)
  choices <- as.character(row[[ch_col[1]]][1])
  if (!nzchar(choices)) return(bare)
  parts <- trimws(unlist(strsplit(choices, "\\|")))
  parts <- gsub("^[0-9]+,\\s*", "", parts)
  lv <- suppressWarnings(as.integer(level))
  if (!is.na(lv) && lv > 0 && lv <= length(parts) && nzchar(parts[lv]) && parts[lv] != "NA")
    return(paste0("Level ", level, ": ", parts[lv]))
  bare
}

.ilp_subcomp_code <- function(domain, goal_code) {
  n <- suppressWarnings(as.integer(goal_code))
  if (is.na(n)) return(NULL)
  switch(domain,
    pcmk    = if (n <= 6) paste0("pc", n) else paste0("mk", n - 6),
    sbppbl  = if (n <= 3) paste0("sbp", n) else paste0("pbl", n - 3),
    profics = if (n <= 4) paste0("prof", n) else paste0("ics", n - 4),
    NULL
  )
}

# ----------------------------------------------------------------------------
# Block renderers
# ----------------------------------------------------------------------------

.ilp_empty <- function(msg) {
  shiny::div(class = "text-muted fst-italic", style = "padding:8px;", msg)
}

.ilp_text_block <- function(label, txt) {
  shiny::div(
    class = "mb-3",
    shiny::tags$h6(label, style = "color:#3498db; margin-bottom:6px;"),
    shiny::div(
      style = "padding:10px 12px; background:#fff; border-radius:4px; white-space:pre-wrap;",
      txt
    )
  )
}

.ilp_current_goal_block <- function(domain, domain_name, cur_data, data_dict) {
  fm   <- .ilp_domain_fields(domain); if (is.null(fm)) return(NULL)
  code <- if (fm$goal  %in% names(cur_data)) as.character(cur_data[[fm$goal]][1])  else ""
  lvl  <- if (fm$level %in% names(cur_data)) as.character(cur_data[[fm$level]][1]) else ""
  how  <- if (fm$how   %in% names(cur_data)) as.character(cur_data[[fm$how]][1])   else ""
  if (!nzchar(code) || code == "NA") return(NULL)

  shiny::div(
    style = sprintf(paste("margin-bottom:20px; padding:15px; background:#fff;",
                          "border-radius:4px; border-left:4px solid %s;"), fm$color),
    shiny::tags$h5(domain_name, style = "color:#e67e22; margin-top:0;"),
    shiny::tags$p(shiny::tags$strong("Selected Goal: "), .ilp_subcomp_label(domain, code)),
    shiny::tags$p(shiny::tags$strong("Target Milestone Level: "),
                  .ilp_level_text(domain, code, lvl, data_dict)),
    if (nzchar(how) && !is.na(how))
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong("How to achieve:")),
        shiny::tags$p(style = "padding-left:15px; color:#34495e; white-space:pre-wrap;", how)
      )
  )
}

.ilp_compare_row <- function(domain, domain_name, prev_data, cur_data, data_dict) {
  fm <- .ilp_domain_fields(domain); if (is.null(fm)) return(NULL)

  fld <- function(row, name) {
    if (is.null(row) || !name %in% names(row)) return("")
    v <- row[[name]][1]; if (is.null(v) || is.na(v)) "" else as.character(v)
  }

  prev_code <- fld(prev_data, fm$goal)
  prev_lvl  <- fld(prev_data, fm$level)
  prev_how  <- fld(prev_data, fm$how)
  achv      <- fld(cur_data,  fm$prior)
  rno       <- fld(cur_data,  fm$review_no)
  ryes      <- fld(cur_data,  fm$review_yes)

  cur_code  <- fld(cur_data, fm$goal)
  cur_lvl   <- fld(cur_data, fm$level)
  cur_how   <- fld(cur_data, fm$how)

  goal_met    <- nzchar(achv) && (achv == "1" || tolower(achv) == "yes")
  has_answer  <- nzchar(achv) && achv != "NA"
  reflection  <- if (goal_met) ryes else rno

  # ---- Achievement callout (the "did they meet it" question, prominent) ----
  achievement_box <- if (!nzchar(prev_code)) NULL
  else if (!has_answer) {
    shiny::div(
      class = "mt-3 p-3",
      style = "background:#f8f9fa; border:1px dashed #adb5bd; border-radius:8px;",
      shiny::tags$div(class = "small fw-bold text-uppercase text-muted mb-1",
                      style = "letter-spacing:.06em;",
                      "Did the resident meet this goal?"),
      shiny::tags$div(class = "fst-italic text-muted",
                      shiny::tags$i(class = "bi bi-hourglass-split me-1"),
                      "Resident has not yet reflected on this goal.")
    )
  } else {
    bg     <- if (goal_met) "#d1f4dd" else "#fff3cd"
    border <- if (goal_met) "#198754" else "#dc7d1f"
    icon   <- if (goal_met) "bi-check-circle-fill" else "bi-x-octagon-fill"
    txt    <- if (goal_met) "Yes \u2014 goal met" else "No \u2014 goal not met"
    color  <- if (goal_met) "#0f5132" else "#7a4500"
    why_label <- if (goal_met) "Why? (what helped, what they learned)"
                 else "Why not? (barriers, what's next)"

    shiny::div(
      class = "mt-3 p-3",
      style = sprintf("background:%s; border-left:5px solid %s; border-radius:8px;",
                      bg, border),
      shiny::tags$div(class = "small fw-bold text-uppercase mb-1",
                      style = sprintf("letter-spacing:.06em; color:%s;", color),
                      "Did the resident meet this goal?"),
      shiny::tags$div(
        class = "d-flex align-items-center mb-2",
        shiny::tags$i(class = paste0("bi ", icon, " me-2"),
                      style = sprintf("font-size:1.4rem; color:%s;", border)),
        shiny::tags$span(style = sprintf("font-size:1.1rem; font-weight:700; color:%s;", color),
                         txt)
      ),
      if (nzchar(trimws(reflection)))
        shiny::tagList(
          shiny::tags$div(class = "small fw-bold mt-2",
                          style = sprintf("color:%s;", color),
                          why_label),
          shiny::tags$div(
            class = "mt-1",
            style = "background:rgba(255,255,255,0.7); padding:8px 12px; border-radius:6px; white-space:pre-wrap; color:#212529;",
            reflection
          )
        )
      else
        shiny::tags$div(class = "fst-italic small text-muted mt-1",
                        "(No reflection text provided.)")
    )
  }

  # ---- Goal info block (used in both panes) ----
  .goal_info <- function(code, lvl, how) {
    shiny::tagList(
      shiny::div(
        class = "p-2 mb-2",
        style = "background:#f8fafc; border-radius:6px;",
        shiny::tags$div(class = "small text-uppercase fw-bold text-muted mb-1",
                        style = "letter-spacing:.05em;",
                        "Subcompetency"),
        shiny::tags$div(style = "font-size:1.05rem; font-weight:600; color:#2c3e50;",
                        .ilp_subcomp_label(domain, code))
      ),
      shiny::div(
        class = "p-2 mb-2",
        style = "background:#f8fafc; border-radius:6px;",
        shiny::tags$div(class = "small text-uppercase fw-bold text-muted mb-1",
                        style = "letter-spacing:.05em;",
                        "Target Milestone Level"),
        shiny::tags$div(style = "color:#34495e;",
                        .ilp_level_text(domain, code, lvl, data_dict))
      ),
      if (nzchar(how))
        shiny::div(
          class = "p-2",
          style = "background:#f8fafc; border-radius:6px;",
          shiny::tags$div(class = "small text-uppercase fw-bold text-muted mb-1",
                          style = "letter-spacing:.05em;",
                          "How they'll get there"),
          shiny::tags$div(style = "color:#34495e; white-space:pre-wrap;", how)
        )
    )
  }

  prev_pane <- if (!nzchar(prev_code)) {
    shiny::div(class = "text-muted fst-italic small p-3",
               "No goal recorded for last period.")
  } else {
    shiny::tagList(.goal_info(prev_code, prev_lvl, prev_how), achievement_box)
  }

  cur_pane <- if (!nzchar(cur_code)) {
    shiny::div(class = "text-muted fst-italic small p-3",
               "Resident has not yet set a goal for this period.")
  } else {
    .goal_info(cur_code, cur_lvl, cur_how)
  }

  pane_header <- function(label, color, icon) {
    shiny::div(
      class = "px-3 py-2 d-flex align-items-center",
      style = sprintf("background:%s; color:#fff; font-weight:700; letter-spacing:.05em;", color),
      shiny::tags$i(class = paste0("bi ", icon, " me-2")),
      shiny::tags$span(class = "text-uppercase small", label)
    )
  }

  shiny::div(
    class = "mb-4 shadow-sm",
    style = sprintf(paste("border:1px solid #e9ecef; border-top:5px solid %s;",
                          "border-radius:8px; background:#fff; overflow:hidden;"),
                    fm$color),
    # Domain header bar
    shiny::div(
      class = "px-3 py-2",
      style = sprintf("background:%s10; border-bottom:1px solid #e9ecef;",
                      substr(fm$color, 1, 7)),
      shiny::tags$h5(class = "mb-0", style = "color:#2c3e50; font-weight:700;",
                     domain_name)
    ),
    shiny::div(
      class = "row g-0",
      # LEFT: Last period
      shiny::div(
        class = "col-md-6",
        style = "border-right:1px solid #e9ecef;",
        pane_header("Last Period", "#6c757d", "bi-clock-history"),
        shiny::div(class = "p-3", prev_pane)
      ),
      # RIGHT: This period
      shiny::div(
        class = "col-md-6",
        pane_header("This Period", fm$color, "bi-arrow-right-circle-fill"),
        shiny::div(class = "p-3", style = "background:#fffdf7;", cur_pane)
      )
    )
  )
}

.ilp_prev_goal_block <- function(domain, domain_name, prev_data, cur_data, data_dict) {
  fm   <- .ilp_domain_fields(domain); if (is.null(fm)) return(NULL)
  # Goal text/level/plan = previous-period ilp row.
  code <- if (fm$goal       %in% names(prev_data)) as.character(prev_data[[fm$goal]][1])       else ""
  lvl  <- if (fm$level      %in% names(prev_data)) as.character(prev_data[[fm$level]][1])      else ""
  how  <- if (fm$how        %in% names(prev_data)) as.character(prev_data[[fm$how]][1])        else ""
  # Achievement + reflection = CURRENT-period ilp row (resident enters these
  # at the start of period N while reviewing period N-1).
  achv <- if (!is.null(cur_data) && fm$prior      %in% names(cur_data)) as.character(cur_data[[fm$prior]][1])      else ""
  rno  <- if (!is.null(cur_data) && fm$review_no  %in% names(cur_data)) as.character(cur_data[[fm$review_no]][1])  else ""
  ryes <- if (!is.null(cur_data) && fm$review_yes %in% names(cur_data)) as.character(cur_data[[fm$review_yes]][1]) else ""
  if (!nzchar(code) || code == "NA") return(NULL)

  goal_met <- nzchar(achv) && (achv == "1" || tolower(achv) == "yes")
  border   <- if (goal_met) "#27ae60" else "#e67e22"
  badge    <- if (goal_met)
    shiny::tags$span(style = "color:#27ae60; font-weight:600;",
                     shiny::tags$i(class = "bi bi-check-circle-fill me-1"), "Goal Met")
  else if (nzchar(achv))
    shiny::tags$span(style = "color:#e67e22; font-weight:600;",
                     shiny::tags$i(class = "bi bi-exclamation-circle-fill me-1"), "Goal Not Yet Met")
  else
    shiny::tags$span(style = "color:#7f8c8d; font-style:italic;",
                     "Resident has not yet reflected on this goal.")

  reflection <- if (goal_met) ryes else rno

  shiny::div(
    style = sprintf(paste("margin-bottom:20px; padding:15px; background:#fff;",
                          "border-radius:4px; border-left:4px solid %s;"), border),
    shiny::tags$h5(domain_name, style = "color:#2980b9; margin-top:0;"),
    shiny::div(style = "margin-bottom:10px;", badge),
    shiny::tags$p(shiny::tags$strong("Selected Goal: "), .ilp_subcomp_label(domain, code)),
    shiny::tags$p(shiny::tags$strong("Target Milestone Level: "),
                  .ilp_level_text(domain, code, lvl, data_dict)),
    if (nzchar(how) && !is.na(how))
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong("How to achieve:")),
        shiny::tags$p(style = "padding-left:15px; color:#34495e; white-space:pre-wrap;", how)
      ),
    if (nzchar(trimws(reflection)))
      shiny::tagList(
        shiny::tags$hr(style = "margin:10px 0; border-top:1px solid #ecf0f1;"),
        shiny::tags$p(shiny::tags$strong("Resident's reflection:"),
                      style = "color:#7f8c8d; margin-bottom:4px;"),
        shiny::tags$p(style = "padding-left:15px; font-style:italic; color:#555; white-space:pre-wrap;",
                      reflection)
      )
  )
}
