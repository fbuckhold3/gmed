#' Module UI for Milestone Rating
#' @param id module id
#' @export
mod_miles_rating_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom message handler for scrolling the score row into view. Registered
    # once on the client; server calls session$sendCustomMessage("milestone_scroll_to_row", ...)
    # only when the active milestone actually changes, so typing in the
    # description textarea never triggers a scroll/focus jump.
    tags$script(HTML(
      "if (!window.__milestone_scroll_bound) {
         Shiny.addCustomMessageHandler('milestone_scroll_to_row', function(msg) {
           setTimeout(function(){
             var el = document.getElementById(msg.id);
             if (el) el.scrollIntoView({behavior:'smooth', block:'center'});
           }, 50);
         });
         window.__milestone_scroll_bound = true;
       }"
    )),
    div(id = ns("moduleContainer"),
        uiOutput(ns("domainMap")),      # clickable dot map showing completion per domain
        uiOutput(ns("mainContent")),    # current image + score buttons
        uiOutput(ns("explanationUI")),  # high-rating justification (conditional)
        uiOutput(ns("navigationButtons"))
    )
  )
}

#' Module Server for Milestone Rating
#'
#' Presents all 22 ACGME milestones one at a time with a clickable dot-map
#' navigation header. Scoring auto-advances to the next unrated item. No
#' "Submit" button — \code{done} becomes TRUE reactively once all items are
#' rated and any required explanations are provided.
#'
#' @param id          module id
#' @param period      reactive returning the selected period label (e.g. "Mid PGY2")
#' @param prev_scores optional reactive returning a named list mapping item keys
#'                    (e.g. \code{PC_1}, \code{MK_2}) to the resident's rating
#'                    from their most recent prior self-evaluation. Displayed as
#'                    a "Last period: N" hint above the score buttons so the
#'                    resident can see what they rated last time. Defaults to
#'                    \code{reactive(NULL)}.
#' @param initial_scores optional reactive returning a named list of item key →
#'                       integer score to prefill into the module when the
#'                       period changes (e.g. ratings already saved for the
#'                       current period). Lets a resident return to a partially
#'                       completed self-eval and see their prior selections.
#' @param initial_descs  optional reactive returning a named list of item key →
#'                       description text to prefill alongside \code{initial_scores}.
#' @export
mod_miles_rating_server <- function(id, period,
                                    prev_scores    = shiny::reactive(NULL),
                                    initial_scores = shiny::reactive(NULL),
                                    initial_descs  = shiny::reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Milestone definitions ───────────────────────────────────────────────
    imageSets <- list(
      PC   = list(title = "Patient Care",
                  images      = paste0("pc",   1:6, ".png"),
                  imageTitles = c("History-taking & Interviewing",
                                  "Physical Examination",
                                  "Clinical Reasoning",
                                  "Patient Management \u2014 Inpatient",
                                  "Patient Management \u2014 Outpatient / Preventive",
                                  "Digital Health")),
      MK   = list(title = "Medical Knowledge",
                  images      = paste0("mk",   1:3, ".png"),
                  imageTitles = c("Applied Foundational Sciences",
                                  "Therapeutic Knowledge",
                                  "Knowledge of Diagnostic Testing")),
      SBP  = list(title = "Systems-Based Practice",
                  images      = paste0("sbp",  1:3, ".png"),
                  imageTitles = c("Patient Safety & Quality Improvement",
                                  "System Navigation for Patient-Centered Care",
                                  "Physician Role in Health Care Systems")),
      PBLI = list(title = "Practice-Based Learning",
                  images      = c("pbli1.png", "pbli2.png"),
                  imageTitles = c("Evidence-Based & Informed Practice",
                                  "Reflective Practice & Personal Growth")),
      PROF = list(title = "Professionalism",
                  images      = paste0("prof", 1:4, ".png"),
                  imageTitles = c("Professional Behavior",
                                  "Ethical Principles",
                                  "Accountability / Conscientiousness",
                                  "Well-Being")),
      ICS  = list(title = "Interpersonal & Communication",
                  images      = paste0("ics",  1:3, ".png"),
                  imageTitles = c("Patient- & Family-Centered Communication",
                                  "Interprofessional & Team Communication",
                                  "Communication within Health Care Systems"))
    )

    # Flat ordered list of all keys: PC_1 … ICS_3
    allKeys <- unlist(lapply(names(imageSets), function(d)
      paste0(d, "_", seq_along(imageSets[[d]]$images))), use.names = FALSE)

    totalN <- length(allKeys)  # 22

    # Explanation required when self-rating >= threshold for the period
    thresholds <- list(
      "Entering Residency" = 3, "Mid Intern" = 4, "End Intern" = 5,
      "Mid PGY2" = 6, "End PGY2" = 7, "Mid PGY3" = 8, "Graduating" = 9
    )

    # ── Reactive state ───────────────────────────────────────────────────────
    state <- reactiveValues(
      setIdx   = 1L,   # current domain index
      imgIdx   = 1L,   # current image within domain
      selections  = list(),   # key → integer 1-9
      descriptions = list()   # key → character
    )

    # ── Prefill from saved data ─────────────────────────────────────────────
    # When a period changes (or initial_scores() arrives late from the data
    # fetch), seed the module's state so returning residents see what they
    # already rated. We only overwrite keys that aren't already present in
    # state$selections (so in-session edits are never clobbered by a later
    # re-fetch of the saved REDCap row).
    seeded_for <- reactiveVal(NULL)
    observe({
      pv <- tryCatch(period(), error = function(e) NULL)
      if (is.null(pv) || !nzchar(pv)) return()
      init <- tryCatch(initial_scores(), error = function(e) NULL)
      id_init <- tryCatch(initial_descs(),  error = function(e) NULL)
      # Re-seed whenever the period changes; skip if we've already seeded this
      # period and nothing new arrived.
      tag <- paste0(pv, "|",
                    length(init %||% list()), "|",
                    length(id_init %||% list()))
      if (isTRUE(identical(tag, seeded_for()))) return()
      seeded_for(tag)
      # On period change, clear prior session state so we don't carry
      # selections across periods.
      state$selections  <- list()
      state$descriptions <- list()
      state$setIdx <- 1L
      state$imgIdx <- 1L
      if (is.list(init)) {
        for (k in names(init)) {
          v <- suppressWarnings(as.integer(init[[k]]))
          if (!is.na(v)) state$selections[[k]] <- v
        }
      }
      if (is.list(id_init)) {
        for (k in names(id_init)) {
          d <- id_init[[k]]
          if (!is.null(d) && nzchar(trimws(as.character(d))))
            state$descriptions[[k]] <- as.character(d)
        }
      }
      # Land on the first unrated item (if any) so the user picks up where
      # they left off; otherwise stay on PC_1.
      if (length(state$selections) > 0) {
        unrated <- setdiff(allKeys, names(state$selections))
        if (length(unrated) > 0) .goToKey(unrated[1])
      }
    })

    # ── Derived helpers ──────────────────────────────────────────────────────
    curDomain  <- reactive(names(imageSets)[state$setIdx])
    curSet     <- reactive(imageSets[[state$setIdx]])
    curKey     <- reactive(paste0(curDomain(), "_", state$imgIdx))
    curImgFile <- reactive(curSet()$images[state$imgIdx])
    uiState    <- reactive(if (is.null(period()) || !nzchar(period())) "none" else "active")

    # All complete: all rated + explanations where needed
    allComplete <- reactive({
      if (!all(allKeys %in% names(state$selections))) return(FALSE)
      pv  <- period()
      thr <- if (!is.null(pv)) thresholds[[pv]] else NULL
      if (is.null(thr)) return(TRUE)
      hi  <- Filter(function(k) isTRUE(state$selections[[k]] >= thr), allKeys)
      all(vapply(hi, function(k)
        k %in% names(state$descriptions) &&
        nzchar(trimws(state$descriptions[[k]] %||% "")), logical(1)))
    })

    # ── Navigate to a specific key ──────────────────────────────────────────
    .goToKey <- function(key) {
      parts <- strsplit(key, "_")[[1]]
      dom   <- parts[1]; idx <- as.integer(parts[2])
      si    <- which(names(imageSets) == dom)
      if (length(si) == 1) { state$setIdx <- si; state$imgIdx <- idx }
    }

    # After scoring, jump to the next unrated item (wraps around)
    .advanceToUnrated <- function() {
      cur_pos <- which(allKeys == isolate(curKey()))
      if (length(cur_pos) == 0) return()
      after <- c(seq(cur_pos + 1, totalN), seq_len(cur_pos - 1))
      sels  <- isolate(names(state$selections))
      nxt   <- Find(function(k) !(k %in% sels), allKeys[after])
      if (!is.null(nxt)) .goToKey(nxt)
      # If all done, stay on current — no action needed
    }

    # ── Domain dot-map header ───────────────────────────────────────────────
    output$domainMap <- renderUI({
      req(uiState() == "active")
      n_rated <- sum(allKeys %in% names(state$selections))

      if (allComplete()) {
        return(div(
          class = "d-flex align-items-center gap-2 mb-3 py-2 px-3",
          style = "background:#f0faf4; border-radius:8px; border:1px solid #a5d6a7;",
          tags$i(class = "bi bi-check-circle-fill", style = "color:#2e7d32; font-size:1.1rem;"),
          tags$span(style = "font-weight:700; color:#1a6b3a; font-size:0.88rem;",
                    "All 22 milestones rated \u2014 your scores have been auto-saved.")
        ))
      }

      domain_blocks <- lapply(names(imageSets), function(d) {
        keys_d  <- paste0(d, "_", seq_along(imageSets[[d]]$images))
        rated_d <- keys_d %in% names(state$selections)
        is_cur  <- d == curDomain()

        dots <- lapply(seq_along(keys_d), function(j) {
          k     <- keys_d[j]
          done  <- rated_d[j]
          is_this <- isTRUE(k == curKey())
          bg    <- if (is_this) "#003d5c"
                   else if (done) "#2e7d32"
                   else "#dee2e6"
          tags$span(
            style = paste0("display:inline-block; width:14px; height:14px;",
                           " border-radius:50%; background:", bg, ";",
                           " margin:1px; cursor:pointer;",
                           if (is_this) " box-shadow:0 0 0 2px #7fb3d3;" else ""),
            title = paste0(imageSets[[d]]$imageTitles[j],
                           if (done) " \u2713" else " (not yet rated)"),
            onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                              ns("jump_to"), k)
          )
        })

        n_d <- sum(rated_d); tot_d <- length(keys_d); all_d <- n_d == tot_d
        div(class = "me-3 mb-1",
          tags$span(style = paste0("font-size:0.7rem; font-weight:", if(is_cur)"700" else "500",
                                   "; color:", if(all_d)"#2e7d32" else if(is_cur)"#003d5c" else "#6c757d",
                                   "; display:block; margin-bottom:2px;"),
                    paste0(imageSets[[d]]$title, " ", n_d, "/", tot_d)),
          dots
        )
      })

      pct <- round(100 * n_rated / totalN)
      div(class = "mb-3",
        div(class = "d-flex justify-content-between align-items-center mb-1",
          tags$span(style = "font-size:0.78rem; font-weight:600; color:#003d5c;",
                    paste0("Milestone Self-Assessment \u2014 ", n_rated, " of ", totalN, " rated")),
          tags$span(style = paste0("font-size:0.78rem; font-weight:700; color:",
                                   if(pct == 100) "#2e7d32" else "#0066a1"),
                    paste0(pct, "%"))
        ),
        div(class = "progress mb-3", style = "height:5px;",
          div(class = "progress-bar",
              style = paste0("width:", pct, "%; background:",
                             if(pct == 100) "#2e7d32" else "#0066a1", ";"))),
        div(class = "d-flex flex-wrap", domain_blocks)
      )
    })

    # ── Main image + score buttons ──────────────────────────────────────────
    output$mainContent <- renderUI({
      if (uiState() == "none") return(div("Select a period to begin."))
      req(allComplete() == FALSE)   # hide image when all done (map shows the success msg)

      key <- curKey()
      sel <- state$selections[[key]]
      pv  <- period()
      thr <- thresholds[[pv]]

      # Score buttons positioned to align with the 5-column grid above:
      #   v=1 at 10%, v=2 at 20%, … v=9 at 90% of the grid width.
      # Odd values sit under column centers (1,3,5,7,9 → cols 1,2,3,4,5);
      # even values sit on column boundaries.
      score_btns <- lapply(1:9, function(v) {
        is_sel   <- isTRUE(sel == v)
        high     <- !is.null(thr) && v >= thr
        bg  <- if (is_sel && high) "#e65100" else if (is_sel) "#003d5c" else "#f5f5f5"
        col <- if (is_sel) "white" else "#546e7a"
        brd <- if (is_sel) bg else "#dee2e6"
        tags$button(
          type  = "button",
          id    = ns(paste0("box_", v)),
          class = "action-button",
          style = paste0("position:absolute; left:", v * 10, "%;",
                         " top:50%; transform:translate(-50%,-50%);",
                         " width:40px; height:40px; padding:0;",
                         " background:", bg, "; color:", col, ";",
                         " border:1px solid ", brd, ";",
                         " border-radius:5px; font-size:0.95rem;",
                         " font-weight:", if(is_sel)"700" else "500", ";",
                         " cursor:pointer;"),
          v
        )
      })

      # Previous self-eval rating for this item (if supplied by the app)
      ps_list <- tryCatch(prev_scores(), error = function(e) NULL)
      prev_v  <- if (!is.null(ps_list) && key %in% names(ps_list)) ps_list[[key]] else NULL
      prev_badge <- if (!is.null(prev_v) && !is.na(prev_v) && nzchar(as.character(prev_v)))
        tags$span(style = "background:#eef2ff; color:#3730a3; border:1px solid #c7d2fe;
                           border-radius:20px; padding:2px 10px; font-size:0.72rem;
                           font-weight:600; margin-left:8px;",
                  tags$i(class = "bi bi-clock-history me-1"),
                  paste0("Last self-eval: ", prev_v))
      else NULL

      div(
        # Header: domain + item title
        div(class = "d-flex align-items-baseline justify-content-between mb-2",
          div(
            tags$span(style = "font-weight:700; color:#003d5c; font-size:0.95rem;",
                      paste0(curDomain(), state$imgIdx, " \u2014 ", curSet()$imageTitles[state$imgIdx])),
            prev_badge),
          tags$span(style = "font-size:0.75rem; color:#9e9e9e;",
                    paste0(curSet()$title))
        ),
        # Milestone image (cap at 55vh so score row is always visible) +
        # STICKY score row pinned to bottom of viewport so users never have to
        # scroll to reach the rating buttons.
        div(style = "width:100%; max-width:1140px;",
          # Image — constrained height keeps score row in view on any screen
          div(style = "max-height:55vh; overflow:auto; border-radius:4px;",
              uiOutput(ns("milestoneImage"))),
          # Label
          div(style = "font-size:0.78rem; color:#546e7a; margin:10px 0 2px;",
              "Your rating \u2014 click the number that best matches your level:"),
          # Sticky button row: stays pinned at the bottom of the viewport so
          # entering scores never requires scrolling.
          div(id = ns("scoreRow"),
              style = "position:sticky; bottom:0; z-index:10;
                       background:rgba(255,255,255,0.96);
                       backdrop-filter:blur(4px);
                       border-top:1px solid #e0e0e0;
                       padding:8px 0;
                       width:100%; height:64px;",
              score_btns)
        )
      )
    })

    # (Scroll-to-row observer removed — score row is now position:sticky at
    # the bottom of the viewport, so it's always visible without scrolling.)

    # Image rendering (reactive to current image file)
    output$milestoneImage <- renderUI({
      req(uiState() == "active", !allComplete())
      img_file <- curImgFile()

      # addResourcePath("milestones", ...) is registered in the app's global.R
      # so images are always served as milestones/<file>.png
      img_src <- paste0("milestones/", img_file)

      tags$img(src = img_src,
               style = "width:100%; max-width:1140px; border-radius:4px; border:1px solid #e0e0e0;",
               alt  = paste("Milestone grid:", img_file))
    })

    # ── Explanation UI ──────────────────────────────────────────────────────
    # Give each milestone its OWN input ID (explanation_PC_1, explanation_MK_3,
    # etc.) so Shiny's client-side input cache can't bleed one item's typed
    # text onto another. Each key's textarea is a fresh DOM node with its own
    # bound input value.
    .expl_id <- function(key) paste0("explanation_", key)

    output$explanationUI <- renderUI({
      req(uiState() == "active")
      key <- curKey()
      sel <- state$selections[[key]]
      if (is.null(sel)) return(NULL)
      pv  <- isolate(period())
      thr <- thresholds[[pv]]
      if (is.null(thr) || sel < thr) return(NULL)

      # Form wrapper with onsubmit=return false prevents Enter from submitting
      # the surrounding Shiny form (which was causing the "gray flash").
      tags$form(onsubmit = "return false;",
        div(class = "mt-2 p-2",
            style = "background:#fff3e0; border-left:3px solid #e65100; border-radius:4px;",
          tags$p(style = "font-size:0.78rem; color:#bf360c; margin:0 0 5px;",
                 tags$i(class = "bi bi-info-circle me-1"),
                 "This rating is higher than typical for your training level. ",
                 "Please briefly justify it before moving on."),
          textAreaInput(ns(.expl_id(key)),
                        label = NULL,
                        value = isolate(state$descriptions[[key]] %||% ""),
                        rows  = 2,
                        width = "100%",
                        placeholder = "Brief justification\u2026")
        )
      )
    })

    # Current textarea value for the active milestone (each key has its own id)
    cur_explanation <- reactive({
      key <- curKey()
      val <- input[[.expl_id(key)]]
      if (is.null(val)) "" else val
    })

    # Persist typing to state (debounced). Because the input ID is unique per
    # key, writes here never affect a different milestone's saved text.
    expl_debounced <- debounce(cur_explanation, 400)
    observeEvent(expl_debounced(), {
      key <- isolate(curKey())
      if (is.null(key) || is.null(isolate(state$selections[[key]]))) return()
      state$descriptions[[key]] <- expl_debounced()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ── Navigation buttons ──────────────────────────────────────────────────
    output$navigationButtons <- renderUI({
      req(uiState() == "active", !allComplete())
      key <- curKey()
      sel <- state$selections[[key]]
      pv  <- period()
      thr <- thresholds[[pv]]

      needs_exp   <- !is.null(sel) && !is.null(thr) && sel >= thr
      cur_expl    <- cur_explanation()
      disable_nxt <- is.null(sel) ||
                     (needs_exp && !nzchar(trimws(cur_expl %||% "")))

      # Is there a previous item?
      cur_pos  <- which(allKeys == key)
      has_prev <- length(cur_pos) > 0 && cur_pos > 1

      div(class = "d-flex gap-2 mt-3",
        if (has_prev)
          actionButton(ns("prev"), label = tagList(tags$i(class="bi bi-chevron-left me-1"), "Previous"),
                       class = "btn btn-sm btn-outline-secondary"),
        actionButton(ns("nxt"),
                     label = tagList("Next \u2014 skip to unrated",
                                     tags$i(class="bi bi-chevron-right ms-1")),
                     class = paste("btn btn-sm",
                                   if (disable_nxt) "btn-outline-secondary disabled"
                                   else "btn-outline-primary"))
      )
    })

    # ── Score click handlers ─────────────────────────────────────────────────
    for (v in 1:9) {
      local({
        vv <- v
        observeEvent(input[[paste0("box_", vv)]], {
          key <- isolate(curKey())
          state$selections[[key]] <- vv
          # Clear stale description if rating drops below threshold
          pv  <- isolate(period())
          thr <- thresholds[[pv]]
          if (!is.null(thr) && vv < thr) state$descriptions[[key]] <- NULL
          # Auto-advance only when no explanation is required. Delay ~350 ms
          # so the selected-button "fill" is visible before the screen moves.
          needs_exp <- !is.null(thr) && vv >= thr
          if (!needs_exp) {
            shinyjs::delay(350, .advanceToUnrated())
          }
        }, ignoreInit = TRUE)
      })
    }

    # ── Dot-map jump ─────────────────────────────────────────────────────────
    observeEvent(input$jump_to, {
      req(nzchar(input$jump_to))
      # Save explanation for current key before jumping
      key <- isolate(curKey())
      ce  <- isolate(cur_explanation())
      if (nzchar(trimws(ce %||% "")))
        state$descriptions[[key]] <- trimws(ce)
      .goToKey(input$jump_to)
    })

    # ── Previous button ───────────────────────────────────────────────────────
    observeEvent(input$prev, {
      key <- isolate(curKey())
      ce  <- isolate(cur_explanation())
      if (nzchar(trimws(ce %||% "")))
        state$descriptions[[key]] <- trimws(ce)
      cur_pos <- which(allKeys == key)
      if (length(cur_pos) > 0 && cur_pos > 1) .goToKey(allKeys[cur_pos - 1])
    })

    # ── Next (skip to unrated) button ─────────────────────────────────────────
    observeEvent(input$nxt, {
      key <- isolate(curKey())
      sel <- isolate(state$selections[[key]])
      pv  <- isolate(period())
      thr <- thresholds[[pv]]
      needs_exp <- !is.null(sel) && !is.null(thr) && sel >= thr
      ce <- isolate(cur_explanation())
      if (needs_exp) {
        if (nzchar(trimws(ce %||% "")))
          state$descriptions[[key]] <- trimws(ce)
        else {
          showNotification("Please briefly justify this rating before moving on.", type = "warning")
          return()
        }
      }
      .advanceToUnrated()
    })

    # ── Return interface (unchanged) ─────────────────────────────────────────
    list(
      done   = reactive(allComplete()),
      scores = reactive(state$selections),
      desc   = reactive(state$descriptions)
    )
  })
}

#' Milestone Dashboard Module UI
#'
#' Creates a self-contained milestone dashboard with spider plot and progression chart
#'
#' @param id Module namespace ID
#' @param milestone_type Type of milestones ("program", "self", "acgme")
#' @param height Height of the dashboard (default: "600px")
#' @return Shiny UI tagList
#' @export
milestone_dashboard_ui <- function(id, milestone_type = "program", height = "600px") {
  
  ns <- NS(id)
  
  # Module title based on type
  module_title <- switch(milestone_type,
                         "program" = "Program Assessment",
                         "self" = "Self-Assessment", 
                         "acgme" = "ACGME Assessment",
                         "Milestone Assessment")
  
  tagList(
    div(class = "milestone-dashboard-container",
        style = paste0("height: ", height, "; border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin: 5px;"),
        
        # Module header
        div(class = "dashboard-header", 
            style = "text-align: center; margin-bottom: 15px; border-bottom: 2px solid #2c3e50; padding-bottom: 10px;",
            h4(module_title, style = "color: #2c3e50; margin: 0; font-weight: bold;")
        ),
        
        # Loading indicator
        conditionalPanel(
          condition = paste0("$('html').hasClass('shiny-busy') && $('#", ns("loading"), "').is(':visible')"),
          div(id = ns("loading"), 
              style = "text-align: center; padding: 20px;",
              icon("spinner", class = "fa-spin"), " Loading milestone data..."
          )
        ),
        
        # Main content
        div(id = ns("main_content"),
            
            # Spider plot section (top half)
            div(class = "spider-section",
                style = "height: 60%; margin-bottom: 15px;",  # Increased from 45% to 60%
                plotlyOutput(ns("spider_plot"), height = "100%")
            ),
            
            # Controls section
            div(class = "controls-section",
                style = "height: 8%; margin-bottom: 10px; padding: 5px;",
                fluidRow(
                  column(12,
                         selectInput(
                           ns("selected_milestone"),
                           label = "Select Milestone for Progression View:",
                           choices = list("Loading milestones..." = ""),  # <- Provide initial choice
                           width = "100%"
                         )
                  )
                )
            ),
            
            # Progression chart section (bottom half)
            div(class = "progression-section",
                style = "height: 35%;",  # Reduced from 42% to 35%
                plotlyOutput(ns("progression_plot"), height = "100%")
            )
        ),
        
        # Error/no data message
        div(id = ns("error_message"),
            style = "display: none; text-align: center; padding: 50px; color: #e74c3c;",
            icon("exclamation-triangle"), 
            p("No milestone data available for the selected criteria.")
        )
    )
  )
}

#' Milestone Dashboard Module Server
#'
#' Server logic for the milestone dashboard module
#'
#' @param id Module namespace ID  
#' @param milestone_results Reactive containing processed milestone results
#' @param record_id Reactive containing selected resident record ID
#' @param period Reactive containing selected period
#' @param milestone_type Type of milestones ("program", "self", "acgme")
#' @param resident_data Reactive containing resident lookup data
#' @return Server function
#' @export
milestone_dashboard_server <- function(id, milestone_results, record_id, period, 
                                       milestone_type = "program", resident_data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive to get milestone data for this type
    milestone_data_obj <- reactive({
      req(milestone_results())
      
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      
      get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = if (milestone_type == "acgme") "program" else milestone_type,
        milestone_system = milestone_system
      )
    })
    
    # Reactive to get available milestones for the selector
    available_milestones <- reactive({
      req(milestone_data_obj())
      
      milestone_data <- milestone_data_obj()$data
      if (is.null(milestone_data) || nrow(milestone_data) == 0) return(NULL)
      
      # Get milestone columns for this type
      milestone_cols <- get_milestone_columns_simple(milestone_data, 
                                                     if (milestone_type == "acgme") "acgme" else milestone_type)
      
      if (length(milestone_cols) == 0) return(NULL)
      
      # Create named vector for selectInput (label = value)
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      milestone_labels <- sapply(milestone_cols, function(x) get_milestone_label(x, milestone_system))
      
      choices <- setNames(milestone_cols, milestone_labels)
      return(choices)
    })
    
    # Update milestone selector choices
    observe({
      choices <- available_milestones()
      
      if (is.null(choices)) {
        updateSelectInput(session, "selected_milestone", 
                          choices = list("No milestones available" = ""))
      } else {
        updateSelectInput(session, "selected_milestone", choices = choices)
      }
    })
    
    # Show/hide content based on data availability
    observe({
      if (is.null(milestone_data_obj()) || is.null(available_milestones())) {
        shinyjs::hide("main_content")
        shinyjs::show("error_message")
      } else {
        shinyjs::show("main_content")
        shinyjs::hide("error_message")
      }
    })
    
    # Spider plot
    output$spider_plot <- renderPlotly({
      req(milestone_data_obj(), record_id(), period())
      
      milestone_data <- milestone_data_obj()$data
      median_data <- milestone_data_obj()$medians
      
      if (is.null(milestone_data) || is.null(median_data)) {
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "No milestone data available", 
                   x = 0.5, y = 0.5, showarrow = FALSE
                 ))
      }
      
      create_enhanced_milestone_spider_plot(
        milestone_data = milestone_data,
        median_data = median_data,
        resident_id = record_id(),
        period_text = period(),
        milestone_type = milestone_type,
        resident_data = resident_data()
      )
    })
    
    # Progression plot
    output$progression_plot <- renderPlotly({
      req(milestone_data_obj(), record_id(), input$selected_milestone)
      
      # Skip if no milestone selected or invalid selection
      if (is.null(input$selected_milestone) || input$selected_milestone == "") {
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "Select a milestone above to view progression", 
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "#666")
                 ))
      }
      
      milestone_system <- if (milestone_type == "acgme") "acgme" else "rep"
      
      create_enhanced_milestone_progression(
        milestone_results = milestone_results(),
        resident_id = record_id(),
        milestone_col = input$selected_milestone,
        milestone_type = if (milestone_type == "acgme") "program" else milestone_type,
        milestone_system = milestone_system,
        resident_data = resident_data(),
        show_national = TRUE
      )
    })
    
    # Return reactive values for parent app if needed
return(list(
  selected_milestone = reactive(input$selected_milestone),
  data_available = reactive(!is.null(milestone_data_obj())),
  current_data = reactive({
    # Return the milestone results the user just entered
    milestone_results()
  }),
  milestone_data_obj = reactive({
    # Return the full milestone data object
    milestone_data_obj()
  })
))
  })
}

#' Helper Function to Create Multiple Dashboard Columns
#'
#' Creates a layout with multiple milestone dashboard modules side by side
#'
#' @param milestone_results Reactive containing processed milestone results
#' @param record_id Reactive containing selected resident record ID  
#' @param period Reactive containing selected period
#' @param resident_data Reactive containing resident lookup data
#' @param milestone_types Vector of milestone types to display (e.g., c("program", "self"))
#' @param column_width Bootstrap column width (e.g., 4 for 3 columns, 6 for 2 columns)
#' @return Shiny UI fluidRow
#' @export
create_milestone_dashboard_layout <- function(milestone_results, record_id, period, resident_data,
                                              milestone_types = c("program", "self"), 
                                              column_width = 6) {
  
  # Create columns for each milestone type
  columns <- lapply(seq_along(milestone_types), function(i) {
    milestone_type <- milestone_types[i]
    module_id <- paste0("milestone_dash_", i)
    
    column(column_width,
           milestone_dashboard_ui(module_id, milestone_type = milestone_type),
           # Call server function in the parent server
           # milestone_dashboard_server(module_id, milestone_results, record_id, period, milestone_type, resident_data)
    )
  })
  
  do.call(fluidRow, columns)
}

#' Example Usage in App Server
#' 
#' @examples
#' # In your app server function:
#' 
#' # Create multiple dashboard modules
#' milestone_dashboard_server("milestone_dash_1", milestone_results, record_id, period, "program", resident_data)
#' milestone_dashboard_server("milestone_dash_2", milestone_results, record_id, period, "self", resident_data)
#' 
#' # In your app UI:
#' fluidRow(
#'   column(6, milestone_dashboard_ui("milestone_dash_1", milestone_type = "program")),
#'   column(6, milestone_dashboard_ui("milestone_dash_2", milestone_type = "self"))
#' )
#' 
#' # Or use the helper function:
#' output$milestone_layout <- renderUI({
#'   create_milestone_dashboard_layout(
#'     milestone_results = reactive(milestone_results),
#'     record_id = reactive(input$resident_select),
#'     period = reactive(input$period_select), 
#'     resident_data = reactive(complete_data$residents),
#'     milestone_types = c("program", "self"),
#'     column_width = 6
#'   )
#' })
#' @export
example_milestone_dashboard_usage <- function() {
  # This is just documentation - see examples above
}

#' Add Required CSS for Milestone Dashboard
#'
#' Adds CSS styling for the milestone dashboard modules
#'
#' @return HTML tags with CSS
#' @export
milestone_dashboard_css <- function() {
  tags$head(
    tags$style(HTML("
      .milestone-dashboard-container {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
      }
      
      .milestone-dashboard-container:hover {
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
        transform: translateY(-2px);
      }
      
      .dashboard-header h4 {
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      .spider-section, .progression-section {
        background: white;
        border-radius: 6px;
        box-shadow: inset 0 1px 3px rgba(0,0,0,0.05);
      }
      
      .controls-section {
        padding: 10px;
        background: rgba(255,255,255,0.7);
        border-radius: 6px;
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .milestone-dashboard-container {
          height: auto !important;
          min-height: 500px;
        }
      }
    "))
  )
}
