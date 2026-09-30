# mod_milestone_growth.R
# Shared milestone growth module: one module used by the coach dashboard,
# imslu.ind.dash and the CCC dashboard. Replaces ind.dash's local
# .milestone_prog_plot_combined(), the CCC dashboard's inline
# output$ccc_milestone_plot and gmed's create_enhanced_milestone_progression().
#
# Views: the familiar spider (radar) chart, range snapshot (each
# subcompetency against where past residents at the same period fell, with
# guidance text), self vs faculty dumbbell, and a drill-down trajectory. ILP goals are a separate module
# (mod_ilp_goal_progress_*). No model fitting happens here: pass the cached fit
# from load_cached_milestone_growth() (fitted by the data-refresh job).
# Without a fit the module uses raw per-period cohort percentiles and draws
# the trajectory without a projection.

.MG_VIEWS <- c("spider", "overview", "dumbbell", "trajectory")

#' Milestone growth module UI
#'
#' @param id Module id.
#' @param show Views to include: any of \code{"spider"} (radar vs cohort
#'   median), \code{"overview"} (range snapshot), \code{"dumbbell"},
#'   \code{"trajectory"}.
#' @return A \code{tagList}.
#' @export
mod_milestone_growth_ui <- function(id, show = c("spider", "overview", "dumbbell", "trajectory")) {
  ns <- shiny::NS(id)
  show <- match.arg(show, .MG_VIEWS, several.ok = TRUE)
  card <- function(title, ...) {
    shiny::div(class = "card mb-3", shiny::div(class = "card-body",
      shiny::tags$h6(class = "card-title text-muted text-uppercase",
                     style = "font-size:0.75rem; letter-spacing:.06em;", title),
      ...))
  }
  shiny::tagList(
    if (any(c("spider", "overview", "dumbbell") %in% show))
      shiny::div(class = "d-flex align-items-center gap-2 mb-2",
        shiny::tags$span(class = "small text-muted", "Period:"),
        shiny::uiOutput(ns("period_ui"), inline = TRUE)),
    if ("spider" %in% show) card(
      "Milestone profile",
      shiny::radioButtons(ns("spider_rater"), NULL, inline = TRUE,
        choices = c("Faculty" = "faculty", "ACGME" = "acgme", "Self" = "self"),
        selected = "faculty"),
      shiny::uiOutput(ns("spider_ui"))),
    if ("overview" %in% show) card(
      "Milestones in context",
      shiny::tags$p(class = "small text-muted mb-1",
        "Grey bars show where past residents at the same period were rated: ",
        "light = the usual range (middle 80%), dark = the middle half, tick = median. ",
        "Click a row to see its trajectory below."),
      shiny::uiOutput(ns("overview_ui")),
      shiny::uiOutput(ns("overview_summary"))),
    if ("dumbbell" %in% show) card(
      "Self vs faculty",
      shiny::uiOutput(ns("dumbbell_ui"))),
    if ("trajectory" %in% show) card(
      "Trajectory",
      shiny::selectInput(ns("subcomp"), NULL, width = "360px",
        choices = .mg_subcomp_choices(), selected = "pc1"),
      shiny::uiOutput(ns("trajectory_ui")),
      shiny::uiOutput(ns("trajectory_readout")))
  )
}

#' Milestone growth module server
#'
#' @param id Module id.
#' @param milestone_data Reactive (or static value) holding long milestone
#'   data from \code{build_milestone_long()} (preferred: build it once at app
#'   start), or the forms list it accepts (\code{all_forms}).
#' @param resident_id Reactive resident record id.
#' @param period Reactive current period (1-6 or label), the default for the
#'   period picker. \code{NULL}/NA = latest period with data.
#' @param fit Optional reactive of the cached \code{milestone_growth_fit}
#'   (\code{load_cached_milestone_growth()}).
#' @param show Views rendered (should match the UI).
#' @param height Plot height.
#' @param show_target Draw the graduation target line (7) on the trajectory.
#' @param resident_name Optional reactive (or value) with the resident's name,
#'   shown in the spider's hover text.
#' @return Invisibly, a list of reactives: \code{selected_subcomp},
#'   \code{period}, \code{trajectory} (the trajectory data incl. readout).
#' @export
mod_milestone_growth_server <- function(id, milestone_data, resident_id, period,
                                        fit = NULL,
                                        show = c("spider", "overview", "dumbbell", "trajectory"),
                                        height = "560px", show_target = TRUE,
                                        resident_name = NULL) {
  show <- match.arg(show, .MG_VIEWS, several.ok = TRUE)
  as_r <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
  milestone_data <- as_r(milestone_data)
  resident_id <- as_r(resident_id)
  period <- as_r(period)
  fit_r <- as_r(fit)
  name_r <- as_r(resident_name)

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    long_all <- shiny::reactive({
      tryCatch(as_milestone_long(milestone_data()), error = function(e) {
        message("mod_milestone_growth: ", e$message); .mg_empty_long()
      })
    })
    long_res <- shiny::reactive({
      rid <- resident_id()
      d <- long_all()
      if (is.null(rid) || !nrow(d)) return(d[0, , drop = FALSE])
      d[d$record_id == as.character(rid), , drop = FALSE]
    })
    selected <- shiny::reactive(select_milestone_rating(long_res()))
    bands <- shiny::reactive({
      f <- fit_r()
      if (!is.null(f) && nrow(f$bands)) f$bands else empirical_cohort_bands(long_all())
    })

    # ── Shared period picker (overview + dumbbell) ─────────────────────────
    avail_periods <- shiny::reactive(sort(unique(long_res()$period)))
    default_period <- shiny::reactive({
      ps <- avail_periods()
      p <- .mg_period_num(if (is.null(period())) NA else period())
      if (!length(ps)) return(p)
      if (!is.na(p) && p %in% ps) p else max(ps[is.na(p) | ps <= p], ps[1])
    })
    output$period_ui <- shiny::renderUI({
      ps <- avail_periods()
      if (!length(ps)) return(NULL)
      shiny::selectInput(ns("period_pick"), NULL, width = "180px",
                         choices = stats::setNames(ps, .mg_period_name(ps)),
                         selected = default_period())
    })
    view_period <- shiny::reactive({
      p <- suppressWarnings(as.integer(input$period_pick))
      if (length(p) && !is.na(p)) p else default_period()
    })

    # ── Spider ────────────────────────────────────────────────────────────
    if ("spider" %in% show) {
      spider <- shiny::reactive({
        rater <- if (is.null(input$spider_rater)) "faculty" else input$spider_rater
        tryCatch(suppressMessages(suppressWarnings(
          plot_milestone_spider(long_res(), long_all(), view_period(), rater, name_r()))),
          error = function(e) { message("mod_milestone_growth spider: ", e$message); NULL })
      })
      output$spider_ui <- shiny::renderUI({
        if (is.null(spider())) return(.mg_empty_state("No ratings from this rater for this period."))
        plotly::plotlyOutput(ns("spider_plot"), height = "480px")
      })
      output$spider_plot <- plotly::renderPlotly({ p <- spider(); shiny::req(p); p })
    }

    # ── Range snapshot ─────────────────────────────────────────────────────
    range_plot <- shiny::reactive({
      plot_milestone_range(selected(), bands(), view_period(), source = ns("range"))
    })
    if ("overview" %in% show) {
      output$overview_ui <- shiny::renderUI({
        if (is.null(range_plot()))
          return(.mg_empty_state("No faculty or ACGME milestone ratings for this period."))
        plotly::plotlyOutput(ns("overview_plot"), height = height)
      })
      output$overview_plot <- plotly::renderPlotly({
        p <- range_plot(); shiny::req(p)
        plotly::event_register(p, "plotly_click")
      })
      output$overview_summary <- shiny::renderUI({
        p <- range_plot()
        s <- if (is.null(p)) NULL else attr(p, "milestone_range_summary")
        if (is.null(s)) return(NULL)
        shiny::tags$p(class = "small mt-1 mb-0", shiny::HTML(s))
      })
      shiny::observeEvent(plotly::event_data("plotly_click", source = ns("range")), {
        ev <- plotly::event_data("plotly_click", source = ns("range"))
        sc <- milestone_subcompetencies()$subcomp
        y <- suppressWarnings(as.integer(round(ev$y[1])))
        if (!is.na(y) && y >= 1 && y <= length(sc))
          shiny::updateSelectInput(session, "subcomp", selected = sc[length(sc) + 1 - y])
      })
    }

    # ── Dumbbell ──────────────────────────────────────────────────────────
    if ("dumbbell" %in% show) {
      output$dumbbell_ui <- shiny::renderUI({
        d <- long_res()
        if (!nrow(d) || !any(d$period == view_period(), na.rm = TRUE))
          return(.mg_empty_state("No self or faculty ratings for this period."))
        plotly::plotlyOutput(ns("dumbbell_plot"), height = height)
      })
      output$dumbbell_plot <- plotly::renderPlotly({
        p <- plot_milestone_dumbbell(long_res(), view_period())
        shiny::req(p)
        p
      })
    }

    # ── Trajectory ────────────────────────────────────────────────────────
    traj <- shiny::reactive({
      shiny::req(input$subcomp)
      if (!any(long_res()$subcomp == input$subcomp)) return(NULL)
      plot_milestone_trajectory(long_res(), input$subcomp, fit = fit_r(),
                                bands = bands(), show_target = show_target)
    })
    if ("trajectory" %in% show) {
      output$trajectory_ui <- shiny::renderUI({
        shiny::req(input$subcomp)
        if (is.null(traj())) return(.mg_empty_state("No ratings yet for this subcompetency."))
        plotly::plotlyOutput(ns("trajectory_plot"), height = height)
      })
      output$trajectory_plot <- plotly::renderPlotly({
        p <- traj(); shiny::req(p); p
      })
      output$trajectory_readout <- shiny::renderUI({
        p <- traj()
        if (is.null(p)) return(NULL)
        td <- attr(p, "milestone_trajectory")
        shiny::div(class = "small mt-1",
                   lapply(td$readout, function(x) shiny::tags$p(class = "mb-1", shiny::HTML(x))))
      })
    }

    invisible(list(
      selected_subcomp = shiny::reactive(input$subcomp),
      period = view_period,
      trajectory = shiny::reactive({ p <- traj(); if (is.null(p)) NULL else attr(p, "milestone_trajectory") })
    ))
  })
}

.mg_subcomp_choices <- function() {
  sc <- milestone_subcompetencies()
  split(stats::setNames(sc$subcomp, sc$label), sc$competency_label)[unique(sc$competency_label)]
}

# roundsui empty state, or a plain fallback when roundsui isn't installed.
.mg_empty_state <- function(message) {
  if (requireNamespace("roundsui", quietly = TRUE)) {
    roundsui::roundsui_empty_state(message, compact = TRUE)
  } else {
    shiny::div(class = "text-muted fst-italic p-3 text-center",
               style = "border:1px dashed #c3c2b7; border-radius:6px;", message)
  }
}
