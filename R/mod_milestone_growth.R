# mod_milestone_growth.R
# Shared milestone growth module: one module used by the coach dashboard,
# imslu.ind.dash and the CCC dashboard. Replaces ind.dash's local
# .milestone_prog_plot_combined(), the CCC dashboard's inline
# output$ccc_milestone_plot and gmed's create_enhanced_milestone_progression().
#
# Views: overview heat table, self vs faculty dumbbell, drill-down trajectory,
# with ILP goals marked in all three. No model fitting happens here: pass the
# cached fit from load_cached_milestone_growth() (fitted by the data-refresh
# job). Without a fit the module still draws the overview and dumbbell
# against raw per-period cohort quantiles, and the trajectory without a
# projection.

.MG_VIEWS <- c("overview", "dumbbell", "trajectory")

#' Milestone growth module UI
#'
#' @param id Module id.
#' @param show Views to include: any of \code{"overview"}, \code{"dumbbell"},
#'   \code{"trajectory"}.
#' @return A \code{tagList}.
#' @export
mod_milestone_growth_ui <- function(id, show = c("overview", "dumbbell", "trajectory")) {
  ns <- shiny::NS(id)
  show <- match.arg(show, .MG_VIEWS, several.ok = TRUE)
  card <- function(title, ...) {
    shiny::div(class = "card mb-3", shiny::div(class = "card-body",
      shiny::tags$h6(class = "card-title text-muted text-uppercase",
                     style = "font-size:0.75rem; letter-spacing:.06em;", title),
      ...))
  }
  shiny::tagList(
    if ("overview" %in% show) card(
      "Milestones vs cohort median",
      shiny::tags$p(class = "small text-muted mb-1",
        "Each cell is the resident's rating minus the median of past residents at the same ",
        shiny::HTML("period. &#9660; marks ratings below the cohort 10th percentile. Click a row to open it below.")),
      shiny::uiOutput(ns("overview_ui"))),
    if ("dumbbell" %in% show) card(
      "Self vs faculty",
      shiny::div(class = "d-flex align-items-center gap-2 mb-1",
        shiny::tags$span(class = "small text-muted", "Period:"),
        shiny::uiOutput(ns("dumbbell_period_ui"), inline = TRUE)),
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
#' @param period Reactive current period (1-6 or label). Used as the default
#'   dumbbell period, the last overview column, and to pick current vs
#'   previous ILP goals. \code{NULL}/NA = latest period with data.
#' @param ilp_data Optional reactive of ILP rows (\code{all_forms$ilp}).
#' @param fit Optional reactive of the cached \code{milestone_growth_fit}
#'   (\code{load_cached_milestone_growth()}).
#' @param show Views rendered (should match the UI).
#' @param height Plot height.
#' @return Invisibly, a list of reactives: \code{selected_subcomp},
#'   \code{trajectory} (the trajectory data incl. readout).
#' @export
mod_milestone_growth_server <- function(id, milestone_data, resident_id, period,
                                        ilp_data = NULL, fit = NULL,
                                        show = c("overview", "dumbbell", "trajectory"),
                                        height = "560px") {
  show <- match.arg(show, .MG_VIEWS, several.ok = TRUE)
  as_r <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
  milestone_data <- as_r(milestone_data)
  resident_id <- as_r(resident_id)
  period <- as_r(period)
  ilp_r <- as_r(ilp_data)
  fit_r <- as_r(fit)

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
    cur_period <- shiny::reactive({
      p <- .mg_period_num(if (is.null(period())) NA else period())
      if (is.na(p) && nrow(long_res())) p <- max(long_res()$period)
      p
    })
    bands <- shiny::reactive({
      f <- fit_r()
      if (!is.null(f) && nrow(f$bands)) f$bands else empirical_cohort_bands(long_all())
    })
    goals <- shiny::reactive({
      il <- ilp_r()
      if (is.null(il)) return(NULL)
      tryCatch(extract_ilp_goals(il, resident_id(), cur_period()),
               error = function(e) NULL)
    })

    # ── Overview ──────────────────────────────────────────────────────────
    if ("overview" %in% show) {
      output$overview_ui <- shiny::renderUI({
        if (!nrow(selected())) return(.mg_empty_state("No faculty or ACGME milestone ratings yet for this resident."))
        plotly::plotlyOutput(ns("overview_plot"), height = height)
      })
      output$overview_plot <- plotly::renderPlotly({
        maxp <- max(c(cur_period(), selected()$period), na.rm = TRUE)
        p <- plot_milestone_heat_table(selected(), bands(), goals(),
                                       max_period = maxp, source = ns("heat"))
        shiny::req(p)
        plotly::event_register(p, "plotly_click")
      })
      shiny::observeEvent(plotly::event_data("plotly_click", source = ns("heat")), {
        ev <- plotly::event_data("plotly_click", source = ns("heat"))
        sc <- milestone_subcompetencies()$subcomp
        y <- suppressWarnings(as.integer(round(ev$y[1])))
        if (!is.na(y) && y >= 1 && y <= length(sc))
          shiny::updateSelectInput(session, "subcomp", selected = sc[length(sc) + 1 - y])
      })
    }

    # ── Dumbbell ──────────────────────────────────────────────────────────
    if ("dumbbell" %in% show) {
      avail_periods <- shiny::reactive(sort(unique(long_res()$period)))
      output$dumbbell_period_ui <- shiny::renderUI({
        ps <- avail_periods()
        if (!length(ps)) return(NULL)
        sel <- if (!is.na(cur_period()) && cur_period() %in% ps) cur_period() else max(ps)
        shiny::selectInput(ns("dumbbell_period"), NULL, width = "180px",
                           choices = stats::setNames(ps, .mg_period_name(ps)),
                           selected = sel)
      })
      dumbbell_period <- shiny::reactive({
        p <- suppressWarnings(as.integer(input$dumbbell_period))
        if (length(p) && !is.na(p)) p else cur_period()
      })
      output$dumbbell_ui <- shiny::renderUI({
        d <- long_res()
        if (!nrow(d) || !any(d$period == dumbbell_period(), na.rm = TRUE))
          return(.mg_empty_state("No self or faculty ratings for this period."))
        plotly::plotlyOutput(ns("dumbbell_plot"), height = height)
      })
      output$dumbbell_plot <- plotly::renderPlotly({
        p <- plot_milestone_dumbbell(long_res(), dumbbell_period(), goals())
        shiny::req(p)
        p
      })
    }

    # ── Trajectory ────────────────────────────────────────────────────────
    traj <- shiny::reactive({
      shiny::req(input$subcomp)
      if (!any(long_res()$subcomp == input$subcomp)) return(NULL)
      plot_milestone_trajectory(long_res(), input$subcomp, fit = fit_r(),
                                bands = bands(), goals = goals())
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
        shiny::div(class = "small mt-1", lapply(td$readout, function(x) shiny::tags$p(class = "mb-1", shiny::HTML(x))))
      })
    }

    invisible(list(
      selected_subcomp = shiny::reactive(input$subcomp),
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
