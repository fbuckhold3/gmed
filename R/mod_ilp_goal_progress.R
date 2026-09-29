# mod_ilp_goal_progress.R
# ILP goal progress, kept separate from the milestone charts. For every ILP
# goal a resident has set, shows the target (goal level -> rating), the
# rating that counted when the goal was set, and the next rating after it.

#' All ILP goals a resident has set
#'
#' @param ilp_data ILP instrument rows (\code{record_id}, \code{year_resident}
#'   and the goal / goal level fields).
#' @param resident_id Resident record id.
#' @return Data frame, one row per goal: \code{period} (when set),
#'   \code{domain}, \code{subcomp}, \code{goal_level}, \code{target_rating}.
#'   Newest first.
#' @export
ilp_goal_history <- function(ilp_data, resident_id) {
  empty <- data.frame(period = integer(0), domain = character(0),
                      subcomp = character(0), goal_level = numeric(0),
                      target_rating = numeric(0), stringsAsFactors = FALSE)
  ilp <- .mg_instrument_rows(ilp_data, "ilp")
  if (is.null(ilp) || !"year_resident" %in% names(ilp) || is.null(resident_id))
    return(empty)
  ilp <- ilp[as.character(ilp$record_id) == as.character(resident_id), , drop = FALSE]
  ilp$period_num <- .mg_period_num(ilp$year_resident)
  ilp <- ilp[!is.na(ilp$period_num), , drop = FALSE]
  if (!nrow(ilp)) return(empty)
  # Last instance per period wins
  ilp <- ilp[!duplicated(ilp$period_num, fromLast = TRUE), , drop = FALSE]
  rows <- lapply(seq_len(nrow(ilp)), function(i) .mg_ilp_goal_rows(ilp[i, , drop = FALSE]))
  out <- do.call(rbind, rows)
  if (is.null(out)) return(empty)
  out <- out[order(-out$period, match(out$domain, names(.MG_ILP_DOMAINS))),
             c("period", "domain", "subcomp", "goal_level", "target_rating")]
  rownames(out) <- NULL
  out
}

#' ILP goal progress: target vs the ratings around it
#'
#' @param ilp_data ILP rows.
#' @param milestone_data Long milestone data (or forms; see
#'   \code{as_milestone_long()}).
#' @param resident_id Resident record id.
#' @return \code{ilp_goal_history()} plus \code{rating_at_set} (the rating that
#'   counted in the period the goal was set), \code{next_period},
#'   \code{next_rating}, \code{next_source} (first rating after the goal was
#'   set) and \code{status}: "Reached" (next rating at or above the target),
#'   "Not yet" or "Awaiting next rating".
#' @export
ilp_goal_progress_data <- function(ilp_data, milestone_data, resident_id) {
  g <- ilp_goal_history(ilp_data, resident_id)
  if (!nrow(g)) {
    g$rating_at_set <- numeric(0); g$next_period <- integer(0)
    g$next_rating <- numeric(0); g$next_source <- character(0)
    g$status <- character(0)
    return(g)
  }
  long <- as_milestone_long(milestone_data)
  sel <- select_milestone_rating(long[long$record_id == as.character(resident_id), , drop = FALSE])
  g$rating_at_set <- NA_real_; g$next_period <- NA_integer_
  g$next_rating <- NA_real_; g$next_source <- NA_character_
  for (i in seq_len(nrow(g))) {
    s <- sel[sel$subcomp == g$subcomp[i], , drop = FALSE]
    at <- s$rating[s$period == g$period[i]]
    if (length(at)) g$rating_at_set[i] <- at[1]
    later <- s[s$period > g$period[i], , drop = FALSE]
    if (nrow(later)) {
      later <- later[which.min(later$period), ]
      g$next_period[i] <- later$period
      g$next_rating[i] <- later$rating
      g$next_source[i] <- later$source
    }
  }
  g$status <- ifelse(is.na(g$next_rating), "Awaiting next rating",
                     ifelse(!is.na(g$target_rating) & g$next_rating >= g$target_rating,
                            "Reached", "Not yet"))
  g
}

#' ILP goal progress module UI
#'
#' @param id Module id.
#' @return UI output.
#' @export
mod_ilp_goal_progress_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(class = "card mb-3", shiny::div(class = "card-body",
    shiny::tags$h6(class = "card-title text-muted text-uppercase",
                   style = "font-size:0.75rem; letter-spacing:.06em;", "ILP goals"),
    shiny::tags$p(class = "small text-muted mb-2",
      "Each goal's target is its milestone level on the 1-9 rating scale ",
      "(level 1 = 1, 2 = 3, 3 = 5, 4 = 7, 5 = 9). 'Next rating' is the first ",
      "rating after the goal was set (ACGME, else CCC, else coach)."),
    shiny::uiOutput(ns("table"))))
}

#' ILP goal progress module server
#'
#' @param id Module id.
#' @param ilp_data Reactive (or static) ILP rows (\code{all_forms$ilp}).
#' @param milestone_data Reactive (or static) long milestone data from
#'   \code{build_milestone_long()}.
#' @param resident_id Reactive resident record id.
#' @param max_periods Show goals from at most this many most recent periods
#'   (\code{NULL} = all).
#' @return Invisibly, a reactive of \code{ilp_goal_progress_data()}.
#' @export
mod_ilp_goal_progress_server <- function(id, ilp_data, milestone_data, resident_id,
                                         max_periods = NULL) {
  as_r <- function(x) if (shiny::is.reactive(x)) x else shiny::reactive(x)
  ilp_r <- as_r(ilp_data); ms_r <- as_r(milestone_data); rid_r <- as_r(resident_id)
  shiny::moduleServer(id, function(input, output, session) {
    prog <- shiny::reactive({
      rid <- rid_r()
      if (is.null(rid)) return(NULL)
      d <- tryCatch(ilp_goal_progress_data(ilp_r(), ms_r(), rid), error = function(e) {
        message("mod_ilp_goal_progress: ", e$message); NULL
      })
      if (!is.null(d) && !is.null(max_periods) && nrow(d)) {
        keep <- utils::head(sort(unique(d$period), decreasing = TRUE), max_periods)
        d <- d[d$period %in% keep, , drop = FALSE]
      }
      d
    })
    output$table <- shiny::renderUI({
      d <- prog()
      if (is.null(d) || !nrow(d)) return(.mg_empty_state("No ILP goals recorded yet."))
      ilp_goal_progress_table(d)
    })
    invisible(prog)
  })
}

#' HTML table for ILP goal progress
#'
#' @param d \code{ilp_goal_progress_data()} output.
#' @return A \code{shiny.tag} table.
#' @export
ilp_goal_progress_table <- function(d) {
  sc <- milestone_subcompetencies()
  badge <- function(status) {
    st <- switch(status, "Reached" = "success", "Not yet" = "warning", "pending")
    if (requireNamespace("roundsui", quietly = TRUE)) {
      roundsui::roundsui_status_badge(status, status = st)
    } else {
      shiny::tags$span(class = paste0("badge text-bg-",
        switch(st, success = "success", warning = "warning", "secondary")), status)
    }
  }
  num <- function(x) if (is.na(x)) shiny::HTML("&#8211;") else format(x)
  domain_lab <- c(pcmk = "PC / MK", sbppbl = "SBP / PBL", profics = "PROF / ICS")
  rows <- lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    shiny::tags$tr(
      shiny::tags$td(.mg_period_name(r$period)),
      shiny::tags$td(domain_lab[[r$domain]]),
      shiny::tags$td(sc$label[sc$subcomp == r$subcomp]),
      shiny::tags$td(if (is.na(r$target_rating)) shiny::HTML("&#8211;") else
        sprintf("Level %s (rating %s)", r$goal_level, r$target_rating)),
      shiny::tags$td(num(r$rating_at_set)),
      shiny::tags$td(if (is.na(r$next_rating)) shiny::HTML("&#8211;") else
        sprintf("%s at %s (%s)", r$next_rating, .mg_period_name(r$next_period),
                .MG_SOURCE_LABEL[[r$next_source]])),
      shiny::tags$td(badge(r$status)))
  })
  shiny::div(class = "table-responsive", shiny::tags$table(
    class = "table table-sm align-middle mb-0", style = "font-size:0.85rem;",
    shiny::tags$thead(shiny::tags$tr(lapply(
      c("Set at", "Domain", "Subcompetency", "Target", "Rating when set",
        "Next rating", "Status"), shiny::tags$th))),
    shiny::tags$tbody(rows)))
}
