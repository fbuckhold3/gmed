# milestone_growth_plots.R
# Pure chart functions for the shared milestone growth module. Each view has
# a *_data() function (testable, no plotly) and a plot_*() function that
# returns a plotly object, or NULL when there is nothing to draw (the module
# shows an empty state instead of a blank chart).

# ── Palette & chrome ─────────────────────────────────────────────────────────

.MG_COL <- list(
  acgme  = "#2a78d6",   # categorical slot 1 (blue)
  ccc    = "#eb6834",   # slot 2 (orange)
  coach  = "#1baf7a",   # slot 3 (aqua)
  self   = "#898781",   # muted ink
  goal   = "#4a3aa7",   # slot 7 (violet)
  ink    = "#0b0b0b",
  ink2   = "#52514e",
  muted  = "#898781",
  grid   = "#e1e0d9",
  band   = "rgba(137,135,129,0.20)",
  proj   = "rgba(42,120,214,0.16)"
)
.MG_SYMBOL <- c(acgme = "diamond", ccc = "square", coach = "circle",
                self = "triangle-up-open")
.MG_SOURCE_LABEL <- c(acgme = "ACGME", ccc = "CCC", coach = "Coach",
                      self = "Self")
# Diverging: red (below cohort median) -> neutral gray -> blue (above)
.MG_DIVERGING <- list(c(0, "#c83b3a"), c(0.25, "#eea3a2"), c(0.5, "#f0efec"),
                      c(0.75, "#86b6ef"), c(1, "#1c5cab"))
.MG_GOAL_GLYPH <- c(current = "&#9670;", previous = "&#9671;")   # ◆ ◇

# roundsui chrome when available; plain plotly layout otherwise.
.mg_style <- function(p, ...) {
  if (requireNamespace("roundsui", quietly = TRUE)) {
    roundsui::roundsui_plotly_layout(p, ...)
  } else {
    plotly::layout(p, font = list(family = "system-ui, sans-serif", size = 12,
                                  color = .MG_COL$ink2),
                   plot_bgcolor = "rgba(0,0,0,0)",
                   paper_bgcolor = "rgba(0,0,0,0)", ...)
  }
}

.mg_fmt <- function(x, d = 1) ifelse(is.na(x), "&#8211;", formatC(x, format = "f", digits = d))

# Subcomp tick labels with ILP goal glyphs appended.
.mg_tick_labels <- function(subcomps, goals = NULL) {
  lab <- toupper(subcomps)
  if (!is.null(goals) && nrow(goals)) {
    for (w in c("previous", "current")) {
      hit <- subcomps %in% goals$subcomp[goals$which == w]
      lab[hit] <- paste(lab[hit], .MG_GOAL_GLYPH[[w]])
    }
  }
  lab
}

.mg_goal_note <- function(goals) {
  if (is.null(goals) || !nrow(goals)) return(NULL)
  paste0(.MG_GOAL_GLYPH[["current"]], " current ILP goal   ",
         .MG_GOAL_GLYPH[["previous"]], " previous ILP goal")
}

# ── View 1: overview heat table ──────────────────────────────────────────────

#' Data for the overview heat table
#'
#' @param ratings Resident's selected ratings
#'   (\code{select_milestone_rating()} rows for one resident).
#' @param bands Cohort bands (\code{fit_cohort_bands()}).
#' @param max_period Last period to show (default: all).
#' @return Data frame, one row per subcomp x period with \code{rating},
#'   \code{source}, \code{p10}, \code{p50}, \code{p90}, \code{diff}
#'   (rating - p50) and \code{below_p10}.
#' @export
milestone_heat_data <- function(ratings, bands, max_period = 6) {
  sc <- milestone_subcompetencies()
  periods <- seq_len(max_period)
  grid <- expand.grid(subcomp = sc$subcomp, period = periods,
                      stringsAsFactors = FALSE)
  r <- ratings[, intersect(c("subcomp", "period", "rating", "source"), names(ratings))]
  out <- merge(grid, r, by = c("subcomp", "period"), all.x = TRUE)
  b <- if (is.null(bands) || !nrow(bands)) .mg_empty_bands() else bands
  out <- merge(out, b[, c("subcomp", "period", "p10", "p50", "p90")],
               by = c("subcomp", "period"), all.x = TRUE)
  out$diff <- out$rating - out$p50
  out$below_p10 <- !is.na(out$rating) & !is.na(out$p10) & out$rating < out$p10
  out[order(match(out$subcomp, sc$subcomp), out$period), , drop = FALSE]
}

#' Overview heat table: resident minus cohort median
#'
#' Rows are the 21 subcompetencies grouped by competency, columns are
#' periods, cells are the resident's rating minus the cohort median on a
#' diverging scale. Cells below the cohort 10th percentile are flagged with a
#' marker. Row y values are 1-21 (PC1 at the top); a click event's \code{y}
#' maps to \code{milestone_subcompetencies()$subcomp[22 - y]}.
#'
#' @inheritParams milestone_heat_data
#' @param goals ILP goals (\code{extract_ilp_goals()}), or \code{NULL}.
#' @param source Plotly event source id (for click handling).
#' @return A plotly object, or \code{NULL} when the resident has no ratings.
#' @export
plot_milestone_heat_table <- function(ratings, bands, goals = NULL,
                                      max_period = 6, source = "mg_heat") {
  if (is.null(ratings) || !nrow(ratings)) return(NULL)
  hd <- milestone_heat_data(ratings, bands, max_period)
  sc <- milestone_subcompetencies()
  periods <- seq_len(max_period)
  pnames <- .mg_period_name(periods)
  ny <- nrow(sc)
  yval <- ny + 1 - match(hd$subcomp, sc$subcomp)      # PC1 at the top

  zmat <- matrix(NA_real_, ny, length(periods))
  tmat <- matrix("", ny, length(periods))
  for (i in seq_len(nrow(hd))) {
    yi <- yval[i]; xi <- hd$period[i]
    zmat[yi, xi] <- hd$diff[i]
    lab <- sc$label[sc$subcomp == hd$subcomp[i]]
    tmat[yi, xi] <- if (is.na(hd$rating[i])) paste0(lab, "<br>", pnames[xi], "<br>No rating") else
      paste0(lab, "<br>", pnames[xi],
             "<br>Rating ", .mg_fmt(hd$rating[i], 0), " (", .MG_SOURCE_LABEL[hd$source[i]], ")",
             "<br>Cohort median ", .mg_fmt(hd$p50[i]), "  [p10 ", .mg_fmt(hd$p10[i]),
             ", p90 ", .mg_fmt(hd$p90[i]), "]",
             "<br>Difference ", ifelse(is.na(hd$diff[i]), "&#8211;", sprintf("%+.1f", hd$diff[i])),
             if (isTRUE(hd$below_p10[i])) "<br><b>Below cohort 10th percentile</b>" else "")
  }

  p <- plotly::plot_ly(source = source) |>
    plotly::add_heatmap(
      x = periods, y = seq_len(ny), z = zmat, text = tmat,
      hoverinfo = "text", zmin = -3, zmax = 3, zmid = 0,
      colorscale = .MG_DIVERGING, xgap = 2, ygap = 2,
      colorbar = list(title = list(text = "vs cohort<br>median"), len = 0.6,
                      tickvals = c(-3, 0, 3), ticktext = c("&#8722;3", "0", "+3")),
      name = "Resident &#8722; cohort median"
    )

  flag <- hd[hd$below_p10, , drop = FALSE]
  if (nrow(flag)) {
    p <- p |> plotly::add_markers(
      x = flag$period, y = ny + 1 - match(flag$subcomp, sc$subcomp),
      marker = list(symbol = "triangle-down", size = 9, color = .MG_COL$ink),
      name = "Below cohort 10th percentile", hoverinfo = "skip"
    )
  }

  # Competency group separators
  grp_end <- cumsum(rle(sc$competency)$lengths)
  seps <- lapply(grp_end[-length(grp_end)], function(k) list(
    type = "line", xref = "paper", x0 = 0, x1 = 1,
    y0 = ny + 0.5 - k, y1 = ny + 0.5 - k,
    line = list(color = .MG_COL$ink2, width = 1.5)))

  note <- .mg_goal_note(goals)
  p |>
    .mg_style(
      xaxis = list(tickvals = periods, ticktext = pnames, title = "",
                   side = "top", showgrid = FALSE, zeroline = FALSE),
      yaxis = list(tickvals = seq_len(ny),
                   ticktext = rev(.mg_tick_labels(sc$subcomp, goals)),
                   title = "", showgrid = FALSE, zeroline = FALSE,
                   range = c(0.5, ny + 0.5)),
      shapes = seps,
      showlegend = nrow(flag) > 0,
      legend = list(orientation = "h", x = 0, y = -0.02, yanchor = "top"),
      annotations = if (!is.null(note)) list(list(
        text = note, xref = "paper", yref = "paper", x = 0, y = -0.09,
        xanchor = "left", showarrow = FALSE,
        font = list(size = 11, color = .MG_COL$goal))) else NULL,
      margin = list(l = 70, r = 20, t = 40, b = 60)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── View 2: self vs faculty dumbbell ─────────────────────────────────────────

#' Data for the self vs faculty dumbbell
#'
#' The faculty rating is the CCC rating if present, else the coach rating;
#' ACGME is kept as a separate third mark.
#'
#' @param long Resident's long milestone data (all raters).
#' @param period Period (1-6).
#' @return Data frame per subcomp: \code{self}, \code{faculty},
#'   \code{faculty_source}, \code{acgme}, \code{gap} (self - faculty), sorted
#'   by absolute gap (largest first; rows without both at the end).
#' @export
milestone_dumbbell_data <- function(long, period) {
  sc <- milestone_subcompetencies()
  d <- long[long$period == period, , drop = FALSE]
  pick <- function(rater) {
    x <- d[d$rater == rater, c("subcomp", "rating")]
    stats::setNames(x$rating, x$subcomp)[sc$subcomp]
  }
  fac <- select_milestone_rating(d, order = c("ccc", "coach"))
  fac_r <- stats::setNames(fac$rating, fac$subcomp)[sc$subcomp]
  fac_s <- stats::setNames(fac$source, fac$subcomp)[sc$subcomp]
  out <- data.frame(subcomp = sc$subcomp, label = sc$label,
                    self = unname(pick("self")), faculty = unname(fac_r),
                    faculty_source = unname(fac_s), acgme = unname(pick("acgme")),
                    stringsAsFactors = FALSE)
  out$gap <- out$self - out$faculty
  out <- out[order(is.na(out$gap), -abs(out$gap), match(out$subcomp, sc$subcomp)), ]
  rownames(out) <- NULL
  out
}

#' Self vs faculty dumbbell for one period
#'
#' @param long Resident's long milestone data (all raters).
#' @param period Period (1-6).
#' @param goals ILP goals, or \code{NULL}.
#' @return A plotly object, or \code{NULL} if the period has no ratings.
#' @export
plot_milestone_dumbbell <- function(long, period, goals = NULL) {
  if (is.null(long) || !nrow(long) || is.na(period)) return(NULL)
  dd <- milestone_dumbbell_data(long, period)
  if (all(is.na(dd$self) & is.na(dd$faculty) & is.na(dd$acgme))) return(NULL)
  n <- nrow(dd)
  dd$y <- rev(seq_len(n))                   # largest gap at the top
  hov <- function(who, val, src = NULL) paste0(
    dd$label, "<br>", who, if (!is.null(src)) paste0(" (", src, ")") else "",
    ": ", .mg_fmt(val, 0),
    ifelse(is.na(dd$gap), "", sprintf("<br>Self &#8722; faculty: %+d", as.integer(dd$gap))))

  both <- !is.na(dd$self) & !is.na(dd$faculty)
  seg_x <- as.vector(rbind(dd$self[both], dd$faculty[both], NA))
  seg_y <- as.vector(rbind(dd$y[both], dd$y[both], NA))
  exp_val <- milestone_program_expectation()$expected[period]

  p <- plotly::plot_ly() |>
    plotly::add_trace(x = seg_x, y = seg_y, type = "scatter", mode = "lines",
                      line = list(color = "#c3c2b7", width = 3),
                      hoverinfo = "skip", showlegend = FALSE)
  fac_src <- ifelse(is.na(dd$faculty_source), "coach", dd$faculty_source)
  for (s in c("coach", "ccc")) {
    k <- !is.na(dd$faculty) & fac_src == s
    if (!any(k)) next
    p <- p |> plotly::add_markers(
      x = dd$faculty[k], y = dd$y[k], name = paste("Faculty:", .MG_SOURCE_LABEL[[s]]),
      marker = list(color = .MG_COL[[s]], symbol = .MG_SYMBOL[[s]], size = 11,
                    line = list(color = "#ffffff", width = 2)),
      text = hov(paste("Faculty"), dd$faculty, .MG_SOURCE_LABEL[fac_src])[k],
      hoverinfo = "text")
  }
  if (any(!is.na(dd$self))) {
    k <- !is.na(dd$self)
    p <- p |> plotly::add_markers(
      x = dd$self[k], y = dd$y[k], name = "Self",
      marker = list(color = .MG_COL$ink2, symbol = "circle-open", size = 11,
                    line = list(width = 2)),
      text = hov("Self", dd$self)[k], hoverinfo = "text")
  }
  if (any(!is.na(dd$acgme))) {
    k <- !is.na(dd$acgme)
    p <- p |> plotly::add_markers(
      x = dd$acgme[k], y = dd$y[k], name = "ACGME",
      marker = list(color = .MG_COL$acgme, symbol = .MG_SYMBOL[["acgme"]], size = 10,
                    line = list(color = "#ffffff", width = 1.5)),
      text = hov("ACGME", dd$acgme)[k], hoverinfo = "text")
  }

  note <- .mg_goal_note(goals)
  p |>
    .mg_style(
      xaxis = list(range = c(0.5, 9.5), dtick = 1, title = "Rating (1&#8211;9)",
                   zeroline = FALSE),
      yaxis = list(tickvals = dd$y, ticktext = .mg_tick_labels(dd$subcomp, goals),
                   title = "", range = c(0.3, n + 0.7), zeroline = FALSE,
                   showgrid = FALSE),
      shapes = list(
        list(type = "line", x0 = exp_val, x1 = exp_val, yref = "paper", y0 = 0, y1 = 1,
             line = list(color = .MG_COL$muted, dash = "dash", width = 1.5)),
        list(type = "line", x0 = .MG_TARGET, x1 = .MG_TARGET, yref = "paper", y0 = 0, y1 = 1,
             line = list(color = .MG_COL$ink, width = 1))),
      annotations = c(
        list(list(x = .MG_TARGET, y = 1, yref = "paper", yanchor = "bottom",
                  text = if (exp_val == .MG_TARGET) "Expected = graduation 7" else "Graduation 7",
                  showarrow = FALSE, font = list(size = 10, color = .MG_COL$ink))),
        if (exp_val != .MG_TARGET)
          list(list(x = exp_val, y = 1, yref = "paper", yanchor = "bottom",
                    text = paste("Expected", exp_val), showarrow = FALSE,
                    font = list(size = 10, color = .MG_COL$muted))),
        if (!is.null(note)) list(list(text = note, xref = "paper", yref = "paper",
                                      x = 0, y = -0.2, xanchor = "left", showarrow = FALSE,
                                      font = list(size = 11, color = .MG_COL$goal)))),
      legend = list(orientation = "h", x = 0, y = -0.1, yanchor = "top"),
      hovermode = "closest",
      margin = list(l = 70, r = 20, t = 30, b = 100)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── View 3: drill-down trajectory ────────────────────────────────────────────

#' Data for the drill-down trajectory
#'
#' @param long Resident's long milestone data (all raters).
#' @param subcomp Subcompetency id (e.g. "pc3").
#' @param fit A \code{milestone_growth_fit} (cached), or \code{NULL}.
#' @param bands Bands to use when \code{fit} is \code{NULL}.
#' @param level Prediction interval level.
#' @return List: \code{points} (all raters), \code{selected} (the rating that
#'   counts per period), \code{band}, \code{projection} (periods after the
#'   last observed one, or \code{NULL}), \code{graduation} (1-row prediction at
#'   period 6 or \code{NULL}), \code{empirical} (lookup list), \code{readout}
#'   (character vector; may contain HTML entities, render with \code{HTML()}).
#' @export
milestone_trajectory_data <- function(long, subcomp, fit = NULL, bands = NULL,
                                      level = 0.8) {
  pts <- long[long$subcomp == subcomp, , drop = FALSE]
  sel <- select_milestone_rating(pts)
  sel <- sel[order(sel$period), , drop = FALSE]
  b <- if (!is.null(fit)) fit$bands else bands
  band <- if (is.null(b)) NULL else b[b$subcomp == subcomp, , drop = FALSE]
  model <- if (!is.null(fit)) fit$models[[subcomp]] else NULL
  last_p <- if (nrow(sel)) max(sel$period) else 0L

  proj <- NULL; grad <- NULL
  if (!is.null(model)) {
    grad <- predict_milestone_growth(model, sel, periods = 6, level = level)
    if (last_p < 6) {
      proj <- predict_milestone_growth(model, sel, periods = seq(last_p + 1, 6), level = level)
    }
  }
  emp <- if (nrow(sel) && !is.null(fit) && last_p < 6)
    empirical_reach_lookup(fit$empirical, subcomp, last_p, sel$rating[nrow(sel)]) else NULL

  readout <- character(0)
  if (last_p == 6) {
    readout <- sprintf("Graduation rating: %s (%s).", .mg_fmt(sel$rating[nrow(sel)], 0),
                       .MG_SOURCE_LABEL[[sel$source[nrow(sel)]]])
  } else if (!is.null(grad)) {
    readout <- sprintf(
      "P(reach 7 by graduation): %d%%. Projected graduation rating %s (%d%% prediction interval %s&#8211;%s).",
      round(100 * grad$p_reach), .mg_fmt(grad$fit), round(100 * level),
      .mg_fmt(grad$lwr), .mg_fmt(grad$upr))
  } else {
    readout <- "Projection unavailable: no fitted growth model for this subcompetency."
  }
  if (!is.null(emp)) {
    readout <- c(readout, if (!is.null(emp$text)) emp$text else
      sprintf("Too few past residents with this rating at this period to show how they did (n = %d).", emp$n))
  }
  list(points = pts, selected = sel, band = band, projection = proj,
       graduation = grad, empirical = emp, readout = readout)
}

#' Drill-down trajectory for one subcompetency
#'
#' Shows the cohort band (10th-90th percentile, median dotted), the program
#' expectation step line, the graduation line at 7, the resident's ratings
#' (marker shape = source), the projection with its prediction interval, the
#' ILP goal targets, and a readout of P(reach 7).
#'
#' @inheritParams milestone_trajectory_data
#' @param goals ILP goals, or \code{NULL}.
#' @param show_self Show self ratings (hidden in the legend by default).
#' @return A plotly object, or \code{NULL} when the resident has no ratings
#'   for this subcompetency. The trajectory data (incl. \code{readout}) is
#'   attached as attribute \code{"milestone_trajectory"}.
#' @export
plot_milestone_trajectory <- function(long, subcomp, fit = NULL, bands = NULL,
                                      goals = NULL, level = 0.8, show_self = FALSE) {
  if (is.null(long) || !nrow(long)) return(NULL)
  td <- milestone_trajectory_data(long, subcomp, fit, bands, level)
  if (!nrow(td$points)) return(NULL)
  sc <- milestone_subcompetencies()
  lab <- sc$label[sc$subcomp == subcomp]
  pnames <- .mg_period_name(1:6)

  p <- plotly::plot_ly()
  band <- td$band
  if (!is.null(band) && nrow(band)) {
    p <- p |>
      plotly::add_ribbons(x = band$period, ymin = band$p10, ymax = band$p90,
                          fillcolor = .MG_COL$band, line = list(width = 0),
                          name = "Cohort 10th&#8211;90th percentile",
                          text = sprintf("%s<br>Cohort p10 %s &#8211; p90 %s", pnames[band$period],
                                         .mg_fmt(band$p10), .mg_fmt(band$p90)),
                          hoverinfo = "text") |>
      plotly::add_lines(x = band$period, y = band$p50, name = "Cohort median",
                        line = list(color = .MG_COL$muted, width = 2, dash = "dot"),
                        text = sprintf("%s<br>Cohort median %s", pnames[band$period],
                                       .mg_fmt(band$p50)), hoverinfo = "text")
  }

  exp <- milestone_program_expectation()
  p <- p |>
    plotly::add_lines(x = c(0.5, exp$period + 0.5), y = c(exp$expected[1], exp$expected),
                      line = list(color = .MG_COL$ink2, width = 2, dash = "dash", shape = "vh"),
                      name = "Program expectation", hoverinfo = "skip") |>
    plotly::add_lines(x = c(0.5, 6.5), y = c(.MG_TARGET, .MG_TARGET),
                      line = list(color = .MG_COL$ink, width = 1),
                      name = "Graduation target (7)", hoverinfo = "skip")

  # Projection from the last observed rating to graduation
  sel <- td$selected
  if (!is.null(td$projection) && nrow(sel)) {
    lp <- sel[nrow(sel), ]
    pr <- td$projection
    px <- c(lp$period, pr$period)
    p <- p |>
      plotly::add_ribbons(x = px, ymin = c(lp$rating, pr$lwr), ymax = c(lp$rating, pr$upr),
                          fillcolor = .MG_COL$proj, line = list(width = 0),
                          name = sprintf("%d%% prediction interval", round(100 * level)),
                          hoverinfo = "skip") |>
      plotly::add_lines(x = px, y = c(lp$rating, pr$fit),
                        line = list(color = .MG_COL$acgme, width = 2, dash = "dash"),
                        name = "Projection",
                        text = c("", sprintf("%s<br>Projected %s (%s&#8211;%s)<br>P(&#8805; 7) %d%%",
                                             pnames[pr$period], .mg_fmt(pr$fit), .mg_fmt(pr$lwr),
                                             .mg_fmt(pr$upr), round(100 * pr$p_reach))),
                        hoverinfo = "text")
  }

  if (nrow(sel)) {
    p <- p |> plotly::add_lines(x = sel$period, y = sel$rating,
                                line = list(color = .MG_COL$ink2, width = 2),
                                name = "Rating that counts", hoverinfo = "skip",
                                showlegend = FALSE)
  }
  pts <- td$points
  for (s in c("coach", "ccc", "acgme", "self")) {
    k <- pts$rater == s
    if (!any(k)) next
    is_self <- s == "self"
    p <- p |> plotly::add_markers(
      x = pts$period[k], y = pts$rating[k], name = .MG_SOURCE_LABEL[[s]],
      visible = if (is_self && !show_self) "legendonly" else TRUE,
      marker = list(color = .MG_COL[[s]], symbol = .MG_SYMBOL[[s]],
                    size = if (is_self) 9 else 11,
                    line = list(color = if (is_self) .MG_COL$self else "#ffffff", width = 2)),
      text = sprintf("%s<br>%s: %s", pnames[pts$period[k]], .MG_SOURCE_LABEL[[s]],
                     .mg_fmt(pts$rating[k], 0)),
      hoverinfo = "text")
  }

  g <- if (is.null(goals)) NULL else goals[goals$subcomp == subcomp & !is.na(goals$target_rating), , drop = FALSE]
  if (!is.null(g) && nrow(g)) {
    for (w in c("current", "previous")) {
      gw <- g[g$which == w, , drop = FALSE]
      if (!nrow(gw)) next
      p <- p |> plotly::add_markers(
        x = pmin(gw$period + 1, 6), y = gw$target_rating,
        name = paste(tools::toTitleCase(w), "ILP goal"),
        marker = list(color = .MG_COL$goal, size = 14,
                      symbol = if (w == "current") "star" else "star-open",
                      line = list(color = .MG_COL$goal, width = 1.5)),
        text = sprintf("%s ILP goal (set %s): level %s = rating %s",
                       tools::toTitleCase(w), pnames[gw$period],
                       .mg_fmt(gw$goal_level, 0), .mg_fmt(gw$target_rating, 0)),
        hoverinfo = "text")
    }
  }

  out <- p |>
    .mg_style(
      title = list(text = paste0("<b>", lab, "</b><br><span style='font-size:12px'>",
                                 td$readout[1], "</span>"),
                   x = 0, xanchor = "left", font = list(size = 14)),
      xaxis = list(tickvals = 1:6, ticktext = pnames, range = c(0.7, 6.3),
                   title = "", zeroline = FALSE),
      yaxis = list(range = c(0.5, 9.5), dtick = 1, title = "Rating (1&#8211;9)",
                   zeroline = FALSE),
      legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top"),
      hovermode = "closest",
      margin = list(l = 55, r = 20, t = 70, b = 90)
    ) |>
    plotly::config(displayModeBar = FALSE)
  attr(out, "milestone_trajectory") <- td
  out
}
