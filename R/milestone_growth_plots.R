# milestone_growth_plots.R
# Pure chart functions for the shared milestone growth module. Each view has
# a *_data() function (testable, no plotly) and a plot_*() function that
# returns a plotly object, or NULL when there is nothing to draw (the module
# shows an empty state instead of a blank chart).
#
# Context is always the cohort range (where past residents at the same period
# fell) plus plain-language guidance. There are no stage-based expectation
# lines, and ILP goals live in their own module (mod_ilp_goal_progress.R).
# Strings use HTML entities rather than non-ASCII characters so they render
# the same in any R locale.

# ── Palette & chrome ─────────────────────────────────────────────────────────

.MG_COL <- list(
  acgme  = "#2a78d6",   # categorical slot 1 (blue)
  ccc    = "#eb6834",   # slot 2 (orange)
  coach  = "#1baf7a",   # slot 3 (aqua)
  self   = "#52514e",   # secondary ink (open marker)
  ink    = "#0b0b0b",
  ink2   = "#52514e",
  muted  = "#898781",
  grid   = "#e1e0d9",
  range_outer = "#e1e0d9",                  # 10th-90th
  range_inner = "#c3c2b7",                  # 25th-75th
  band_outer  = "rgba(137,135,129,0.14)",
  band_inner  = "rgba(137,135,129,0.26)",
  proj   = "rgba(42,120,214,0.16)"
)
.MG_SYMBOL <- c(acgme = "diamond", ccc = "square", coach = "circle",
                self = "circle-open")
.MG_SOURCE_LABEL <- c(acgme = "ACGME", ccc = "CCC", coach = "Coach",
                      self = "Self")

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

# Horizontal lines between competency groups, for charts whose rows are the
# 21 subcompetencies at y = 21 (PC1) ... 1 (ICS3).
.mg_group_separators <- function() {
  sc <- milestone_subcompetencies()
  ny <- nrow(sc)
  grp_end <- cumsum(rle(sc$competency)$lengths)
  lapply(grp_end[-length(grp_end)], function(k) list(
    type = "line", xref = "paper", x0 = 0, x1 = 1,
    y0 = ny + 0.5 - k, y1 = ny + 0.5 - k,
    line = list(color = .MG_COL$grid, width = 1)))
}

# ── View 1: range snapshot ───────────────────────────────────────────────────

#' Data for the range snapshot
#'
#' @param ratings Resident's selected ratings
#'   (\code{select_milestone_rating()} rows for one resident).
#' @param bands Cohort bands (\code{fit_cohort_bands()}).
#' @param period Period (1-6).
#' @return Data frame, one row per subcompetency (display order) with
#'   \code{rating}, \code{source}, the cohort percentiles and
#'   \code{position} (\code{milestone_range_position()}).
#' @export
milestone_range_data <- function(ratings, bands, period) {
  sc <- milestone_subcompetencies()
  r <- ratings[ratings$period == period, c("subcomp", "rating", "source"), drop = FALSE]
  b <- if (is.null(bands) || !nrow(bands)) .mg_empty_bands() else bands
  b <- b[b$period == period, c("subcomp", "p10", "p25", "p50", "p75", "p90"), drop = FALSE]
  out <- merge(data.frame(subcomp = sc$subcomp, label = sc$label,
                          competency = sc$competency, stringsAsFactors = FALSE),
               r, by = "subcomp", all.x = TRUE)
  out <- merge(out, b, by = "subcomp", all.x = TRUE)
  out$period <- as.integer(period)
  out$position <- milestone_range_position(out$rating, out$p10, out$p25, out$p75, out$p90)
  out <- out[match(sc$subcomp, out$subcomp), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' One-paragraph guidance for a range snapshot
#'
#' @param rd \code{milestone_range_data()} output.
#' @return Character (HTML entities allowed) or \code{NULL} if nothing is rated.
#' @export
milestone_range_summary <- function(rd) {
  rated <- rd[!is.na(rd$position), , drop = FALSE]
  if (!nrow(rated)) return(NULL)
  n_in <- sum(rated$position %in% c("lower", "middle", "upper"))
  txt <- sprintf(
    "At %s, %d of %d rated subcompetencies are within the usual range for past residents at the same point (middle 80%%).",
    .mg_period_name(rd$period[1]), n_in, nrow(rated))
  codes <- function(pos) paste(toupper(rated$subcomp[rated$position == pos]), collapse = ", ")
  if (any(rated$position == "above")) txt <- paste0(txt, " Above the usual range: ", codes("above"), ".")
  if (any(rated$position == "below")) txt <- paste0(txt, " Below the usual range: ", codes("below"), ".")
  txt
}

#' Range snapshot: each subcompetency against the cohort range
#'
#' For one period, one row per subcompetency (grouped by competency) showing
#' where past residents at the same period fell (light bar: 10th-90th
#' percentile, the "usual range"; darker bar: 25th-75th; tick: median) and
#' the resident's rating that counts, with marker shape and colour showing
#' its source. Row y values are 21 (PC1) down to 1 (ICS3); a click event's
#' \code{y} maps to \code{milestone_subcompetencies()$subcomp[22 - y]}.
#'
#' @inheritParams milestone_range_data
#' @param source Plotly event source id (for click handling).
#' @return A plotly object (with the summary sentence as attribute
#'   \code{"milestone_range_summary"}), or \code{NULL} when the resident has
#'   no rating in that period.
#' @export
plot_milestone_range <- function(ratings, bands, period, source = "mg_range") {
  if (is.null(ratings) || !nrow(ratings) || is.na(period)) return(NULL)
  rd <- milestone_range_data(ratings, bands, period)
  if (all(is.na(rd$rating))) return(NULL)
  ny <- nrow(rd)
  rd$y <- rev(seq_len(ny))
  seg <- function(lo, hi) {
    k <- !is.na(lo) & !is.na(hi)
    list(x = as.vector(rbind(lo[k], hi[k], NA)), y = as.vector(rbind(rd$y[k], rd$y[k], NA)))
  }
  outer <- seg(rd$p10, rd$p90)
  inner <- seg(rd$p25, rd$p75)
  band_hover <- sprintf("%s<br>Past residents at %s:<br>middle 80%%: %s&#8211;%s<br>middle half: %s&#8211;%s<br>median: %s",
                        rd$label, .mg_period_name(period), .mg_fmt(rd$p10), .mg_fmt(rd$p90),
                        .mg_fmt(rd$p25), .mg_fmt(rd$p75), .mg_fmt(rd$p50))

  p <- plotly::plot_ly(source = source) |>
    plotly::add_trace(x = outer$x, y = outer$y, type = "scatter", mode = "lines",
                      line = list(color = .MG_COL$range_outer, width = 10),
                      name = "Usual range (10th&#8211;90th percentile)", hoverinfo = "skip") |>
    plotly::add_trace(x = inner$x, y = inner$y, type = "scatter", mode = "lines",
                      line = list(color = .MG_COL$range_inner, width = 10),
                      name = "Middle half (25th&#8211;75th)", hoverinfo = "skip") |>
    plotly::add_markers(x = rd$p50, y = rd$y, name = "Cohort median",
                        marker = list(symbol = "line-ns-open", size = 16, color = .MG_COL$ink2,
                                      line = list(color = .MG_COL$ink2, width = 2)),
                        text = band_hover, hoverinfo = "text")

  for (s in c("coach", "ccc", "acgme")) {
    k <- !is.na(rd$rating) & rd$source == s
    if (!any(k)) next
    p <- p |> plotly::add_markers(
      x = rd$rating[k], y = rd$y[k], name = .MG_SOURCE_LABEL[[s]],
      marker = list(color = .MG_COL[[s]], symbol = .MG_SYMBOL[[s]], size = 12,
                    line = list(color = "#ffffff", width = 2)),
      text = paste0(rd$label[k], "<br>Rating ", .mg_fmt(rd$rating[k], 0), " (",
                    .MG_SOURCE_LABEL[[s]], ")<br>",
                    ifelse(is.na(rd$position[k]), "No cohort range for this period",
                           paste0("Sits ", .MG_POSITION_LABEL[rd$position[k]]))),
      hoverinfo = "text")
  }

  out <- p |>
    .mg_style(
      xaxis = list(range = c(0.5, 9.5), dtick = 1, title = "Rating (1&#8211;9)",
                   zeroline = FALSE),
      yaxis = list(tickvals = rd$y, ticktext = toupper(rd$subcomp), title = "",
                   range = c(0.4, ny + 0.6), zeroline = FALSE, showgrid = FALSE),
      shapes = .mg_group_separators(),
      legend = list(orientation = "h", x = 0, y = -0.11, yanchor = "top"),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 20, b = 90)
    ) |>
    plotly::config(displayModeBar = FALSE)
  attr(out, "milestone_range_summary") <- milestone_range_summary(rd)
  out
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
#' @return A plotly object, or \code{NULL} if the period has no ratings.
#' @export
plot_milestone_dumbbell <- function(long, period) {
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

  p <- plotly::plot_ly() |>
    plotly::add_trace(x = seg_x, y = seg_y, type = "scatter", mode = "lines",
                      line = list(color = .MG_COL$range_inner, width = 3),
                      hoverinfo = "skip", showlegend = FALSE)
  fac_src <- ifelse(is.na(dd$faculty_source), "coach", dd$faculty_source)
  for (s in c("coach", "ccc")) {
    k <- !is.na(dd$faculty) & fac_src == s
    if (!any(k)) next
    p <- p |> plotly::add_markers(
      x = dd$faculty[k], y = dd$y[k], name = paste("Faculty:", .MG_SOURCE_LABEL[[s]]),
      marker = list(color = .MG_COL[[s]], symbol = .MG_SYMBOL[[s]], size = 11,
                    line = list(color = "#ffffff", width = 2)),
      text = hov("Faculty", dd$faculty, .MG_SOURCE_LABEL[fac_src])[k],
      hoverinfo = "text")
  }
  if (any(!is.na(dd$self))) {
    k <- !is.na(dd$self)
    p <- p |> plotly::add_markers(
      x = dd$self[k], y = dd$y[k], name = "Self",
      marker = list(color = .MG_COL$self, symbol = .MG_SYMBOL[["self"]], size = 11,
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

  p |>
    .mg_style(
      xaxis = list(range = c(0.5, 9.5), dtick = 1, title = "Rating (1&#8211;9)",
                   zeroline = FALSE),
      yaxis = list(tickvals = dd$y, ticktext = toupper(dd$subcomp),
                   title = "", range = c(0.3, n + 0.7), zeroline = FALSE,
                   showgrid = FALSE),
      legend = list(orientation = "h", x = 0, y = -0.11, yanchor = "top"),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 20, b = 90)
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
  if (nrow(sel) && !is.null(band)) {
    g <- milestone_range_guidance(sel$rating[nrow(sel)], band[band$period == last_p, , drop = FALSE])
    if (!is.null(g)) readout <- g
  }
  if (last_p == 6) {
    readout <- c(readout, sprintf("Graduation rating: %s (%s).", .mg_fmt(sel$rating[nrow(sel)], 0),
                                  .MG_SOURCE_LABEL[[sel$source[nrow(sel)]]]))
  } else if (!is.null(grad)) {
    readout <- c(readout, sprintf(
      "Projected graduation rating %s (%d%% prediction interval %s&#8211;%s); chance of reaching 7 by graduation: %d%%.",
      .mg_fmt(grad$fit), round(100 * level), .mg_fmt(grad$lwr), .mg_fmt(grad$upr),
      round(100 * grad$p_reach)))
  } else if (nrow(sel)) {
    readout <- c(readout, "Projection unavailable: no fitted growth model for this subcompetency.")
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
#' Shows the cohort range by period (light: 10th-90th percentile, darker:
#' 25th-75th, dotted: median), the resident's ratings (marker shape =
#' source), the projection with its prediction interval, and a readout with
#' range guidance and P(reach 7). The graduation target line at 7 is
#' optional.
#'
#' @inheritParams milestone_trajectory_data
#' @param show_self Show self ratings (hidden in the legend by default).
#' @param show_target Draw the graduation target line at 7.
#' @return A plotly object, or \code{NULL} when the resident has no ratings
#'   for this subcompetency. The trajectory data (incl. \code{readout}) is
#'   attached as attribute \code{"milestone_trajectory"}.
#' @export
plot_milestone_trajectory <- function(long, subcomp, fit = NULL, bands = NULL,
                                      level = 0.8, show_self = FALSE,
                                      show_target = TRUE) {
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
                          fillcolor = .MG_COL$band_outer, line = list(width = 0),
                          name = "Usual range (10th&#8211;90th percentile)",
                          text = sprintf("%s<br>Past residents: middle 80%% %s&#8211;%s",
                                         pnames[band$period], .mg_fmt(band$p10), .mg_fmt(band$p90)),
                          hoverinfo = "text") |>
      plotly::add_ribbons(x = band$period, ymin = band$p25, ymax = band$p75,
                          fillcolor = .MG_COL$band_inner, line = list(width = 0),
                          name = "Middle half (25th&#8211;75th)",
                          text = sprintf("%s<br>Past residents: middle half %s&#8211;%s",
                                         pnames[band$period], .mg_fmt(band$p25), .mg_fmt(band$p75)),
                          hoverinfo = "text") |>
      plotly::add_lines(x = band$period, y = band$p50, name = "Cohort median",
                        line = list(color = .MG_COL$muted, width = 2, dash = "dot"),
                        text = sprintf("%s<br>Cohort median %s", pnames[band$period],
                                       .mg_fmt(band$p50)), hoverinfo = "text")
  }
  if (isTRUE(show_target)) {
    p <- p |> plotly::add_lines(x = c(0.7, 6.3), y = c(.MG_TARGET, .MG_TARGET),
                                line = list(color = .MG_COL$ink, width = 1),
                                name = "Graduation target (7)", hoverinfo = "skip")
  }

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

  out <- p |>
    .mg_style(
      title = list(text = paste0("<b>", lab, "</b>"), x = 0, xanchor = "left",
                   font = list(size = 14)),
      xaxis = list(tickvals = 1:6, ticktext = pnames, range = c(0.7, 6.3),
                   title = "", zeroline = FALSE),
      yaxis = list(range = c(0.5, 9.5), dtick = 1, title = "Rating (1&#8211;9)",
                   zeroline = FALSE),
      legend = list(orientation = "h", x = 0, y = -0.12, yanchor = "top"),
      hovermode = "closest",
      margin = list(l = 55, r = 20, t = 40, b = 90)
    ) |>
    plotly::config(displayModeBar = FALSE)
  attr(out, "milestone_trajectory") <- td
  out
}

# ── View 4: spider (the familiar radar) ──────────────────────────────────────

#' Cohort median per subcompetency for one period and rater
#'
#' @param long Long milestone data for all residents.
#' @param period Period (1-6).
#' @param rater "faculty" (the rating that counts: ACGME, else CCC, else
#'   coach), "acgme" or "self".
#' @return Named numeric vector (names = subcomp ids).
#' @export
milestone_cohort_medians <- function(long, period, rater = c("faculty", "acgme", "self")) {
  rater <- match.arg(rater)
  d <- long[long$period == period, , drop = FALSE]
  d <- if (rater == "faculty") select_milestone_rating(d) else d[d$rater == rater, , drop = FALSE]
  if (!nrow(d)) return(stats::setNames(numeric(0), character(0)))
  m <- tapply(d$rating, d$subcomp, stats::median, na.rm = TRUE)
  m[intersect(milestone_subcompetencies()$subcomp, names(m))]
}

#' Milestone spider (radar) for one period
#'
#' The radar chart the programs already use, drawn by
#' \code{create_enhanced_milestone_spider_plot()} (unchanged look), fed from
#' long milestone data: the resident's ratings for one period against the
#' cohort median for the same period and rater.
#'
#' @param long_res Resident's long milestone data (all raters).
#' @param long_all Long milestone data for the cohort (for the medians).
#' @param period Period (1-6).
#' @param rater "faculty" (rating that counts), "acgme" or "self".
#' @param resident_name Optional name for the hover text.
#' @return A plotly object, or \code{NULL} when the resident has no ratings
#'   from that rater in that period.
#' @export
plot_milestone_spider <- function(long_res, long_all, period,
                                  rater = c("faculty", "acgme", "self"),
                                  resident_name = NULL) {
  rater <- match.arg(rater)
  if (is.null(long_res) || !nrow(long_res) || is.na(period)) return(NULL)
  d <- long_res[long_res$period == period, , drop = FALSE]
  r <- if (rater == "faculty") select_milestone_rating(d) else d[d$rater == rater, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  med <- milestone_cohort_medians(long_all, period, rater)

  sc <- milestone_subcompetencies()
  field <- switch(rater, faculty = sc$field_coach, acgme = sc$field_acgme, self = sc$field_self)
  pfield <- switch(rater, faculty = "prog_mile_period", acgme = "acgme_mile_period",
                   self = "prog_mile_period_self")
  type <- switch(rater, faculty = "program", acgme = "acgme", self = "self")

  rid <- as.character(r$record_id[1])
  wide <- data.frame(record_id = rid, stringsAsFactors = FALSE)
  wide[[pfield]] <- as.character(period)
  med_df <- data.frame(x = as.character(period), stringsAsFactors = FALSE)
  names(med_df) <- pfield
  for (j in seq_len(nrow(sc))) {
    wide[[field[j]]] <- r$rating[match(sc$subcomp[j], r$subcomp)]
    med_df[[field[j]]] <- unname(med[sc$subcomp[j]])
  }
  res_data <- if (!is.null(resident_name))
    data.frame(record_id = rid, name = resident_name, stringsAsFactors = FALSE) else NULL
  p <- create_enhanced_milestone_spider_plot(
    milestone_data = wide, median_data = med_df, resident_id = rid,
    period_text = .mg_period_name(period), milestone_type = type,
    resident_data = res_data)
  if (requireNamespace("roundsui", quietly = TRUE)) p <- roundsui::roundsui_plotly_layout(p)
  p
}
