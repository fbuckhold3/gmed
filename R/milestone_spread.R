# milestone_spread.R
# Program spread of milestone ratings by period (median + 25th-75th
# percentile), a resident's own rating history, and the small progression
# chart the milestone rating module shows above each subcompetency.
#
# Method follows Park et al., JAMA Netw Open 2021 (Fig. 1): median and
# interquartile range per reporting period. Their trajectory groups need
# ~500+ learners per subcompetency, so they are not attempted here.
#
# Flow:
#   rdm-data-refresh: milestone_spread(historical_medians, grad_years)
#                     -> write_milestone_spread_cache()  (app_cache record)
#   apps:             load_cached_milestone_spread(), falling back to
#                     milestone_spread(historical_medians) when missing.

# Period codes as stored in REDCap (7 = Entering Residency, the self-eval
# baseline; ACGME reports start at 1), in chronological order.
.MS_PERIODS <- data.frame(
  period      = c("7", "1", "2", "3", "4", "5", "6"),
  period_name = c("Entering Residency", "Mid Intern", "End Intern",
                  "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating"),
  short       = c("Entry", "Mid PGY1", "End PGY1", "Mid PGY2", "End PGY2",
                  "Mid PGY3", "Grad"),
  chron       = 0:6,
  stringsAsFactors = FALSE
)

.MS_SPREAD_SOURCES <- c(acgme = "acgme_miles", self = "milestone_selfevaluation_c33c")

# rep_pc1 / rep_pc1_self / acgme_pc1 -> "pc1"
.ms_subcomp <- function(col) sub("^(rep_|acgme_)(.*?)(_self)?$", "\\2", col)

# grad_yr raw dropdown code -> year (1-13 = 2023-2035, 14-36 = 2000-2022);
# 4-digit years pass through. Same decode as period_mapping.R.
.ms_grad_year <- function(x) {
  n <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(n >= 1 & n <= 13, n + 2022, ifelse(n >= 14 & n <= 36, n + 1986,
         ifelse(n > 100, n, NA_real_)))
}

# One form's processed rows -> long (record_id, period, subcomp, rating),
# keeping only valid 1-9 ratings.
.ms_long <- function(df, cols) {
  if (is.null(df) || !nrow(df) || !length(cols)) return(NULL)
  per <- as.character(df$prog_mile_period %||% df$acgme_mile_period %||%
                      df$prog_mile_period_self %||% df$redcap_repeat_instance)
  per <- sub("\\.0+$", "", per)
  out <- do.call(rbind, lapply(cols, function(cl) {
    data.frame(record_id = as.character(df$record_id), period = per,
               subcomp = .ms_subcomp(cl),
               rating = suppressWarnings(as.numeric(as.character(df[[cl]]))),
               stringsAsFactors = FALSE)
  }))
  out[!is.na(out$rating) & out$rating >= 1 & out$rating <= 9 &
        out$period %in% .MS_PERIODS$period, , drop = FALSE]
}

#' Program milestone spread by period
#'
#' Median and 25th/75th percentiles of every subcompetency for each review
#' period, for ACGME (CCC-reported) ratings and resident self-ratings.
#' All residents in the input are used, graduates included.
#'
#' @param medians The \code{historical_medians} list from
#'   \code{load_rdm_complete()} (it must still carry \code{processed_data},
#'   i.e. not the slimmed cache version).
#' @param grad_years Optional data frame with \code{record_id} and
#'   \code{grad_yr} (raw code or year) for every resident, graduates
#'   included. When given, only ratings from academic years starting on or
#'   after \code{cohort_start} are kept, and residents without a graduation
#'   year are left out. When \code{NULL}, every rating is used.
#' @param cohort_start First academic year (July) to include. Default 2021.
#' @param min_n Periods with fewer ratings than this get \code{NA}
#'   percentiles (the count is still reported). Default 10.
#' @return Data frame: \code{source} ("acgme"/"self"), \code{subcomp}
#'   ("pc1"...), \code{period} (REDCap code, "7" = Entering), \code{period_name},
#'   \code{n}, \code{q25}, \code{median}, \code{q75}.
#' @export
milestone_spread <- function(medians, grad_years = NULL, cohort_start = 2021,
                             min_n = 10) {
  empty <- data.frame(source = character(), subcomp = character(),
                      period = character(), period_name = character(),
                      n = integer(), q25 = numeric(), median = numeric(),
                      q75 = numeric(), stringsAsFactors = FALSE)
  if (!is.list(medians)) return(empty)

  gy <- NULL
  if (!is.null(grad_years) && nrow(grad_years)) {
    gy <- stats::setNames(.ms_grad_year(grad_years$grad_yr),
                          as.character(grad_years$record_id))
  }

  rows <- lapply(names(.MS_SPREAD_SOURCES), function(src) {
    m <- medians[[.MS_SPREAD_SOURCES[[src]]]]
    if (is.null(m) || is.null(m$processed_data)) return(NULL)
    long <- .ms_long(m$processed_data, m$columns)
    if (is.null(long) || !nrow(long)) return(NULL)

    if (!is.null(gy)) {
      chron <- .MS_PERIODS$chron[match(long$period, .MS_PERIODS$period)]
      ay <- unname(gy[long$record_id]) - 3 + pmax(chron - 1, 0) %/% 2
      long <- long[!is.na(ay) & ay >= cohort_start, , drop = FALSE]
    }
    if (!nrow(long)) return(NULL)

    key <- paste(long$subcomp, long$period, sep = "|")
    agg <- lapply(split(long$rating, key), function(v) {
      q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
      if (length(v) < min_n) q <- c(NA_real_, NA_real_, NA_real_)
      c(n = length(v), q)
    })
    parts <- do.call(rbind, strsplit(names(agg), "|", fixed = TRUE))
    vals  <- do.call(rbind, agg)
    data.frame(source = src, subcomp = parts[, 1], period = parts[, 2],
               n = as.integer(vals[, 1]), q25 = vals[, 2], median = vals[, 3],
               q75 = vals[, 4], stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) return(empty)
  out$period_name <- .MS_PERIODS$period_name[match(out$period, .MS_PERIODS$period)]
  out <- out[order(out$source, out$subcomp,
                   .MS_PERIODS$chron[match(out$period, .MS_PERIODS$period)]), ]
  rownames(out) <- NULL
  out[, names(empty)]
}

#' One resident's milestone rating history
#'
#' @param acgme Rows of the \code{acgme_miles} form (any residents; filter
#'   with \code{record_id}).
#' @param self Rows of the \code{milestone_selfevaluation_c33c} form.
#' @param record_id Optional record to keep.
#' @return Data frame: \code{source}, \code{subcomp}, \code{period}, \code{rating}.
#' @export
milestone_history <- function(acgme = NULL, self = NULL, record_id = NULL) {
  one <- function(df, pattern, src) {
    if (is.null(df) || !nrow(df)) return(NULL)
    if (!is.null(record_id)) df <- df[as.character(df$record_id) == as.character(record_id), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    if (src == "self" && "prog_mile_period_self" %in% names(df))
      df$prog_mile_period <- df$prog_mile_period_self
    if (src == "acgme" && "acgme_mile_period" %in% names(df))
      df$prog_mile_period <- df$acgme_mile_period
    # A blank period field falls back to the repeat instance (= period code)
    if ("redcap_repeat_instance" %in% names(df)) {
      p <- as.character(df$prog_mile_period %||% NA)
      blank <- is.na(p) | !nzchar(p)
      p[blank] <- as.character(df$redcap_repeat_instance[blank])
      df$prog_mile_period <- p
    }
    long <- .ms_long(df, grep(pattern, names(df), value = TRUE))
    if (is.null(long) || !nrow(long)) return(NULL)
    data.frame(source = src, subcomp = long$subcomp, period = long$period,
               rating = long$rating, stringsAsFactors = FALSE)
  }
  out <- rbind(one(acgme, "^acgme_(pc|mk|sbp|pbl|prof|ics)\\d+$", "acgme"),
               one(self,  "^rep_(pc|mk|sbp|pbl|prof|ics)\\d+_self$", "self"))
  if (is.null(out)) {
    return(data.frame(source = character(), subcomp = character(),
                      period = character(), rating = numeric(),
                      stringsAsFactors = FALSE))
  }
  # One rating per source/subcomp/period (last row wins)
  out[!duplicated(out[, c("source", "subcomp", "period")], fromLast = TRUE), ]
}

# ── Cache ─────────────────────────────────────────────────────────────────────

#' Write the milestone spread to the REDCap app_cache record
#'
#' Stores \code{milestone_spread()} output as JSON in the
#' \code{cache_milestone_spread_json} field of the \code{app_cache} record.
#'
#' @param spread Output of \code{milestone_spread()}.
#' @param rdm_token,redcap_url REDCap credentials.
#' @param cache_record_id The app_cache record (the env var must be set; a
#'   write never falls back to a default record).
#' @param cohort_start Recorded alongside the data for reference.
#' @return \code{TRUE}/\code{FALSE}, invisibly.
#' @export
write_milestone_spread_cache <- function(spread,
                                         rdm_token       = Sys.getenv("RDM_TOKEN"),
                                         redcap_url      = Sys.getenv("REDCAP_URL"),
                                         cache_record_id = Sys.getenv("CACHE_RECORD_ID"),
                                         cohort_start    = 2021) {
  if (!nzchar(cache_record_id))
    stop("write_milestone_spread_cache: CACHE_RECORD_ID env var not set.")
  if (!is.data.frame(spread) || !nrow(spread)) {
    warning("write_milestone_spread_cache: spread is empty - nothing written")
    return(invisible(FALSE))
  }
  json <- jsonlite::toJSON(
    list(generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         cohort_start = cohort_start, rows = spread),
    auto_unbox = TRUE, na = "null", digits = 4)
  message(sprintf("write_milestone_spread_cache: %.1f KB, %d rows",
                  nchar(json, type = "bytes") / 1024, nrow(spread)))
  res <- REDCapR::redcap_write_oneshot(
    ds = data.frame(record_id = cache_record_id,
                    cache_milestone_spread_json = as.character(json),
                    stringsAsFactors = FALSE),
    redcap_uri = redcap_url, token = rdm_token, verbose = FALSE)
  if (!isTRUE(res$success)) {
    warning("write_milestone_spread_cache: REDCap write failed - ", res$outcome_message)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' Load the cached milestone spread
#'
#' @param rdm_token,redcap_url REDCap credentials.
#' @param cache_record_id The app_cache record (default: \code{CACHE_RECORD_ID}
#'   env var, else the prod record).
#' @param max_age_hours Report (message) when the cache is older than this.
#' @return The spread data frame (with attribute \code{generated_at}), or
#'   \code{NULL} when unavailable, so callers can fall back to
#'   \code{milestone_spread()}.
#' @export
load_cached_milestone_spread <- function(rdm_token       = Sys.getenv("RDM_TOKEN"),
                                         redcap_url      = Sys.getenv("REDCAP_URL"),
                                         cache_record_id = default_cache_record_id(),
                                         max_age_hours   = 36) {
  res <- tryCatch(
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_url, token = rdm_token, records = cache_record_id,
      fields = c("record_id", "cache_milestone_spread_json"),
      guess_type = FALSE, verbose = FALSE),
    error = function(e) NULL)
  dat <- if (!is.null(res) && isTRUE(res$success)) res$data else NULL
  if (is.null(dat) || !nrow(dat) || !"cache_milestone_spread_json" %in% names(dat))
    return(NULL)
  txt <- dat$cache_milestone_spread_json[1]
  if (is.na(txt) || !nzchar(txt)) return(NULL)
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE),
                  error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj$rows) || !nrow(obj$rows)) return(NULL)
  out <- obj$rows
  out$period <- as.character(out$period)
  gen <- tryCatch(as.POSIXct(obj$generated_at), error = function(e) NA)
  if (!is.na(gen)) {
    age <- as.numeric(difftime(Sys.time(), gen, units = "hours"))
    if (is.finite(max_age_hours) && age > max_age_hours)
      message(sprintf("load_cached_milestone_spread: cache is %.0f h old", age))
  }
  attr(out, "generated_at") <- obj$generated_at
  out
}

# ── Chart ─────────────────────────────────────────────────────────────────────

#' Milestone progression chart for one subcompetency
#'
#' A small inline SVG: the program's 25th-75th percentile band of ACGME
#' ratings by period (median dashed), the resident's own ACGME ratings
#' (filled dots) and self-ratings (open dots), with the current period
#' highlighted. Colors use roundsui tokens, so light/dark themes follow the app.
#'
#' @param subcomp Subcompetency id, e.g. "pc1".
#' @param spread Output of \code{milestone_spread()} (or the cached copy).
#' @param history Output of \code{milestone_history()} for one resident.
#' @param current_period REDCap period code being rated ("7", "1"...); only
#'   the resident's ratings from earlier periods are drawn.
#' @return An htmltools tag, or \code{NULL} when there is nothing to show.
#' @export
milestone_trajectory_svg <- function(subcomp, spread = NULL, history = NULL,
                                     current_period = NULL) {
  P <- .MS_PERIODS
  cur_chron <- if (!is.null(current_period)) P$chron[match(as.character(current_period), P$period)] else NA
  band <- if (is.data.frame(spread) && nrow(spread))
    spread[spread$source == "acgme" & spread$subcomp == subcomp & !is.na(spread$q25), , drop = FALSE]
  else NULL
  hist <- if (is.data.frame(history) && nrow(history))
    history[history$subcomp == subcomp, , drop = FALSE] else NULL
  if (!is.null(hist) && !is.na(cur_chron)) {
    hist <- hist[P$chron[match(hist$period, P$period)] < cur_chron, , drop = FALSE]
  }
  if ((is.null(band) || !nrow(band)) && (is.null(hist) || !nrow(hist))) return(NULL)

  W <- 640; H <- 168; ml <- 34; mr <- 14; mt <- 14; mb <- 30
  xs <- function(ch) ml + (ch + 0.5) * (W - ml - mr) / 7
  ys <- function(v) mt + (9 - v) * (H - mt - mb) / 8
  f  <- function(x) formatC(x, format = "f", digits = 1)
  num <- function(v) ifelse(v == round(v), format(round(v)), formatC(v, format = "f", digits = 1))

  el <- list()
  # Current period column
  if (!is.na(cur_chron)) {
    cw <- (W - ml - mr) / 7
    el[[length(el) + 1]] <- sprintf(
      '<rect x="%s" y="%s" width="%s" height="%s" rx="4" fill="var(--roundsui-accent-tint, #EAEFFC)"/>',
      f(xs(cur_chron) - cw / 2), mt - 8, f(cw), H - mt - mb + 8)
  }
  # Grid + y labels (odd ratings = milestone levels)
  for (v in c(1, 3, 5, 7, 9)) {
    el[[length(el) + 1]] <- sprintf(
      '<line x1="%d" x2="%d" y1="%s" y2="%s" stroke="var(--roundsui-border, #E2E5EC)" stroke-width="1"/><text x="%d" y="%s" text-anchor="end" dominant-baseline="middle" font-size="11" fill="var(--roundsui-ink-faint, #8991A0)">%d</text>',
      ml, W - mr, f(ys(v)), f(ys(v)), ml - 8, f(ys(v)), v)
  }
  # X labels
  for (i in seq_len(nrow(P))) {
    is_cur <- isTRUE(P$chron[i] == cur_chron)
    el[[length(el) + 1]] <- sprintf(
      '<text x="%s" y="%d" text-anchor="middle" font-size="11" font-weight="%s" fill="%s">%s</text>',
      f(xs(P$chron[i])), H - 10, if (is_cur) "700" else "400",
      if (is_cur) "var(--roundsui-accent, #3355D8)" else "var(--roundsui-ink-muted, #5B6472)",
      if (is_cur) paste0(P$short[i], " (now)") else P$short[i])
  }
  # Program band + median
  if (!is.null(band) && nrow(band)) {
    band$chron <- P$chron[match(band$period, P$period)]
    band <- band[order(band$chron), ]
    # Contiguous runs only, so a missing period leaves a gap
    run <- cumsum(c(1, diff(band$chron) != 1))
    for (r in unique(run)) {
      b <- band[run == r, ]
      if (nrow(b) == 1) {
        el[[length(el) + 1]] <- sprintf(
          '<rect x="%s" y="%s" width="16" height="%s" fill="var(--roundsui-border-strong, #CBD0DB)" opacity="0.55"/>',
          f(xs(b$chron) - 8), f(ys(b$q75)), f(max(ys(b$q25) - ys(b$q75), 2)))
      } else {
        pts <- c(paste(f(xs(b$chron)), f(ys(b$q75)), sep = ","),
                 rev(paste(f(xs(b$chron)), f(ys(b$q25)), sep = ",")))
        el[[length(el) + 1]] <- sprintf(
          '<polygon points="%s" fill="var(--roundsui-border-strong, #CBD0DB)" opacity="0.55"/>',
          paste(pts, collapse = " "))
        el[[length(el) + 1]] <- sprintf(
          '<polyline points="%s" fill="none" stroke="var(--roundsui-ink-faint, #8991A0)" stroke-width="1.5" stroke-dasharray="4 3"/>',
          paste(paste(f(xs(b$chron)), f(ys(b$median)), sep = ","), collapse = " "))
      }
    }
    # Hover targets per period (full column height)
    cw <- (W - ml - mr) / 7
    for (i in seq_len(nrow(band))) {
      b <- band[i, ]
      el[[length(el) + 1]] <- sprintf(
        '<rect x="%s" y="%d" width="%s" height="%d" fill="transparent"><title>%s: program middle half %s–%s, median %s (%d ACGME ratings)</title></rect>',
        f(xs(b$chron) - cw / 2), mt - 8, f(cw), H - mt - mb + 8,
        b$period_name, num(b$q25), num(b$q75), num(b$median), b$n)
    }
  }
  # Resident series
  series <- list(
    acgme = list(col = "var(--roundsui-accent, #3355D8)", open = FALSE, lab = "ACGME rating"),
    self  = list(col = "var(--gmed-ms-self, #13926a)",     open = TRUE,  lab = "Your self-rating"))
  if (!is.null(hist) && nrow(hist)) {
    hist$chron <- P$chron[match(hist$period, P$period)]
    for (src in names(series)) {
      h <- hist[hist$source == src, , drop = FALSE]
      if (!nrow(h)) next
      h <- h[order(h$chron), ]
      s <- series[[src]]
      # Small horizontal offset so equal self/ACGME values don't hide each other
      dx <- if (src == "self") -5 else 5
      if (nrow(h) > 1)
        el[[length(el) + 1]] <- sprintf(
          '<polyline points="%s" fill="none" stroke="%s" stroke-width="2"%s/>',
          paste(paste(f(xs(h$chron) + dx), f(ys(h$rating)), sep = ","), collapse = " "),
          s$col, if (s$open) ' stroke-dasharray="5 3"' else "")
      for (i in seq_len(nrow(h))) {
        el[[length(el) + 1]] <- sprintf(
          '<circle cx="%s" cy="%s" r="5" fill="%s" stroke="%s" stroke-width="2"><title>%s, %s: %s</title></circle>',
          f(xs(h$chron[i]) + dx), f(ys(h$rating[i])),
          if (s$open) "var(--roundsui-surface, #FFFFFF)" else s$col,
          if (s$open) s$col else "var(--roundsui-surface, #FFFFFF)",
          s$lab, P$period_name[match(h$period[i], P$period)], num(h$rating[i]))
      }
      # Direct label on the latest point only
      last <- h[nrow(h), ]
      el[[length(el) + 1]] <- sprintf(
        '<text x="%s" y="%s" text-anchor="middle" font-size="11" font-weight="600" fill="var(--roundsui-ink, #13161F)">%s</text>',
        f(xs(last$chron) + dx), f(ys(last$rating) + if (src == "self") 17 else -10), num(last$rating))
    }
  }

  series_txt <- character()
  if (!is.null(hist) && nrow(hist)) {
    for (src in names(series)) {
      h <- hist[hist$source == src, , drop = FALSE]
      if (!nrow(h)) next
      h <- h[order(h$chron), ]
      series_txt <- c(series_txt, paste0(series[[src]]$lab, "s: ",
        paste0(P$short[match(h$period, P$period)], " ", num(h$rating), collapse = ", ")))
    }
  }
  summary_txt <- paste0("Progression chart for ", toupper(subcomp), ". ",
                        if (length(series_txt)) paste(series_txt, collapse = ". ")
                        else "No earlier ratings yet.")

  svg <- sprintf(
    '<svg viewBox="0 0 %d %d" width="100%%" role="img" aria-label="%s" style="display:block; max-width:%dpx; height:auto; font-family:var(--roundsui-font-ui, inherit);">%s</svg>',
    W, H, htmltools::htmlEscape(summary_txt, attribute = TRUE), W, paste(unlist(el), collapse = ""))

  key_item <- function(swatch, text) htmltools::tags$span(class = "me-3 d-inline-flex align-items-center",
                                               htmltools::HTML(swatch), htmltools::tags$span(class = "ms-1", text))
  dot <- function(fill, stroke) sprintf(
    '<svg width="12" height="12" aria-hidden="true"><circle cx="6" cy="6" r="4.5" fill="%s" stroke="%s" stroke-width="2"/></svg>', fill, stroke)

  # Table view (same numbers as the chart)
  tbl <- {
    per <- P$period
    get <- function(src, p) {
      if (is.null(hist) || !nrow(hist)) return("")
      v <- hist$rating[hist$source == src & hist$period == p]
      if (length(v)) num(v[1]) else ""
    }
    bnd <- function(p) {
      if (is.null(band) || !nrow(band)) return("")
      b <- band[band$period == p, ]
      if (!nrow(b)) "" else paste0(num(b$median[1]), " (", num(b$q25[1]), "–", num(b$q75[1]), ")")
    }
    htmltools::tags$table(class = "table table-sm mb-0", style = "font-size:0.75rem;",
      htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Period"), htmltools::tags$th("ACGME rating"),
                         htmltools::tags$th("Self-rating"), htmltools::tags$th("Program median (middle half)"))),
      htmltools::tags$tbody(lapply(seq_along(per), function(i)
        htmltools::tags$tr(htmltools::tags$td(P$period_name[i]), htmltools::tags$td(get("acgme", per[i])),
                htmltools::tags$td(get("self", per[i])), htmltools::tags$td(bnd(per[i]))))))
  }

  htmltools::div(class = "gmed-ms-traj mb-2",
    htmltools::tags$style(htmltools::HTML(
      ".gmed-ms-traj{--gmed-ms-self:#13926a}
       @media (prefers-color-scheme: dark){:root:not([data-theme='light']):not([data-bs-theme='light']) .gmed-ms-traj{--gmed-ms-self:#199e70}}
       [data-theme='dark'] .gmed-ms-traj,[data-bs-theme='dark'] .gmed-ms-traj{--gmed-ms-self:#199e70}")),
    htmltools::div(style = "font-size:0.78rem; font-weight:600; color:var(--roundsui-ink-muted, #5B6472); margin-bottom:2px;",
        "Your progress on this milestone"),
    htmltools::HTML(svg),
    htmltools::div(style = "font-size:0.72rem; color:var(--roundsui-ink-muted, #5B6472); margin-top:2px;",
      key_item(dot("var(--roundsui-accent, #3355D8)", "var(--roundsui-accent, #3355D8)"), "Your ACGME (CCC) rating"),
      key_item(dot("var(--roundsui-surface, #FFFFFF)", "var(--gmed-ms-self, #13926a)"), "Your self-rating"),
      key_item('<svg width="16" height="10" aria-hidden="true"><rect width="16" height="10" rx="2" fill="var(--roundsui-border-strong, #CBD0DB)" opacity="0.55"/><line x1="0" x2="16" y1="5" y2="5" stroke="var(--roundsui-ink-faint, #8991A0)" stroke-width="1.5" stroke-dasharray="3 2"/></svg>',
               "Program residents' ACGME ratings: middle half, median dashed"),
      htmltools::tags$details(class = "d-inline", htmltools::tags$summary(style = "display:inline; cursor:pointer;", "Table"), tbl))
  )
}
