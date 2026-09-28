# milestone_growth_models.R
# Statistics for the shared milestone growth module:
#   1. cohort bands   - smoothed 10th/50th/90th percentiles by period
#                       (quantile regression on a low-df natural spline)
#   2. growth model   - lme4 mixed model per subcompetency; predictions for a
#                       resident are computed from the cached coefficients
#                       (BLUP + prediction variance), so apps never refit
#   3. empirical check- "Of past residents rated X at period P, N% reached 7"
#
# fit_milestone_growth() does all three once; the data-refresh job caches the
# result with write_milestone_growth_cache() and apps read it with
# load_cached_milestone_growth(). lme4 / quantreg are only needed for fitting.

# ── 1. Cohort bands ──────────────────────────────────────────────────────────

#' Fit smoothed cohort percentile bands
#'
#' For each subcompetency, fits quantile regressions of rating on period at
#' the 10th, 50th and 90th percentiles using a natural spline with
#' \code{df} degrees of freedom, so sparse periods borrow strength from their
#' neighbours. Quantile crossings are removed by sorting the fitted
#' quantiles within each period (monotone rearrangement) and values are
#' clipped to 1-9. Falls back to raw per-period quantiles when a
#' subcompetency has too little data (or when quantreg is not installed).
#'
#' This describes where residents actually fall, unlike the 95\% CI of the
#' mean used by the older charts.
#'
#' @param ratings Selected ratings (\code{select_milestone_rating()} output):
#'   \code{record_id}, \code{period}, \code{subcomp}, \code{rating}.
#' @param taus Quantiles (default 0.1, 0.5, 0.9).
#' @param df Spline degrees of freedom (capped at distinct periods - 1).
#' @param min_n Minimum observations for the smoothed fit.
#' @return Data frame: \code{subcomp}, \code{period}, \code{p10}, \code{p50},
#'   \code{p90}, \code{n}, \code{method} ("rq" or "empirical").
#' @export
fit_cohort_bands <- function(ratings, taus = c(0.1, 0.5, 0.9), df = 3, min_n = 30) {
  if (is.null(ratings) || !nrow(ratings)) return(.mg_empty_bands())
  has_rq <- requireNamespace("quantreg", quietly = TRUE)
  out <- lapply(split(ratings, ratings$subcomp), function(d) {
    d <- d[!is.na(d$rating) & !is.na(d$period), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    periods <- sort(unique(d$period))
    grid <- data.frame(period = seq(min(periods), max(periods)))
    n_by <- table(factor(d$period, levels = grid$period))
    q <- NULL
    k <- min(df, length(periods) - 1)
    if (has_rq && nrow(d) >= min_n && k >= 1) {
      q <- tryCatch({
        fit <- suppressWarnings(quantreg::rq(
          rating ~ splines::ns(period, df = k), tau = taus, data = d))
        m <- suppressWarnings(stats::predict(fit, newdata = grid))
        matrix(m, nrow = nrow(grid))
      }, error = function(e) NULL)
    }
    method <- if (is.null(q)) "empirical" else "rq"
    if (is.null(q)) {
      q <- t(vapply(grid$period, function(p) {
        v <- d$rating[d$period == p]
        if (!length(v)) return(rep(NA_real_, length(taus)))
        stats::quantile(v, taus, names = FALSE, type = 7)
      }, numeric(length(taus))))
    }
    q <- t(apply(q, 1, function(r) if (anyNA(r)) r else sort(r)))
    q <- pmin(pmax(q, 1), 9)
    data.frame(subcomp = d$subcomp[1], period = grid$period,
               p10 = q[, 1], p50 = q[, 2], p90 = q[, 3],
               n = as.integer(n_by), method = method,
               stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, out)
  if (is.null(res)) return(.mg_empty_bands())
  rownames(res) <- NULL
  res
}

.mg_empty_bands <- function() {
  data.frame(subcomp = character(0), period = integer(0), p10 = numeric(0),
             p50 = numeric(0), p90 = numeric(0), n = integer(0),
             method = character(0), stringsAsFactors = FALSE)
}

# ── 2. Growth model ──────────────────────────────────────────────────────────

.MG_CENTER <- 3.5

#' Fit the mixed-effects growth model for one subcompetency
#'
#' \code{rating ~ t + t^2 + source + (1 + t | record_id)} with
#' \code{t = period - 3.5}, fitted by REML with lme4. Falls back to a random
#' intercept only when the slope model fails or is singular. Returns only the
#' pieces needed to predict (fixed effects, their covariance, the random
#' effect covariance and residual SD), not the lmer object, so it can be
#' cached as JSON.
#'
#' @param d Selected ratings for one subcompetency (\code{record_id},
#'   \code{period}, \code{rating}, \code{source}).
#' @return A list (class \code{milestone_growth_model}) or \code{NULL}.
#' @export
fit_growth_model <- function(d) {
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("fit_growth_model: package 'lme4' is required for model fitting")
  d <- d[!is.na(d$rating) & !is.na(d$period), , drop = FALSE]
  if (nrow(d) < 10 || length(unique(d$record_id)) < 5) return(NULL)
  d$t <- d$period - .MG_CENTER
  src_levels <- intersect(c("coach", "ccc", "acgme"), unique(d$source))
  d$source <- factor(d$source, levels = src_levels)
  fixed <- "rating ~ t + I(t^2)"
  if (length(src_levels) > 1) fixed <- paste(fixed, "+ source")
  if (length(unique(d$period)) < 3) fixed <- sub(" \\+ I\\(t\\^2\\)", "", fixed)

  ctrl <- lme4::lmerControl(calc.derivs = FALSE)
  try_fit <- function(re) {
    tryCatch(
      suppressMessages(suppressWarnings(lme4::lmer(
        stats::as.formula(paste(fixed, "+", re)), data = d, REML = TRUE,
        control = ctrl))),
      error = function(e) NULL)
  }
  fit <- try_fit("(1 + t | record_id)")
  re_terms <- c("(Intercept)", "t")
  if (is.null(fit) || lme4::isSingular(fit)) {
    fit2 <- try_fit("(1 | record_id)")
    if (!is.null(fit2)) { fit <- fit2; re_terms <- "(Intercept)" }
  }
  if (is.null(fit)) return(NULL)

  beta <- lme4::fixef(fit)
  G <- as.matrix(lme4::VarCorr(fit)$record_id)
  structure(list(
    formula       = paste(deparse(stats::formula(fit)), collapse = ""),
    beta_names    = names(beta),
    beta          = unname(beta),
    vcov          = unname(as.matrix(stats::vcov(fit))),
    G             = unname(G[re_terms, re_terms, drop = FALSE]),
    re_terms      = re_terms,
    sigma         = stats::sigma(fit),
    center        = .MG_CENTER,
    source_levels = src_levels,
    n_obs         = nrow(d),
    n_residents   = length(unique(d$record_id))
  ), class = "milestone_growth_model")
}

# Fixed-effect design row(s) for given periods / sources.
.mg_X <- function(model, period, source) {
  t <- period - model$center
  cols <- lapply(model$beta_names, function(b) {
    if (b == "(Intercept)") return(rep(1, length(t)))
    if (b == "t") return(t)
    if (b == "I(t^2)") return(t^2)
    if (startsWith(b, "source")) return(as.numeric(source == sub("^source", "", b)))
    rep(0, length(t))
  })
  matrix(unlist(cols), nrow = length(t))
}

.mg_Z <- function(model, period) {
  t <- period - model$center
  if (length(model$re_terms) == 2) cbind(1, t) else matrix(1, nrow = length(t))
}

#' Predict a resident's ratings from a cached growth model
#'
#' Uses the resident's observed (selected) ratings to compute the BLUP of
#' their random effects from the cached fixed effects and variance
#' components, then predicts at \code{periods}. The prediction variance
#' includes random-effect uncertainty, fixed-effect uncertainty and residual
#' variance (Henderson; variance parameters treated as known). Predictions
#' and interval bounds are clipped to 1-9.
#'
#' P(rating >= threshold) uses a continuity correction (ratings are
#' integers, so a latent value >= threshold - 0.5 rounds to >= threshold).
#'
#' @param model A \code{milestone_growth_model} (from the cached fit).
#' @param obs Resident's observations for this subcompetency:
#'   \code{period}, \code{rating}, \code{source}. May have zero rows.
#' @param periods Periods to predict (default 6 = graduation).
#' @param target_source Source to predict on (default "acgme", the rating that
#'   counts at graduation; the reference source is used if it's not in the
#'   model).
#' @param level Prediction interval level (default 0.8).
#' @param threshold Graduation target (default 7).
#' @return Data frame: \code{period}, \code{fit}, \code{se}, \code{lwr},
#'   \code{upr}, \code{p_reach}.
#' @export
predict_milestone_growth <- function(model, obs, periods = 6,
                                     target_source = "acgme",
                                     level = 0.8, threshold = .MG_TARGET) {
  beta <- model$beta
  Vb <- model$vcov
  G <- model$G
  s2 <- model$sigma^2
  if (!target_source %in% model$source_levels) target_source <- "__ref__"
  x0 <- .mg_X(model, periods, rep(target_source, length(periods)))
  z0 <- .mg_Z(model, periods)

  obs <- obs[!is.na(obs$rating) & !is.na(obs$period), , drop = FALSE]
  if (nrow(obs)) {
    X <- .mg_X(model, obs$period, as.character(obs$source))
    Z <- .mg_Z(model, obs$period)
    V <- Z %*% G %*% t(Z) + diag(s2, nrow(obs))
    Vi <- solve(V)
    C <- G %*% t(Z) %*% Vi                        # BLUP operator
    b <- C %*% (obs$rating - X %*% beta)
    fit <- as.vector(x0 %*% beta + z0 %*% b)
    Gpost <- G - C %*% Z %*% G                    # Var(b - b_hat)
    A <- x0 - z0 %*% C %*% X
  } else {
    fit <- as.vector(x0 %*% beta)
    Gpost <- G
    A <- x0
  }
  var <- rowSums((z0 %*% Gpost) * z0) + rowSums((A %*% Vb) * A) + s2
  se <- sqrt(pmax(var, 0))
  zq <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(
    period  = periods,
    fit     = .mg_clip(fit),
    se      = se,
    lwr     = .mg_clip(fit - zq * se),
    upr     = .mg_clip(fit + zq * se),
    p_reach = 1 - stats::pnorm((threshold - 0.5 - fit) / se)
  )
}

.mg_clip <- function(x, lo = 1, hi = 9) pmin(pmax(x, lo), hi)

# ── 3. Empirical check ───────────────────────────────────────────────────────

#' Empirical "reached 7 by graduation" table
#'
#' Among past residents with a graduation (period 6) rating, for each
#' subcompetency, period P (1-5) and rating X at P, the share whose
#' graduation rating was >= \code{target}. Cells with fewer than
#' \code{min_n} residents are suppressed (\code{pct = NA},
#' \code{suppressed = TRUE}); \code{n} is always returned.
#'
#' @param ratings Selected ratings.
#' @param target Graduation target (default 7).
#' @param min_n Suppression threshold (default 10).
#' @return Data frame: \code{subcomp}, \code{period}, \code{rating}, \code{n},
#'   \code{n_reached}, \code{pct}, \code{suppressed}.
#' @export
empirical_reach_table <- function(ratings, target = .MG_TARGET, min_n = 10) {
  empty <- data.frame(subcomp = character(0), period = integer(0),
                      rating = numeric(0), n = integer(0),
                      n_reached = integer(0), pct = numeric(0),
                      suppressed = logical(0), stringsAsFactors = FALSE)
  if (is.null(ratings) || !nrow(ratings)) return(empty)
  grad <- ratings[ratings$period == 6, c("record_id", "subcomp", "rating")]
  if (!nrow(grad)) return(empty)
  names(grad)[3] <- "grad_rating"
  early <- ratings[ratings$period < 6, c("record_id", "subcomp", "period", "rating")]
  m <- merge(early, grad, by = c("record_id", "subcomp"))
  if (!nrow(m)) return(empty)
  m$rating <- round(m$rating)
  m$reached <- m$grad_rating >= target
  agg <- stats::aggregate(reached ~ subcomp + period + rating, data = m,
                          FUN = function(v) c(n = length(v), k = sum(v)))
  out <- data.frame(subcomp = agg$subcomp, period = agg$period,
                    rating = agg$rating, n = as.integer(agg$reached[, "n"]),
                    n_reached = as.integer(agg$reached[, "k"]),
                    stringsAsFactors = FALSE)
  out <- .mg_suppress(out, min_n)
  out[order(out$subcomp, out$period, out$rating), , drop = FALSE]
}

.mg_suppress <- function(tab, min_n = 10) {
  tab$suppressed <- tab$n < min_n
  tab$pct <- ifelse(tab$suppressed, NA_real_, tab$n_reached / tab$n)
  # Hide the numerator of suppressed cells too, so it can't be back-derived
  tab$n_reached[tab$suppressed] <- NA_integer_
  tab
}

#' Look up the empirical check for one rating
#'
#' @param table \code{empirical_reach_table()} output (e.g. \code{fit$empirical}).
#' @param subcomp,period,rating The resident's subcompetency, period and rating.
#' @param min_n Suppression threshold (default 10).
#' @return List: \code{n}, \code{pct} (\code{NA} if suppressed),
#'   \code{suppressed}, \code{text} (display sentence or \code{NULL}).
#' @export
empirical_reach_lookup <- function(table, subcomp, period, rating, min_n = 10) {
  r <- round(rating)
  row <- if (is.null(table) || !nrow(table)) table else
    table[table$subcomp == subcomp & table$period == period & table$rating == r, , drop = FALSE]
  n <- if (is.null(row) || !nrow(row)) 0L else as.integer(row$n[1])
  if (n < min_n) {
    return(list(n = n, pct = NA_real_, suppressed = TRUE, text = NULL))
  }
  pct <- row$pct[1]
  if (is.na(pct)) pct <- row$n_reached[1] / n
  list(n = n, pct = pct, suppressed = FALSE,
       text = sprintf("Of past residents rated %d at %s, %d%% reached 7 by graduation (n = %d).",
                      r, .mg_period_name(period), round(100 * pct), n))
}

# ── Fit everything ───────────────────────────────────────────────────────────

#' Fit all milestone growth statistics once
#'
#' Called by the data-refresh job (never per session). Fits the cohort bands,
#' one growth model per subcompetency and the empirical check table.
#'
#' @param data Long milestone data (\code{build_milestone_long()}), or a list
#'   of RDM forms (passed to \code{build_milestone_long()}).
#' @param residents Optional resident table (only used when \code{data} is a
#'   list of forms).
#' @param exclude_record_ids Residents to leave out of the fit (e.g. test
#'   records).
#' @param min_n Empirical suppression threshold.
#' @return Object of class \code{milestone_growth_fit}: list with
#'   \code{bands}, \code{models}, \code{empirical}, \code{meta}.
#' @export
fit_milestone_growth <- function(data, residents = NULL, exclude_record_ids = NULL,
                                 min_n = 10) {
  long <- as_milestone_long(data, residents = residents)
  if (length(exclude_record_ids))
    long <- long[!long$record_id %in% as.character(exclude_record_ids), , drop = FALSE]
  sel <- select_milestone_rating(long)
  models <- list()
  if (nrow(sel)) {
    for (sc in milestone_subcompetencies()$subcomp) {
      d <- sel[sel$subcomp == sc, , drop = FALSE]
      if (nrow(d)) models[[sc]] <- fit_growth_model(d)
    }
  }
  structure(list(
    version   = 1L,
    fitted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    target    = .MG_TARGET,
    bands     = fit_cohort_bands(sel),
    models    = Filter(Negate(is.null), models),
    empirical = empirical_reach_table(sel, min_n = min_n),
    meta      = list(n_residents = length(unique(sel$record_id)),
                     n_obs = nrow(sel),
                     source_counts = as.list(table(sel$source)))
  ), class = "milestone_growth_fit")
}

#' @export
print.milestone_growth_fit <- function(x, ...) {
  cat("<milestone_growth_fit> fitted", x$fitted_at, "\n")
  cat(" residents:", x$meta$n_residents, " ratings:", x$meta$n_obs, "\n")
  cat(" models:", length(x$models), "of 21 subcompetencies\n")
  invisible(x)
}

#' Raw (unsmoothed) cohort quantiles
#'
#' Cheap fallback for the module when no cached fit is available: per-period
#' 10th/50th/90th percentiles without model fitting.
#'
#' @param data Long milestone data.
#' @return Bands data frame, as \code{fit_cohort_bands()}.
#' @export
empirical_cohort_bands <- function(data) {
  sel <- select_milestone_rating(as_milestone_long(data))
  fit_cohort_bands(sel, min_n = Inf)
}

# ── Serialisation + REDCap cache (same pattern as redcap_cache.R) ────────────

#' Serialise / deserialise a milestone growth fit
#'
#' JSON, gzip-compressed and base64-encoded (see \code{write_amion_cache()}
#' for why: REDCap Notes Box fields silently truncate at 65,535 bytes).
#'
#' @param fit A \code{milestone_growth_fit}.
#' @param encoded Character string from \code{serialize_milestone_growth()}.
#' @return Encoded string / \code{milestone_growth_fit}.
#' @export
serialize_milestone_growth <- function(fit) {
  slim <- fit
  slim$models <- lapply(fit$models, unclass)
  json <- jsonlite::toJSON(unclass(slim), auto_unbox = TRUE, na = "null",
                           digits = I(6), matrix = "rowmajor")
  jsonlite::base64_enc(memCompress(charToRaw(as.character(json)), type = "gzip"))
}

#' @rdname serialize_milestone_growth
#' @export
deserialize_milestone_growth <- function(encoded) {
  json <- rawToChar(memDecompress(jsonlite::base64_dec(encoded), type = "gzip"))
  x <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  as_mat <- function(m, k) matrix(as.numeric(unlist(m)), nrow = k, byrow = TRUE)
  x$models <- lapply(x$models, function(m) {
    k <- length(m$beta)
    m$vcov <- as_mat(m$vcov, k)
    m$G <- as_mat(m$G, length(m$re_terms))
    structure(m, class = "milestone_growth_model")
  })
  for (nm in c("bands", "empirical"))
    if (!is.data.frame(x[[nm]])) x[[nm]] <- as.data.frame(x[[nm]])
  structure(x, class = "milestone_growth_fit")
}

#' Write the milestone growth fit to the REDCap app_cache record
#'
#' Needs two fields on the \code{app_cache} instrument:
#' \code{cache_milestone_growth_json} (Notes Box) and
#' \code{cache_milestone_growth_updated_at} (Text).
#'
#' @param fit A \code{milestone_growth_fit}.
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Cache record id (default: \code{CACHE_RECORD_ID}).
#' @return Invisible \code{TRUE} on success, \code{FALSE} on failure.
#' @export
write_milestone_growth_cache <- function(
    fit,
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID")
) {
  if (!nzchar(cache_record_id))
    stop("write_milestone_growth_cache: CACHE_RECORD_ID env var not set.")
  if (!inherits(fit, "milestone_growth_fit")) {
    warning("write_milestone_growth_cache: not a milestone_growth_fit - nothing written")
    return(invisible(FALSE))
  }
  encoded <- serialize_milestone_growth(fit)
  bytes <- nchar(encoded, type = "bytes")
  message(sprintf("write_milestone_growth_cache: gzip+base64 %.1f KB", bytes / 1024))
  if (bytes > 65000) {
    warning("write_milestone_growth_cache: payload over REDCap's ~64 KB Notes Box ",
            "ceiling. Refusing to write (would be silently truncated).")
    return(invisible(FALSE))
  }
  cache_row <- data.frame(
    record_id                         = cache_record_id,
    cache_milestone_growth_json       = encoded,
    cache_milestone_growth_updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  result <- REDCapR::redcap_write(ds_to_write = cache_row, redcap_uri = redcap_url,
                                  token = rdm_token, verbose = FALSE)
  if (isTRUE(result$success)) {
    message("write_milestone_growth_cache: written to record ", cache_record_id)
    invisible(TRUE)
  } else {
    warning("write_milestone_growth_cache: REDCap write failed - ", result$outcome_message)
    invisible(FALSE)
  }
}

#' Load the cached milestone growth fit
#'
#' @inheritParams load_cached_medians
#' @param max_age_hours Message if the cache is older than this (default 192).
#' @return A \code{milestone_growth_fit}, or \code{NULL} if unavailable.
#' @export
load_cached_milestone_growth <- function(
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = default_cache_record_id(),
    max_age_hours   = 192
) {
  if (!nzchar(cache_record_id)) return(NULL)
  result <- tryCatch(
    REDCapR::redcap_read_oneshot(redcap_uri = redcap_url, token = rdm_token,
                                 records = cache_record_id, forms = "app_cache",
                                 verbose = FALSE),
    error = function(e) {
      message("load_cached_milestone_growth: REDCap read error - ", e$message)
      NULL
    })
  if (is.null(result) || !isTRUE(result$success) || nrow(result$data) == 0) return(NULL)
  row <- result$data
  fld <- "cache_milestone_growth_json"
  if (!fld %in% names(row) || is.na(row[[fld]][1]) || !nzchar(row[[fld]][1])) {
    message("load_cached_milestone_growth: cache field empty or missing")
    return(NULL)
  }
  upd <- row$cache_milestone_growth_updated_at[1]
  if (!is.null(upd) && !is.na(upd) && nzchar(upd)) {
    age <- as.numeric(difftime(Sys.time(), as.POSIXct(upd), units = "hours"))
    if (is.finite(max_age_hours) && age > max_age_hours)
      message(sprintf("load_cached_milestone_growth: cache is %.1f h old", age))
  }
  tryCatch(deserialize_milestone_growth(row[[fld]][1]),
           error = function(e) {
             message("load_cached_milestone_growth: decode failed - ", e$message)
             NULL
           })
}

#' Refit and cache the milestone growth statistics (for rdm-data-refresh)
#'
#' @param all_forms Forms list as held by the refresh job (must include
#'   archived/graduated residents; the history is the point).
#' @param residents Resident table (\code{record_id}, \code{grad_yr}).
#' @param ... Passed to \code{write_milestone_growth_cache()}.
#' @return Invisible fit.
#' @export
refresh_milestone_growth_cache <- function(all_forms, residents = NULL, ...) {
  fit <- fit_milestone_growth(all_forms, residents = residents)
  write_milestone_growth_cache(fit, ...)
  invisible(fit)
}
