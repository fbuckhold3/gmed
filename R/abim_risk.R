#' ABIM Board Risk — McDonald et al. 2020 Nomogram (deprecated)
#'
#' Deprecated: use the iteviz package (`iteviz::pass_prob()`,
#' `iteviz::risk_tier()`, `iteviz::prob_label()`). iteviz mirrors the spec in
#' im-ite-analyzer (`ite_core/nomogram.py`) and is checked against it by parity
#' tests. These wrappers stay so deployed apps keep working until they migrate.
#'
#' P(pass ABIM) = 1 / (1 + exp(-(b0 + b1 * ITE_pct_correct)))
#'
#' @name abim_risk
NULL

.abim_deprecated <- function(fn, use) {
  rlang::inform(
    sprintf("gmed::%s() is deprecated; use %s.", fn, use),
    .frequency = "once", .frequency_id = paste0("gmed_abim_", fn)
  )
}

#' Nomogram coefficients (deprecated)
#'
#' Deprecated; use `iteviz::NOMOGRAM`. Built from it, so the values always match.
#' @export
NOMOGRAM_PARAMS <- lapply(iteviz::NOMOGRAM, function(b) list(b0 = b[["b0"]], b1 = b[["b1"]]))

#' Calculate ABIM pass probability from ITE % correct (deprecated)
#'
#' Deprecated; delegates to `iteviz::pass_prob()`.
#'
#' @param pct Numeric. ITE percent correct (0–100).
#' @param pgy Integer or character. PGY year (1, 2, or 3). Clamped to 1–3.
#' @return Numeric probability (0–1), or NA if inputs are missing.
#' @export
pass_prob <- function(pct, pgy) {
  .abim_deprecated("pass_prob", "iteviz::pass_prob()")
  iteviz::pass_prob(pct, pgy)
}

#' Classify ABIM pass probability into risk tier (deprecated)
#'
#' Deprecated; use `iteviz::risk_tier()` (cutoffs 50% / 90%). This function keeps
#' its old 50% / 75% cutoffs so an app's tiers only change when it migrates.
#'
#' @param prob Numeric. Pass probability (0–1) from \code{pass_prob()}.
#' @return Named list with \code{level} (character), \code{color} (hex), and
#'   \code{label} (character description).
#' @export
risk_from_prob <- function(prob) {
  .abim_deprecated("risk_from_prob", "iteviz::risk_tier()")
  if (is.na(prob)) return(list(level = "No Data",      color = "#adb5bd",
                                label = "No data available"))
  if (prob < 0.50) return(list(level = "High Risk",     color = "#d32f2f",
                                label = "High risk — intervention recommended"))
  if (prob < 0.75) return(list(level = "Moderate Risk", color = "#e65100",
                                label = "Moderate risk — targeted prep recommended"))
  list(                         level = "Low Risk",      color = "#2e7d32",
                                label = "Low risk — continue current preparation")
}
