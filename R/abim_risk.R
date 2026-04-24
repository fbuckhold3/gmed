#' ABIM Board Risk — McDonald et al. 2020 Nomogram
#'
#' Logistic regression coefficients per PGY year, derived from published Figure 2
#' anchor points in McDonald et al. (2020), Academic Medicine.
#'
#' P(pass ABIM) = 1 / (1 + exp(-(b0 + b1 * ITE_pct_correct)))
#'
#' @name abim_risk
NULL

#' @export
NOMOGRAM_PARAMS <- list(
  `1` = list(b0 = -7.0641,  b1 = 0.1811),
  `2` = list(b0 = -8.1445,  b1 = 0.1733),
  `3` = list(b0 = -11.9491, b1 = 0.2173)
)

#' Calculate ABIM pass probability from ITE % correct
#'
#' @param pct Numeric. ITE percent correct (0–100).
#' @param pgy Integer or character. PGY year (1, 2, or 3). Clamped to 1–3.
#' @return Numeric probability (0–1), or NA if inputs are missing.
#' @export
pass_prob <- function(pct, pgy) {
  if (is.na(pct) || is.na(pgy)) return(NA_real_)
  pgy <- as.character(max(1L, min(3L, as.integer(pgy))))
  p   <- NOMOGRAM_PARAMS[[pgy]]
  1.0 / (1.0 + exp(-(p$b0 + p$b1 * pct)))
}

#' Classify ABIM pass probability into risk tier
#'
#' @param prob Numeric. Pass probability (0–1) from \code{pass_prob()}.
#' @return Named list with \code{level} (character), \code{color} (hex), and
#'   \code{label} (character description).
#' @export
risk_from_prob <- function(prob) {
  if (is.na(prob)) return(list(level = "No Data",      color = "#adb5bd",
                                label = "No data available"))
  if (prob < 0.50) return(list(level = "High Risk",     color = "#d32f2f",
                                label = "High risk \u2014 intervention recommended"))
  if (prob < 0.75) return(list(level = "Moderate Risk", color = "#e65100",
                                label = "Moderate risk \u2014 targeted prep recommended"))
  list(                         level = "Low Risk",      color = "#2e7d32",
                                label = "Low risk \u2014 continue current preparation")
}
