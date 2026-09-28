# milestone_growth_simulate.R
# Synthetic RDM-shaped milestone data for tests, the example app and the
# backtest demo. No real resident data is used anywhere in this file.

#' Simulate milestone data in RDM form shape
#'
#' Produces the same forms the apps load (\code{milestone_entry},
#' \code{milestone_selfevaluation_c33c}, \code{acgme_miles},
#' \code{ccc_review}, \code{ilp}) plus a resident table, for residents in
#' graduated and current classes. Ratings follow a quadratic growth curve
#' with resident-level intercept and slope variation, rounded to 1-9.
#'
#' @param n_per_class Residents per class.
#' @param grad_years Graduating classes. Classes with \code{grad_yr >
#'   current_year} are truncated to the periods they would have reached.
#' @param current_year Academic year used to truncate current classes
#'   (a class graduating in \code{current_year + k} has completed
#'   \code{6 - 2k} periods; \code{k = 0} is graduating now and has periods 1-5).
#' @param seed Random seed.
#' @return List with \code{all_forms} and \code{residents}.
#' @export
simulate_milestone_cohort <- function(n_per_class = 15,
                                      grad_years = 2020:2029,
                                      current_year = 2026,
                                      seed = 1) {
  set.seed(seed)
  sc <- milestone_subcompetencies()
  k <- nrow(sc)
  sub_shift <- stats::rnorm(k, 0, 0.3)          # some subcomps run harder/easier
  residents <- data.frame(
    record_id = as.character(seq_len(n_per_class * length(grad_years)) + 1000),
    grad_yr   = rep(as.character(grad_years), each = n_per_class),
    name      = paste("Resident", seq_len(n_per_class * length(grad_years))),
    stringsAsFactors = FALSE
  )
  entry <- list(); self <- list(); acgme <- list(); ccc <- list(); ilp <- list()
  for (i in seq_len(nrow(residents))) {
    rid <- residents$record_id[i]
    gy <- as.integer(residents$grad_yr[i])
    last_p <- if (gy < current_year) 6L else max(0L, 5L - 2L * (gy - current_year))
    if (last_p < 1) next
    a <- stats::rnorm(1, 0, 0.8)                 # resident intercept
    s <- stats::rnorm(1, 0, 0.18)                # resident slope
    for (p in seq_len(last_p)) {
      t <- p - 3.5
      mu <- 5.6 + 0.95 * t - 0.06 * t^2 + a + s * t + sub_shift
      coach <- .mg_clip(round(mu + stats::rnorm(k, 0, 0.7)))
      self_r <- .mg_clip(round(mu + 0.3 + stats::rnorm(k, 0, 0.9)))
      row <- data.frame(record_id = rid, redcap_repeat_instrument = "milestone_entry",
                        redcap_repeat_instance = p, prog_mile_period = as.character(p),
                        stringsAsFactors = FALSE)
      row[sc$field_coach] <- as.list(coach)
      entry[[length(entry) + 1]] <- row
      srow <- data.frame(record_id = rid,
                         redcap_repeat_instrument = "milestone_selfevaluation_c33c",
                         redcap_repeat_instance = p, prog_mile_period_self = as.character(p),
                         stringsAsFactors = FALSE)
      srow[sc$field_self] <- as.list(self_r)
      self[[length(self) + 1]] <- srow
      if (p %in% c(2, 4, 6) || stats::runif(1) < 0.3) {
        arow <- data.frame(record_id = rid, redcap_repeat_instrument = "acgme_miles",
                           redcap_repeat_instance = p, acgme_mile_period = as.character(p),
                           stringsAsFactors = FALSE)
        arow[sc$field_acgme] <- as.list(.mg_clip(round(mu + 0.2 + stats::rnorm(k, 0, 0.6))))
        acgme[[length(acgme) + 1]] <- arow
      }
      ccc[[length(ccc) + 1]] <- data.frame(
        record_id = rid, redcap_repeat_instrument = "ccc_review",
        redcap_repeat_instance = p, ccc_session = as.character(p),
        ccc_mile = if (stats::runif(1) < 0.25) "1" else "0", stringsAsFactors = FALSE)
      ilp[[length(ilp) + 1]] <- data.frame(
        record_id = rid, redcap_repeat_instrument = "ilp", redcap_repeat_instance = p,
        year_resident = as.character(p),
        goal_pcmk = as.character(sample(1:9, 1)),
        goal_level_pcmk = as.character(min(5, ceiling(p / 2) + 2)),
        goal_sbppbl = as.character(sample(1:5, 1)),
        goal_level_sbppbl = as.character(min(5, ceiling(p / 2) + 1)),
        goal_subcomp_profics = as.character(sample(1:7, 1)),
        goal_level_profics = as.character(min(5, ceiling(p / 2) + 2)),
        stringsAsFactors = FALSE)
    }
  }
  bind <- function(x) { d <- do.call(rbind, x); rownames(d) <- NULL; d }
  list(
    all_forms = list(
      milestone_entry = bind(entry),
      milestone_selfevaluation_c33c = bind(self),
      acgme_miles = bind(acgme),
      ccc_review = bind(ccc),
      ilp = bind(ilp)
    ),
    residents = residents
  )
}
