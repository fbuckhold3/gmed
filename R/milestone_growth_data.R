# milestone_growth_data.R
# Data layer for the shared milestone growth module (mod_milestone_growth_*).
#
# Everything here is pure (no Shiny, no REDCap calls) so it can be tested on
# synthetic data. The model-fitting step lives in milestone_growth_models.R,
# the charts in milestone_growth_plots.R and the module in
# mod_milestone_growth.R.
#
# Long format used throughout ("milestone long data"):
#   record_id  chr   resident
#   period     int   1 Mid Intern ... 6 Graduating
#   subcomp    chr   "pc1" ... "ics3" (21 IM Milestones 2.0 subcompetencies)
#   rater      chr   "acgme" | "ccc" | "coach" | "self"
#   rating     num   1-9 (our internal scale)
#   grad_yr    chr   graduating class, when resident data was supplied

# ── Constants ─────────────────────────────────────────────────────────────────

#' IM Milestones 2.0 subcompetency table
#'
#' One row per subcompetency (21), in display order, with the competency
#' group and the REDCap field stems for each rating source.
#'
#' @return Data frame with columns \code{subcomp} ("pc1"), \code{code}
#'   ("PC1"), \code{competency} ("PC"), \code{competency_label},
#'   \code{label}, \code{field_coach} ("rep_pc1"), \code{field_self}
#'   ("rep_pc1_self"), \code{field_acgme} ("acgme_pc1").
#' @export
milestone_subcompetencies <- function() {
  subcomp <- c(paste0("pc", 1:6), paste0("mk", 1:3), paste0("sbp", 1:3),
               paste0("pbl", 1:2), paste0("prof", 1:4), paste0("ics", 1:3))
  competency <- toupper(sub("\\d+$", "", subcomp))
  comp_labels <- c(PC = "Patient Care", MK = "Medical Knowledge",
                   SBP = "Systems-Based Practice",
                   PBL = "Practice-Based Learning and Improvement",
                   PROF = "Professionalism",
                   ICS = "Interpersonal and Communication Skills")
  data.frame(
    subcomp          = subcomp,
    code             = toupper(subcomp),
    competency       = competency,
    competency_label = unname(comp_labels[competency]),
    label            = vapply(subcomp, .get_milestone_label_full, character(1),
                              USE.NAMES = FALSE),
    field_coach      = paste0("rep_", subcomp),
    field_self       = paste0("rep_", subcomp, "_self"),
    field_acgme      = paste0("acgme_", subcomp),
    stringsAsFactors = FALSE
  )
}

#' Milestone period table (1 Mid Intern ... 6 Graduating)
#'
#' @return Data frame with \code{period} (1-6), \code{period_name} and
#'   \code{pgy} (1-3).
#' @export
milestone_periods <- function() {
  data.frame(
    period      = 1:6,
    period_name = c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2",
                    "Mid PGY3", "Graduating"),
    pgy         = c(1L, 1L, 2L, 2L, 3L, 3L),
    stringsAsFactors = FALSE
  )
}

# Graduation target on the internal 1-9 scale. Same for every rating source,
# ACGME included (7 here = 4.0 on ACGME's native 1-5 scale).
.MG_TARGET <- 7

# Rating sources in fallback order: the first present one is "the" rating.
.MG_SOURCE_ORDER <- c("acgme", "ccc", "coach")

#' Program expectation by period
#'
#' Step function of what the program expects at each period, drawn separately
#' from the cohort band:
#' \itemize{
#'   \item 3 for intern year: period 1 (Mid Intern)
#'   \item 5 for late intern / early PGY2: periods 2 (End Intern) and
#'     3 (Mid PGY2)
#'   \item 7 for late PGY2 / PGY3: periods 4 (End PGY2), 5 (Mid PGY3) and
#'     6 (Graduating)
#' }
#'
#' @return Data frame with \code{period}, \code{period_name}, \code{expected}.
#' @export
milestone_program_expectation <- function() {
  p <- milestone_periods()
  p$expected <- c(3, 5, 5, 7, 7, 7)
  p[, c("period", "period_name", "expected")]
}

#' Convert an ILP goal level to a milestone rating
#'
#' ILP goals are chosen on the 5 milestone levels; the ratings are on the
#' internal 1-9 scale, where each level is an odd rating:
#' 1 -> 1, 2 -> 3, 3 -> 5, 4 -> 7, 5 -> 9.
#'
#' @param level Numeric or character goal level (1-5).
#' @return Numeric rating; \code{NA} for anything outside 1-5.
#' @export
ilp_goal_level_to_rating <- function(level) {
  lv <- suppressWarnings(as.numeric(as.character(level)))
  # Label exports ("Level 3", "3 - Competent"): use the first number
  lab <- is.na(lv) & grepl("^\\D*\\d", as.character(level))
  lv[lab] <- as.numeric(sub("^\\D*(\\d+(\\.\\d+)?).*$", "\\1", as.character(level)[lab]))
  out <- 2 * lv - 1
  out[is.na(lv) | lv < 1 | lv > 5 | lv != round(lv)] <- NA_real_
  out
}

# ILP domain choice code -> subcompetency. Matches the REDCap choice lists
# used by mod_ilp_display.R (.ilp_subcomp_label) and ind.dash mod_self_eval.
.MG_ILP_DOMAINS <- list(
  pcmk    = list(goal = "goal_pcmk", level = "goal_level_pcmk",
                 codes = c(paste0("pc", 1:6), paste0("mk", 1:3))),
  sbppbl  = list(goal = "goal_sbppbl", level = "goal_level_sbppbl",
                 codes = c(paste0("sbp", 1:3), paste0("pbl", 1:2))),
  profics = list(goal = "goal_subcomp_profics", level = "goal_level_profics",
                 codes = c(paste0("prof", 1:4), paste0("ics", 1:3)))
)

#' Map an ILP domain choice code to a subcompetency
#'
#' @param domain One of "pcmk", "sbppbl", "profics".
#' @param code The REDCap choice code stored in the goal field (raw export),
#'   or its label (e.g. "PC1 - History"; label export).
#' @return Subcompetency id (e.g. "mk2") or \code{NA}.
#' @export
ilp_goal_subcomp <- function(domain, code) {
  codes <- .MG_ILP_DOMAINS[[domain]]$codes
  if (is.null(codes)) return(rep(NA_character_, length(code)))
  i <- suppressWarnings(as.integer(as.character(code)))
  out <- rep(NA_character_, length(code))
  ok <- !is.na(i) & i >= 1 & i <= length(codes)
  out[ok] <- codes[i[ok]]
  # Label exports ("PC1 - History", "PBLI2: ..."): read the leading code
  lab <- toupper(trimws(as.character(code)))
  m <- regmatches(lab, regexec("^(PC|MK|SBP|PBLI|PBL|PROF|ICS)\\s*(\\d+)", lab))
  from_lab <- vapply(m, function(x) if (length(x) == 3)
    paste0(tolower(sub("PBLI", "PBL", x[2])), x[3]) else NA_character_, character(1))
  use <- is.na(out) & from_lab %in% codes
  out[use] <- from_lab[use]
  out
}

# ── Helpers ───────────────────────────────────────────────────────────────────

# Period code or label -> integer 1-6 (NA otherwise, incl. 7 = Entering).
.mg_period_num <- function(x) {
  x <- as.character(x)
  p <- milestone_periods()
  out <- suppressWarnings(as.integer(x))
  lab <- match(sub("^Period ", "", x), c(p$period_name, "Graduation"))
  lab[!is.na(lab) & lab == 7L] <- 6L
  out[is.na(out)] <- lab[is.na(out)]
  out[!is.na(out) & (out < 1L | out > 6L)] <- NA_integer_
  out
}

.mg_period_name <- function(period) {
  milestone_periods()$period_name[match(period, 1:6)]
}

.mg_empty_long <- function() {
  data.frame(record_id = character(0), period = integer(0),
             subcomp = character(0), rater = character(0),
             rating = numeric(0), stringsAsFactors = FALSE)
}

# Keep only rows of a (possibly multi-instrument) REDCap export that belong
# to `instrument`, when the export carries redcap_repeat_instrument.
.mg_instrument_rows <- function(df, instrument) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if ("redcap_repeat_instrument" %in% names(df)) {
    rri <- as.character(df$redcap_repeat_instrument)
    keep <- !is.na(rri) & rri == instrument
    if (any(keep)) df <- df[keep, , drop = FALSE]
  }
  df
}

# Wide form -> long for one rater.
.mg_wide_to_long <- function(df, period_field, stems, rater) {
  if (is.null(df) || nrow(df) == 0 || !period_field %in% names(df))
    return(.mg_empty_long())
  sc <- milestone_subcompetencies()
  fields <- sc[[stems]]
  present <- fields %in% names(df)
  if (!any(present)) return(.mg_empty_long())
  period <- .mg_period_num(df[[period_field]])
  rid <- as.character(df$record_id)
  pieces <- lapply(which(present), function(j) {
    v <- suppressWarnings(as.numeric(as.character(df[[fields[j]]])))
    keep <- !is.na(v) & !is.na(period)
    if (!any(keep)) return(NULL)
    data.frame(record_id = rid[keep], period = period[keep],
               subcomp = sc$subcomp[j], rater = rater, rating = v[keep],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), pieces))
  if (is.null(out)) .mg_empty_long() else out
}

# ── Build long data ───────────────────────────────────────────────────────────

#' Build long-format milestone ratings from RDM forms
#'
#' Reads the forms apps already hold in memory (\code{all_forms} from the
#' gmed loaders, or the per-form \code{$data} slots of
#' \code{create_milestone_workflow_from_dict()} output), so no extra REDCap
#' call is made.
#'
#' Sources:
#' \describe{
#'   \item{coach}{\code{milestone_entry}: \code{rep_*} by \code{prog_mile_period}.}
#'   \item{ccc}{The same \code{milestone_entry} row, relabelled when the
#'     resident's \code{ccc_review} for that period has \code{ccc_mile == "1"}
#'     ("Any changes to milestones?" = Yes). The CCC dashboard writes its
#'     revised ratings back into \code{milestone_entry} (same instance), and
#'     \code{ccc_review} itself stores only that flag and free-text
#'     \code{ccc_mile_notes}, not ratings, so the flag is the only way to tell
#'     a CCC-revised rating from a coach rating.}
#'   \item{self}{\code{milestone_selfevaluation_c33c}: \code{rep_*_self} by
#'     \code{prog_mile_period_self}.}
#'   \item{acgme}{\code{acgme_miles}: \code{acgme_*} by \code{acgme_mile_period}.}
#' }
#'
#' @param all_forms Named list of form data frames (\code{milestone_entry},
#'   \code{milestone_selfevaluation_c33c}, \code{acgme_miles}, optional
#'   \code{ccc_review}). May also be the result of
#'   \code{create_milestone_workflow_from_dict()}.
#' @param residents Optional resident table with \code{record_id} and
#'   \code{grad_yr}; adds a \code{grad_yr} column (needed for the backtest).
#' @param acgme_scale \code{"internal"} (default) if \code{acgme_*} fields are
#'   already stored on the 1-9 scale, or \code{"native"} if they are on ACGME's
#'   1-5 scale, in which case \code{convert_acgme_to_internal_scale()} is
#'   applied.
#' @return Long data frame (see file header).
#' @export
build_milestone_long <- function(all_forms, residents = NULL,
                                 acgme_scale = c("internal", "native")) {
  acgme_scale <- match.arg(acgme_scale)
  if (is.null(all_forms) || !is.list(all_forms)) return(.mg_empty_long())

  forms <- .mg_forms_from_input(all_forms)

  coach <- .mg_wide_to_long(.mg_instrument_rows(forms$milestone_entry, "milestone_entry"),
                            "prog_mile_period", "field_coach", "coach")
  self_df <- .mg_instrument_rows(forms$milestone_selfevaluation_c33c,
                                 "milestone_selfevaluation_c33c")
  self_pf <- if (!is.null(self_df) && "prog_mile_period_self" %in% names(self_df))
    "prog_mile_period_self" else "prog_mile_period"
  self <- .mg_wide_to_long(self_df, self_pf, "field_self", "self")
  acgme <- .mg_wide_to_long(.mg_instrument_rows(forms$acgme_miles, "acgme_miles"),
                            "acgme_mile_period", "field_acgme", "acgme")
  if (acgme_scale == "native" && nrow(acgme) > 0)
    acgme$rating <- convert_acgme_to_internal_scale(acgme$rating)

  # Relabel CCC-revised milestone_entry rows
  ccc_keys <- .mg_ccc_revised_keys(forms$ccc_review)
  if (length(ccc_keys) && nrow(coach)) {
    is_ccc <- paste(coach$record_id, coach$period) %in% ccc_keys
    coach$rater[is_ccc] <- "ccc"
  }

  out <- rbind(coach, self, acgme)
  if (!nrow(out)) return(.mg_empty_long())
  # One rating per resident/period/subcomp/rater (last instance wins)
  out <- out[!duplicated(out[, c("record_id", "period", "subcomp", "rater")],
                         fromLast = TRUE), , drop = FALSE]

  if (!is.null(residents) && all(c("record_id", "grad_yr") %in% names(residents))) {
    gy <- stats::setNames(as.character(residents$grad_yr),
                          as.character(residents$record_id))
    out$grad_yr <- unname(gy[out$record_id])
  }
  rownames(out) <- NULL
  out
}

# Accept all_forms or create_milestone_workflow_from_dict() output.
.mg_forms_from_input <- function(x) {
  wanted <- c("milestone_entry", "milestone_selfevaluation_c33c",
              "acgme_miles", "ccc_review")
  if (any(wanted %in% names(x))) return(x[intersect(wanted, names(x))])
  # workflow results: list of configs each with $config$form_name and $data
  forms <- list()
  for (nm in names(x)) {
    el <- x[[nm]]
    if (is.list(el) && !is.null(el$config$form_name) && is.data.frame(el$data))
      forms[[el$config$form_name]] <- el$data
  }
  forms
}

# "record_id period" keys where the CCC flagged a milestone change.
.mg_ccc_revised_keys <- function(ccc_review) {
  cr <- .mg_instrument_rows(ccc_review, "ccc_review")
  if (is.null(cr) || !all(c("record_id", "ccc_mile") %in% names(cr)))
    return(character(0))
  pf <- intersect(c("ccc_session", "ccc_period"), names(cr))
  if (!length(pf)) return(character(0))
  per <- .mg_period_num(cr[[pf[1]]])
  flag <- tolower(as.character(cr$ccc_mile)) %in% c("1", "yes")   # raw or label export
  keep <- !is.na(per) & !is.na(flag) & flag
  paste(as.character(cr$record_id[keep]), per[keep])
}

#' Coerce module input to long milestone data
#'
#' @param x A long data frame (from \code{build_milestone_long()}) or a list
#'   of forms accepted by \code{build_milestone_long()}.
#' @param ... Passed to \code{build_milestone_long()}.
#' @return Long data frame.
#' @export
as_milestone_long <- function(x, ...) {
  if (is.null(x)) return(.mg_empty_long())
  if (is.data.frame(x) &&
      all(c("record_id", "period", "subcomp", "rater", "rating") %in% names(x))) {
    x$record_id <- as.character(x$record_id)
    x$period <- as.integer(x$period)
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) return(build_milestone_long(x, ...))
  stop("as_milestone_long: expected long milestone data or a list of RDM forms")
}

#' Pick the rating that counts for each resident, period and subcompetency
#'
#' Fallback order: ACGME if present, else CCC, else coach. Self ratings are
#' never used here.
#'
#' @param long Long milestone data.
#' @param order Source preference order.
#' @return Data frame with one row per record_id/period/subcomp and columns
#'   \code{rating} and \code{source}.
#' @export
select_milestone_rating <- function(long, order = .MG_SOURCE_ORDER) {
  f <- long[long$rater %in% order & !is.na(long$rating), , drop = FALSE]
  if (!nrow(f)) {
    out <- .mg_empty_long()[, c("record_id", "period", "subcomp", "rating")]
    out$source <- character(0)
    return(out)
  }
  f <- f[order(f$record_id, f$period, f$subcomp, match(f$rater, order)), , drop = FALSE]
  f <- f[!duplicated(f[, c("record_id", "period", "subcomp")]), , drop = FALSE]
  f$source <- f$rater
  f$rater <- NULL
  rownames(f) <- NULL
  f
}

#' Extract current and previous ILP goals for a resident
#'
#' @param ilp_data ILP instrument rows (fields \code{record_id},
#'   \code{year_resident} and the goal / goal level fields).
#' @param resident_id Resident record id.
#' @param period Current period (1-6 or label).
#' @return Data frame with \code{subcomp}, \code{domain}, \code{goal_level},
#'   \code{target_rating}, \code{period}, \code{which} ("current" or
#'   "previous"). Previous = the latest ILP before \code{period}.
#' @export
extract_ilp_goals <- function(ilp_data, resident_id, period) {
  empty <- data.frame(subcomp = character(0), domain = character(0),
                      goal_level = numeric(0), target_rating = numeric(0),
                      period = integer(0), which = character(0),
                      stringsAsFactors = FALSE)
  ilp <- .mg_instrument_rows(ilp_data, "ilp")
  if (is.null(ilp) || !"year_resident" %in% names(ilp) || is.null(resident_id))
    return(empty)
  ilp <- ilp[as.character(ilp$record_id) == as.character(resident_id), , drop = FALSE]
  if (!nrow(ilp)) return(empty)
  ilp$period_num <- .mg_period_num(ilp$year_resident)
  cur <- .mg_period_num(period)
  if (is.na(cur)) cur <- suppressWarnings(max(ilp$period_num, na.rm = TRUE))

  rows_for <- function(r, which) {
    do.call(rbind, lapply(names(.MG_ILP_DOMAINS), function(d) {
      f <- .MG_ILP_DOMAINS[[d]]
      if (!f$goal %in% names(r)) return(NULL)
      sc <- ilp_goal_subcomp(d, r[[f$goal]][1])
      if (is.na(sc)) return(NULL)
      lv <- if (f$level %in% names(r)) suppressWarnings(as.numeric(r[[f$level]][1])) else NA_real_
      data.frame(subcomp = sc, domain = d, goal_level = lv,
                 target_rating = ilp_goal_level_to_rating(lv),
                 period = r$period_num[1], which = which,
                 stringsAsFactors = FALSE)
    }))
  }

  out <- list()
  c_row <- ilp[!is.na(ilp$period_num) & ilp$period_num == cur, , drop = FALSE]
  if (nrow(c_row)) out$cur <- rows_for(c_row[nrow(c_row), , drop = FALSE], "current")
  prev <- ilp[!is.na(ilp$period_num) & ilp$period_num < cur, , drop = FALSE]
  if (nrow(prev)) {
    prev <- prev[prev$period_num == max(prev$period_num), , drop = FALSE]
    out$prev <- rows_for(prev[nrow(prev), , drop = FALSE], "previous")
  }
  res <- do.call(rbind, out)
  if (is.null(res)) empty else { rownames(res) <- NULL; res }
}
