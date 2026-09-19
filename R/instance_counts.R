# instance_counts.R ── generic windowed count of repeating-instrument rows
# for one resident, keyed by a date field. Added for the weekly resident
# digest (imslu.resident.digest / imslu.email extension, 2026-09) but
# generic enough for any "how many X in the last N days" need — e.g.
# faculty/fellow evaluations received (assessment.ass_date), faculty
# evaluations the resident has completed (faculty_evaluation.fac_eval_date).

#' Count repeating-instrument rows for one record within a trailing window.
#'
#' Pulls only record_id/redcap_repeat_instrument/redcap_repeat_instance/
#' `date_field` for the given instrument+record (not the full form), so this
#' stays cheap even on instruments with many other fields.
#'
#' @param rdm_token REDCap API token (test or prod — caller's choice).
#' @param redcap_url REDCap API base URL.
#' @param record_id Record to count for.
#' @param instrument Repeating instrument name (e.g. "assessment",
#'   "faculty_evaluation").
#' @param date_field Name of the date field on that instrument to window by
#'   (e.g. "ass_date", "fac_eval_date"). Expected as an ISO-ish date string;
#'   parsed with `as.Date()`.
#' @param days Trailing window size in days (inclusive of today).
#' @param as_of Reference "today" for the window — defaults to `Sys.Date()`,
#'   overridable for testing/backfill scenarios.
#' @return A list: `n` (integer count within the window), `dates` (the
#'   in-window dates, for callers that want to show them), and `n_total`
#'   (total rows on the instrument for this record, any date — lets a
#'   caller distinguish "zero ever" from "zero this window").
#' @export
count_recent_instances <- function(rdm_token, redcap_url, record_id, instrument,
                                   date_field, days, as_of = Sys.Date()) {
  empty <- list(n = 0L, dates = as.Date(character()), n_total = 0L)
  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token = rdm_token, content = "record", action = "export",
        format = "json", type = "flat",
        records    = as.character(record_id),
        `forms[0]` = instrument,
        `fields[0]` = "record_id",
        `fields[1]` = date_field,
        rawOrLabel = "raw", rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false", exportSurveyFields = "false",
        exportDataAccessGroups = "false", returnFormat = "json"
      ),
      encode = "form", httr::timeout(30)
    )
    if (httr::status_code(resp) != 200) return(empty)
    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    if (!is.data.frame(dat) || nrow(dat) == 0) return(empty)
    dat <- dat[!is.na(dat$redcap_repeat_instrument) &
                 dat$redcap_repeat_instrument == instrument, , drop = FALSE]
    if (nrow(dat) == 0 || !date_field %in% names(dat)) return(empty)
    dates <- suppressWarnings(as.Date(dat[[date_field]]))
    dates <- dates[!is.na(dates)]
    in_window <- dates[dates >= (as.Date(as_of) - days) & dates <= as.Date(as_of)]
    list(n = length(in_window), dates = sort(in_window), n_total = length(dates))
  }, error = function(e) empty)
}
