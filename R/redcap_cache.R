# redcap_cache.R
# Read and write pre-computed milestone medians to/from a designated
# REDCap cache record (instrument: app_cache, field: cache_medians_json).
#
# Workflow:
#   rdm-data-refresh  →  write_medians_cache()  →  REDCap app_cache record
#   Each app startup  →  load_cached_medians()  →  medians ready in ~1 sec
#
# Setup:
#   - Create an app_cache instrument in REDCap with fields:
#       cache_medians_json  (Notes Box)
#       cache_updated_at    (Text)
#   - Create one record for the cache; set res_archive = "1" on it
#   - Set CACHE_RECORD_ID env var on all apps and rdm-data-refresh

# ── Write ─────────────────────────────────────────────────────────────────────

#' Write milestone medians to REDCap cache
#'
#' Serialises the output of \code{calculate_all_milestone_medians()} to JSON
#' and writes it to a designated cache record in REDCap.  Only the
#' \code{medians}, \code{type}, \code{columns}, and \code{form_name} slots are
#' written — \code{processed_data} is deliberately omitted to keep the payload
#' small.
#'
#' @param medians Named list returned by \code{calculate_all_milestone_medians()}.
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#'
#' @return Invisible \code{TRUE} on success, \code{FALSE} on failure.
#' @export
write_medians_cache <- function(
    medians,
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID")
) {

  if (!nzchar(cache_record_id)) {
    stop("write_medians_cache: CACHE_RECORD_ID env var not set. ",
         "Set it to the record_id of your app_cache record in REDCap.")
  }

  if (!is.list(medians) || length(medians) == 0) {
    warning("write_medians_cache: medians list is empty — nothing to cache")
    return(invisible(FALSE))
  }

  # Strip processed_data from each form (large per-resident df, not needed)
  slim <- lapply(medians, function(x) {
    list(
      medians   = x$medians,
      type      = x$type,
      columns   = x$columns,
      form_name = x$form_name
    )
  })

  # Serialise — NA values become JSON null, single-element vectors stay arrays
  json_str <- tryCatch(
    jsonlite::toJSON(slim, auto_unbox = TRUE, na = "null"),
    error = function(e) stop("write_medians_cache: JSON serialisation failed: ", e$message)
  )

  size_kb <- nchar(json_str, type = "bytes") / 1024
  message(sprintf("write_medians_cache: JSON size = %.1f KB (%d forms)", size_kb, length(slim)))
  if (size_kb > 500)
    warning("write_medians_cache: JSON is ", round(size_kb), " KB — unexpectedly large")

  cache_row <- data.frame(
    record_id          = cache_record_id,
    cache_medians_json = as.character(json_str),
    cache_updated_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors   = FALSE
  )

  result <- REDCapR::redcap_write(
    ds_to_write = cache_row,
    redcap_uri  = redcap_url,
    token       = rdm_token,
    verbose     = FALSE
  )

  if (isTRUE(result$success)) {
    message(sprintf("write_medians_cache: written to record %s at %s",
                    cache_record_id, format(Sys.time(), "%H:%M:%S")))
    return(invisible(TRUE))
  } else {
    warning("write_medians_cache: REDCap write failed — ", result$outcome_message)
    return(invisible(FALSE))
  }
}

# ── Read ──────────────────────────────────────────────────────────────────────

#' Load milestone medians from REDCap cache
#'
#' Reads the pre-computed milestone medians written by
#' \code{write_medians_cache()}.  Returns \code{NULL} silently if the cache
#' record is unavailable or empty so that callers can fall back gracefully to
#' computing medians directly.
#'
#' The returned list matches the structure of
#' \code{calculate_all_milestone_medians()} minus \code{processed_data}:
#' \describe{
#'   \item{medians}{Data frame of period-level medians.}
#'   \item{type}{Character, one of \code{"program"}, \code{"self"}, \code{"acgme"}.}
#'   \item{columns}{Character vector of milestone field names.}
#'   \item{form_name}{Character, REDCap form name.}
#' }
#'
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#' @param max_age_hours Warn if the cache is older than this many hours.
#'   Set to \code{Inf} to silence the warning.
#'
#' @return Named list of medians per milestone form, or \code{NULL}.
#' @export
load_cached_medians <- function(
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID"),
    max_age_hours   = 3
) {

  if (!nzchar(cache_record_id)) {
    message("load_cached_medians: CACHE_RECORD_ID not set — skipping cache")
    return(NULL)
  }

  result <- tryCatch(
    REDCapR::redcap_read_oneshot(
      redcap_uri = redcap_url,
      token      = rdm_token,
      records    = cache_record_id,
      forms      = "app_cache",
      verbose    = FALSE
    ),
    error = function(e) {
      message("load_cached_medians: REDCap read error — ", e$message)
      NULL
    }
  )

  if (is.null(result) || !isTRUE(result$success) || nrow(result$data) == 0) {
    message("load_cached_medians: cache unavailable — will compute medians directly")
    return(NULL)
  }

  row <- result$data

  if (!nzchar(row$cache_medians_json[1])) {
    message("load_cached_medians: cache record exists but is empty")
    return(NULL)
  }

  # Age check
  if ("cache_updated_at" %in% names(row) && nzchar(row$cache_updated_at[1])) {
    updated <- tryCatch(as.POSIXct(row$cache_updated_at[1]), error = function(e) NULL)
    if (!is.null(updated)) {
      age_hrs <- as.numeric(difftime(Sys.time(), updated, units = "hours"))
      if (is.finite(max_age_hours) && age_hrs > max_age_hours)
        message(sprintf("load_cached_medians: cache is %.1f h old (threshold: %g h)",
                        age_hrs, max_age_hours))
    }
  }

  # Deserialise
  medians <- tryCatch(
    jsonlite::fromJSON(row$cache_medians_json[1], simplifyDataFrame = TRUE),
    error = function(e) {
      message("load_cached_medians: JSON parse failed — ", e$message)
      NULL
    }
  )

  if (is.null(medians)) return(NULL)

  # fromJSON returns medians$<form>$medians as a data.frame — ensure it is
  medians <- lapply(medians, function(x) {
    if (!is.null(x$medians) && !is.data.frame(x$medians))
      x$medians <- as.data.frame(x$medians)
    x
  })

  message(sprintf("load_cached_medians: loaded %d form(s) from cache", length(medians)))
  medians
}
