# redcap_cache.R
# Read and write pre-computed milestone medians to/from a designated
# REDCap cache record (instrument: app_cache, field: cache_medians_json).
# Also holds the equivalent pair for amiontools' aggregate Amion summaries
# (cache_amion_json / cache_amion_updated_at, added 2026-09-03) and the
# RLE-encoded per-day expected-conference calendar
# (cache_expected_calendar_json / cache_expected_calendar_updated_at,
# added 2026-09-04) — same record, same pattern each time.
#
# Workflow:
#   rdm-data-refresh        →  write_medians_cache()          →  REDCap app_cache record
#   rdm-data-refresh (Amion)→  write_amion_cache()            →  REDCap app_cache record
#   rdm-data-refresh (Amion)→  write_expected_calendar_cache()→  REDCap app_cache record
#   Each app startup        →  load_cached_medians() / load_cached_amion() /
#                               load_cached_expected_calendar()
#
# Setup:
#   - app_cache instrument in REDCap has fields:
#       cache_medians_json               (Notes Box)
#       cache_updated_at                 (Text)
#       cache_amion_json                 (Notes Box)
#       cache_amion_updated_at           (Text)
#       cache_expected_calendar_json     (Notes Box)
#       cache_expected_calendar_updated_at (Text)
#   - One record for the cache; set res_archive = "1" on it
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

# ── Amion summaries: Write ──────────────────────────────────────────────────

#' Write amiontools aggregate summaries to REDCap cache
#'
#' Serialises a named list of amiontools' aggregate summary data frames
#' (rotation/team/time-allocation class/program averages and per-resident
#' wide tables) to JSON, gzip-compresses and base64-encodes it, and writes
#' it to the same cache record used by \code{write_medians_cache()}.
#' Deliberately aggregate-only — full per-resident-day detail
#' (\code{build_daily_detail()}) is far too large for a REDCap Notes Box
#' field (measured ~12 MB vs. ~80 KB for the aggregates) and must never be
#' passed here.
#'
#' \strong{Why gzip+base64 (found the hard way, 2026-09-03):} REDCap's
#' "Notes Box" field type is backed by a MySQL \code{TEXT} column — a hard
#' \strong{65,535-byte} ceiling, not the ~500 KB figure
#' \code{write_medians_cache()} warns at (that figure was never verified
#' against REDCap's actual storage limit). A raw-JSON write of this
#' function's ~79 KB payload was silently truncated to exactly 65,535
#' bytes by REDCap on write — no error from the API, \code{redcap_write()}
#' reported success, and the corruption only surfaced later as a JSON parse
#' failure on read. Compression reduces the same payload to ~20 KB,
#' comfortably under the ceiling with headroom as the program grows.
#' \code{write_medians_cache()}'s existing raw-JSON path carries the same
#' latent risk if its payload ever exceeds 64 KB (currently ~8.5 KB, so not
#' an active problem, but the same silent-truncation failure mode).
#'
#' @param summaries Named list of data frames — the aggregate outputs of
#'   \code{amiontools::build_rotation_summary()},
#'   \code{build_team_summary()}, and \code{build_time_allocation_summary()}.
#'   Caller assembles the list (this function doesn't call amiontools
#'   directly, keeping gmed free of an amiontools dependency).
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#'
#' @return Invisible \code{TRUE} on success, \code{FALSE} on failure.
#' @export
write_amion_cache <- function(
    summaries,
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID")
) {

  if (!nzchar(cache_record_id)) {
    stop("write_amion_cache: CACHE_RECORD_ID env var not set. ",
         "Set it to the record_id of your app_cache record in REDCap.")
  }

  if (!is.list(summaries) || length(summaries) == 0) {
    warning("write_amion_cache: summaries list is empty — nothing to cache")
    return(invisible(FALSE))
  }

  json_str <- tryCatch(
    jsonlite::toJSON(summaries, auto_unbox = TRUE, na = "null"),
    error = function(e) stop("write_amion_cache: JSON serialisation failed: ", e$message)
  )

  raw_kb <- nchar(json_str, type = "bytes") / 1024

  encoded <- jsonlite::base64_enc(memCompress(charToRaw(as.character(json_str)), type = "gzip"))
  encoded_bytes <- nchar(encoded, type = "bytes")

  message(sprintf("write_amion_cache: JSON %.1f KB -> gzip+base64 %.1f KB (%d tables)",
                  raw_kb, encoded_bytes / 1024, length(summaries)))

  # REDCap "Notes Box" = MySQL TEXT column, hard 65,535-byte ceiling —
  # REDCap silently truncates over this (no API error), which corrupts the
  # payload rather than failing loudly. Refuse to write past a safety
  # margin below that rather than risk a silent truncation.
  if (encoded_bytes > 65000) {
    warning("write_amion_cache: encoded payload is ", round(encoded_bytes / 1024),
            " KB — over REDCap's ~64 KB Notes Box ceiling. Refusing to write ",
            "(would be silently truncated/corrupted by REDCap). Trim summaries or ",
            "split across multiple cache fields.")
    return(invisible(FALSE))
  }

  cache_row <- data.frame(
    record_id               = cache_record_id,
    cache_amion_json        = encoded,
    cache_amion_updated_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors        = FALSE
  )

  result <- REDCapR::redcap_write(
    ds_to_write = cache_row,
    redcap_uri  = redcap_url,
    token       = rdm_token,
    verbose     = FALSE
  )

  if (isTRUE(result$success)) {
    message(sprintf("write_amion_cache: written to record %s at %s",
                    cache_record_id, format(Sys.time(), "%H:%M:%S")))
    return(invisible(TRUE))
  } else {
    warning("write_amion_cache: REDCap write failed — ", result$outcome_message)
    return(invisible(FALSE))
  }
}

# ── Amion summaries: Read ───────────────────────────────────────────────────

#' Load amiontools aggregate summaries from REDCap cache
#'
#' Reads the aggregate Amion summaries written by \code{write_amion_cache()}.
#' Returns \code{NULL} silently if the cache record is unavailable or empty
#' so callers can fall back gracefully to a live Amion fetch.
#'
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#' @param max_age_hours Warn if the cache is older than this many hours.
#'   Set to \code{Inf} to silence the warning. Default 192h (8 days) —
#'   the Amion cache refreshes weekly, unlike the medians cache's 3h default.
#'
#' @return Named list of data frames (same shape passed to
#'   \code{write_amion_cache()}), or \code{NULL}.
#' @export
load_cached_amion <- function(
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID"),
    max_age_hours   = 192
) {

  if (!nzchar(cache_record_id)) {
    message("load_cached_amion: CACHE_RECORD_ID not set — skipping cache")
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
      message("load_cached_amion: REDCap read error — ", e$message)
      NULL
    }
  )

  if (is.null(result) || !isTRUE(result$success) || nrow(result$data) == 0) {
    message("load_cached_amion: cache unavailable — will fetch Amion live")
    return(NULL)
  }

  row <- result$data

  if (!"cache_amion_json" %in% names(row) || !nzchar(row$cache_amion_json[1])) {
    message("load_cached_amion: cache record exists but is empty")
    return(NULL)
  }

  if ("cache_amion_updated_at" %in% names(row) && nzchar(row$cache_amion_updated_at[1])) {
    updated <- tryCatch(as.POSIXct(row$cache_amion_updated_at[1]), error = function(e) NULL)
    if (!is.null(updated)) {
      age_hrs <- as.numeric(difftime(Sys.time(), updated, units = "hours"))
      if (is.finite(max_age_hours) && age_hrs > max_age_hours)
        message(sprintf("load_cached_amion: cache is %.1f h old (threshold: %g h)",
                        age_hrs, max_age_hours))
    }
  }

  # Decode+decompress the gzip+base64 payload written by write_amion_cache()
  # (see its docs for why: REDCap's Notes Box field has a hard 65,535-byte
  # ceiling that silently truncates raw JSON over that size).
  json_str <- tryCatch(
    rawToChar(memDecompress(jsonlite::base64_dec(row$cache_amion_json[1]), type = "gzip")),
    error = function(e) {
      message("load_cached_amion: base64/gzip decode failed — ", e$message)
      NA_character_
    }
  )

  if (is.na(json_str)) return(NULL)

  summaries <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE),
    error = function(e) {
      message("load_cached_amion: JSON parse failed — ", e$message)
      NULL
    }
  )

  if (is.null(summaries)) return(NULL)

  summaries <- lapply(summaries, function(x) {
    if (!is.data.frame(x)) x <- as.data.frame(x)
    x
  })

  message(sprintf("load_cached_amion: loaded %d table(s) from cache", length(summaries)))
  summaries
}

# ── Expected-conference calendar: Write ─────────────────────────────────────

#' Write the RLE-encoded expected-conference calendar to REDCap cache
#'
#' Serialises a data frame of (record_id, start, end, expected) blocks —
#' one row per consecutive run of the same expected-conference value for a
#' resident — to JSON, gzip-compresses and base64-encodes it (same reason
#' as \code{write_amion_cache()}: REDCap's Notes Box 65,535-byte ceiling),
#' and writes it to the same cache record.
#'
#' RLE, not one row per resident-day, because a raw per-day table measured
#' ~55 KB compressed (found 2026-09-04) — too close to the ceiling for
#' comfort as the roster/season grows. Rotation blocks run for days/weeks
#' at a time, so collapsing consecutive same-value runs per resident
#' shrinks this ~3.5x (measured: 19,171 raw rows / 54.6 KB -> 5,448 RLE
#' rows / 31.3 KB, same underlying data).
#'
#' @param calendar_rle Data frame with columns record_id, start, end
#'   (Date or integer day-count — caller's choice, this function doesn't
#'   care as long as \code{load_cached_expected_calendar()}'s caller knows
#'   which), expected ("S"/"V"/"N" or similar short codes).
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#'
#' @return Invisible \code{TRUE} on success, \code{FALSE} on failure.
#' @export
write_expected_calendar_cache <- function(
    calendar_rle,
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID")
) {

  if (!nzchar(cache_record_id)) {
    stop("write_expected_calendar_cache: CACHE_RECORD_ID env var not set. ",
         "Set it to the record_id of your app_cache record in REDCap.")
  }

  if (!is.data.frame(calendar_rle) || nrow(calendar_rle) == 0) {
    warning("write_expected_calendar_cache: calendar_rle is empty — nothing to cache")
    return(invisible(FALSE))
  }

  json_str <- tryCatch(
    jsonlite::toJSON(calendar_rle, auto_unbox = TRUE, na = "null"),
    error = function(e) stop("write_expected_calendar_cache: JSON serialisation failed: ", e$message)
  )

  raw_kb <- nchar(json_str, type = "bytes") / 1024
  encoded <- jsonlite::base64_enc(memCompress(charToRaw(as.character(json_str)), type = "gzip"))
  encoded_bytes <- nchar(encoded, type = "bytes")

  message(sprintf("write_expected_calendar_cache: JSON %.1f KB -> gzip+base64 %.1f KB (%d rows)",
                  raw_kb, encoded_bytes / 1024, nrow(calendar_rle)))

  if (encoded_bytes > 65000) {
    warning("write_expected_calendar_cache: encoded payload is ", round(encoded_bytes / 1024),
            " KB — over REDCap's ~64 KB Notes Box ceiling. Refusing to write.")
    return(invisible(FALSE))
  }

  cache_row <- data.frame(
    record_id                          = cache_record_id,
    cache_expected_calendar_json       = encoded,
    cache_expected_calendar_updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors                   = FALSE
  )

  result <- REDCapR::redcap_write(
    ds_to_write = cache_row,
    redcap_uri  = redcap_url,
    token       = rdm_token,
    verbose     = FALSE
  )

  if (isTRUE(result$success)) {
    message(sprintf("write_expected_calendar_cache: written to record %s at %s",
                    cache_record_id, format(Sys.time(), "%H:%M:%S")))
    return(invisible(TRUE))
  } else {
    warning("write_expected_calendar_cache: REDCap write failed — ", result$outcome_message)
    return(invisible(FALSE))
  }
}

# ── Expected-conference calendar: Read ──────────────────────────────────────

#' Load the RLE-encoded expected-conference calendar from REDCap cache
#'
#' @param rdm_token REDCap API token (default: \code{RDM_TOKEN} env var).
#' @param redcap_url REDCap API URL.
#' @param cache_record_id Record ID of the cache record
#'   (default: \code{CACHE_RECORD_ID} env var).
#' @param max_age_hours Warn if the cache is older than this many hours.
#'   Default 192h (8 days) — refreshes weekly, same as the Amion cache.
#'
#' @return Data frame (record_id, start, end, expected), or \code{NULL}.
#' @export
load_cached_expected_calendar <- function(
    rdm_token       = Sys.getenv("RDM_TOKEN"),
    redcap_url      = "https://redcapsurvey.slu.edu/api/",
    cache_record_id = Sys.getenv("CACHE_RECORD_ID"),
    max_age_hours   = 192
) {

  if (!nzchar(cache_record_id)) {
    message("load_cached_expected_calendar: CACHE_RECORD_ID not set — skipping cache")
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
      message("load_cached_expected_calendar: REDCap read error — ", e$message)
      NULL
    }
  )

  if (is.null(result) || !isTRUE(result$success) || nrow(result$data) == 0) {
    message("load_cached_expected_calendar: cache unavailable — will build live")
    return(NULL)
  }

  row <- result$data

  if (!"cache_expected_calendar_json" %in% names(row) || !nzchar(row$cache_expected_calendar_json[1])) {
    message("load_cached_expected_calendar: cache record exists but is empty")
    return(NULL)
  }

  if ("cache_expected_calendar_updated_at" %in% names(row) && nzchar(row$cache_expected_calendar_updated_at[1])) {
    updated <- tryCatch(as.POSIXct(row$cache_expected_calendar_updated_at[1]), error = function(e) NULL)
    if (!is.null(updated)) {
      age_hrs <- as.numeric(difftime(Sys.time(), updated, units = "hours"))
      if (is.finite(max_age_hours) && age_hrs > max_age_hours)
        message(sprintf("load_cached_expected_calendar: cache is %.1f h old (threshold: %g h)",
                        age_hrs, max_age_hours))
    }
  }

  json_str <- tryCatch(
    rawToChar(memDecompress(jsonlite::base64_dec(row$cache_expected_calendar_json[1]), type = "gzip")),
    error = function(e) {
      message("load_cached_expected_calendar: base64/gzip decode failed — ", e$message)
      NA_character_
    }
  )

  if (is.na(json_str)) return(NULL)

  calendar_rle <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE),
    error = function(e) {
      message("load_cached_expected_calendar: JSON parse failed — ", e$message)
      NULL
    }
  )

  if (is.null(calendar_rle)) return(NULL)
  if (!is.data.frame(calendar_rle)) calendar_rle <- as.data.frame(calendar_rle)

  message(sprintf("load_cached_expected_calendar: loaded %d RLE row(s) from cache", nrow(calendar_rle)))
  calendar_rle
}
