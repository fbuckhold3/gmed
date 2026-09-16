# redcap_write.R ── generic single-instance REDCap repeating-instrument
# writer, promoted from imslu.ind.dash's local `.rc_save()` (mod_self_eval.R)
# so packages/apps other than ind.dash (amiontools' duty-hour confirm form,
# gmed's own scholarship entry form) can write without each rolling their
# own httr::POST + verification logic. Behavior is unchanged from the
# original — same request shape, same "REDCap returned 200 but wrote
# nothing" empty-body check, same round-trip verify-by-reread — only the
# REDCap credentials moved from an implicit `app_config` global to explicit
# parameters, since this function has callers outside ind.dash's own
# environment.
#
# ind.dash's own mod_self_eval.R keeps its local `.rc_save()` as-is (not
# touched by this promotion) — only the two callers that moved into shared
# packages (duty-hour confirm, scholarship entry) were switched to this.

#' Write one repeating-instrument instance to REDCap, with a round-trip
#' verification read-back.
#'
#' @param record_id REDCap record_id to write under.
#' @param instrument Repeating instrument name (e.g. "duty_hour_log",
#'   "scholarship").
#' @param instance Repeat instance number (existing, to overwrite; or the
#'   next available, to append).
#' @param fields Named list of field -> value to write.
#' @param redcap_url REDCap API base URL.
#' @param rdm_token REDCap API token (test or prod — caller's choice).
#' @return A list: `success` (logical) and either `ts`/`body` (on success)
#'   or `message` (on failure, human-readable).
#' @export
rc_save_instance <- function(record_id, instrument, instance, fields,
                             redcap_url, rdm_token) {
  tryCatch({
    row <- as.data.frame(
      c(list(record_id = as.character(record_id),
             redcap_repeat_instrument = instrument,
             redcap_repeat_instance   = as.character(instance)),
        lapply(fields, function(x)
          if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) ""
          else as.character(x))),
      stringsAsFactors = FALSE, check.names = FALSE)
    resp <- httr::POST(
      url  = redcap_url,
      body = list(token = rdm_token, content = "record",
                  format = "json", type = "flat",
                  overwriteBehavior = "overwrite",
                  data = jsonlite::toJSON(row, auto_unbox = TRUE),
                  returnContent = "ids", returnFormat = "json"),
      encode = "form", httr::timeout(30))
    status <- httr::status_code(resp)
    body   <- httr::content(resp, "text", encoding = "UTF-8")
    body_trim <- trimws(body)
    has_err <- grepl("\"error\"", body, fixed = TRUE) ||
               grepl("^ERROR", body, ignore.case = TRUE)
    # REDCap returns "[]" (or similar empty body) when the POST was accepted
    # (HTTP 200) but zero records were actually written — typically a schema
    # mismatch (instrument not set up as repeating, field doesn't exist, etc).
    empty_body <- body_trim %in% c("", "[]", "{}") ||
                  grepl("\"count\"\\s*:\\s*0", body, perl = TRUE)
    message("[rc_save_instance] record=", record_id, " instrument=", instrument,
            " instance=", instance, " n_fields=", length(fields),
            " status=", status, " body=", substr(body_trim, 1, 120))
    initial_ok <- status == 200 && !has_err && !empty_body
    if (!initial_ok) {
      return(list(success = FALSE,
                  message = paste0("REDCap (HTTP ", status, "): ",
                                   if (empty_body) "empty response — 0 records written. "
                                   else "",
                                   substr(body, 1, 500))))
    }

    # Verification read-back: REDCap can return the record id (so the body
    # check passes) even when it silently rejected every field — most
    # commonly when the API token has read but not write access to the
    # form. Pick a field whose VALUE we just wrote (skipping key/period
    # fields whose value is preserved from any pre-existing record) and
    # confirm REDCap returns the exact value we sent. If the round-trip
    # disagrees, surface a real save failure.
    skip_keys <- c("record_id", "redcap_repeat_instrument",
                   "redcap_repeat_instance", "year_resident", "s_e_period",
                   "prog_mile_period_self")
    chk_field <- NULL; written <- ""
    for (fn in names(fields)) {
      if (fn %in% skip_keys) next
      v <- as.character(fields[[fn]])
      if (length(v) >= 1 && !is.na(v[1]) && nzchar(trimws(v[1]))) {
        chk_field <- fn; written <- v[1]; break
      }
    }
    if (!is.null(chk_field)) {
      verify <- tryCatch({
        v_resp <- httr::POST(
          url  = redcap_url,
          body = list(token = rdm_token, content = "record",
                      format = "json", type = "flat",
                      `records[0]` = as.character(record_id),
                      `fields[0]`  = "record_id",
                      `fields[1]`  = chk_field,
                      `forms[0]`   = instrument,
                      returnFormat = "json"),
          encode = "form", httr::timeout(30))
        if (httr::status_code(v_resp) != 200) NULL
        else jsonlite::fromJSON(
          httr::content(v_resp, "text", encoding = "UTF-8"),
          simplifyVector = TRUE)
      }, error = function(e) NULL)
      verify_ok <- !is.null(verify) && is.data.frame(verify) && nrow(verify) > 0 &&
                   chk_field %in% names(verify) &&
                   "redcap_repeat_instance" %in% names(verify)
      if (verify_ok) {
        match_row <- verify[as.character(verify$redcap_repeat_instance) ==
                              as.character(instance), , drop = FALSE]
        actual <- if (nrow(match_row) > 0)
                    trimws(as.character(match_row[[chk_field]][1])) else ""
        if (!identical(actual, trimws(written))) {
          message("[rc_save_instance] VERIFY FAILED instrument=", instrument,
                  " instance=", instance, " field=", chk_field,
                  " expected='", substr(written, 1, 60),
                  "' actual='", substr(actual, 1, 60), "'")
          return(list(success = FALSE,
                      message = paste0(
                        "REDCap returned success but the round-trip read of '",
                        chk_field, "' did not match what was sent. ",
                        "The API token likely lacks write access to the '",
                        instrument, "' form (Read-Only or No Access). ",
                        "Update User Rights in REDCap and try again.")))
        }
      } else {
        v_class <- if (is.null(verify)) "NULL" else paste(class(verify), collapse = ",")
        v_n     <- if (is.data.frame(verify)) nrow(verify) else NA_integer_
        v_names <- if (!is.null(verify) && !is.null(names(verify)))
                     paste(head(names(verify), 8), collapse = ",") else ""
        message("[rc_save_instance] VERIFY SKIPPED instrument=", instrument,
                " instance=", instance, " field=", chk_field,
                " verify_class=", v_class, " nrow=", v_n,
                " names=", v_names)
      }
    }
    list(success = TRUE,
         ts      = format(Sys.time(), "%b %d %I:%M %p"),
         body    = substr(body, 1, 200))
  }, error = function(e) list(success = FALSE,
                              message = paste("Error:", e$message)))
}
