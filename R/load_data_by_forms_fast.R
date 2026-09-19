#' Parallel per-instrument REDCap export
#'
#' Fetches each requested REDCap instrument with its own \code{forms[]}
#' export, running the requests concurrently via \code{curl}'s multi
#' interface. Returns one data frame per instrument (all columns character).
#'
#' Why this exists: a single flat export of the whole RDM project is ~180 MB
#' of mostly-empty JSON (every repeating instrument's columns padded onto
#' every row) and takes ~25 s, ~18 s of which is \code{jsonlite} parsing.
#' Per-instrument exports total ~19 MB and the slowest single instrument
#' takes ~1.5 s. Measured against prod RDM, 2026-09-19.
#'
#' @param token REDCap API token.
#' @param url REDCap API URL.
#' @param forms Character vector of instrument names (REDCap unique form names).
#' @param raw_or_label "raw" or "label".
#' @param max_parallel Maximum concurrent connections (default 6).
#' @param timeout Per-request timeout in seconds.
#'
#' @return Named list (one element per instrument) of data frames. Stops with
#'   an error naming the failed instruments if any request fails, so callers
#'   can fall back to the single-export path rather than silently returning
#'   partial data.
#' @keywords internal
pull_forms_parallel <- function(token, url, forms,
                                raw_or_label = "raw",
                                max_parallel = 6L,
                                timeout = 180) {

  make_body <- function(form) {
    paste0(
      "token=",              utils::URLencode(token, reserved = TRUE),
      "&content=record",
      "&action=export",
      "&format=json",
      "&type=flat",
      "&rawOrLabel=",        utils::URLencode(raw_or_label, reserved = TRUE),
      "&rawOrLabelHeaders=raw",
      "&exportCheckboxLabel=false",
      "&exportSurveyFields=true",
      "&exportDataAccessGroups=false",
      "&returnFormat=json",
      "&forms%5B0%5D=",      utils::URLencode(form, reserved = TRUE),
      # REDCap omits record_id / redcap_repeat_* from a forms[]-only export of
      # any instrument that doesn't contain the record-ID field (rows would
      # arrive with no way to tell which resident they belong to). Requesting
      # record_id explicitly restores them, at the cost of also returning each
      # record's blank base row, which the caller's row filter drops.
      "&fields%5B0%5D=record_id"
    )
  }

  parse_one <- function(raw_bytes) {
    txt <- rawToChar(raw_bytes)
    Encoding(txt) <- "UTF-8"
    df <- jsonlite::fromJSON(txt, flatten = TRUE)
    if (length(df) == 0) return(data.frame(stringsAsFactors = FALSE))
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, as.character)
    df
  }

  pool    <- curl::new_pool(total_con = max_parallel, host_con = max_parallel)
  results <- stats::setNames(vector("list", length(forms)), forms)
  errors  <- character()

  for (form in forms) {
    local({
      f <- form
      h <- curl::new_handle(
        postfields      = make_body(f),
        ssl_verifypeer  = FALSE,   # matches httr::set_config() used elsewhere in gmed
        ssl_verifyhost  = FALSE,
        timeout         = timeout
      )
      curl::handle_setheaders(h, "Content-Type" = "application/x-www-form-urlencoded")
      curl::curl_fetch_multi(
        url, pool = pool, handle = h,
        done = function(res) {
          if (res$status_code != 200) {
            errors <<- c(errors, sprintf("%s (HTTP %d)", f, res$status_code))
          } else {
            parsed <- tryCatch(parse_one(res$content), error = function(e) e)
            if (inherits(parsed, "error")) {
              errors <<- c(errors, sprintf("%s (parse: %s)", f, conditionMessage(parsed)))
            } else {
              results[[f]] <<- parsed
            }
          }
        },
        fail = function(msg) errors <<- c(errors, sprintf("%s (%s)", f, msg))
      )
    })
  }

  curl::multi_run(pool = pool)

  if (length(errors) > 0) {
    stop("pull_forms_parallel failed for: ", paste(errors, collapse = "; "), call. = FALSE)
  }
  results
}


#' Load data organised by form using per-instrument parallel exports
#'
#' Drop-in alternative to \code{load_data_by_forms()} for
#' \code{load_rdm_complete(per_form = TRUE)}. Produces the same
#' \code{$forms} structure: per instrument, only the dictionary fields (plus
#' REDCap metadata and checkbox expansions), and only rows where at least one
#' of that instrument's own fields is non-empty. It does NOT build the wide
#' all-instruments frame, so \code{$raw_data} is \code{NULL}.
#'
#' @param rdm_token,redcap_url,raw_or_label As in \code{load_data_by_forms()}.
#' @param data_dict Optional pre-fetched data dictionary (avoids a second
#'   metadata call when the caller already has it).
#' @param max_parallel Maximum concurrent connections.
#' @param verbose Print timing message.
#'
#' @return List with \code{raw_data = NULL}, \code{data_dict}, \code{forms},
#'   \code{resident_data}, and \code{metadata}.
#' @keywords internal
load_data_by_forms_fast <- function(rdm_token = NULL,
                                    redcap_url = "https://redcapsurvey.slu.edu/api/",
                                    raw_or_label = "raw",
                                    data_dict = NULL,
                                    max_parallel = 6L,
                                    verbose = FALSE) {

  if (is.null(rdm_token) || !nzchar(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN")
    if (!nzchar(rdm_token)) {
      stop("RDM_TOKEN not provided and not found in environment variables")
    }
  }

  t0 <- Sys.time()
  if (is.null(data_dict)) {
    data_dict <- get_evaluation_dictionary(token = rdm_token, url = redcap_url)
  }

  form_names <- unique(data_dict$form_name)
  form_names <- form_names[!is.na(form_names)]

  raw_by_form <- pull_forms_parallel(
    token = rdm_token, url = redcap_url, forms = form_names,
    raw_or_label = raw_or_label, max_parallel = max_parallel
  )

  metadata_fields <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
                       "redcap_event_name", "redcap_survey_identifier")

  forms <- list()
  for (current_form in form_names) {
    df <- raw_by_form[[current_form]]
    if (is.null(df) || nrow(df) == 0) next

    # Every downstream consumer (archive filtering, per-resident lookups)
    # keys on these. If REDCap omitted them, refuse rather than return
    # frames that silently skip archived-resident filtering; the caller
    # falls back to the single full export.
    missing_meta <- setdiff(c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance"),
                            names(df))
    if (length(missing_meta) > 0) {
      stop("per-instrument export for '", current_form, "' is missing column(s): ",
           paste(missing_meta, collapse = ", "), call. = FALSE)
    }

    form_fields <- data_dict$field_name[data_dict$form_name == current_form]

    # Checkbox expansions (field___1, field___2, ...)
    checkbox_fields <- character()
    for (field in form_fields) {
      checkbox_fields <- c(checkbox_fields,
                           grep(paste0("^", field, "___"), names(df), value = TRUE))
    }

    existing_fields <- intersect(c(metadata_fields, form_fields, checkbox_fields), names(df))

    if (length(existing_fields) <= length(metadata_fields)) next

    form_data <- df[, existing_fields, drop = FALSE]

    # Same row rule as load_data_by_forms(): keep rows where at least one of
    # this instrument's own fields has data.
    form_specific_fields <- setdiff(existing_fields, metadata_fields)
    if (length(form_specific_fields) > 0) {
      has_data <- Reduce(`|`, lapply(form_specific_fields, function(col) {
        !is.na(form_data[[col]]) & form_data[[col]] != ""
      }))
      form_data <- form_data[has_data, , drop = FALSE]
    }

    clean_form_name <- tolower(gsub("[^a-zA-Z0-9_]", "_", current_form))
    clean_form_name <- gsub("_+", "_", clean_form_name)
    clean_form_name <- gsub("^_|_$", "", clean_form_name)

    rownames(form_data) <- NULL
    forms[[clean_form_name]] <- form_data
  }

  if (verbose) {
    message(sprintf("load_data_by_forms_fast: %d instruments in %.1f s",
                    length(forms), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  list(
    raw_data      = NULL,
    data_dict     = data_dict,
    forms         = forms,
    resident_data = forms$resident_data,
    metadata      = list(
      total_records   = sum(vapply(forms, nrow, integer(1))),
      residents       = if (!is.null(forms$resident_data)) nrow(forms$resident_data) else 0L,
      forms_with_data = length(forms),
      all_form_names  = form_names,
      loaded_at       = Sys.time()
    )
  )
}
