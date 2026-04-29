#' Coach-scoped REDCap fetch
#'
#' Two-step fetch optimized for the coach dashboard: pull the lightweight
#' residents-only roster, filter to residents assigned to a given coach
#' (either as primary \code{coach} or as \code{second_rev}), then pull all
#' records for that small set in a single batched API call.
#'
#' Faster than \code{load_rdm_complete()} because the heavy second call is
#' bounded to the coach's caseload (typically 6–10 residents), and faster
#' than calling \code{load_rdm_for_resident()} per resident because it
#' batches into one HTTP request.
#'
#' Returns the same list structure as \code{load_rdm_complete()} so existing
#' modules accept it unchanged. The \code{residents} data frame gains a
#' \code{review_role} column ("primary" or "second") indicating the caller's
#' role for each resident.
#'
#' @param coach_name Character. Coach name to filter on. Matched against
#'   both \code{coach} and \code{second_rev} columns of \code{resident_data}.
#' @param rdm_token  Character. REDCap API token. Defaults to env
#'   \code{RDM_TOKEN}.
#' @param redcap_url Character. REDCap API URL.
#' @param raw_or_label Character. "raw" (default) or "label". Used for both
#'   the residents pre-fetch and the per-resident batched fetch.
#' @param include_second Logical. If TRUE (default) include residents where
#'   the coach is the second reviewer.
#'
#' @return List with keys \code{residents}, \code{assessment},
#'   \code{all_forms}, \code{data_dict}, \code{milestone_workflow} (NULL
#'   until enriched externally), \code{historical_medians} (empty list),
#'   \code{data_loaded}, \code{load_timestamp}, \code{coach_name},
#'   \code{review_roles}.  Returns NULL on error.
#' @export
load_rdm_for_coach <- function(coach_name,
                               rdm_token      = NULL,
                               redcap_url     = "https://redcapsurvey.slu.edu/api/",
                               raw_or_label   = "raw",
                               include_second = TRUE) {

  if (missing(coach_name) || is.null(coach_name) || !nzchar(coach_name)) {
    message("load_rdm_for_coach: coach_name required")
    return(NULL)
  }

  if (is.null(rdm_token) || !nzchar(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN", unset = "")
  }
  if (!nzchar(rdm_token)) {
    message("load_rdm_for_coach: RDM_TOKEN not set")
    return(NULL)
  }

  # ── 1. Roster (Phase 1) ───────────────────────────────────────────────────
  # Always fetch with raw_or_label="label" so the coach/second_rev columns
  # come back as readable names that match coach_name. The downstream batched
  # fetch honors the caller's raw_or_label preference.
  residents <- load_rdm_residents_only(
    rdm_token    = rdm_token,
    redcap_url   = redcap_url,
    raw_or_label = "label"
  )
  if (is.null(residents) || nrow(residents) == 0) {
    message("load_rdm_for_coach: residents-only fetch failed")
    return(NULL)
  }

  # ── 2. Filter to coach's caseload ─────────────────────────────────────────
  has_coach  <- "coach"      %in% names(residents)
  has_second <- "second_rev" %in% names(residents)
  if (!has_coach && !has_second) {
    message("load_rdm_for_coach: residents form has neither 'coach' nor 'second_rev' columns")
    return(NULL)
  }

  is_primary <- if (has_coach) {
    !is.na(residents$coach) & residents$coach == coach_name
  } else rep(FALSE, nrow(residents))

  is_second <- if (include_second && has_second) {
    !is.na(residents$second_rev) & residents$second_rev == coach_name
  } else rep(FALSE, nrow(residents))

  keep <- is_primary | is_second
  if (!any(keep)) {
    message("load_rdm_for_coach: no residents found for coach '", coach_name, "'")
    return(list(
      residents          = residents[FALSE, , drop = FALSE],
      assessment         = data.frame(),
      all_forms          = list(),
      data_dict          = NULL,
      milestone_workflow = NULL,
      historical_medians = list(),
      data_loaded        = TRUE,
      load_timestamp     = Sys.time(),
      coach_name         = coach_name,
      review_roles       = setNames(character(0), character(0))
    ))
  }

  scoped <- residents[keep, , drop = FALSE]
  scoped$review_role <- ifelse(is_primary[keep], "primary", "second")
  record_ids <- as.character(scoped$record_id)

  # ── 3. Data dictionary (used to split forms) ──────────────────────────────
  data_dict <- tryCatch(
    get_evaluation_dictionary(token = rdm_token, url = redcap_url),
    error = function(e) {
      message("load_rdm_for_coach: dict error: ", e$message); NULL
    }
  )
  if (is.null(data_dict)) return(NULL)

  # ── 4. Batched fetch of all records for the caseload ──────────────────────
  records_params <- paste0(
    "&records%5B", seq_along(record_ids) - 1L, "%5D=",
    vapply(record_ids, utils::URLencode, character(1), reserved = TRUE),
    collapse = ""
  )

  body <- paste0(
    "token=",    utils::URLencode(rdm_token, reserved = TRUE),
    "&content=record&action=export&format=json&type=flat",
    "&rawOrLabel=",         utils::URLencode(raw_or_label, reserved = TRUE),
    "&rawOrLabelHeaders=raw",
    "&exportCheckboxLabel=false",
    "&exportSurveyFields=false",
    "&exportDataAccessGroups=false",
    "&returnFormat=json",
    records_params
  )

  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  resp <- tryCatch(
    httr::POST(redcap_url, body = body, encode = "raw",
               httr::content_type("application/x-www-form-urlencoded"),
               httr::timeout(120)),
    error = function(e) {
      message("load_rdm_for_coach: API error: ", e$message); NULL
    }
  )
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("load_rdm_for_coach: non-200 response for caseload of ",
            length(record_ids), " residents")
    return(NULL)
  }

  json_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  raw <- tryCatch({
    df <- jsonlite::fromJSON(json_text, flatten = TRUE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, as.character)
    df
  }, error = function(e) {
    message("load_rdm_for_coach: parse error: ", e$message); NULL
  })
  if (is.null(raw) || nrow(raw) == 0) {
    message("load_rdm_for_coach: no rows returned for caseload")
    return(NULL)
  }

  # ── 5. Split into per-form data frames ────────────────────────────────────
  meta_fields <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
                   "redcap_event_name", "redcap_survey_identifier")
  form_names <- unique(data_dict$form_name)
  form_names <- form_names[!is.na(form_names)]

  all_forms <- list()

  non_rep <- raw[is.na(raw$redcap_repeat_instrument) |
                   raw$redcap_repeat_instrument == "", , drop = FALSE]
  if (nrow(non_rep) > 0) all_forms[["resident_data"]] <- non_rep

  for (form in form_names) {
    if (form == "resident_data") next

    form_fields <- data_dict$field_name[data_dict$form_name == form]

    cb_fields <- character()
    for (fld in form_fields) {
      pat <- paste0("^", fld, "___")
      cb_fields <- c(cb_fields, grep(pat, names(raw), value = TRUE))
    }

    wanted  <- unique(c(meta_fields, form_fields, cb_fields))
    present <- intersect(wanted, names(raw))
    if (length(present) <= length(meta_fields)) next

    form_rows <- raw[!is.na(raw$redcap_repeat_instrument) &
                       raw$redcap_repeat_instrument == form, , drop = FALSE]
    if (nrow(form_rows) == 0) next

    all_forms[[form]] <- form_rows[, intersect(present, names(form_rows)), drop = FALSE]
  }

  # ── 6. Carry coach/second_rev + review_role onto residents form ───────────
  res_df <- if (!is.null(all_forms[["resident_data"]])) all_forms[["resident_data"]] else scoped
  if (!"name" %in% names(res_df)) {
    name_col <- intersect(c("Name", "resident_name", "full_name", "first_name"),
                          names(res_df))[1]
    res_df$name <- if (!is.na(name_col)) res_df[[name_col]] else paste("Resident", res_df$record_id)
  }
  role_lookup <- setNames(scoped$review_role, scoped$record_id)
  res_df$review_role <- unname(role_lookup[as.character(res_df$record_id)])

  # ── 7. Return compatible structure ────────────────────────────────────────
  list(
    residents          = res_df,
    assessment         = if (!is.null(all_forms[["assessment"]])) all_forms[["assessment"]] else data.frame(),
    all_forms          = all_forms,
    data_dict          = data_dict,
    milestone_workflow = NULL,
    historical_medians = list(),
    data_loaded        = TRUE,
    load_timestamp     = Sys.time(),
    coach_name         = coach_name,
    review_roles       = role_lookup
  )
}
