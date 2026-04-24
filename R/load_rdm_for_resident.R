#' Per-resident REDCap fetch
#'
#' Fetches all REDCap records for a single resident and splits them by form.
#' Used for Phase 3 (post-login) loading — much faster than pulling all
#' residents because it limits the API call to one record_id.
#'
#' Returns the same list structure as \code{load_rdm_complete} so modules
#' receive a compatible object.  \code{milestone_workflow} and
#' \code{historical_medians} are NULL/empty until Phase 2 finishes; the app
#' enriches the object server-side once Phase 2 data is available.
#'
#' @param rdm_token  Character. REDCap API token.
#' @param redcap_url Character. REDCap API URL.
#' @param record_id  Character or numeric. The resident's REDCap record_id.
#' @param raw_or_label Character. "raw" (default) or "label".
#'
#' @return List with keys: residents, assessment, all_forms, data_dict,
#'   milestone_workflow, historical_medians.  NULL on error.
#' @export
load_rdm_for_resident <- function(rdm_token   = NULL,
                                   redcap_url  = "https://redcapsurvey.slu.edu/api/",
                                   record_id,
                                   raw_or_label = "raw") {

  if (is.null(rdm_token) || !nzchar(rdm_token))
    rdm_token <- Sys.getenv("RDM_TOKEN", unset = "")
  if (!nzchar(rdm_token)) {
    message("load_rdm_for_resident: RDM_TOKEN not set")
    return(NULL)
  }
  record_id <- as.character(record_id)

  # ── 1. Data dictionary (small/fast; used to split forms) ──────────────────
  data_dict <- tryCatch(
    get_evaluation_dictionary(token = rdm_token, url = redcap_url),
    error = function(e) { message("load_rdm_for_resident: dict error: ", e$message); NULL }
  )
  if (is.null(data_dict)) return(NULL)

  # ── 2. All records for this resident ──────────────────────────────────────
  body <- paste0(
    "token=",    utils::URLencode(rdm_token,  reserved = TRUE),
    "&content=record&action=export&format=json&type=flat",
    "&rawOrLabel=",         utils::URLencode(raw_or_label, reserved = TRUE),
    "&rawOrLabelHeaders=raw",
    "&exportCheckboxLabel=false",
    "&exportSurveyFields=false",
    "&exportDataAccessGroups=false",
    "&returnFormat=json",
    "&records%5B0%5D=",     utils::URLencode(record_id,   reserved = TRUE)
  )

  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  resp <- tryCatch(
    httr::POST(redcap_url, body = body, encode = "raw",
               httr::content_type("application/x-www-form-urlencoded"),
               httr::timeout(60)),
    error = function(e) { message("load_rdm_for_resident: API error: ", e$message); NULL }
  )
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("load_rdm_for_resident: non-200 response for record ", record_id)
    return(NULL)
  }

  json_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  raw <- tryCatch({
    df <- jsonlite::fromJSON(json_text, flatten = TRUE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df[] <- lapply(df, as.character)
    df
  }, error = function(e) {
    message("load_rdm_for_resident: parse error: ", e$message)
    NULL
  })
  if (is.null(raw) || nrow(raw) == 0) {
    message("load_rdm_for_resident: no rows returned for record ", record_id)
    return(NULL)
  }

  # ── 3. Split raw data into per-form data frames ───────────────────────────
  meta_fields <- c("record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
                   "redcap_event_name", "redcap_survey_identifier")
  form_names  <- unique(data_dict$form_name)
  form_names  <- form_names[!is.na(form_names)]

  all_forms <- list()

  # Non-repeating row → resident_data form
  non_rep <- raw[is.na(raw$redcap_repeat_instrument) |
                   raw$redcap_repeat_instrument == "", , drop = FALSE]
  if (nrow(non_rep) > 0) all_forms[["resident_data"]] <- non_rep

  for (form in form_names) {
    if (form == "resident_data") next

    form_fields <- data_dict$field_name[data_dict$form_name == form]

    # Include checkbox sub-columns (field___code)
    cb_fields <- character()
    for (fld in form_fields) {
      pat <- paste0("^", fld, "___")
      cb_fields <- c(cb_fields, grep(pat, names(raw), value = TRUE))
    }

    wanted <- unique(c(meta_fields, form_fields, cb_fields))
    present <- intersect(wanted, names(raw))

    if (length(present) <= length(meta_fields)) next

    # Repeating rows for this form
    form_rows <- raw[!is.na(raw$redcap_repeat_instrument) &
                       raw$redcap_repeat_instrument == form, , drop = FALSE]
    if (nrow(form_rows) == 0) next

    all_forms[[form]] <- form_rows[, intersect(present, names(form_rows)), drop = FALSE]
  }

  # ── 4. Build residents row with name + Level ──────────────────────────────
  res_row <- all_forms[["resident_data"]]
  if (!is.null(res_row) && nrow(res_row) > 0) {
    if (!"name" %in% names(res_row)) {
      name_col <- intersect(c("Name", "resident_name", "full_name", "first_name"),
                            names(res_row))[1]
      res_row$name <- if (!is.na(name_col)) res_row[[name_col]] else paste("Resident", record_id)
    }

    if (all(c("type", "grad_yr") %in% names(res_row))) {
      cur_yr  <- as.numeric(format(Sys.Date(), "%Y"))
      acad_yr <- ifelse(as.numeric(format(Sys.Date(), "%m")) >= 7, cur_yr, cur_yr - 1)
      type_v  <- res_row$type[1]
      grad_v  <- res_row$grad_yr[1]
      res_row$Level <- if (is.na(type_v)) {
        "Unknown"
      } else if (type_v == "Preliminary") {
        "Intern"
      } else if (type_v == "Categorical" && !is.na(grad_v)) {
        yrs <- as.numeric(grad_v) - acad_yr
        if      (yrs == 3) "Intern"
        else if (yrs == 2) "PGY2"
        else if (yrs == 1) "PGY3"
        else if (yrs == 0) "Graduating"
        else if (yrs  < 0) "Graduated"
        else "Unknown"
      } else "Unknown"
    } else {
      res_row$Level <- "Unknown"
    }
  }

  # ── 5. Return compatible structure ────────────────────────────────────────
  list(
    residents          = res_row,
    assessment         = if (!is.null(all_forms[["assessment"]])) all_forms[["assessment"]] else data.frame(),
    all_forms          = all_forms,
    data_dict          = data_dict,
    # Cohort-level fields — populated externally once Phase 2 completes
    milestone_workflow  = NULL,
    historical_medians  = list(),
    data_loaded        = TRUE,
    load_timestamp     = Sys.time()
  )
}
