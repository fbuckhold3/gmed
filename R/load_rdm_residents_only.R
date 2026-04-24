#' Fast residents-only REDCap fetch
#'
#' Fetches only the `resident_data` form from REDCap (~4 s vs ~30 s for full
#' load).  Intended for Phase 1 of a two-phase startup: show the login form
#' quickly, then load full data in the background.
#'
#' @param rdm_token Character. REDCap API token.
#' @param redcap_url Character. REDCap API URL.
#' @param raw_or_label Character. "raw" (default) or "label".
#'
#' @return Data frame of active residents with a `Level` column, or NULL on
#'   error.
#' @export
load_rdm_residents_only <- function(rdm_token = NULL,
                                    redcap_url = "https://redcapsurvey.slu.edu/api/",
                                    raw_or_label = "raw") {

  if (is.null(rdm_token) || !nzchar(rdm_token)) {
    rdm_token <- Sys.getenv("RDM_TOKEN", unset = "")
  }
  if (!nzchar(rdm_token)) {
    message("load_rdm_residents_only: RDM_TOKEN not set:skipping Phase 1")
    return(NULL)
  }

  # Build URL-encoded body manually so we can pass `forms[0]=resident_data`.
  # httr's encode="form" doesn't support array parameters.
  body <- paste0(
    "token=",              utils::URLencode(rdm_token,     reserved = TRUE),
    "&content=record",
    "&action=export",
    "&format=json",
    "&type=flat",
    "&rawOrLabel=",        utils::URLencode(raw_or_label,  reserved = TRUE),
    "&rawOrLabelHeaders=raw",
    "&exportCheckboxLabel=false",
    "&exportSurveyFields=false",
    "&exportDataAccessGroups=false",
    "&returnFormat=json",
    "&forms%5B0%5D=resident_data"
  )

  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  resp <- tryCatch(
    httr::POST(
      url  = redcap_url,
      body = body,
      encode = "raw",
      httr::content_type("application/x-www-form-urlencoded"),
      httr::timeout(60)
    ),
    error = function(e) {
      message("load_rdm_residents_only: API error:", e$message)
      NULL
    }
  )

  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("load_rdm_residents_only: non-200 response:falling back to full load")
    return(NULL)
  }

  json_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  dat <- tryCatch({
    df <- jsonlite::fromJSON(json_text, flatten = TRUE)
    as.data.frame(df, stringsAsFactors = FALSE)
  }, error = function(e) {
    message("load_rdm_residents_only: JSON parse error:", e$message)
    NULL
  })

  if (is.null(dat) || nrow(dat) == 0) return(NULL)

  dat[] <- lapply(dat, as.character)

  # Filter archived
  if ("res_archive" %in% names(dat)) {
    dat <- dat[is.na(dat$res_archive) | !(dat$res_archive %in% c("Yes", "1")), ]
  }

  # Add name column
  if (!"name" %in% names(dat)) {
    name_col <- intersect(c("Name", "resident_name", "full_name", "first_name"),
                          names(dat))[1]
    dat$name <- if (!is.na(name_col)) dat[[name_col]] else paste("Resident", dat$record_id)
  }

  # Calculate Level
  if (all(c("type", "grad_yr") %in% names(dat))) {
    current_year  <- as.numeric(format(Sys.Date(), "%Y"))
    academic_year <- ifelse(as.numeric(format(Sys.Date(), "%m")) >= 7,
                            current_year, current_year - 1)
    # REDCap data dictionary: type choices are "1, Preliminary | 2, Categorical
    # | 3, Dismissed". When the caller uses raw_or_label="raw" (as imslu.ind.dash
    # does for speed), type arrives as "1"/"2"/"3" rather than the label. Accept
    # both forms so Level is computed correctly regardless of caller setting.
    type_is <- function(v, label) {
      if (is.na(v)) return(FALSE)
      v <- as.character(v)
      switch(label,
        Preliminary = v == "Preliminary" || v == "1",
        Categorical = v == "Categorical" || v == "2",
        Dismissed   = v == "Dismissed"   || v == "3",
        FALSE)
    }
    # grad_yr may similarly arrive as a raw code when raw_or_label="raw".
    # Decode values < 2000 via the "grad_yr" dropdown choices cached by Phase 1
    # callers. If no dict is available here, fall back to the raw numeric and
    # hope it's already a year. Treating anything in [14, 36] as 1986+code
    # matches the convention used in period_mapping.R.
    decode_grad_yr <- function(gy) {
      g <- suppressWarnings(as.numeric(gy))
      if (is.na(g)) return(NA_real_)
      if (g >= 2000) g
      else if (g >= 14 && g <= 36) g + 1986
      else NA_real_
    }
    dat$Level <- mapply(function(type, grad_yr) {
      if (is.na(type)) return("Unknown")
      if (type_is(type, "Preliminary")) return("Intern")
      if (type_is(type, "Dismissed"))   return("Dismissed")
      if (type_is(type, "Categorical")) {
        gy_actual <- decode_grad_yr(grad_yr)
        if (is.na(gy_actual)) return("Unknown")
        yrs <- gy_actual - academic_year
        if      (yrs == 3) "Intern"
        else if (yrs == 2) "PGY2"
        else if (yrs == 1) "PGY3"
        else if (yrs == 0) "Graduating"
        else if (yrs <  0) "Graduated"
        else "Unknown"
      } else {
        "Unknown"
      }
    }, dat$type, dat$grad_yr, USE.NAMES = FALSE)
  } else {
    dat$Level <- "Unknown"
  }

  dat
}
