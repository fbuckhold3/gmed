#' Load Faculty Roster from REDCap
#'
#' Fetches the faculty roster from a REDCap project using the provided API token.
#' Returns only active records that have a non-empty faculty name.
#'
#' @param fac_token Character. REDCap API token for the faculty project.
#'   If NULL or empty, returns NULL silently.
#' @param redcap_url Character. REDCap API endpoint URL.
#'   Default: \code{"https://redcapsurvey.slu.edu/api/"}.
#' @return Data frame of active faculty records, or NULL on failure / missing token.
#' @export
load_faculty_roster <- function(fac_token = NULL,
                                redcap_url = "https://redcapsurvey.slu.edu/api/") {
  if (is.null(fac_token)) fac_token <- Sys.getenv("FAC_TOKEN", unset = "")

  if (!nzchar(fac_token)) {
    message("FAC_TOKEN not set \u2014 faculty roster unavailable (set FAC_TOKEN in .Renviron)")
    return(NULL)
  }

  tryCatch({
    resp <- httr::POST(
      redcap_url,
      body = list(
        token                  = fac_token,
        content                = "record",
        format                 = "json",
        type                   = "flat",
        rawOrLabel             = "raw",
        rawOrLabelHeaders      = "raw",
        exportCheckboxLabel    = "false",
        exportSurveyFields     = "false",
        exportDataAccessGroups = "false",
        returnFormat           = "json"
      ),
      encode = "form"
    )
    if (httr::status_code(resp) != 200)
      stop("Faculty API status: ", httr::status_code(resp))

    dat <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    dat <- dat[!is.na(dat$fac_name) & nzchar(dat$fac_name), , drop = FALSE]
    if ("archived" %in% names(dat))
      dat <- dat[is.na(dat$archived) | dat$archived == "0", , drop = FALSE]

    message("Faculty roster loaded \u2014 ", nrow(dat), " active records")
    dat
  }, error = function(e) {
    message("Faculty roster load failed: ", e$message)
    NULL
  })
}
