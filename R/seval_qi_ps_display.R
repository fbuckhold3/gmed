# =============================================================================
# seval_qi_ps_display.R — QI / patient safety / committee progress
#
# Read-only summary of the self-evaluation's QI, patient-safety and committee
# answers for one resident, for coach and CCC views. A QI project or patient
# safety review reported in an EARLIER period counts as completed (residents
# aren't asked again once done), the same rule ind.dash's self-eval uses.
# =============================================================================

# Period order. s_e_period arrives as the raw code ("7", "1".."6") or, in apps
# that translate on load, as its label.
.QIPS_PERIOD_ORDER <- c("7" = 0L, "1" = 1L, "2" = 2L, "3" = 3L, "4" = 4L, "5" = 5L, "6" = 6L,
                        "entering residency" = 0L, "mid intern" = 1L, "end intern" = 2L,
                        "mid pgy2" = 3L, "end pgy2" = 4L, "mid pgy3" = 5L,
                        "graduating" = 6L, "graduation" = 6L)
.QIPS_PERIOD_LABEL <- c("Entering Residency", "Mid Intern", "End Intern", "Mid PGY2",
                        "End PGY2", "Mid PGY3", "Graduating")

.qips_rank <- function(p) {
  unname(.QIPS_PERIOD_ORDER[tolower(trimws(as.character(p)))])
}

.qips_val <- function(row, f) {
  if (is.null(row) || !f %in% names(row)) return("")
  v <- row[[f]][1]
  if (is.null(v) || is.na(v)) "" else trimws(as.character(v))
}

.qips_yes <- function(v) tolower(v) %in% c("1", "yes")
.qips_no  <- function(v) tolower(v) %in% c("0", "no")

#' QI, patient safety and committee progress
#'
#' Read-only display of a resident's self-evaluation answers on quality
#' improvement, patient safety and committee work for the period under
#' review. A QI project or patient-safety review reported in an earlier
#' period is shown as completed, with its description.
#'
#' @param seval Data frame of the resident's `s_eval` rows (all periods).
#'   `s_e_period` may be raw codes or labels.
#' @param cur_period The period under review, as a raw code (`"1"`..`"6"`,
#'   `"7"`) or label (`"Mid PGY3"`).
#' @param data_dict Optional REDCap data dictionary, used for committee names.
#' @return A `shiny::tagList`.
#' @export
seval_qi_ps_ui <- function(seval, cur_period, data_dict = NULL) {
  empty <- function(msg) shiny::div(class = "text-muted fst-italic", style = "font-size:0.9rem;", msg)
  if (is.null(seval) || !is.data.frame(seval) || nrow(seval) == 0 || !"s_e_period" %in% names(seval))
    return(empty("No self-evaluation on file."))

  ranks <- .qips_rank(seval$s_e_period)
  cur   <- .qips_rank(cur_period)
  if (is.na(cur)) return(empty("Period not recognised."))
  row <- seval[!is.na(ranks) & ranks == cur, , drop = FALSE]
  if (nrow(row) == 0) row <- NULL

  # Most recent earlier period where the resident said yes.
  prior <- function(did, desc) {
    if (!did %in% names(seval)) return(NULL)
    hit <- !is.na(ranks) & ranks < cur & .qips_yes(trimws(as.character(seval[[did]])))
    if (!any(hit)) return(NULL)
    r <- seval[hit, , drop = FALSE]; r <- r[order(-ranks[hit]), , drop = FALSE]
    d <- if (desc %in% names(r)) trimws(as.character(r[[desc]])) else character(0)
    d <- d[!is.na(d) & nzchar(d)]
    list(desc = if (length(d)) d[1] else "",
         period = .QIPS_PERIOD_LABEL[.qips_rank(r$s_e_period[1]) + 1L])
  }

  line <- function(title, did, desc) {
    now <- .qips_val(row, did); txt <- .qips_val(row, desc)
    pr  <- prior(did, desc)
    status <- if (.qips_yes(now)) {
      list(lbl = "Yes, this period", col = "var(--roundsui-success, #16A34A)", txt = txt)
    } else if (!is.null(pr)) {
      list(lbl = paste0("Completed (reported ", pr$period, ")"),
           col = "var(--roundsui-success, #16A34A)", txt = pr$desc)
    } else if (.qips_no(now)) {
      list(lbl = "Not yet", col = "var(--roundsui-warning, #C2760A)", txt = "")
    } else {
      list(lbl = "Not answered", col = "var(--roundsui-ink-faint, #8991A0)", txt = "")
    }
    shiny::div(
      class = "mb-2 pb-2",
      style = "border-bottom:1px solid var(--roundsui-border, #E2E5EC);",
      shiny::div(style = "display:flex; justify-content:space-between; gap:8px; flex-wrap:wrap;",
                 shiny::tags$strong(title),
                 shiny::tags$span(style = sprintf("font-weight:600; color:%s;", status$col), status$lbl)),
      if (nzchar(status$txt))
        shiny::div(style = "font-size:0.9rem; white-space:pre-wrap; margin-top:2px;", status$txt)
    )
  }

  # Committees: checkbox columns s_e_committee___N, named from the dictionary.
  comm_line <- function() {
    did <- .qips_val(row, "s_e_did_committee")
    cols <- grep("^s_e_committee___", names(row %||% list()), value = TRUE)
    picked <- cols[vapply(cols, function(cl) .qips_yes(.qips_val(row, cl)), logical(1))]
    codes <- sub("^s_e_committee___", "", picked)
    lbls <- codes
    if (length(codes) && is.data.frame(data_dict) && "field_name" %in% names(data_dict)) {
      ch <- data_dict$select_choices_or_calculations[data_dict$field_name == "s_e_committee"]
      if (length(ch) && !is.na(ch[1])) {
        parts <- trimws(strsplit(ch[1], "\\|")[[1]])
        map <- stats::setNames(trimws(sub("^[^,]*,", "", parts)), trimws(sub(",.*$", "", parts)))
        lbls <- ifelse(codes %in% names(map), map[codes], codes)
      }
    }
    oth <- .qips_val(row, "s_e_committee_other")
    if (nzchar(oth)) lbls <- c(setdiff(lbls, "Other"), oth)
    role <- .qips_val(row, "s_e_committee_role")
    status <- if (.qips_yes(did)) "Serving now" else if (.qips_no(did)) "Not currently" else "Not answered"
    shiny::div(
      class = "mb-1",
      shiny::div(style = "display:flex; justify-content:space-between; gap:8px; flex-wrap:wrap;",
                 shiny::tags$strong("Committee"),
                 shiny::tags$span(style = "font-weight:600; color:var(--roundsui-ink-muted, #5B6472);", status)),
      if (length(lbls)) shiny::div(style = "font-size:0.9rem;", paste(unname(lbls), collapse = ", ")),
      if (nzchar(role)) shiny::div(style = "font-size:0.9rem;", shiny::tags$span(class = "text-muted", "Role: "), role)
    )
  }

  shiny::tagList(
    line("Quality improvement project", "s_e_did_qi", "s_e_qi_description"),
    line("Patient safety review", "s_e_did_ps", "s_e_ps_description"),
    comm_line()
  )
}
