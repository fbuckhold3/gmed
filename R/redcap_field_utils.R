#' REDCap Field Utility Helpers
#'
#' Small utilities for working with REDCap field values and rendering
#' completion status. Promoted from imslu.ccc.dashboard so they are
#' shared across coach.dash, ind.dash, and the faculty dashboard.
#'
#' @name redcap_field_utils
NULL


#' Translate Checkbox Field Values to Labels
#'
#' Converts the set of "checked" REDCap checkbox columns (e.g.
#' \code{ccc_competency___1}, \code{ccc_competency___3}) into a
#' comma-separated string of human-readable labels using the data
#' dictionary.
#'
#' @param data_dict Data dictionary data frame.
#' @param field_name Base field name without the \code{___N} suffix
#'   (e.g. \code{"ccc_competency"}).
#' @param checked_cols Character vector of column names that are
#'   currently checked (value == "1") for this record.
#' @param sep Separator between labels. Default \code{", "}.
#'
#' @return Character. Comma-separated labels, or empty string when no
#'   choices match.
#' @export
translate_checkbox_values <- function(data_dict, field_name, checked_cols, sep = ", ") {
  choices <- tryCatch(
    get_field_choices(data_dict, field_name, return_type = "named_vector"),
    error = function(e) NULL
  )
  if (is.null(choices) || length(choices) == 0 || length(checked_cols) == 0) {
    return("")
  }

  # get_field_choices returns labels-as-names, codes-as-values; flip so we
  # can look up labels by code.
  label_for_code <- setNames(names(choices), unname(choices))

  codes  <- gsub(paste0("^", field_name, "___"), "", checked_cols)
  labels <- label_for_code[codes]
  labels <- labels[!is.na(labels) & nzchar(labels)]

  if (length(labels) == 0) "" else paste(labels, collapse = sep)
}


#' Completion Status Indicator
#'
#' Renders a small green check or red bullet for use in dashboards.
#'
#' @param is_complete Logical scalar.
#' @param size CSS font-size for the icon. Default \code{"1.2em"}.
#' @return A \code{shiny::tags$span} HTML tag.
#' @export
completion_icon <- function(is_complete, size = "1.2em") {
  if (isTRUE(is_complete)) {
    shiny::tags$span(
      style = paste0("color: #28a745; font-size: ", size, ";"),
      title = "Completed",
      "✓"
    )
  } else {
    shiny::tags$span(
      style = paste0("color: #dc3545; font-size: ", size, ";"),
      title = "Not completed",
      "●"
    )
  }
}


#' Clean a Raw REDCap String
#'
#' Trims whitespace and converts \code{NA} or the literal string
#' \code{"NA"} to an empty string. Useful when concatenating optional
#' free-text fields for display.
#'
#' @param x A length-1 value coercible to character.
#' @return Character scalar.
#' @export
clean_redcap_string <- function(x) {
  s <- trimws(as.character(x))
  if (length(s) == 0 || is.na(s) || s == "NA") "" else s
}
