#' Today's date in the program's time zone
#'
#' Posit Connect runs on UTC, so `Sys.Date()` there rolls over to tomorrow
#' at 6 pm Central (7 pm during daylight time). Use this for any date a
#' resident or faculty member would read as "today", such as the date
#' stamped on a saved evaluation.
#'
#' @param tz Time zone name. Defaults to the `gmed.tz` option, else
#'   "America/Chicago".
#' @return A `Date`.
#' @examples
#' program_today()
#' @export
program_today <- function(tz = getOption("gmed.tz", "America/Chicago")) {
  as.Date(format(Sys.time(), "%Y-%m-%d", tz = tz))
}
