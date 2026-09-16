# =============================================================================
# Pipe operator import.
#
# Several gmed functions (e.g. calculate_resident_level() in
# period_mapping.R) use %>% internally without an explicit import — they've
# only "worked" because every known caller happens to already have library
# (dplyr) attached before calling into gmed, which puts %>% on the search
# path. A caller that doesn't do that (amiontools, found 2026-08-14) hits
# "could not find function '%>%'". Importing it here (re-exported from
# dplyr, already a gmed dependency — no new package needed) fixes this for
# every internal use, not just the one that happened to get noticed first.
# =============================================================================

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
