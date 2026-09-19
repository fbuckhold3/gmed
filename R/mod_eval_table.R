# ============================================================================
# EVALUATION TABLE MODULE
# Resident-facing table: one row per evaluation, dynamic item columns,
# click-to-expand written feedback (plus / delta).
# ============================================================================

# Metadata fields that are never shown as item columns
.eval_table_meta <- c(
  "record_id", "redcap_repeat_instrument", "redcap_repeat_instance",
  "ass_date", "ass_level", "ass_plus", "ass_delta",
  "ass_faculty", "ass_specialty", "ass_rotator", "ass_cc_quart",
  "ass_obs_type", "fac_eval_level", "q_level", "source_form",
  "ass_grade", "ass_levelnum"
)


# Internal choice parser (avoids dependency on export order)
parse_redcap_choices_internal <- function(choices_str) {
  if (is.null(choices_str) || is.na(choices_str) || !nzchar(trimws(choices_str)))
    return(character(0))
  parts <- trimws(strsplit(choices_str, "\\|")[[1]])
  result <- character(0)
  for (part in parts) {
    kv <- strsplit(part, ",\\s*", perl = TRUE)[[1]]
    if (length(kv) >= 2) {
      code  <- trimws(kv[1])
      label <- trimws(paste(kv[-1], collapse = ", "))
      result[code] <- label
    }
  }
  result
}
