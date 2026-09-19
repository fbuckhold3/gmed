# submit_overwrite_data()/submit_additive_data()/get_next_additive_instance()
# duplicated functions already defined in redcap_submission.R (2026-09-19) —
# this file's copies were silently shadowed at load time (R sources files
# alphabetically; "redcap_submission.R" < "submission_patterns.R"), so
# redcap_submission.R's versions were the only ones ever actually running.
# Kept redcap_submission.R as canonical (it also owns get_redcap_instance(),
# which these functions call, plus map_inputs_to_coach_rev() and
# check_record_exists(), neither of which existed here). See git history to
# restore this file's versions if ever needed.
