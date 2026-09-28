# Run the milestone growth backtest and write the markdown report.
#
#   Rscript inst/scripts/run_milestone_backtest.R synthetic   # no REDCap
#   Rscript inst/scripts/run_milestone_backtest.R test        # RDM_TOKEN_TEST
#   Rscript inst/scripts/run_milestone_backtest.R prod        # RDM_TOKEN, only after test
#
# Real runs need archived/graduated residents included in the load, since the
# graduated classes are the whole point.
suppressMessages(if (file.exists("DESCRIPTION")) devtools::load_all(quiet = TRUE) else library(gmed))

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode)) mode <- "synthetic"

if (mode == "synthetic") {
  sim <- simulate_milestone_cohort(n_per_class = 25, grad_years = 2019:2029, seed = 2026)
  all_forms <- sim$all_forms; residents <- sim$residents
  note <- paste("**Synthetic data** from `simulate_milestone_cohort(n_per_class = 25,",
                "grad_years = 2019:2029, seed = 2026)`, not real residents. These numbers only",
                "show the pipeline works; rerun with `test`, then `prod`, for real results.")
  out <- "docs/milestone_growth_backtest.md"
} else {
  token <- Sys.getenv(if (mode == "test") "RDM_TOKEN_TEST" else "RDM_TOKEN")
  if (!nzchar(token)) stop("Token for mode '", mode, "' is not set")
  # Unfiltered load (archived = graduated residents kept), raw codes
  rdm <- load_data_by_forms(rdm_token = token, filter_archived = FALSE,
                            calculate_levels = FALSE, raw_or_label = "raw")
  all_forms <- rdm$forms; residents <- rdm$resident_data
  note <- if (mode == "test") "RDM **test** project (RDM_TOKEN_TEST)" else "RDM production project"
  out <- sprintf("docs/milestone_growth_backtest_%s.md", mode)
}

long <- build_milestone_long(all_forms, residents)
bt <- backtest_milestone_growth(long, first_periods = c(2, 3))
write_milestone_backtest_report(bt, out, note)
print(bt$summary)
message("Report written to ", out)
