
# gmed

<!-- badges: start -->
<!-- badges: end -->

The goal of gmed is to ...

## Installation

You can install the development version of gmed from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("fbuckhold3/gmed")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gmed)
## basic example code
```


## Milestone growth module

One shared milestone chart module for the coach dashboard, `imslu.ind.dash`
and `imslu.ccc.dashboard`. It replaces ind.dash's
`.milestone_prog_plot_combined()`, the CCC dashboard's inline
`output$ccc_milestone_plot` and gmed's `create_enhanced_milestone_progression()`
(now marked superseded but kept for existing callers).

It has three views, and ILP goals are marked in all of them:

1. **Overview heat table.** Rows are the 21 subcompetencies grouped by
   competency and columns are periods. Each cell is the resident's rating
   minus the cohort median, and ▼ marks a rating below the cohort 10th
   percentile. Clicking a row opens it in view 3.
2. **Self vs faculty dumbbell.** Shows one period. The faculty rating is the
   CCC rating, else the coach rating, and ACGME is drawn as a third mark.
   Rows are sorted by the size of the self vs faculty gap.
3. **Trajectory.** Shows the cohort 10th–90th percentile band, the program
   expectation step line (3 / 5 / 5 / 7 / 7 / 7 for periods 1–6) and the
   graduation line at 7. The resident's points use marker shape to show the
   source. It also draws the projection with an 80% prediction interval and
   gives P(reach 7 by graduation) plus the empirical check, which is
   suppressed when n < 10.

**Which rating counts:** ACGME if present, else CCC, else coach. The
`source` column records which one was used.

**Where CCC ratings are stored:** `ccc_review` does not store milestone
ratings. It stores only `ccc_mile` ("Any changes to milestones?") and
`ccc_mile_notes`. When the CCC changes ratings, the CCC dashboard writes them
back into the `milestone_entry` instance for that period, over the coach's
values. `build_milestone_long()` labels a `milestone_entry` row as `ccc` when
that resident's `ccc_review` for the same period has `ccc_mile = 1`, and as
`coach` otherwise. The coach's original rating is not kept anywhere once the
CCC revises it.

### Fitting and caching (data-refresh job only)

```r
# rdm-data-refresh: needs graduated/archived residents, raw codes
rdm  <- load_data_by_forms(filter_archived = FALSE, calculate_levels = FALSE,
                           raw_or_label = "raw")
refresh_milestone_growth_cache(rdm$forms, rdm$resident_data)
```

This fits the quantile-regression bands, one lme4 growth model per
subcompetency and the empirical table, then writes them (gzip+base64, about
12 KB) to the `app_cache` record. Before it can run, **add two fields to the
`app_cache` instrument**: `cache_milestone_growth_json` (Notes Box) and
`cache_milestone_growth_updated_at` (Text). Apps only read the cache. They
compute each resident's projection from the cached coefficients, so nothing
is fitted per session. `lme4` and `quantreg` are only used when fitting.

### Calling it from each app

All three apps share the same setup at startup:

```r
milestone_long <- build_milestone_long(all_forms, residents)  # once, cheap
milestone_fit  <- load_cached_milestone_growth()             # NULL if not cached yet
```

**Coach dashboard.** Use all three views for the coachee and the review
period:

```r
mod_milestone_growth_ui("growth")
mod_milestone_growth_server("growth",
  milestone_data = milestone_long,
  resident_id    = reactive(selected_resident()$record_id),
  period         = reactive(review_period()),       # 1-6 or label
  ilp_data       = reactive(app_data()$all_forms$ilp),
  fit            = milestone_fit)
```

The coach app still uses `create_milestone_spider_plot_final()` and
`create_milestone_overview_dashboard()`, and both are unchanged.

**imslu.ind.dash** (`mod_milestones.R`). Replace the combined progression
card with the module. The spider plots can stay:

```r
mod_milestone_growth_ui(ns("growth"), show = c("dumbbell", "trajectory"))
mod_milestone_growth_server("growth",
  milestone_data = reactive(rdm_data()$milestone_long),  # built in global.R
  resident_id    = resident_id,
  period         = reactive(NULL),                       # latest with data
  ilp_data       = reactive(rdm_data()$all_forms$ilp),
  fit            = reactive(rdm_data()$milestone_fit))
```

**imslu.ccc.dashboard** (`server.R`). Put the module in
`output$ccc_mile_section` in place of `ccc_mile_selector` and
`ccc_milestone_plot`:

```r
output$ccc_mile_section <- renderUI({
  req(identical(active_toggle(), "mile"), selected_resident_id())
  mod_milestone_growth_ui("ccc_growth", show = c("overview", "trajectory"))
})
mod_milestone_growth_server("ccc_growth",
  milestone_data = reactive(build_milestone_long(app_data()$all_forms, app_data()$residents)),
  resident_id    = selected_resident_id,
  period         = reactive(app_data()$residents$current_period[
                     app_data()$residents$record_id == selected_resident_id()]),
  ilp_data       = reactive(app_data()$all_forms$ilp),
  fit            = milestone_fit)
```

In the CCC dashboard, rebuild `milestone_long` after a CCC submit refreshes
`milestone_entry`, so the new ratings show up straight away.

### Example, tests, backtest

- Example app: `shiny::runApp(system.file("examples/milestone_growth", package = "gmed"))`.
  It uses synthetic data by default. To use the RDM test project, set
  `MG_USE_REDCAP=1` and `RDM_TOKEN_TEST`.
- Tests: `devtools::test(filter = "milestone-growth")`. They run on synthetic
  data only.
- Backtest: `Rscript inst/scripts/run_milestone_backtest.R synthetic|test|prod`
  writes `docs/milestone_growth_backtest*.md`. The committed report comes from
  **synthetic data**. Run `test` first, and `prod` only after that.
