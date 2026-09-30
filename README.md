
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

It has four views. None of them builds in a per-year target: the context
is always where past residents at the same period were rated (the "usual
range", i.e. the middle 80%), with plain-language guidance.

1. **Milestone profile** (`spider`). The same radar chart the apps already
   show (`create_enhanced_milestone_spider_plot()`, unchanged look): the
   resident's ratings for the chosen period against the cohort median for
   that period. A switch picks Faculty (the rating that counts), ACGME or
   Self; each is compared with the median of the same kind of rating.
2. **Range snapshot** (`overview`). Shows one period, with one row per
   subcompetency grouped by competency. Grey bars show the cohort range
   (light = 10th–90th percentile, dark = 25th–75th, tick = median), with the
   resident's rating on top; marker shape and colour show its source. A
   guidance line under the chart reads, for example, "At Mid PGY2, 18 of 21
   rated subcompetencies are within the usual range … Above the usual range:
   PC6, ICS1, ICS3." Clicking a row opens it in view 4.
3. **Self vs faculty dumbbell.** Shows the same period. The faculty rating is
   the CCC rating, else the coach rating, and ACGME is drawn as a third mark.
   Rows are sorted by the size of the self vs faculty gap.
4. **Trajectory.** Shows the cohort range across periods (same two bands plus
   the median) and the resident's points. It also draws the projection with
   an 80% prediction interval. The readout gives range guidance for the
   latest rating, the projected graduation rating, P(reach 7 by graduation)
   and the empirical check (suppressed when n < 10). The graduation line at
   7 is on by default; `show_target = FALSE` turns it off.

**ILP goals are a separate module**, `mod_ilp_goal_progress_ui/server()`,
so each app can place it on its own. It lists every goal the resident has
set, newest first, with columns for:
- the target (goal level → rating: 1→1, 2→3, 3→5, 4→7, 5→9)
- the rating when the goal was set
- the next rating after that
- a status: Reached, Not yet, or Awaiting next rating

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

**Coach dashboard.** Use all four views for the coachee and the review
period, with goals on their own:

```r
mod_milestone_growth_ui("growth")
mod_ilp_goal_progress_ui("goals")

mod_milestone_growth_server("growth",
  milestone_data = milestone_long,
  resident_id    = reactive(selected_resident()$record_id),
  period         = reactive(review_period()),       # 1-6 or label
  fit            = milestone_fit)
mod_ilp_goal_progress_server("goals",
  ilp_data       = reactive(app_data()$all_forms$ilp),
  milestone_data = milestone_long,
  resident_id    = reactive(selected_resident()$record_id))
```

The coach app still uses `create_milestone_spider_plot_final()` and
`create_milestone_overview_dashboard()`, and both are unchanged.

**imslu.ind.dash** (`mod_milestones.R`). Replace the combined progression
card with the module. The spider plots can stay:

```r
mod_milestone_growth_ui(ns("growth"), show = c("spider", "dumbbell", "trajectory"))
mod_milestone_growth_server("growth",
  milestone_data = reactive(rdm_data()$milestone_long),  # built in global.R
  resident_id    = resident_id,
  period         = reactive(NULL),                       # latest with data
  fit            = reactive(rdm_data()$milestone_fit))
```

ind.dash already shows ILP goals in its own learning tab, so it can add
`mod_ilp_goal_progress_*` there instead of next to the charts.

**imslu.ccc.dashboard** (`server.R`). Put the module in
`output$ccc_mile_section` in place of `ccc_mile_selector` and
`ccc_milestone_plot`:

```r
output$ccc_mile_section <- renderUI({
  req(identical(active_toggle(), "mile"), selected_resident_id())
  mod_milestone_growth_ui("ccc_growth", show = c("spider", "overview", "trajectory"))
})
mod_milestone_growth_server("ccc_growth",
  milestone_data = reactive(build_milestone_long(app_data()$all_forms, app_data()$residents)),
  resident_id    = selected_resident_id,
  period         = reactive(app_data()$residents$current_period[
                     app_data()$residents$record_id == selected_resident_id()]),
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
