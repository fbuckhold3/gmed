# Milestone growth model: backtest

**Data:** **Synthetic data** from `simulate_milestone_cohort(n_per_class = 25, grad_years = 2019:2029, seed = 2026)`, not real residents. These numbers only show the pipeline works; rerun with `test`, then `prod`, for real results.  
**Run:** 2026-09-28 18:13  
**Test classes:** 2021, 2022, 2023, 2024, 2025 (training: earlier graduated classes only)  
**Model:** `rating ~ t + t^2 + source + (1 + t | resident)` per subcompetency (lme4, REML), predicted for the ACGME source at graduation. 80% prediction intervals.

Each graduated resident's graduation rating (the rating that counts: ACGME, else CCC,
else coach) is predicted from their first *k* periods only. The baseline gives every
resident the training classes' median graduation rating and their share reaching 7.

## Summary

| First k periods | Predictions | Residents | MAE | MAE (baseline) | RMSE | PI coverage | Brier P(>=7) | Brier (baseline) | Observed reach 7 | Mean predicted P(>=7) |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 2625 | 125 | 0.73 | 0.82 | 0.90 | 84% | 0.10 | 0.11 | 88% | 84% |
| 3 | 2625 | 125 | 0.70 | 0.82 | 0.87 | 83% | 0.09 | 0.11 | 88% | 84% |

PI coverage = share of observed graduation ratings inside the prediction interval.

## Calibration of P(rating >= 7 at graduation), by decile of prediction

**From the first 2 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 264 | 53% | 62% |
| 2 | 262 | 70% | 78% |
| 3 | 262 | 77% | 82% |
| 4 | 262 | 83% | 87% |
| 5 | 263 | 87% | 86% |
| 6 | 264 | 90% | 94% |
| 7 | 260 | 93% | 96% |
| 8 | 263 | 95% | 96% |
| 9 | 262 | 97% | 97% |
| 10 | 263 | 99% | 99% |

**From the first 3 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 263 | 46% | 55% |
| 2 | 262 | 67% | 78% |
| 3 | 263 | 77% | 82% |
| 4 | 262 | 83% | 90% |
| 5 | 263 | 88% | 89% |
| 6 | 262 | 92% | 93% |
| 7 | 262 | 94% | 94% |
| 8 | 263 | 97% | 97% |
| 9 | 262 | 98% | 98% |
| 10 | 263 | 99% | 100% |

## MAE by subcompetency (first 3 periods)

| Subcompetency | MAE | MAE (baseline) |
|---|---|---|
| PC1 | 0.63 | 0.70 |
| PC2 | 0.76 | 0.85 |
| PC3 | 0.62 | 0.74 |
| PC4 | 0.65 | 0.75 |
| PC5 | 0.81 | 1.00 |
| PC6 | 0.79 | 0.90 |
| MK1 | 0.62 | 0.78 |
| MK2 | 0.75 | 0.92 |
| MK3 | 0.70 | 0.80 |
| SBP1 | 0.68 | 0.82 |
| SBP2 | 0.62 | 0.80 |
| SBP3 | 0.69 | 0.93 |
| PBL1 | 0.69 | 0.82 |
| PBL2 | 0.76 | 0.86 |
| PROF1 | 0.80 | 0.85 |
| PROF2 | 0.62 | 0.73 |
| PROF3 | 0.68 | 0.77 |
| PROF4 | 0.74 | 0.90 |
| ICS1 | 0.65 | 0.80 |
| ICS2 | 0.68 | 0.70 |
| ICS3 | 0.68 | 0.82 |

