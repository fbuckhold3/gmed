# Milestone growth model: backtest

**Data:** **Synthetic data** from `simulate_milestone_cohort(n_per_class = 25, grad_years = 2019:2029, seed = 2026)`, not real residents. These numbers only show the pipeline works; rerun with `test`, then `prod`, for real results.  
**Run:** 2026-10-01 09:30  
**Test classes:** 2024, 2025 (training: earlier graduated classes only)  
**Data from:** July 2021 on  
**Model:** `rating ~ t + t^2 + source + (1 + t | resident)` per subcompetency (lme4, REML), predicted for the ACGME source at graduation. 80% prediction intervals.

Each graduated resident's graduation rating (the rating that counts: ACGME, else CCC,
else coach) is predicted from their first *k* periods only. The baseline gives every
resident the training classes' median graduation rating and their share reaching 7.

## Summary

| First k periods | Predictions | Residents | MAE | MAE (baseline) | RMSE | PI coverage | Brier P(>=7) | Brier (baseline) | Observed reach 7 | Mean predicted P(>=7) |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 1050 | 50 | 0.73 | 0.79 | 0.90 | 81% | 0.08 | 0.09 | 91% | 86% |
| 3 | 1050 | 50 | 0.70 | 0.79 | 0.87 | 82% | 0.08 | 0.09 | 91% | 86% |

PI coverage = share of observed graduation ratings inside the prediction interval.

## Calibration of P(rating >= 7 at graduation), by decile of prediction

**From the first 2 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 106 | 52% | 70% |
| 2 | 105 | 71% | 82% |
| 3 | 106 | 80% | 83% |
| 4 | 103 | 85% | 89% |
| 5 | 106 | 89% | 92% |
| 6 | 104 | 92% | 96% |
| 7 | 105 | 95% | 97% |
| 8 | 105 | 97% | 98% |
| 9 | 105 | 99% | 100% |
| 10 | 105 | 100% | 98% |

**From the first 3 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 105 | 47% | 68% |
| 2 | 105 | 69% | 77% |
| 3 | 105 | 80% | 86% |
| 4 | 105 | 86% | 90% |
| 5 | 105 | 91% | 98% |
| 6 | 105 | 94% | 92% |
| 7 | 105 | 96% | 97% |
| 8 | 105 | 98% | 99% |
| 9 | 105 | 99% | 99% |
| 10 | 105 | 100% | 99% |

## MAE by subcompetency (first 3 periods)

| Subcompetency | MAE | MAE (baseline) |
|---|---|---|
| PC1 | 0.57 | 0.66 |
| PC2 | 0.84 | 0.88 |
| PC3 | 0.57 | 0.64 |
| PC4 | 0.75 | 0.79 |
| PC5 | 0.89 | 1.00 |
| PC6 | 0.80 | 0.88 |
| MK1 | 0.75 | 0.84 |
| MK2 | 0.78 | 0.96 |
| MK3 | 0.75 | 0.62 |
| SBP1 | 0.70 | 0.86 |
| SBP2 | 0.65 | 0.78 |
| SBP3 | 0.74 | 1.06 |
| PBL1 | 0.55 | 0.72 |
| PBL2 | 0.74 | 0.82 |
| PROF1 | 0.82 | 0.72 |
| PROF2 | 0.61 | 0.64 |
| PROF3 | 0.65 | 0.76 |
| PROF4 | 0.67 | 0.82 |
| ICS1 | 0.58 | 0.64 |
| ICS2 | 0.69 | 0.70 |
| ICS3 | 0.64 | 0.70 |

