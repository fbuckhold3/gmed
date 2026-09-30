# Milestone growth model: backtest

**Data:** RDM **test** project (RDM_TOKEN_TEST)  
**Run:** 2026-09-30 14:14  
**Test classes:** 2022, 2023, 2024, 2025, 2026, 2027 (training: earlier graduated classes only)  
**Model:** `rating ~ t + t^2 + source + (1 + t | resident)` per subcompetency (lme4, REML), predicted for the ACGME source at graduation. 80% prediction intervals.

Each graduated resident's graduation rating (the rating that counts: ACGME, else CCC,
else coach) is predicted from their first *k* periods only. The baseline gives every
resident the training classes' median graduation rating and their share reaching 7.

## Summary

| First k periods | Predictions | Residents | MAE | MAE (baseline) | RMSE | PI coverage | Brier P(>=7) | Brier (baseline) | Observed reach 7 | Mean predicted P(>=7) |
|---|---|---|---|---|---|---|---|---|---|---|
| 2 | 1113 | 53 | 0.62 | 0.48 | 0.81 | 79% | 0.08 | 0.05 | 94% | 82% |
| 3 | 1701 | 81 | 0.55 | 0.46 | 0.72 | 83% | 0.06 | 0.05 | 95% | 86% |

PI coverage = share of observed graduation ratings inside the prediction interval.

## Calibration of P(rating >= 7 at graduation), by decile of prediction

**From the first 2 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 113 | 54% | 88% |
| 2 | 112 | 67% | 87% |
| 3 | 121 | 76% | 95% |
| 4 | 105 | 80% | 94% |
| 5 | 111 | 83% | 98% |
| 6 | 109 | 85% | 95% |
| 7 | 114 | 88% | 99% |
| 8 | 105 | 92% | 99% |
| 9 | 111 | 94% | 96% |
| 10 | 112 | 99% | 91% |

**From the first 3 periods**

| Decile | n | Mean predicted | Observed |
|---|---|---|---|
| 1 | 177 | 57% | 84% |
| 2 | 167 | 73% | 89% |
| 3 | 167 | 80% | 96% |
| 4 | 180 | 85% | 97% |
| 5 | 160 | 87% | 93% |
| 6 | 173 | 90% | 99% |
| 7 | 168 | 92% | 98% |
| 8 | 169 | 95% | 96% |
| 9 | 170 | 98% | 98% |
| 10 | 170 | 100% | 97% |

## MAE by subcompetency (first 3 periods)

| Subcompetency | MAE | MAE (baseline) |
|---|---|---|
| PC1 | 0.53 | 0.67 |
| PC2 | 0.53 | 0.64 |
| PC3 | 0.48 | 0.27 |
| PC4 | 0.59 | 0.37 |
| PC5 | 0.64 | 0.73 |
| PC6 | 0.42 | 0.17 |
| MK1 | 0.52 | 0.35 |
| MK2 | 0.54 | 0.31 |
| MK3 | 0.53 | 0.35 |
| SBP1 | 0.67 | 0.53 |
| SBP2 | 0.50 | 0.54 |
| SBP3 | 0.59 | 0.63 |
| PBL1 | 0.42 | 0.28 |
| PBL2 | 0.56 | 0.40 |
| PROF1 | 0.69 | 0.72 |
| PROF2 | 0.62 | 0.65 |
| PROF3 | 0.55 | 0.31 |
| PROF4 | 0.59 | 0.31 |
| ICS1 | 0.65 | 0.53 |
| ICS2 | 0.50 | 0.35 |
| ICS3 | 0.52 | 0.51 |

