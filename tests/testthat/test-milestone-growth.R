# Tests for the shared milestone growth module (synthetic data only).

sim <- simulate_milestone_cohort(n_per_class = 12, seed = 11)
long <- build_milestone_long(sim$all_forms, sim$residents)
sel <- select_milestone_rating(long)

test_that("long data covers all four raters and the 21 subcompetencies", {
  expect_setequal(unique(long$rater), c("coach", "ccc", "self", "acgme"))
  expect_setequal(unique(long$subcomp), milestone_subcompetencies()$subcomp)
  expect_true(all(long$period %in% 1:6))
  expect_true(all(long$rating >= 1 & long$rating <= 9))
})

test_that("source fallback order is ACGME, then CCC, then coach", {
  d <- data.frame(
    record_id = "1", subcomp = "pc1",
    period = c(1, 1, 1, 2, 2, 3, 4),
    rater  = c("coach", "ccc", "acgme", "coach", "ccc", "coach", "self"),
    rating = c(3, 4, 5, 4, 6, 5, 9),
    stringsAsFactors = FALSE)
  s <- select_milestone_rating(d)
  expect_equal(s$source[s$period == 1], "acgme")
  expect_equal(s$rating[s$period == 1], 5)
  expect_equal(s$source[s$period == 2], "ccc")
  expect_equal(s$source[s$period == 3], "coach")
  expect_false(4 %in% s$period)            # self never counts
})

test_that("CCC-flagged milestone_entry rows are labelled ccc", {
  forms <- list(
    milestone_entry = data.frame(record_id = c("1", "1"), prog_mile_period = c("1", "2"),
                                 rep_pc1 = c(3, 5), stringsAsFactors = FALSE),
    ccc_review = data.frame(record_id = "1", ccc_session = "2", ccc_mile = "1",
                            stringsAsFactors = FALSE))
  l <- build_milestone_long(forms)
  expect_equal(l$rater[l$period == 1], "coach")
  expect_equal(l$rater[l$period == 2], "ccc")
})

test_that("native ACGME scale is converted to the internal 1-9 scale", {
  forms <- list(acgme_miles = data.frame(record_id = "1", acgme_mile_period = "6",
                                         acgme_pc1 = 4, stringsAsFactors = FALSE))
  expect_equal(build_milestone_long(forms, acgme_scale = "native")$rating, 7)
  expect_equal(build_milestone_long(forms)$rating, 4)
})

test_that("ILP goal level maps 1,2,3,4,5 to 1,3,5,7,9", {
  expect_equal(ilp_goal_level_to_rating(1:5), c(1, 3, 5, 7, 9))
  expect_equal(ilp_goal_level_to_rating(c("4", "2")), c(7, 3))
  expect_true(all(is.na(ilp_goal_level_to_rating(c(0, 6, NA, "x", 2.5)))))
})

test_that("ILP goal codes map to subcompetencies and current/previous goals are found", {
  expect_equal(ilp_goal_subcomp("pcmk", c(1, 7, 9)), c("pc1", "mk1", "mk3"))
  expect_equal(ilp_goal_subcomp("sbppbl", 4), "pbl1")
  expect_equal(ilp_goal_subcomp("profics", c(4, 5)), c("prof4", "ics1"))
  ilp <- data.frame(record_id = "1", year_resident = c("2", "3"),
                    goal_pcmk = c("3", "8"), goal_level_pcmk = c("2", "3"),
                    stringsAsFactors = FALSE)
  g <- extract_ilp_goals(ilp, "1", 3)
  expect_equal(g$subcomp[g$which == "current"], "mk2")
  expect_equal(g$target_rating[g$which == "current"], 5)
  expect_equal(g$subcomp[g$which == "previous"], "pc3")
})

test_that("program expectation steps 3 / 5 / 7 across the six periods", {
  expect_equal(milestone_program_expectation()$expected, c(3, 5, 5, 7, 7, 7))
})

test_that("cohort bands are ordered p10 <= p50 <= p90 and within 1-9", {
  b <- fit_cohort_bands(sel)
  expect_equal(nrow(b), 21 * 6)
  expect_true(all(b$p10 <= b$p50 + 1e-9 & b$p50 <= b$p90 + 1e-9))
  expect_true(all(b$p10 >= 1 & b$p90 <= 9))
  expect_true(all(b$method == "rq"))
  # Fallback path (too little data for the smoothed fit) is ordered too
  e <- fit_cohort_bands(sel[sel$subcomp == "pc1", ], min_n = Inf)
  expect_true(all(e$method == "empirical"))
  expect_true(all(e$p10 <= e$p50 & e$p50 <= e$p90))
})

test_that("band ordering survives quantile crossing", {
  # Tiny, noisy data make crossing likely; rearrangement must still order them
  set.seed(3)
  d <- data.frame(record_id = as.character(1:40), subcomp = "pc1",
                  period = rep(1:6, length.out = 40),
                  rating = sample(1:9, 40, replace = TRUE))
  b <- fit_cohort_bands(d, min_n = 10, df = 3)
  expect_true(all(b$p10 <= b$p50 & b$p50 <= b$p90))
})

skip_if_not_installed("lme4")
fit <- fit_milestone_growth(long)

test_that("growth models fit for all 21 subcompetencies", {
  expect_s3_class(fit, "milestone_growth_fit")
  expect_length(fit$models, 21)
})

test_that("predictions and intervals are clipped to 1-9", {
  m <- fit$models$pc1
  hi <- data.frame(period = 1:3, rating = 9, source = "coach")
  lo <- data.frame(period = 1:3, rating = 1, source = "coach")
  ph <- predict_milestone_growth(m, hi, periods = 4:6)
  pl <- predict_milestone_growth(m, lo, periods = 4:6)
  for (p in list(ph, pl)) {
    expect_true(all(p$fit >= 1 & p$fit <= 9))
    expect_true(all(p$lwr >= 1 & p$upr <= 9))
    expect_true(all(p$lwr <= p$fit & p$fit <= p$upr))
    expect_true(all(p$p_reach >= 0 & p$p_reach <= 1))
  }
  expect_equal(max(ph$fit), 9)          # would exceed 9 unclipped
  expect_gt(ph$p_reach[3], pl$p_reach[3])
  # No observations: population prediction
  expect_equal(nrow(predict_milestone_growth(m, hi[0, ], periods = 6)), 1)
})

test_that("cached-coefficient BLUP matches lme4::predict", {
  d <- sel[sel$subcomp == "pc3", ]
  d$t <- d$period - 3.5
  d$source <- factor(d$source, levels = fit$models$pc3$source_levels)
  f <- lme4::lmer(stats::as.formula(fit$models$pc3$formula), data = d)
  rid <- d$record_id[1]
  nd <- data.frame(t = (1:6) - 3.5, record_id = rid,
                   source = factor("acgme", levels = levels(d$source)))
  mine <- predict_milestone_growth(fit$models$pc3, d[d$record_id == rid, ], periods = 1:6)
  expect_equal(mine$fit, .mg_clip(unname(stats::predict(f, newdata = nd))), tolerance = 1e-4)
})

test_that("empirical check is suppressed below n = 10", {
  tab <- data.frame(subcomp = "pc1", period = 2L, rating = c(4, 5),
                    n = c(9L, 10L), n_reached = c(5L, 8L))
  tab <- gmed:::.mg_suppress(tab, 10)
  expect_equal(tab$suppressed, c(TRUE, FALSE))
  expect_true(is.na(tab$pct[1]) && is.na(tab$n_reached[1]))
  expect_equal(tab$pct[2], 0.8)
  a <- empirical_reach_lookup(tab, "pc1", 2, 4)
  expect_true(a$suppressed); expect_equal(a$n, 9L); expect_null(a$text)
  b <- empirical_reach_lookup(tab, "pc1", 2, 5)
  expect_false(b$suppressed); expect_equal(b$pct, 0.8)
  expect_match(b$text, "80%.*n = 10")
  expect_true(empirical_reach_lookup(tab, "pc1", 3, 5)$suppressed)   # no row = n 0
  full <- empirical_reach_table(sel)
  expect_true(all(is.na(full$pct[full$n < 10])))
  expect_true(all(!is.na(full$pct[full$n >= 10])))
})

test_that("fit survives serialisation for the REDCap cache", {
  enc <- serialize_milestone_growth(fit)
  expect_lt(nchar(enc), 65000)
  fit2 <- deserialize_milestone_growth(enc)
  obs <- sel[sel$record_id == sel$record_id[1] & sel$subcomp == "mk2", ]
  expect_equal(predict_milestone_growth(fit2$models$mk2, obs),
               predict_milestone_growth(fit$models$mk2, obs), tolerance = 1e-4)
  expect_equal(nrow(fit2$bands), nrow(fit$bands))
})

test_that("charts build, and return NULL instead of a blank chart when empty", {
  rid <- sel$record_id[1]
  r_long <- long[long$record_id == rid, ]
  goals <- extract_ilp_goals(sim$all_forms$ilp, rid, 4)
  expect_s3_class(plot_milestone_heat_table(sel[sel$record_id == rid, ], fit$bands, goals), "plotly")
  expect_s3_class(plot_milestone_dumbbell(r_long, 2, goals), "plotly")
  tp <- plot_milestone_trajectory(r_long, "pc1", fit, goals = goals)
  expect_s3_class(tp, "plotly")
  expect_true(length(attr(tp, "milestone_trajectory")$readout) >= 1)
  expect_null(plot_milestone_heat_table(sel[0, ], fit$bands))
  expect_null(plot_milestone_dumbbell(r_long[0, ], 2))
  expect_null(plot_milestone_trajectory(r_long[0, ], "pc1", fit))
})

test_that("heat data flags cells below the cohort 10th percentile", {
  b <- data.frame(subcomp = "pc1", period = 1:2, p10 = 3, p50 = 4, p90 = 6)
  r <- data.frame(subcomp = "pc1", period = 1:2, rating = c(2, 5), source = "coach")
  h <- milestone_heat_data(r, b, max_period = 2)
  h <- h[h$subcomp == "pc1", ]
  expect_equal(h$below_p10, c(TRUE, FALSE))
  expect_equal(h$diff, c(-2, 1))
})

test_that("module server renders with and without a fit", {
  rid <- sel$record_id[1]
  for (f in list(fit, NULL)) {
    shiny::testServer(mod_milestone_growth_server,
      args = list(milestone_data = long, resident_id = shiny::reactive(rid),
                  period = shiny::reactive(3), ilp_data = sim$all_forms$ilp, fit = f), {
        session$setInputs(subcomp = "pc2", dumbbell_period = "2")
        expect_false(is.null(output$overview_plot))
        expect_false(is.null(output$trajectory_plot))
        expect_false(is.null(output$dumbbell_plot))
      })
  }
})
