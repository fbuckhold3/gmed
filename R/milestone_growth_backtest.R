# milestone_growth_backtest.R
# Validation of the growth model on graduated classes: predict each class's
# graduation ratings from its first 2-3 periods, using models trained on
# other classes, then report calibration of P(>= 7) and MAE.

#' Backtest the milestone growth model on graduated classes
#'
#' For each graduated class (a \code{grad_yr} with period-6 ratings), fits the
#' growth models on the training classes, then for every resident in the class
#' and every subcompetency predicts the graduation rating from the ratings in
#' periods \code{1..k} (\code{k} in \code{first_periods}) and compares it with
#' the observed graduation rating (the rating that counts: ACGME > CCC >
#' coach).
#'
#' A no-model baseline is scored alongside: every resident gets the training
#' classes' median graduation rating and their share reaching 7.
#'
#' @param data Long milestone data with a \code{grad_yr} column
#'   (\code{build_milestone_long(all_forms, residents)}).
#' @param first_periods Numbers of early periods to predict from.
#' @param training \code{"prior"} (default): train only on classes that
#'   graduated before the test class, as the model would be used in practice;
#'   classes with fewer than \code{min_train_classes} earlier classes are not
#'   tested. \code{"other"}: leave-one-class-out (uses later classes too).
#' @param min_train_classes Minimum training classes for \code{"prior"}.
#' @param level Prediction interval level.
#' @return List (class \code{milestone_backtest}): \code{predictions}
#'   (one row per resident x subcomp x k), \code{summary}, \code{calibration},
#'   \code{by_subcomp}, \code{classes}, \code{settings}.
#' @export
backtest_milestone_growth <- function(data, first_periods = c(2, 3),
                                      training = c("prior", "other"),
                                      min_train_classes = 2, level = 0.8) {
  training <- match.arg(training)
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("backtest_milestone_growth: package 'lme4' is required")
  long <- as_milestone_long(data)
  if (!"grad_yr" %in% names(long))
    stop("backtest_milestone_growth: data needs a grad_yr column ",
         "(pass residents to build_milestone_long())")
  sel <- select_milestone_rating(long)
  sel <- sel[!is.na(sel$grad_yr), , drop = FALSE]
  truth <- sel[sel$period == 6, c("record_id", "subcomp", "rating", "grad_yr")]
  names(truth)[3] <- "observed"
  classes <- .mg_sort_classes(unique(truth$grad_yr))
  subcomps <- milestone_subcompetencies()$subcomp

  preds <- list()
  tested <- character(0)
  for (cl in classes) {
    train_classes <- if (training == "prior") classes[seq_len(match(cl, classes) - 1)] else
      setdiff(unique(sel$grad_yr), cl)
    if (training == "prior" && length(train_classes) < min_train_classes) next
    train <- sel[sel$grad_yr %in% train_classes, , drop = FALSE]
    test <- sel[sel$grad_yr == cl, , drop = FALSE]
    tr_grad <- train[train$period == 6, ]
    tested <- c(tested, cl)
    for (sc in subcomps) {
      m <- fit_growth_model(train[train$subcomp == sc, , drop = FALSE])
      if (is.null(m)) next
      base_fit <- stats::median(tr_grad$rating[tr_grad$subcomp == sc])
      base_p <- mean(tr_grad$rating[tr_grad$subcomp == sc] >= .MG_TARGET)
      tt <- truth[truth$grad_yr == cl & truth$subcomp == sc, , drop = FALSE]
      for (i in seq_len(nrow(tt))) {
        obs_all <- test[test$record_id == tt$record_id[i] & test$subcomp == sc, , drop = FALSE]
        for (k in first_periods) {
          obs <- obs_all[obs_all$period <= k, , drop = FALSE]
          if (!nrow(obs)) next
          pr <- predict_milestone_growth(m, obs, periods = 6, level = level)
          preds[[length(preds) + 1]] <- data.frame(
            grad_yr = cl, record_id = tt$record_id[i], subcomp = sc, k = k,
            n_obs = nrow(obs), observed = tt$observed[i],
            fit = pr$fit, lwr = pr$lwr, upr = pr$upr, p_reach = pr$p_reach,
            baseline_fit = base_fit, baseline_p = base_p,
            stringsAsFactors = FALSE)
        }
      }
    }
  }
  pred <- do.call(rbind, preds)
  if (is.null(pred)) stop("backtest_milestone_growth: no graduated class could be tested")
  pred$reached <- pred$observed >= .MG_TARGET

  summ <- do.call(rbind, lapply(split(pred, pred$k), function(d) data.frame(
    k = d$k[1], n = nrow(d), residents = length(unique(d$record_id)),
    mae = mean(abs(d$fit - d$observed)),
    mae_baseline = mean(abs(d$baseline_fit - d$observed)),
    rmse = sqrt(mean((d$fit - d$observed)^2)),
    pi_coverage = mean(d$observed >= d$lwr & d$observed <= d$upr),
    brier = mean((d$p_reach - d$reached)^2),
    brier_baseline = mean((d$baseline_p - d$reached)^2),
    observed_reach = mean(d$reached),
    mean_p_reach = mean(d$p_reach))))

  calib <- do.call(rbind, lapply(split(pred, pred$k), function(d) {
    br <- unique(stats::quantile(d$p_reach, seq(0, 1, 0.1), names = FALSE))
    dec <- if (length(br) > 2) cut(d$p_reach, br, include.lowest = TRUE, labels = FALSE) else 1L
    do.call(rbind, lapply(split(d, dec), function(x) data.frame(
      k = x$k[1], decile = NA_integer_, n = nrow(x),
      mean_predicted = mean(x$p_reach), observed = mean(x$reached))))
  }))
  calib$decile <- stats::ave(calib$k, calib$k, FUN = seq_along)

  by_sc <- stats::aggregate(cbind(abs_err = abs(fit - observed),
                                  abs_err_baseline = abs(baseline_fit - observed)) ~ subcomp + k,
                            data = pred, FUN = mean)
  names(by_sc)[3:4] <- c("mae", "mae_baseline")

  structure(list(
    predictions = pred, summary = summ, calibration = calib, by_subcomp = by_sc,
    classes = tested,
    settings = list(first_periods = first_periods, training = training,
                    level = level, run_at = format(Sys.time(), "%Y-%m-%d %H:%M"))
  ), class = "milestone_backtest")
}

#' Write a backtest report as markdown
#'
#' @param bt A \code{milestone_backtest}.
#' @param path Output file.
#' @param data_note One-line description of the data (e.g. "RDM test project"
#'   or "Synthetic data").
#' @return Invisible path.
#' @export
write_milestone_backtest_report <- function(bt, path, data_note) {
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  pc <- function(x) paste0(round(100 * x), "%")
  s <- bt$summary
  lines <- c(
    "# Milestone growth model: backtest",
    "",
    paste0("**Data:** ", data_note, "  "),
    paste0("**Run:** ", bt$settings$run_at, "  "),
    paste0("**Test classes:** ", paste(bt$classes, collapse = ", "),
           " (training: ", if (bt$settings$training == "prior")
             "earlier graduated classes only" else "all other classes", ")  "),
    paste0("**Model:** `rating ~ t + t^2 + source + (1 + t | resident)` per subcompetency ",
           "(lme4, REML), predicted for the ACGME source at graduation. ",
           round(100 * bt$settings$level), "% prediction intervals."),
    "",
    "Each graduated resident's graduation rating (the rating that counts: ACGME, else CCC,",
    "else coach) is predicted from their first *k* periods only. The baseline gives every",
    "resident the training classes' median graduation rating and their share reaching 7.",
    "",
    "## Summary",
    "",
    "| First k periods | Predictions | Residents | MAE | MAE (baseline) | RMSE | PI coverage | Brier P(>=7) | Brier (baseline) | Observed reach 7 | Mean predicted P(>=7) |",
    "|---|---|---|---|---|---|---|---|---|---|---|",
    sprintf("| %d | %d | %d | %s | %s | %s | %s | %s | %s | %s | %s |", s$k, s$n, s$residents,
            f2(s$mae), f2(s$mae_baseline), f2(s$rmse), pc(s$pi_coverage), f2(s$brier),
            f2(s$brier_baseline), pc(s$observed_reach), pc(s$mean_p_reach)),
    "",
    "PI coverage = share of observed graduation ratings inside the prediction interval.",
    "",
    "## Calibration of P(rating >= 7 at graduation), by decile of prediction",
    ""
  )
  for (k in unique(bt$calibration$k)) {
    cb <- bt$calibration[bt$calibration$k == k, ]
    lines <- c(lines, sprintf("**From the first %d periods**", k), "",
               "| Decile | n | Mean predicted | Observed |", "|---|---|---|---|",
               sprintf("| %d | %d | %s | %s |", cb$decile, cb$n, pc(cb$mean_predicted),
                       pc(cb$observed)), "")
  }
  bs <- bt$by_subcomp
  bs <- bs[bs$k == max(bs$k), ]
  bs <- bs[match(milestone_subcompetencies()$subcomp, bs$subcomp), ]
  bs <- bs[!is.na(bs$subcomp), ]
  lines <- c(lines, sprintf("## MAE by subcompetency (first %d periods)", max(bt$summary$k)), "",
             "| Subcompetency | MAE | MAE (baseline) |", "|---|---|---|",
             sprintf("| %s | %s | %s |", toupper(bs$subcomp), f2(bs$mae), f2(bs$mae_baseline)),
             "")
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

# Class order: numeric when grad_yr is a year or a raw REDCap code, else text.
.mg_sort_classes <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (!anyNA(n)) x[order(n)] else sort(x)
}
