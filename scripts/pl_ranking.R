# Plackett-Luce model for the full learner ranking
#
# One PL model fit on all learners (excluding KM and NEL baselines).
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).

library(PlackettLuce)
library(data.table)
library(ggplot2)
library(cli)

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
scores_all <- readRDS(fs::path(result_path, "scores.rds"))

exclude <- c("KM", "NEL")

# -- Analysis function ------------------------------------------------------
run_pl_ranking <- function(scores_all, measure, minimize, exclude, result_path) {
  cli_h1("Plackett-Luce ranking: {measure}")

  # Filter to tune_measure and exclude baselines
  scores <- scores_all[grepl(pattern = measure, tune_measure) & !learner_id %in% exclude]

  # Average score per learner-task, then rank
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]
  scores_avg[, rank_score := frank(if (minimize) score else -score), by = task_id]

  # Wide format: rows = tasks, columns = learners
  ranks_wide <- dcast(scores_avg, task_id ~ learner_id, value.var = "rank_score")
  task_ids <- ranks_wide$task_id
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- task_ids

  rankings <- as.rankings(rank_mat)
  stopifnot(!anyNA(rank_mat))

  # Fit PL model
  pl_fit <- PlackettLuce(rankings, trace = TRUE)

  cli_alert_info("Log-likelihood: {round(logLik(pl_fit), 2)}")
  cli_alert_info("AIC: {round(AIC(pl_fit), 2)}")
  cli_alert_info("Iterations: {pl_fit$iter}")

  cli_h2("Log-worth parameters")
  print(coef(pl_fit))

  worth <- coef(pl_fit, log = FALSE)
  cli_h2("Worth parameters (sorted)")
  print(sort(worth, decreasing = TRUE))

  # Quasi-variances for SE estimation
  qv <- qvcalc(pl_fit)
  qvdt <- as.data.table(qv$qvframe, keep.rownames = "learner")

  # Plot 1: log-worth with quasi-SEs, sorted by estimate
  qvdt[, learner := reorder(learner, estimate)]

  p1 <- ggplot(qvdt, aes(x = learner, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = estimate - quasiSE, ymax = estimate + quasiSE)) +
    coord_flip() +
    labs(x = NULL, y = "Log-worth (quasi-SE)") +
    theme_minimal()

  save_plot(p1, name = paste0("pl_worth_", measure), width = 8, height = 6, formats = "pdf")

  # Plot 2: re-referenced to CPH
  cph_estimate <- qvdt[learner == "CPH", estimate]
  qvdt[, estimate_vs_cph := estimate - cph_estimate]
  qvdt[, learner_cph := reorder(learner, estimate_vs_cph)]

  p2 <- ggplot(qvdt, aes(x = learner_cph, y = estimate_vs_cph)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(ymin = estimate_vs_cph - quasiSE, ymax = estimate_vs_cph + quasiSE)) +
    coord_flip() +
    labs(x = NULL, y = "Log-worth relative to CPH (quasi-SE)") +
    theme_minimal()

  save_plot(p2, name = paste0("pl_worth_", measure, "_cphref"), width = 8, height = 6, formats = "pdf")

  invisible(list(pl_fit = pl_fit, qv = qv, worth = worth))
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_ranking(scores_all, "harrell_c", minimize = FALSE, exclude, result_path)
res_isbs <- run_pl_ranking(scores_all, "isbs", minimize = TRUE, exclude, result_path)
