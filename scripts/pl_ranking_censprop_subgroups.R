# Plackett-Luce models stratified by censoring proportion
#
# Fits two separate PL models on all learners (excluding KM, NEL):
#   - Tasks with low censoring (censprop <= median)
#   - Tasks with high censoring (censprop > median)
#
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
msr_tbl <- measures_tbl()

# -- Analysis function ------------------------------------------------------
run_pl_censprop_subgroups <- function(scores_all, measure, minimize, exclude, tasktab, result_path) {
  cli_h1("Censoring proportion subgroup analysis: {measure}")
  measure_label <- msr_tbl[id == measure, label]

  # Filter to tune_measure and exclude baselines
  scores <- scores_all[grepl(pattern = measure, tune_measure) & !learner_id %in% exclude]

  # Average score per learner-task, then rank
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]
  scores_avg[, rank_score := frank(if (minimize) score else -score, ties.method = "random"), by = task_id]

  # Wide format
  ranks_wide <- dcast(scores_avg, task_id ~ learner_id, value.var = "rank_score")
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  # Match task metadata
  ranks_wide <- tasktab[, .(task_id, censprop)][ranks_wide, on = "task_id"]

  # Build rankings
  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- ranks_wide$task_id
  rankings <- as.rankings(rank_mat)
  stopifnot(!anyNA(rank_mat))

  # Split at median censoring proportion
  cens_med <- median(ranks_wide$censprop)
  lo_idx <- which(ranks_wide$censprop <= cens_med)
  hi_idx <- which(ranks_wide$censprop > cens_med)

  cli_alert_info("Median censoring proportion: {round(cens_med, 3)}")
  cli_alert_info("Tasks with low censoring (<= median): {length(lo_idx)}")
  cli_alert_info("Tasks with high censoring (> median): {length(hi_idx)}")

  # Fit separate PL models
  cli_h2("Low censoring")
  pl_lo <- PlackettLuce(rankings[lo_idx, ], trace = TRUE)
  cli_alert_info("Log-likelihood: {round(logLik(pl_lo), 2)}")
  cli_alert_info("AIC: {round(AIC(pl_lo), 2)}")

  cli_h2("High censoring")
  pl_hi <- PlackettLuce(rankings[hi_idx, ], trace = TRUE)
  cli_alert_info("Log-likelihood: {round(logLik(pl_hi), 2)}")
  cli_alert_info("AIC: {round(AIC(pl_hi), 2)}")

  # Worth parameters
  worth_lo <- coef(pl_lo, log = FALSE)
  worth_hi <- coef(pl_hi, log = FALSE)

  cli_h2("Worth: low censoring (sorted)")
  print(sort(worth_lo, decreasing = TRUE))

  cli_h2("Worth: high censoring (sorted)")
  print(sort(worth_hi, decreasing = TRUE))

  # Rank comparison
  rank_comparison <- data.table(
    learner = names(sort(worth_lo, decreasing = TRUE)),
    rank_lo = seq_along(worth_lo),
    worth_lo = sort(worth_lo, decreasing = TRUE)
  )
  rank_comparison[, let(
    rank_hi = match(learner, names(sort(worth_hi, decreasing = TRUE))),
    worth_hi = worth_hi[learner]
  )]
  rank_comparison[, rank_change := rank_lo - rank_hi]

  cli_h2("Rank comparison (sorted by low-censoring rank)")
  print(rank_comparison)

  # Quasi-variance plots
  qv_lo <- qvcalc(pl_lo)
  qv_hi <- qvcalc(pl_hi)

  qvdt_lo <- as.data.table(qv_lo$qvframe, keep.rownames = "learner")
  qvdt_lo[, subgroup := paste0("Low censoring (<= ", round(cens_med, 2), ")")]
  qvdt_hi <- as.data.table(qv_hi$qvframe, keep.rownames = "learner")
  qvdt_hi[, subgroup := paste0("High censoring (> ", round(cens_med, 2), ")")]

  qvdt <- rbind(qvdt_lo, qvdt_hi)

  # Re-reference to CPH
  qvdt[, estimate_vs_cph := estimate - estimate[learner == "CPH"], by = subgroup]
  qvdt[, learner_ord := reorder(learner, estimate_vs_cph, FUN = mean)]

  p <- ggplot(qvdt, aes(x = learner_ord, y = estimate_vs_cph)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(aes(
      ymin = estimate_vs_cph - quasiSE,
      ymax = estimate_vs_cph + quasiSE
    )) +
    coord_flip() +
    facet_wrap(~subgroup) +
    labs(x = NULL, y = "Log-worth relative to CPH (quasi-SE)", caption = glue::glue("Measure: {measure_label}")) +
    theme_minimal()

  save_plot(p, name = paste0("pl_worth_censprop_subgroups_", measure), width = 12, height = 6, formats = "pdf")

  invisible(list(
    pl_lo = pl_lo,
    pl_hi = pl_hi,
    rank_comparison = rank_comparison
  ))
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_censprop_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab, result_path)
res_isbs <- run_pl_censprop_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab, result_path)
