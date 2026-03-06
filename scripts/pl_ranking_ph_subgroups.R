# Plackett-Luce models stratified by PH assumption violation
#
# Fits two separate PL models on all learners (excluding KM, NEL):
#   - Tasks where PH is NOT violated (zph_pval_processed >= 0.05)
#   - Tasks where PH IS violated (zph_pval_processed < 0.05 or NA)
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
tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

exclude <- c("KM", "NEL")

# -- Analysis function ------------------------------------------------------
run_pl_ph_subgroups <- function(scores_all, measure, minimize, exclude, tasktab, result_path) {
  cli_h1("PH subgroup analysis: {measure}")

  # Filter to tune_measure and exclude baselines
  scores <- scores_all[grepl(pattern = measure, tune_measure) & !learner_id %in% exclude]

  # Average score per learner-task, then rank
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]
  scores_avg[, rank_score := frank(if (minimize) score else -score), by = task_id]

  # Wide format
  ranks_wide <- dcast(scores_avg, task_id ~ learner_id, value.var = "rank_score")
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  # Match task metadata
  ranks_wide <- tasktab[, .(task_id, ph_violated)][ranks_wide, on = "task_id"]

  # Build rankings
  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- ranks_wide$task_id
  rankings <- as.rankings(rank_mat)
  stopifnot(!anyNA(rank_mat))

  ph0_idx <- which(ranks_wide$ph_violated == 0)
  ph1_idx <- which(ranks_wide$ph_violated == 1)

  cli_alert_info("Tasks with PH not violated: {length(ph0_idx)}")
  cli_alert_info("Tasks with PH violated: {length(ph1_idx)}")

  # Fit separate PL models
  cli_h2("PH NOT violated")
  pl_ph0 <- PlackettLuce(rankings[ph0_idx, ], trace = TRUE)
  cli_alert_info("Log-likelihood: {round(logLik(pl_ph0), 2)}")
  cli_alert_info("AIC: {round(AIC(pl_ph0), 2)}")

  cli_h2("PH violated")
  pl_ph1 <- PlackettLuce(rankings[ph1_idx, ], trace = TRUE)
  cli_alert_info("Log-likelihood: {round(logLik(pl_ph1), 2)}")
  cli_alert_info("AIC: {round(AIC(pl_ph1), 2)}")

  # Worth parameters
  worth_ph0 <- coef(pl_ph0, log = FALSE)
  worth_ph1 <- coef(pl_ph1, log = FALSE)

  cli_h2("Worth: PH not violated (sorted)")
  print(sort(worth_ph0, decreasing = TRUE))

  cli_h2("Worth: PH violated (sorted)")
  print(sort(worth_ph1, decreasing = TRUE))

  # Rank comparison
  rank_comparison <- data.table(
    learner = names(sort(worth_ph0, decreasing = TRUE)),
    rank_ph_ok = seq_along(worth_ph0),
    worth_ph_ok = sort(worth_ph0, decreasing = TRUE)
  )
  rank_comparison[, `:=`(
    rank_ph_violated = match(learner, names(sort(worth_ph1, decreasing = TRUE))),
    worth_ph_violated = worth_ph1[learner]
  )]
  rank_comparison[, rank_change := rank_ph_ok - rank_ph_violated]

  cli_h2("Rank comparison (sorted by PH-ok rank)")
  print(rank_comparison)

  # Quasi-variance plots
  qv_ph0 <- qvcalc(pl_ph0)
  qv_ph1 <- qvcalc(pl_ph1)

  qvdt_ph0 <- as.data.table(qv_ph0$qvframe, keep.rownames = "learner")
  qvdt_ph0[, subgroup := "PH not violated"]
  qvdt_ph1 <- as.data.table(qv_ph1$qvframe, keep.rownames = "learner")
  qvdt_ph1[, subgroup := "PH violated"]

  qvdt <- rbind(qvdt_ph0, qvdt_ph1)

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
    labs(x = NULL, y = "Log-worth relative to CPH (quasi-SE)") +
    theme_minimal()

  save_plot(p, name = paste0("pl_worth_ph_subgroups_", measure), width = 12, height = 6, formats = "pdf")

  invisible(list(
    pl_ph0 = pl_ph0, pl_ph1 = pl_ph1,
    rank_comparison = rank_comparison
  ))
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_ph_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab, result_path)
res_isbs      <- run_pl_ph_subgroups(scores_all, "isbs",      minimize = TRUE,  exclude, tasktab, result_path)
