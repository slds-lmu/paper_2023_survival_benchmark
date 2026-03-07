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

# -- Analysis function ------------------------------------------------------
run_pl_censprop_subgroups <- function(scores_all, measure, minimize, exclude, tasktab) {
  cli_h1("Censoring proportion subgroup analysis: {measure}")

  prep <- pl_prepare_rankings(scores_all, measure, minimize, exclude)

  # Split at median censoring proportion
  tt <- tasktab[match(prep$task_ids, task_id)]
  cens_med <- median(tt$censprop)
  lo_idx <- which(tt$censprop <= cens_med)
  hi_idx <- which(tt$censprop > cens_med)

  cli_alert_info("Median censoring proportion: {round(cens_med, 3)}")

  subgroups <- setNames(
    list(lo_idx, hi_idx),
    c(paste0("Low censoring (<= ", round(cens_med, 2), ")"),
      paste0("High censoring (> ", round(cens_med, 2), ")"))
  )

  pl_subgroup_analysis(
    rankings = prep$rankings,
    subgroups = subgroups,
    measure = measure,
    plot_name = paste0("pl_worth_censprop_subgroups_", measure)
  )
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_censprop_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab)
res_isbs <- run_pl_censprop_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab)
