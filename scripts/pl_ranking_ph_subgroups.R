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
run_pl_ph_subgroups <- function(scores_all, measure, minimize, exclude, tasktab) {
  cli_h1("PH subgroup analysis: {measure}")

  prep <- pl_prepare_rankings(scores_all, measure, minimize, exclude)

  # Build subgroups based on PH violation status
  tt <- tasktab[match(prep$task_ids, task_id)]
  ph0_idx <- which(tt$ph_violated == 0)
  ph1_idx <- which(tt$ph_violated == 1)

  pl_subgroup_analysis(
    rankings = prep$rankings,
    subgroups = list("PH not violated" = ph0_idx, "PH violated" = ph1_idx),
    measure = measure,
    plot_name = paste0("pl_worth_ph_subgroups_", measure)
  )
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_ph_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab)
res_isbs <- run_pl_ph_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab)
