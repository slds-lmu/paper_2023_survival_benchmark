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
library(mlr3proba)
library(data.table)
library(ggplot2)
library(cli)

source("R/plackettluce.R")

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

exclude <- c("KM", "NEL")

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_ph_subgroups(scores_all, "harrell_c", minimize = FALSE, exclude, tasktab)
res_isbs <- run_pl_ph_subgroups(scores_all, "isbs", minimize = TRUE, exclude, tasktab)
