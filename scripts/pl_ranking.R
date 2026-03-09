# Plackett-Luce model for the full learner ranking
#
# One PL model fit on all learners (excluding KM and NEL baselines).
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).

library(PlackettLuce)
library(mlr3proba)
library(data.table)
library(ggplot2)

source("R/plackettluce.R")

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
scores_all <- readRDS(fs::path(result_path, "scores.rds"))

exclude <- c("KM", "NEL")

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_ranking(scores_all, "harrell_c", minimize = FALSE, exclude, result_path)
res_isbs <- run_pl_ranking(scores_all, "isbs", minimize = TRUE, exclude, result_path)
