# Plackett-Luce trees: model-based partitioning of learner rankings by task covariates
#
# Uses pltree() (MOB-based) to test whether task characteristics
# (PH violation, censoring proportion, n/p ratio) affect learner rankings.
#
# Two configurations:
#   a) All learners (excl. KM, NEL) -- more items but lower power to detect splits
#   b) Representative subset (one per family) -- fewer items, higher power
#
# Per-iteration scores are averaged per learner-task, then ranked
# (one complete ranking per task).
# Runs for both tuning measures: harrell_c (higher = better) and isbs (lower = better).

library(PlackettLuce)
library(strucchange)
library(data.table)
library(ggplot2)
source("R/plackettluce.R")

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper", "PL")

# -- Data -------------------------------------------------------------------
msr_tbl <- load_msr_table()
tasktab <- load_tasktab()

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

# -- Learner sets -----------------------------------------------------------
exclude <- c("KM", "NEL")
all_learners <- function(scores_all, measure) {
  setdiff(unique(scores_all[grepl(pattern = measure, tune_measure), learner_id]), exclude)
}

# -- Run --------------------------------------------------------------------
set.seed(2026) # PL models / trees are not fully deterministic, fixing seed just in case.

# All learners, stricter settings
res_all_hc <- run_pl_tree(
  scores_all = scores_all,
  measure = "harrell_c",
  minimize = FALSE,
  learners = all_learners(scores_all, "harrell_c"),
  tasktab = tasktab,
  plot_path = plot_path,
  plot_name = "all-strict",
  covariates = c("log_noverp", "ph_violated", "censprop"),
  bonferroni = TRUE,
  gamma = FALSE,
  minsize = 10,
  alpha = .1,
  msr_tbl = msr_tbl,
  width = 12,
  height = 8
)

res_all_isbs <- run_pl_tree(
  scores_all = scores_all,
  measure = "isbs",
  minimize = TRUE,
  learners = all_learners(scores_all, "isbs"),
  tasktab = tasktab,
  plot_path = plot_path,
  plot_name = "all-strict",
  covariates = c("log_noverp", "ph_violated", "censprop"),
  bonferroni = TRUE,
  gamma = FALSE,
  minsize = 10,
  alpha = .1,
  msr_tbl = msr_tbl,
  width = 12,
  height = 8
)

# All learners  with higher alpha (more noise, but better chance to find something at all)
res_all_hc <- run_pl_tree(
  scores_all = scores_all,
  measure = "harrell_c",
  minimize = FALSE,
  learners = all_learners(scores_all, "harrell_c"),
  tasktab = tasktab,
  plot_path = plot_path,
  plot_name = "all-lenient",
  covariates = c("log_noverp", "ph_violated", "censprop"),
  bonferroni = TRUE,
  gamma = FALSE,
  minsize = 5,
  alpha = .2,
  msr_tbl = msr_tbl,
  width = 12,
  height = 8
)

res_all_isbs <- run_pl_tree(
  scores_all = scores_all,
  measure = "isbs",
  minimize = TRUE,
  learners = all_learners(scores_all, "isbs"),
  tasktab = tasktab,
  plot_path = plot_path,
  plot_name = "all-lenient",
  covariates = c("log_noverp", "ph_violated", "censprop"),
  bonferroni = TRUE,
  gamma = FALSE,
  minsize = 5,
  alpha = .2,
  msr_tbl = msr_tbl,
  width = 12,
  height = 8
)
