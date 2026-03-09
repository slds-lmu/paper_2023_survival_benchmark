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
library(mlr3proba)
library(strucchange)
library(data.table)
library(ggplot2)

source("R/plackettluce.R")

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]
tasktab[, noverp := n / p]

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

# -- Learner sets -----------------------------------------------------------
exclude <- c("KM", "NEL")
all_learners <- function(scores_all, measure) {
  setdiff(unique(scores_all[grepl(pattern = measure, tune_measure), learner_id]), exclude)
}
representative <- c(
  "CPH",
  "AFT",
  "GAM",
  # Penalized
  "GLMN",
  # Trees
  "RFSRC",
  "ORSF",
  # Boosting
  "CoxB",
  "MBSTAFT",
  # Other (Harrell's C only)
  "SSVM"
)

# -- Run --------------------------------------------------------------------
# Representative subset (fast, more power to detect splits)
res_rep_hc <- run_pl_tree(
  scores_all = scores_all,
  measure = "harrell_c",
  minimize = FALSE,
  learners = representative,
  tasktab = tasktab,
  plot_name = "representative",
  alpha = .1,
  width = 10,
  height = 7
)
res_rep_isbs <- run_pl_tree(
  scores_all = scores_all,
  measure = "isbs",
  minimize = TRUE,
  learners = representative,
  tasktab = tasktab,
  plot_name = "representative",
  alpha = .1,
  width = 10,
  height = 7
)

# All learners (lower power)
res_all_hc <- run_pl_tree(
  scores_all = scores_all,
  measure = "harrell_c",
  minimize = FALSE,
  learners = all_learners(scores_all, "harrell_c"),
  tasktab = tasktab,
  plot_name = "all_learners",
  alpha = .1,
  width = 12,
  height = 8
)
res_all_isbs <- run_pl_tree(
  scores_all = scores_all,
  measure = "isbs",
  minimize = TRUE,
  learners = all_learners(scores_all, "isbs"),
  tasktab = tasktab,
  plot_name = "all_learners",
  alpha = .1,
  width = 12,
  height = 8
)


# All learners (lower power), with higher alpha (more noise, but better chance to find something at all)
res_all_hc <- run_pl_tree(
  scores_all = scores_all,
  measure = "harrell_c",
  minimize = FALSE,
  learners = all_learners(scores_all, "harrell_c"),
  tasktab = tasktab,
  plot_name = "all_learners-alpha05",
  alpha = .5,
  width = 12,
  height = 8
)
res_all_isbs <- run_pl_tree(
  scores_all = scores_all,
  measure = "isbs",
  minimize = TRUE,
  learners = all_learners(scores_all, "isbs"),
  tasktab = tasktab,
  plot_name = "all_learners-alpha05",
  alpha = .5,
  width = 12,
  height = 8
)

# all learners, different covariates, high alpha, for experimenting
if (FALSE) {
  res_testing_hc <- run_pl_tree(
    scores_all = scores_all,
    measure = "harrell_c",
    minimize = FALSE,
    covariates = c(
      "noverp",
      "censprop"
      # "zph_pval_processed",
      # "ph_violated"
      # "n",
      # "p"
    ),
    learners = all_learners(scores_all, "harrell_c"),
    tasktab = tasktab,
    plot_name = "all_learners-allcov-alpha-5",
    alpha = .8,
    width = 12,
    height = 8
  )

  res_testing_isbs <- run_pl_tree(
    scores_all = scores_all,
    measure = "isbs",
    minimize = TRUE,
    covariates = c(
      "noverp",
      "censprop",
      # "zph_pval_processed"
      "ph_violated"
    ),
    learners = all_learners(scores_all, "isbs"),
    tasktab = tasktab,
    plot_name = "all_learners-allcov-alpha-5",
    alpha = .8,
    width = 12,
    height = 8
  )
}
