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
library(cli)

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
  "XGBAFT",
  # Other (Harrell's C only)
  "SSVM"
)


# -- Helper: average scores and build rankings + model_data -----------------
build_pltree_data <- function(scores_avg, measure, minimize, learner_subset, tasktab) {
  dt <- scores_avg[learner_id %in% learner_subset]
  dt[, rank_score := frank(if (minimize) score else -score, ties.method = "random"), by = task_id]

  ranks_wide <- dcast(dt, task_id ~ learner_id, value.var = "rank_score")
  task_ids <- ranks_wide$task_id
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- task_ids
  rankings <- as.rankings(rank_mat)

  tt <- copy(tasktab)[match(task_ids, task_id)]
  stopifnot(all.equal(tt$task_id, task_ids))

  G <- group(rankings, index = seq_len(nrow(rankings)))
  model_data <- data.frame(
    G = G,
    noverp = tt$noverp,
    n = tt$n,
    p = tt$p,
    ph_violated = factor(tt$ph_violated),
    zph_pval_processed = tt$zph_pval_processed,
    censprop = tt$censprop
  )

  list(rankings = rankings, model_data = model_data, learner_ids = learner_ids)
}

# -- Analysis function ------------------------------------------------------
run_pl_tree <- function(
  scores_all,
  measure,
  minimize,
  learners,
  tasktab,
  plot_name,
  covariates = c("noverp", "ph_violated", "censprop"),
  alpha = 0.10,
  width = 10,
  height = 7
) {
  cli_h1("Plackett-Luce tree: {measure} / {plot_name} (alpha = {alpha})")
  cli_alert_info("Covariates: {paste(covariates, collapse = ', ')}")

  scores <- scores_all[grepl(pattern = measure, tune_measure)]
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]

  dat <- build_pltree_data(scores_avg, measure, minimize, learners, tasktab)

  cli_alert_info("Learners ({length(dat$learner_ids)}): {paste(dat$learner_ids, collapse = ', ')}")

  fml <- reformulate(covariates, response = "G")

  # do.call forces evaluation of all arguments before pltree captures them
  # via match.call(). Without this, mob() re-evaluates e.g. `alpha` in an
  # internal environment where the caller's variables don't exist.
  tree <- do.call(
    pltree,
    list(
      formula = fml,
      data = dat$model_data,
      alpha = alpha,
      maxdepth = 4,
      minsize = 5,
      npseudo = 0.5,
      gamma = FALSE,
      trace = TRUE
    )
  )
  tree

  cli_h2("Instability tests (root node)")
  print(sctest(tree, node = 1))

  cli_h2("Worth parameters")
  print(sort(coef(tree, log = FALSE), decreasing = TRUE))

  if (length(tree) > 1) {
    measure_label <- measures_tbl()[id == measure, label]
    p <- plot_pltree_gg(tree, caption = glue::glue("Measure: {measure_label}"))
    save_plot(p, name = paste0("pltree_", plot_name, "_", measure), width = width, height = height, formats = "pdf")
  }

  invisible(tree)
}

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
