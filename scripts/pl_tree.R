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
library(data.table)
library(cli)

result_path <- fs::path(here::here("results", "production"))
plot_path <- here::here("results_paper")

# -- Data -------------------------------------------------------------------
tasktab <- load_tasktab()
tasktab[, ph_violated := ifelse(zph_pval_processed < 0.05 | is.na(zph_pval_processed), 1, 0)]
tasktab[, noverp := n / p]

scores_all <- readRDS(fs::path(result_path, "scores.rds"))

# -- Helper: average scores and build rankings + model_data -----------------
build_pltree_data <- function(scores_avg, measure, minimize, learner_subset, tasktab) {
  dt <- scores_avg[learner_id %in% learner_subset]
  dt[, rank_score := frank(if (minimize) score else -score), by = task_id]

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
    ph_violated = factor(tt$ph_violated),
    censprop = tt$censprop
  )

  list(rankings = rankings, model_data = model_data, learner_ids = learner_ids)
}

get_pvals <- function(tree) {
  st <- sctest(tree, node = 1)
  setNames(st$p.value, rownames(st))
}

# -- Analysis function ------------------------------------------------------
run_pl_tree <- function(scores_all, measure, minimize, tasktab, result_path, alpha = 0.05) {
  cli_h1("Plackett-Luce tree: {measure} (alpha = {alpha})")

  scores <- scores_all[grepl(pattern = measure, tune_measure)]
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]

  exclude <- c("KM", "NEL")

  # a) All learners
  all_learners <- setdiff(unique(scores_avg$learner_id), exclude)
  dat_all <- build_pltree_data(scores_avg, measure, minimize, all_learners, tasktab)

  cli_h2("All learners ({length(dat_all$learner_ids)})")
  cli_alert_info("Learners: {paste(dat_all$learner_ids, collapse = ', ')}")

  tree_all <- pltree(
    G ~ noverp + ph_violated + censprop,
    data = dat_all$model_data,
    alpha = alpha,
    maxdepth = 3,
    minsize = 5,
    trace = TRUE
  )
  tree_all

  cli_h3("Instability tests (root node)")
  print(sctest(tree_all, node = 1))

  cli_h3("Worth parameters")
  print(sort(coef(tree_all, log = FALSE), decreasing = TRUE))

  if (length(tree_all) > 1) {
    pdf(file = fs::path(plot_path, paste0("pltree_all_learners_", measure, ".pdf")),
        width = 12, height = 8)
    plot(tree_all, names = TRUE, abbreviate = 3, ylines = 2)
    dev.off()
  }

  # b) Representative subset
  representative <- c("CPH", "RFSRC", "CoxB", "XGBAFT")
  dat_rep <- build_pltree_data(scores_avg, measure, minimize, representative, tasktab)

  cli_h2("Representative subset ({length(dat_rep$learner_ids)})")
  cli_alert_info("Learners: {paste(dat_rep$learner_ids, collapse = ', ')}")

  tree_rep <- pltree(
    G ~ noverp + ph_violated + censprop,
    data = dat_rep$model_data,
    alpha = alpha,
    maxdepth = 3,
    minsize = 5,
    trace = TRUE
  )
  tree_rep

  cli_h3("Instability tests (root node)")
  print(sctest(tree_rep, node = 1))

  cli_h3("Worth parameters")
  print(coef(tree_rep, log = FALSE))

  if (length(tree_rep) > 1) {
    pdf(file = fs::path(plot_path, paste0("pltree_representative_", measure, ".pdf")),
        width = 10, height = 7)
    plot(tree_rep, names = TRUE, abbreviate = 5, ylines = 3)
    dev.off()
  }

  # Summary
  cli_h2("Instability p-value comparison")
  pvals <- data.table(
    covariate = names(get_pvals(tree_all)),
    p_all_learners = get_pvals(tree_all),
    p_representative = get_pvals(tree_rep)
  )
  print(pvals, digits = 4)

  invisible(list(tree_all = tree_all, tree_rep = tree_rep, pvals = pvals))
}

# -- Run for both measures --------------------------------------------------
res_harrell_c <- run_pl_tree(scores_all, "harrell_c", minimize = FALSE, tasktab, result_path)
res_isbs      <- run_pl_tree(scores_all, "isbs",      minimize = TRUE,  tasktab, result_path)
