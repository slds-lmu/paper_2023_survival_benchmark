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
library(ggplot2)
library(ggparty)
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
    ph_violated = factor(tt$ph_violated),
    censprop = tt$censprop
  )

  list(rankings = rankings, model_data = model_data, learner_ids = learner_ids)
}

get_pvals <- function(tree) {
  st <- sctest(tree, node = 1)
  setNames(st$p.value, rownames(st))
}

# -- ggparty plotting helper ------------------------------------------------
plot_pltree_gg <- function(tree) {
  # Extract worth parameters per terminal node into a data.frame with 'id' column
  term_ids <- nodeids(tree, terminal = TRUE)
  worth_df <- do.call(
    rbind,
    lapply(term_ids, function(nid) {
      w <- coef(tree, node = nid, log = FALSE)
      data.frame(id = nid, learner = names(w), worth = as.numeric(w))
    })
  )

  # Order learners by overall mean worth (ascending, so best is at top after coord_flip)
  learner_order <- tapply(worth_df$worth, worth_df$learner, mean)
  worth_df$learner <- factor(worth_df$learner, levels = names(sort(learner_order)))

  ggparty(tree, terminal_space = 0.6) +
    geom_edge(linewidth = 0.8) +
    geom_edge_label(size = 3.5) +
    geom_node_label(
      aes(label = splitvar),
      ids = "inner",
      size = 4,
      fontface = "bold"
    ) +
    geom_node_label(
      aes(label = paste0("Node ", id, ", n = ", nodesize)),
      ids = "terminal",
      size = 3,
      nudge_y = 0.01
    ) +
    geom_node_plot(
      gglist = list(
        geom_col(
          data = worth_df,
          aes(x = learner, y = worth),
          width = 0.7,
          fill = "steelblue"
        ),
        coord_flip(),
        labs(x = NULL, y = "Worth"),
        theme_minimal(base_size = 9),
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
        )
      ),
      shared_axis_labels = TRUE,
      shared_legend = FALSE,
      ids = "terminal"
    ) +
    theme_void()
}

# -- Analysis function ------------------------------------------------------
run_pl_tree <- function(
  scores_all,
  measure,
  minimize,
  learners,
  tasktab,
  plot_name,
  alpha = 0.05,
  width = 10,
  height = 7
) {
  cli_h1("Plackett-Luce tree: {measure} / {plot_name} (alpha = {alpha})")

  scores <- scores_all[grepl(pattern = measure, tune_measure)]
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]

  dat <- build_pltree_data(scores_avg, measure, minimize, learners, tasktab)

  cli_alert_info("Learners ({length(dat$learner_ids)}): {paste(dat$learner_ids, collapse = ', ')}")

  tree <- pltree(
    G ~ noverp + ph_violated + censprop,
    data = dat$model_data,
    alpha = alpha,
    maxdepth = 3,
    minsize = 5,
    trace = TRUE
  )
  tree

  cli_h2("Instability tests (root node)")
  print(sctest(tree, node = 1))

  cli_h2("Worth parameters")
  print(sort(coef(tree, log = FALSE), decreasing = TRUE))

  if (length(tree) > 1) {
    p <- plot_pltree_gg(tree)
    save_plot(p, name = paste0("pltree_", plot_name, "_", measure), width = width, height = height, formats = "pdf")
  }

  invisible(tree)
}

# -- Learner sets -----------------------------------------------------------
exclude <- c("KM", "NEL")
all_learners <- function(scores_all, measure) {
  setdiff(unique(scores_all[grepl(pattern = measure, tune_measure), learner_id]), exclude)
}
representative <- c("CPH", "RFSRC", "CoxB", "XGBAFT")

# -- Run --------------------------------------------------------------------
# Representative subset (fast, more power to detect splits)
res_rep_hc <- run_pl_tree(
  scores_all,
  "harrell_c",
  FALSE,
  representative,
  tasktab,
  "representative",
  width = 10,
  height = 7
)
res_rep_isbs <- run_pl_tree(scores_all, "isbs", TRUE, representative, tasktab, "representative", width = 10, height = 7)

# All learners (slow, lower power)
res_all_hc <- run_pl_tree(
  scores_all,
  "harrell_c",
  FALSE,
  all_learners(scores_all, "harrell_c"),
  tasktab,
  "all_learners",
  width = 12,
  height = 8
)
res_all_isbs <- run_pl_tree(
  scores_all,
  "isbs",
  TRUE,
  all_learners(scores_all, "isbs"),
  tasktab,
  "all_learners",
  width = 12,
  height = 8
)
