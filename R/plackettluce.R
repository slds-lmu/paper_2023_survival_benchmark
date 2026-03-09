# Shared Plackett-Luce helper and analysis functions
#
# Sourced by scripts/pl_*.R via source("R/plackettluce.R").
# Assumes R/helpers.R and R/plotting.R are already loaded (via .Rprofile).

# -- Shared helpers -----------------------------------------------------------

#' Prepare rankings for Plackett-Luce analysis
#'
#' Filters scores by measure, averages per learner-task, ranks, and builds
#' a PlackettLuce rankings object.
#'
#' @param scores_all data.table of all scores.
#' @param measure Character, measure ID column name.
#' @param minimize Logical, whether lower scores are better.
#' @param exclude Character vector of learner IDs to exclude.
#' @return A list with components: rankings, task_ids, learner_ids, scores_avg.
pl_prepare_rankings <- function(scores_all, measure, minimize, exclude = c("KM", "NEL")) {
  scores <- scores_all[grepl(pattern = measure, tune_measure) & !learner_id %in% exclude]
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]
  scores_avg[, rank_score := frank(if (minimize) score else -score, ties.method = "random"), by = task_id]

  ranks_wide <- dcast(scores_avg, task_id ~ learner_id, value.var = "rank_score")
  task_ids <- ranks_wide$task_id
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- task_ids
  rankings <- PlackettLuce::as.rankings(rank_mat)
  stopifnot(!anyNA(rank_mat))

  list(rankings = rankings, task_ids = task_ids, learner_ids = learner_ids, scores_avg = scores_avg)
}

#' Build data for pltree (rankings + task covariates)
#'
#' @param scores_avg data.table with columns learner_id, task_id, score.
#' @param measure Character, measure ID.
#' @param minimize Logical.
#' @param learner_subset Character vector of learner IDs to include.
#' @param tasktab data.table of task metadata (must have noverp, n, p, ph_violated, etc.).
#' @return A list with rankings, model_data (data.frame for pltree), learner_ids.
build_pltree_data <- function(scores_avg, measure, minimize, learner_subset, tasktab) {
  dt <- scores_avg[learner_id %in% learner_subset]
  dt[, rank_score := frank(if (minimize) score else -score, ties.method = "random"), by = task_id]

  ranks_wide <- dcast(dt, task_id ~ learner_id, value.var = "rank_score")
  task_ids <- ranks_wide$task_id
  learner_ids <- setdiff(names(ranks_wide), "task_id")

  rank_mat <- as.matrix(ranks_wide[, learner_ids, with = FALSE])
  rownames(rank_mat) <- task_ids
  rankings <- PlackettLuce::as.rankings(rank_mat)

  tt <- copy(tasktab)[match(task_ids, task_id)]
  stopifnot(all.equal(tt$task_id, task_ids))

  G <- PlackettLuce::group(rankings, index = seq_len(nrow(rankings)))
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

#' Run Plackett-Luce subgroup analysis
#'
#' Fits separate PL models per subgroup, computes worth parameters and quasi-variances,
#' builds a rank comparison table, and produces a faceted quasi-SE plot.
#'
#' @param rankings A PlackettLuce rankings object (from `pl_prepare_rankings`).
#' @param subgroups A named list of index vectors, one per subgroup.
#'   Names are used as subgroup labels.
#' @param measure Character, measure ID for labeling.
#' @param plot_name Character, base name for the saved plot file.
#' @param reference Character, learner to re-reference log-worth to (default "CPH").
#' @param width,height Plot dimensions.
#' @return Invisibly, a list with pl_fits (named list of PL model fits) and rank_comparison.
pl_subgroup_analysis <- function(
  rankings,
  subgroups,
  measure,
  plot_name,
  measure_label = NULL,
  reference = "CPH",
  width = 12,
  height = 6
) {
  if (is.null(measure_label)) {
    m_ <- measure
    measure_label <- measures_tbl()[id == m_, label]
  }
  subgroup_names <- names(subgroups)

  # Fit PL models per subgroup
  pl_fits <- list()
  qvdt_list <- list()

  for (nm in subgroup_names) {
    idx <- subgroups[[nm]]
    cli::cli_h2("{nm} (n = {length(idx)})")

    pl_fit <- PlackettLuce::PlackettLuce(rankings[idx, ], trace = TRUE)
    pl_fits[[nm]] <- pl_fit

    cli::cli_alert_info("Log-likelihood: {round(logLik(pl_fit), 2)}")
    cli::cli_alert_info("AIC: {round(AIC(pl_fit), 2)}")

    worth <- coef(pl_fit, log = FALSE)
    cli::cli_alert_info("Worth (sorted):")
    print(sort(worth, decreasing = TRUE))

    qv <- PlackettLuce::qvcalc(pl_fit)
    qvdt <- as.data.table(qv$qvframe, keep.rownames = "learner")
    qvdt[, subgroup := nm]
    qvdt_list[[nm]] <- qvdt
  }

  # Rank comparison table (first subgroup as reference)
  worths <- lapply(pl_fits, function(fit) coef(fit, log = FALSE))
  ref_worth <- worths[[1]]
  rank_comparison <- data.table(
    learner = names(sort(ref_worth, decreasing = TRUE)),
    rank_1 = seq_along(ref_worth),
    worth_1 = sort(ref_worth, decreasing = TRUE)
  )
  setnames(rank_comparison, c("rank_1", "worth_1"), paste0(c("rank_", "worth_"), subgroup_names[[1]]))
  for (i in seq_along(worths)[-1]) {
    nm <- subgroup_names[[i]]
    w <- worths[[i]]
    rank_comparison[, paste0("rank_", nm) := match(learner, names(sort(w, decreasing = TRUE)))]
    rank_comparison[, paste0("worth_", nm) := w[learner]]
  }

  cli::cli_h2("Rank comparison")
  print(rank_comparison)

  # Quasi-variance plot
  qvdt <- rbindlist(qvdt_list)
  qvdt[, estimate_vs_ref := estimate - estimate[learner == reference], by = subgroup]
  qvdt[, learner_ord := reorder(learner, estimate_vs_ref, FUN = mean)]

  p <- ggplot2::ggplot(qvdt, ggplot2::aes(x = learner_ord, y = estimate_vs_ref)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(
      ymin = estimate_vs_ref - quasiSE,
      ymax = estimate_vs_ref + quasiSE
    )) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~subgroup) +
    ggplot2::labs(
      x = NULL,
      y = glue::glue("Log-worth relative to {reference} (quasi-SE)"),
      caption = glue::glue("Measure: {measure_label}")
    ) +
    ggplot2::theme_minimal()

  save_plot(p, name = plot_name, width = width, height = height, formats = "pdf")

  invisible(list(pl_fits = pl_fits, rank_comparison = rank_comparison))
}

# -- Analysis functions -------------------------------------------------------

#' Full Plackett-Luce ranking (all learners)
run_pl_ranking <- function(scores_all, measure, minimize, exclude, result_path) {
  cli::cli_h1("Plackett-Luce ranking: {measure}")
  m_ <- measure
  measure_label <- measures_tbl()[id == m_, label]

  prep <- pl_prepare_rankings(scores_all, measure, minimize, exclude)
  rankings <- prep$rankings

  # Fit PL model
  pl_fit <- PlackettLuce::PlackettLuce(rankings, trace = TRUE)

  cli::cli_alert_info("Log-likelihood: {round(logLik(pl_fit), 2)}")
  cli::cli_alert_info("AIC: {round(AIC(pl_fit), 2)}")
  cli::cli_alert_info("Iterations: {pl_fit$iter}")

  cli::cli_h2("Log-worth parameters")
  print(coef(pl_fit))

  worth <- coef(pl_fit, log = FALSE)
  cli::cli_h2("Worth parameters (sorted)")
  print(sort(worth, decreasing = TRUE))

  # Quasi-variances for SE estimation
  qv <- PlackettLuce::qvcalc(pl_fit)
  qvdt <- as.data.table(qv$qvframe, keep.rownames = "learner")

  # Plot 1: log-worth with quasi-SEs, sorted by estimate
  qvdt[, learner := reorder(learner, estimate)]

  p1 <- ggplot2::ggplot(qvdt, ggplot2::aes(x = learner, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = estimate - quasiSE, ymax = estimate + quasiSE)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Log-worth (quasi-SE)", caption = glue::glue("Measure: {measure_label}")) +
    ggplot2::theme_minimal()

  save_plot(p1, name = paste0("pl_worth_", measure), width = 8, height = 6, formats = "pdf")

  # Plot 2: re-referenced to CPH
  cph_estimate <- qvdt[learner == "CPH", estimate]
  qvdt[, estimate_vs_cph := estimate - cph_estimate]
  qvdt[, learner_cph := reorder(learner, estimate_vs_cph)]

  p2 <- ggplot2::ggplot(qvdt, ggplot2::aes(x = learner_cph, y = estimate_vs_cph)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = estimate_vs_cph - quasiSE, ymax = estimate_vs_cph + quasiSE)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Log-worth relative to CPH (quasi-SE)",
      caption = glue::glue("Measure: {measure_label}")
    ) +
    ggplot2::theme_minimal()

  save_plot(p2, name = paste0("pl_worth_", measure, "_cphref"), width = 8, height = 6, formats = "pdf")

  invisible(list(pl_fit = pl_fit, qv = qv, worth = worth))
}

#' Plackett-Luce tree analysis
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
  cli::cli_h1("Plackett-Luce tree: {measure} / {plot_name} (alpha = {alpha})")
  cli::cli_alert_info("Covariates: {paste(covariates, collapse = ', ')}")

  scores <- scores_all[grepl(pattern = measure, tune_measure)]
  scores_avg <- scores[, .(score = mean(get(measure), na.rm = TRUE)), by = .(learner_id, task_id)]

  dat <- build_pltree_data(scores_avg, measure, minimize, learners, tasktab)

  cli::cli_alert_info("Learners ({length(dat$learner_ids)}): {paste(dat$learner_ids, collapse = ', ')}")

  fml <- reformulate(covariates, response = "G")

  # do.call forces evaluation of all arguments before pltree captures them
  # via match.call(). Without this, mob() re-evaluates e.g. `alpha` in an
  # internal environment where the caller's variables don't exist.
  tree <- do.call(
    PlackettLuce::pltree,
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

  cli::cli_h2("Instability tests (root node)")
  print(strucchange::sctest(tree, node = 1))

  cli::cli_h2("Worth parameters")
  print(sort(coef(tree, log = FALSE), decreasing = TRUE))

  if (length(tree) > 1) {
    m_ <- measure
    measure_label <- measures_tbl()[id == m_, label]
    p <- plot_pltree_gg(tree, caption = glue::glue("Measure: {measure_label}"))
    save_plot(p, name = paste0("pltree_", plot_name, "_", measure), width = width, height = height, formats = "pdf")
  }

  invisible(tree)
}

#' PH subgroup analysis
run_pl_ph_subgroups <- function(scores_all, measure, minimize, exclude, tasktab) {
  cli::cli_h1("PH subgroup analysis: {measure}")

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

#' Censoring proportion subgroup analysis
run_pl_censprop_subgroups <- function(scores_all, measure, minimize, exclude, tasktab) {
  cli::cli_h1("Censoring proportion subgroup analysis: {measure}")

  prep <- pl_prepare_rankings(scores_all, measure, minimize, exclude)

  # Split at median censoring proportion
  tt <- tasktab[match(prep$task_ids, task_id)]
  cens_med <- median(tt$censprop)
  lo_idx <- which(tt$censprop <= cens_med)
  hi_idx <- which(tt$censprop > cens_med)

  cli::cli_alert_info("Median censoring proportion: {round(cens_med, 3)}")

  subgroups <- setNames(
    list(lo_idx, hi_idx),
    c(paste0("Low censoring (<= ", round(cens_med, 2), ")"), paste0("High censoring (> ", round(cens_med, 2), ")"))
  )

  pl_subgroup_analysis(
    rankings = prep$rankings,
    subgroups = subgroups,
    measure = measure,
    plot_name = paste0("pl_worth_censprop_subgroups_", measure)
  )
}
