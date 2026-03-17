# This script creates all plots required for the submitted paper (main text and appendices),
# putting them in the `results_paper` directory.
# Putting this script together last minute has taught me that I really should have
# a) started with this rather than the quarto doc
# b) used targets instead of re-running the script a bunch.

# Setup ---------------------------------------------------------------------------------------
if (!exists(".canary")) {
  # Source only if not already sourced via .Rprofile
  source(here::here("R/helpers.R"))
  source(here::here("R/plotting.R"))
}

# Packages
# requires package PMCMRplus, was not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(mlr3benchmark)
library(mlr3proba)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(data.table)

# Load results --------------------------------------------------------------------------------
plot_path = here::here("results_paper")
lrntab = load_lrntab()
tasktab = load_tasktab()

# Helper table to collect all measures and their attributes
msr_tbl = load_msr_table()
# bma is the BenchmarkAggr for use with mlr3benchmark
bma_harrell_c = readRDS(fs::path(conf$result_path, "bma_harrell_c.rds"))
bma_isbs = readRDS(fs::path(conf$result_path, "bma_isbs.rds"))

# These are data.tables with additional columns for presentation
aggr_scores = readRDS(fs::path(conf$result_path, "aggr.rds"))
scores = readRDS(fs::path(conf$result_path, "scores.rds"))

aggr_scores[, learner_id := factor(learner_id, lrntab$id)]
scores[, learner_id := factor(learner_id, lrntab$id)]

# Create the scaled version of aggregated scores where KM is 0 and best model is 1
aggr_scores_scaled = rescale_aggr_scores(aggr_scores, msr_tbl)

stopifnot(any(aggr_scores_scaled[grepl("harrell_c", tune_measure), harrell_c] == 1))
stopifnot(!aggr_scores_scaled[grepl("isbs", tune_measure), isbs] > 1)
stopifnot(!aggr_scores_scaled[grepl("isbs", tune_measure), isbs] < 0)

# Table of errors -----------------------------------------------------------------------------
cli::cli_h1("Producing paper tables")

cli::cli_h2("Error table")

errs_table = scores |>
  summarise(
    affected_iters = sum(errors_cnt > 0),
    total_iters = n(),
    errors_perc = 100 * affected_iters / total_iters,
    .by = c(learner_id, task_id, tune_measure)
  ) |>
  filter(affected_iters > 0) |>
  tidyr::pivot_wider(
    id_cols = c("learner_id", "task_id"),
    names_from = "tune_measure",
    values_from = c("affected_iters", "errors_perc"),
    values_fill = 0
  ) |>
  dplyr::mutate(total = affected_iters_harrell_c + affected_iters_isbs)

errs_table |>
  mutate(
    harrell_c = glue::glue("{affected_iters_harrell_c} ({round(errors_perc_harrell_c, 1)}%)"),
    isbs = glue::glue("{affected_iters_isbs} ({round(errors_perc_isbs, 1)}%)")
  ) |>
  arrange(learner_id, task_id) |>
  select(learner_id, task_id, harrell_c, isbs, total) |>
  kableExtra::kbl(
    col.names = c("Model", "Dataset", "Harrell's C", "ISBS", "Total Errors"),
    caption = "Number of errors per outer resampling iteration (up to 30), separated by model, dataset, and tuning measure.\\label{tab:errors}",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    format = "latex"
  ) |>
  kableExtra::kable_styling() |>
  readr::write_lines(fs::path(plot_path, "errors-table.tex"))
#kableExtra::add_header_above(c(" " = 2, "Tuning Measure" = 2, " " = 1))

# Plots ------------------------------------------------------------------
cli::cli_h1("Producing paper plots")
cli::cli_h2("Critical Difference Plots")

save_cd_plot = function(p, tuning_measure, formats = "pdf") {
  save_plot(
    p,
    name = paste0("critical-difference-baseline-diff-", tuning_measure),
    plot_path = plot_path,
    height = 6.25 / 1.5,
    width = 10 / 1.5,
    formats = formats
  )
}

cd_ratio = 0.8

p = plot_bma(
  bma = bma_harrell_c,
  type = "cd_bd",
  measure_id = "harrell_c",
  tuning_measure_id = "harrell_c",
  ratio = cd_ratio,
  baseline = "CPH"
)
save_cd_plot(p, "harrell_c")

p = plot_bma(
  bma = bma_isbs,
  type = "cd_bd",
  measure_id = "isbs",
  tuning_measure_id = "isbs",
  ratio = cd_ratio,
  baseline = "CPH"
)
save_cd_plot(p, "isbs")


cli::cli_h2("Aggregated Boxplots")

save_aggr_plot = function(
  p,
  type = c("box", "violin"),
  eval_measure_id,
  tuning_measure_id,
  tag = "score",
  width = 8,
  height = 6.5,
  formats = "pdf"
) {
  type = match.arg(type)
  prefix = if (type == "box") "aggr-boxplot" else "aggr-violin"
  save_plot(
    p,
    name = paste(prefix, tuning_measure_id, eval_measure_id, tag, sep = "-"),
    plot_path = plot_path,
    width = width,
    height = height,
    formats = formats
  )
}


cli::cli_h3("Discrimination (raw scores)")
for (measure_id in msr_tbl[(type == "Discrimination") & !erv, id]) {
  for (ptype in c("box", "violin")) {
    plot_data = if (ptype == "violin") aggr_scores[!(learner_id %in% c("KM", "NEL"))] else aggr_scores
    p = plot_aggr_scores(
      plot_data,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "harrell_c",
      dodge = FALSE,
      flip = TRUE
    )
    save_aggr_plot(p, type = ptype, eval_measure_id = measure_id, tuning_measure_id = "harrell_c")
  }
}

cli::cli_h3("Discrimination (scaled)")
for (measure_id in msr_tbl[(type == "Discrimination") & !erv, id]) {
  for (ptype in c("box", "violin")) {
    ptype_label = if (ptype == "box") "Boxplot" else "Violin plot"
    plot_data = if (ptype == "violin") aggr_scores_scaled[!(learner_id %in% c("KM", "NEL"))] else aggr_scores_scaled
    p = plot_aggr_scores(
      plot_data,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "harrell_c",
      dodge = FALSE,
      flip = TRUE
    ) %+%
      labs(
        title = glue::glue("{msr_tbl[id == measure_id, label]} [Scaled]"),
        subtitle = glue::glue(
          "{ptype_label} of aggregated scores across all tasks\nScaled such that 0 = KM, 1 = Best model"
        )
      )
    save_aggr_plot(p, type = ptype, eval_measure_id = measure_id, tuning_measure_id = "harrell_c", tag = "scaled")
  }
}


cli::cli_h3("Scoring rules (raw scores)")
for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  for (ptype in c("box", "violin")) {
    plot_data = if (ptype == "violin") aggr_scores[!(learner_id %in% c("KM", "NEL"))] else aggr_scores
    p = plot_aggr_scores(
      plot_data,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "isbs",
      dodge = FALSE,
      flip = TRUE
    )
    save_aggr_plot(p, type = ptype, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "score")
  }
}

cli::cli_h3("Scoring rules (ERV)")
for (measure_id in msr_tbl[type == "Scoring Rule" & erv, id]) {
  for (ptype in c("box", "violin")) {
    plot_data = if (ptype == "violin") aggr_scores[!(learner_id %in% c("KM", "NEL"))] else aggr_scores
    p = plot_aggr_scores(
      plot_data,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "isbs",
      dodge = FALSE,
      flip = TRUE
    )
    save_aggr_plot(p, type = ptype, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "erv")
  }
}

cli::cli_h3("Scoring rules (ERV, without outlier learners)")
for (measure_id in msr_tbl[type == "Scoring Rule" & erv, id]) {
  for (ptype in c("box", "violin")) {
    exclude = if (ptype == "violin") c("AK", "NCV", "KM", "NEL") else c("AK", "NCV")
    p = plot_aggr_scores(
      aggr_scores[!(learner_id %in% exclude)],
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "isbs",
      dodge = FALSE,
      flip = TRUE
    )
    save_aggr_plot(
      p,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "isbs",
      tag = "erv-no-outlier-learners"
    )
  }
}


cli::cli_h3("Scoring rules (scaled)")
for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  for (ptype in c("box", "violin")) {
    ptype_label = if (ptype == "box") "Boxplot" else "Violin plot"
    plot_data = if (ptype == "violin") aggr_scores_scaled[!(learner_id %in% c("KM", "NEL"))] else aggr_scores_scaled
    p = plot_aggr_scores(
      plot_data,
      type = ptype,
      eval_measure_id = measure_id,
      tuning_measure_id = "isbs",
      dodge = FALSE,
      flip = TRUE
    ) %+%
      labs(
        title = glue::glue("{msr_tbl[id == measure_id, label]} [Scaled]"),
        subtitle = glue::glue(
          "{ptype_label} of aggregated scores across all tasks\nScaled such that 0 = KM, 1 = Best model"
        )
      )
    save_aggr_plot(p, type = ptype, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "scaled")
  }
}

cli::cli_h3("Combined scaling comparison for ISBS (raw / ERV / scaled)")
measure_normal = "isbs"
measure_erv = paste0(measure_normal, "_erv")
measure_scaled = paste0(measure_normal, "_scaled")

aggr_temp = data.table::copy(aggr_scores[,
  .SD,
  .SDcols = c("task_id", "learner_id", "tune_measure", "learner_group", measure_normal, measure_erv)
])
aggr_scaled_temp = data.table::copy(aggr_scores_scaled[,
  .SD,
  .SDcols = c("task_id", "learner_id", "tune_measure", "learner_group", measure_normal)
])
data.table::setnames(aggr_scaled_temp, old = measure_normal, new = measure_scaled)
aggr_temp = aggr_temp[aggr_scaled_temp, on = .(task_id, learner_id, tune_measure, learner_group)]

p_base = aggr_temp |>
  dplyr::filter(grepl("isbs", .data[["tune_measure"]])) |>
  dplyr::filter(isbs_scaled <= 1, isbs_scaled >= 0) |>
  dplyr::filter(!(learner_id %in% c("AK", "NCV", "KM", "NEL"))) |>
  tidyr::pivot_longer(
    cols = tidyselect::all_of(c(measure_normal, measure_erv, measure_scaled)),
    names_to = "measure",
    values_to = "score"
  ) |>
  dplyr::mutate(
    measure = dplyr::case_when(
      measure == measure_normal ~ "a) Raw Scores",
      measure == measure_erv ~ "b) ERV",
      measure == measure_scaled ~ "c) Scaled"
    ),
    measure = forcats::fct_inorder(measure),
    learner_id = factor(learner_id, levels = lrntab$id)
  ) |>
  ggplot(aes(x = score, y = learner_id, color = learner_group, fill = learner_group)) +
  facet_wrap(vars(measure), scales = "free", ncol = 3) +
  scale_color_manual(values = palette_groups, aesthetics = c("color", "fill")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  )

p_threes_box = p_base +
  geom_boxplot(alpha = 1 / 4, key_glyph = "rect") +
  labs(
    title = "Integrated Survival Brier Score (ISBS)",
    subtitle = "Boxplot of aggregated scores",
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL
    # caption = "AK omitted for largely out of scale values"
  )

p_threes_violin = p_base +
  geom_violin(alpha = 1 / 4, key_glyph = "rect", draw_quantiles = c(.25, .75)) +
  labs(
    title = "Integrated Survival Brier Score (ISBS)",
    subtitle = "Violin plot of aggregated scores",
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL
    # caption = "AK omitted for largely out of scale values"
  )

save_plot(
  p = p_threes_box,
  name = "aggr-boxplot-threes-isbs-isbs",
  plot_path = plot_path,
  width = 12,
  height = 5,
  formats = "pdf"
)

save_plot(
  p = p_threes_violin,
  name = "aggr-violin-threes-isbs-isbs",
  plot_path = plot_path,
  width = 12,
  height = 5,
  formats = "pdf"
)


cli::cli_h2("Calibration plots")
cli::cli_h3("D-Calibration heatmap")

p = aggr_scores |>
  dplyr::filter(grepl("isbs", .data[["tune_measure"]])) |>
  dplyr::mutate(
    dcalib_p = pchisq(dcalib, 10 - 1, lower.tail = FALSE),
    dcalib_label = fifelse(dcalib_p < 0.05, "X", "")
  ) |>
  ggplot(aes(x = forcats::fct_reorder(learner_id, dcalib_p), y = forcats::fct_rev(task_id), fill = dcalib_p)) +
  geom_tile(color = "#EEEEEE") +
  geom_text(aes(label = dcalib_label), color = "white", size = 3) +
  # scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "blue"), labels = c(`TRUE` = "Signif.", `FALSE` = "Not Signif.")) +
  scale_fill_viridis_c(breaks = seq(0, 1, .1)) +
  guides(
    x = guide_axis(n.dodge = 2),
    fill = guide_colorbar(
      title.vjust = .8,
      barwidth = unit(200, "pt")
    )
  ) +
  labs(
    title = "D-Calibration p-values by task and learner",
    subtitle = glue::glue(
      "Models tuned on {msr_tbl[id == 'isbs', label]}\n",
      "Learners ordered by average p-value. X denotes p < 0.05"
    ),
    y = "Task",
    x = "Learner",
    color = NULL,
    fill = "p-value"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(5, "mm"),
    panel.background = element_rect(fill = "#EEEEEE", color = "#EEEEEE")
  )

save_plot(
  p,
  name = paste("calib-d-heatmap", "isbs", sep = "-"),
  plot_path = plot_path,
  width = 8,
  height = 8,
  formats = "pdf"
)

cli::cli_h3("Alpha-Calibration (distribution)")
p_alpha_dist = aggr_scores |>
  dplyr::filter(grepl("isbs", .data[["tune_measure"]])) |>
  dplyr::filter(!(learner_id %in% c("AK", "NCV"))) |>
  ggplot(aes(x = alpha_calib)) +
  facet_wrap(vars(learner_id), scales = "free_y", ncol = 2) +
  geom_histogram(alpha = 1 / 4, color = "gray", fill = "darkgray") +
  geom_density(aes(y = after_stat(density)), color = "black") +
  geom_vline(xintercept = 1, color = "darkred") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  # scale_x_log10() +
  labs(
    title = glue::glue("Alpha-Calibration scores across tasks (tuned on ISBS)"),
    y = "Count",
    x = "Alpha"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = rel(0.75))
    # panel.spacing.x = unit(5, "mm"),
    # panel.background = element_rect(fill = "#EEEEEE", color = "#EEEEEE")
  )

save_plot(
  p_alpha_dist,
  name = paste("calib-alpha-ratio-plot-dist", "isbs", sep = "-"),
  plot_path = plot_path,
  width = 7,
  height = 8.5,
  formats = "pdf"
)

cli::cli_h2("Score plots per dataset")
scores[, learner_id := factor(learner_id, levels = rev(lrntab$id))]

save_scores_plot = function(
  p,
  type = c("box", "violin"),
  eval_measure_id,
  tuning_measure_id,
  grouping,
  width,
  height,
  formats = c("png", "pdf")
) {
  type = match.arg(type)
  prefix = if (type == "box") "scores-boxplot" else "scores-violin"
  save_plot(
    p,
    name = paste(prefix, tuning_measure_id, eval_measure_id, grouping, sep = "-"),
    plot_path = plot_path,
    width = width,
    height = height,
    formats = formats
  )
}

# future::plan("multisession", workers = parallelly::availableCores() - 1)

cli::cli_h3("Discrimination measures")
for (measure_id in msr_tbl[type == "Discrimination" & !erv, id]) {
  for (ptype in c("box", "violin")) {
    for (scores_color_var in c("learner_group", "learner_hspace")) {
      # future::future({
      p = plot_scores(
        scores[!(learner_id %in% c("KM", "NEL"))],
        type = ptype,
        eval_measure_id = measure_id,
        tuning_measure_id = "harrell_c",
        dodge = FALSE,
        flip = TRUE,
        ncol = 5,
        color_var = scores_color_var,
        msr_tbl = msr_tbl,
        tasktab = tasktab
      )
      save_scores_plot(
        p,
        type = ptype,
        eval_measure_id = measure_id,
        tuning_measure_id = "harrell_c",
        grouping = scores_color_var,
        width = 10,
        height = 17,
        formats = "pdf"
      )
      # })
    }
  }
}

cli::cli_h3("Scoring rules")
for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  for (ptype in c("box", "violin")) {
    for (scores_color_var in c("learner_group", "learner_hspace")) {
      # future::future({
      p = plot_scores(
        scores,
        type = ptype,
        eval_measure_id = measure_id,
        tuning_measure_id = "isbs",
        dodge = FALSE,
        flip = TRUE,
        ncol = 5,
        color_var = scores_color_var,
        msr_tbl = msr_tbl,
        tasktab = tasktab
      )
      save_scores_plot(
        p,
        type = ptype,
        eval_measure_id = measure_id,
        tuning_measure_id = "isbs",
        grouping = scores_color_var,
        width = 10,
        height = 16,
        formats = "pdf"
      )
      # })
    }
  }
}
