# This script creates all plots required for the submitted paper (main text and appendices),
# putting them in the `results_paper` directory.
# Putting this script together last minute has taught me that I really should have
# a) started with this rather than the quarto doc
# b) used targets instead of re-running the script a bunch.

# Setup ---------------------------------------------------------------------------------------
if (!exists(".canary")) {
  source(here::here("R/helpers.R"))
} # Source only if not already sourced via .Rprofile

# Packages
# requires package PMCMRplus, not included in renv because of issues installing it on cluster (libmpfr.so.6)
library(mlr3benchmark)
library(mlr3proba)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(data.table)

# Load results --------------------------------------------------------------------------------
plot_path = here::here("results_paper")
stopifnot(ensure_directory(plot_path))
lrntab = load_lrntab()
# Helper table to collect all measures and their attributed
msr_tbl = measures_tbl()
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

# Table of errors -----------------------------------------------------------------------------

errs_table = scores |>
  summarise(
    affected_iters = sum(errors_cnt),
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
    caption = "Number of errors per outer resampling iteration (up to five), separated by model, dataset, and tuning measure.",
    booktabs = TRUE,
    format = "latex"
  ) |>
  kableExtra::kable_styling() |>
  readr::write_lines(fs::path(plot_path, "errors-table.tex"))
#kableExtra::add_header_above(c(" " = 2, "Tuning Measure" = 2, " " = 1))

# Critical Difference Plots -------------------------------------------------------------------
cli::cli_h2("Critical Difference Plots")

cd_ratio = 10 / 12

save_cd_plot = function(p, name) {
  cli::cli_alert_info("Saving critical difference plot {.val {name}}")
  ggsave(
    plot = p,
    filename = fs::path(plot_path, paste0("critical-difference-baseline-diff-", name), ext = "png"),
    width = 10,
    height = 6.25,
    dpi = 300,
    bg = "white"
  )
}

# critical-difference-baseline-diff-harrell-c-harrell-c
p = plot_results(
  bma = bma_harrell_c,
  type = "cd_bd",
  measure_id = "harrell_c",
  tuning_measure_id = "harrell_c",
  ratio = cd_ratio,
  baseline = "CPH"
)
save_cd_plot(p, "harrell_c")

# critical-difference-baseline-diff-isbs-isbs
p = plot_results(
  bma = bma_isbs,
  type = "cd_bd",
  measure_id = "isbs",
  tuning_measure_id = "isbs",
  ratio = cd_ratio,
  baseline = "CPH"
)
save_cd_plot(p, "isbs")


cli::cli_h2("Aggregated Boxplots")

save_boxplot_plot = function(p, eval_measure_id, tuning_measure_id, tag = "score", width = 8.25, height = 6) {
  cli::cli_alert_info(
    "Saving aggregated boxplot for {.val {eval_measure_id}} tuned on {.val {tuning_measure_id}} ({tag})"
  )
  ggsave(
    plot = p,
    filename = fs::path(
      plot_path,
      paste("aggr-boxplot", eval_measure_id, tuning_measure_id, tag, sep = "-"),
      ext = "png"
    ),
    width = width,
    height = height,
    dpi = 300,
    bg = "white" #,
    #device = ragg::agg_png
  )
}


# Harrell's C Boxplots ---------------------------------------------------
# Harrell's C, raw scores

for (measure_id in msr_tbl[(type == "Discrimination") & !erv, id]) {
  p = plot_aggr_scores(
    aggr_scores,
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "harrell_c",
    dodge = FALSE,
    flip = TRUE
  )
  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "harrell_c")
}

# Harrell's C (Scaled)

# aggr-boxplot-harrell-c-scaled

for (measure_id in msr_tbl[(type == "Discrimination") & !erv, id]) {
  p = plot_aggr_scores(
    aggr_scores_scaled,
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "harrell_c",
    dodge = FALSE,
    flip = TRUE
  ) %+%
    labs(
      title = glue::glue("{msr_tbl[id == measure_id, label]} [Scaled]"),
      subtitle = "Boxplot of aggregated scores across all tasks\nScaled such that 0 = KM, 1 = Best model"
    )

  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "harrell_c", tag = "scaled")
}


# ISBS Boxplots ----------------------------------------------------------
# ISBS (Raw scores)

for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  p = plot_aggr_scores(
    aggr_scores,
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "isbs",
    dodge = FALSE,
    flip = TRUE
  )
  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "score")
}

# ISBS (ERV)

for (measure_id in msr_tbl[type == "Scoring Rule" & erv, id]) {
  p = plot_aggr_scores(
    aggr_scores,
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "isbs",
    dodge = FALSE,
    flip = TRUE
  )
  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "erv")
}

# ISBS (ERV) without AK
for (measure_id in msr_tbl[type == "Scoring Rule" & erv, id]) {
  p = plot_aggr_scores(
    aggr_scores[learner_id != "AK"],
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "isbs",
    dodge = FALSE,
    flip = TRUE
  )
  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "erv-noAK")
}


# Scaled ISBS Boxplots ---------------------------------------------------
# Scaled ISBS

#aggr-boxplot-isbs-scaled}

for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  p = plot_aggr_scores(
    aggr_scores_scaled,
    type = "box",
    eval_measure_id = measure_id,
    tuning_measure_id = "isbs",
    dodge = FALSE,
    flip = TRUE
  ) %+%
    labs(
      title = glue::glue("{msr_tbl[id == measure_id, label]} [Scaled]"),
      subtitle = "Boxplot of aggregated scores across all tasks\nScaled such that 0 = KM, 1 = Best model"
    )
  save_boxplot_plot(p, eval_measure_id = measure_id, tuning_measure_id = "isbs", tag = "scaled")
}

# Aggregated Boxplots with 3 types of scaling -------------------------------------------------

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

p = aggr_temp |>
  dplyr::filter(grepl("isbs", .data[["tune_measure"]])) |>
  dplyr::filter(isbs_scaled <= 1 & isbs_scaled >= 0) |>
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
  geom_boxplot(alpha = 1 / 4, key_glyph = "rect") +
  scale_color_manual(values = palette_groups, aesthetics = c("color", "fill")) +
  labs(
    title = "Integrated Survival Brier Score (ISBS)",
    subtitle = "Boxplot of aggregated scores across all tasks transformations",
    x = NULL,
    y = "Model",
    color = NULL,
    fill = NULL,
    caption = " Tuning measure: Integrated Survival Brier Score (ISBS)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot"
  )

cli::cli_alert_info("Saving three-way-scaling aggregated boxplot")
ggsave(
  p,
  filename = fs::path(plot_path, "aggr-boxplot-threes-isbs-isbs", ext = "png"),
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Aggregated Calibration plots ----------------------------------------------------------------
cli::cli_h2("Aggregated Calibration plots")

# aggr_temp = data.table::copy(aggr_scores)
# aggr_temp[, dcalib_p := pchisq(dcalib, 10 - 1, lower.tail = FALSE)]
# aggr_temp[, dcalib_label := fifelse(dcalib_p < 0.05, "X", "")]

for (tuned_on in c("isbs")) {
  p = aggr_scores |>
    dplyr::filter(grepl(tuned_on, .data[["tune_measure"]])) |>
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
        "Models tuned on {msr_tbl[id == tuned_on, label]}\n",
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

  cli::cli_alert_info("Saving d-calib heatmap, tuned on {.val {tuned_on}}")
  ggsave(
    plot = p,
    fs::path(plot_path, paste("calib-d-heatmap", tuned_on, sep = "-"), ext = "png"),
    width = 8,
    height = 8,
    dpi = 300,
    bg = "white"
  )
}

# Alpha Calibration

for (tuned_on in c("isbs")) {
  p = aggr_scores |>
    dplyr::filter(grepl(tuned_on, .data[["tune_measure"]]), learner_id != "AK") |>
    ggplot(aes(y = forcats::fct_rev(learner_id), x = alpha_calib)) +
    geom_point() +
    geom_vline(xintercept = 1) +
    scale_x_log10() +
    labs(
      title = "Alpha-Calibration by task and learner",
      subtitle = glue::glue(
        "Models tuned on {msr_tbl[id == tuned_on, label]}\n",
        "Values close to 1 indicate reasonable calibration"
      ),
      y = "Learner",
      x = "Alpha (log10)",
      caption = "Omitted AK due to off-scale values"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
      # panel.spacing.x = unit(5, "mm"),
      # panel.background = element_rect(fill = "#EEEEEE", color = "#EEEEEE")
    )

  cli::cli_alert_info("Saving alpha-calib ratio plot, tuned on {.val {tuned_on}}")
  ggsave(
    plot = p,
    fs::path(plot_path, paste("calib-alpha-ratio-plot", tuned_on, sep = "-"), ext = "png"),
    width = 7,
    height = 7,
    dpi = 300,
    bg = "white"
  )
}


# Score Boxplots per Dataset ------------------------------------------------------------------
cli::cli_h2("Score Boxplots per Dataset")

save_boxplot_plot_scores = function(p, eval_measure_id, tuning_measure_id, width = 12, height = 12) {
  cli::cli_alert_info("Saving score boxplot for {.val {measure_id}} tuned on {.val {tuning_measure_id}}")

  ggsave(
    plot = p,
    filename = fs::path(plot_path, paste("scores-boxplot", eval_measure_id, tuning_measure_id, sep = "-"), ext = "png"),
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

for (measure_id in msr_tbl[(id == "isbs" | type == "Discrimination") & !erv, id]) {
  p = plot_scores(scores, eval_measure_id = measure_id, tuning_measure_id = "harrell_c", dodge = FALSE, flip = TRUE)
  save_boxplot_plot_scores(p, eval_measure_id = measure_id, tuning_measure_id = "harrell_c")
}

for (measure_id in msr_tbl[type == "Scoring Rule" & !erv, id]) {
  p = plot_scores(scores, eval_measure_id = measure_id, tuning_measure_id = "isbs", dodge = FALSE, flip = TRUE)
  save_boxplot_plot_scores(p, eval_measure_id = measure_id, tuning_measure_id = "isbs")
}
