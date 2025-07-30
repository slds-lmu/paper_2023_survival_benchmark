# Color palette for learner groups
palette_groups = RColorBrewer::brewer.pal(4, "Dark2")
names(palette_groups) = c("Baseline", "Classical", "Trees", "Boosting")


#' Boxplot of scores separated by violation of PH assumption
#'
#' @param xdf A `data.table` as contained in a `BenchmarkAggr`'s `$data` slot.
#' @param eval_measure E.g. `"harrell_c"`.
#' @param tuning_measure E.g. `"harrell_c"`.
#'
#' @return
#'
#' @example
#' plot_aggr_ph(bma, tuning_measure = "harrell_c")
plot_aggr_ph = function(
  xdf,
  eval_measure = "harrell_c",
  tuning_measure = NULL,
  learners_exclude = NULL,
  tasks_exclude = NULL
) {
  checkmate::assert_data_table(xdf)
  checkmate::assert_subset(c("tune_measure", "p"), choices = colnames(xdf))
  checkmate::assert_subset(eval_measure, colnames(xdf))
  checkmate::assert_subset(tuning_measure, unique(xdf[["tune_measure"]]))

  xdf |>
    dplyr::filter(.data[["tune_measure"]] == tuning_measure) |>
    dplyr::filter(!(eval_measure %in% c("harrell_c", "uno_c")) | !(learner_id %in% c("KM", "NA"))) |>
    ggplot(aes(x = learner_id, y = .data[[eval_measure]], color = p, fill = p)) +
    facet_grid(cols = vars(learner_group), scales = "free_x", space = "free_x") +
    geom_boxplot(alpha = 1 / 4, key_glyph = "rect") +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_brewer(palette = "Dark2", aesthetics = c("color", "fill"), direction = -1) +
    labs(
      title = msr_tbl[id == eval_measure, label],
      subtitle = paste(
        "Performance separated by PH violation.",
        "Based on uncorrected p < 0.05 of global Schoenfeld test per task",
        sep = "\n"
      ),
      caption = glue::glue("Tuned on {msr_tbl[id == tuning_measure, label]}"),
      x = NULL,
      y = msr_tbl[id == eval_measure, label],
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(
      base_size = 15 #,
      # Would want to use custom fonts but reproducibility... :(
      # base_family = "Fira Sans"
    ) +
    theme(
      legend.position = "top",
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot"
    )
}

#' BenchmarkAggr plot wrapper
#'
#' Unified interface for various autoplot.BenchmarkAggr types.
#'
#' @param bma (`BenchmarkAggr`)
#' @param type One of `"mean", "box", "fn"` or `"cd_n"` for Critical Differences (Nemenyi) or
#'   `"cd_bd"` for Critical Differences with baseline comparison
#' @param measure_id,tuning_measure_id (`character(1)`) Measure ids as per `measures_tbl()`.
#' @param flip (`FALSE`) If `TRUE`, applies `coord_flip()`.
#' @param dodge (`TRUE`) If `TRUE`, applies `scale_x_discrete(guide = guide_axis(n.dodge = 2))`.
#' @param ... Additional arguments passed to `autoplot()`.
#'
#' @return
#' @export
#'
#' @example
#' plot_bma(bma_harrell_c, type = "box")
#' plot_bma(bma_harrell_c, type = "mean")
#' plot_bma(bma_harrell_c, type = "fn")
#' plot_bma(bma_harrell_c, type = "cd_n", ratio = 1)
plot_bma = function(
  bma,
  type = "box",
  measure_id = "harrell_c",
  tuning_measure_id = "harrell_c",
  exclude_learners = "",
  flip = FALSE,
  dodge = TRUE,
  ...
) {
  checkmate::assert_subset(type, choices = c("mean", "box", "fn", "cd_n", "cd_bd"))
  measure_label = msr_tbl[id == measure_id, label]
  tuning_measure_label = msr_tbl[id == tuning_measure_id, label]

  if (type %in% c("box", "mean")) {
    if (inherits(bma, "BenchmarkAggr")) {
      xdat = bma$data
    } else {
      xdat = bma
    }
    xdat = bma$data
    bma = mlr3benchmark::as_benchmark_aggr(xdat[!(learner_id %in% exclude_learners), ])
  }

  plot_type_label = switch(
    type,
    mean = "Mean ± SE",
    box = "Boxplot",
    fn = "Post-hoc Friedman-Nemenyi",
    cd_n = "Critical Differences (Nemenyi)",
    cd_bd = "Critical Differences (Bonferroni-Dunn)"
  )

  minimize = msr_tbl[id == measure_id, minimize]
  # browser()
  if (type %in% c("cd_n", "cd_bd")) {
    test = switch(type, cd_n = "nemenyi", cd_bd = "bd")

    #cli::cli_alert_info("Performing type {type} and measure {measure_id} and test {test}, minimize = {minimize}")

    p = mlr3viz::autoplot(bma, type = "cd", meas = measure_id, test = test, minimize = minimize, ...) +
      labs(
        caption = glue::glue(
          "Evaluation measure: {measure_label}
                             Tuning measure: {tuning_measure_label}"
        )
      )
  } else if (type %in% c("mean", "box", "fn")) {
    p = mlr3viz::autoplot(bma, type = type, meas = measure_id, ...)

    p = p +
      labs(
        title = measure_label,
        subtitle = if (type != "box") plot_type_label else NULL,
        x = NULL,
        y = measure_label,
        caption = glue::glue("Tuning measure: {tuning_measure_label}")
      )
  } else {
    mlr3misc::stopf("Unknown type %s", type)
  }

  if (type %in% c("box", "mean")) {
    if (dodge) {
      p = p + scale_x_discrete(guide = guide_axis(n.dodge = 2))
    }
    if (flip) {
      p = p + coord_flip() + scale_x_discrete(limits = rev)
    }
    p = p + theme_minimal(base_size = 15)
    p = p + theme(plot.background = element_rect(fill = "transparent", color = NA))
  }

  if (type == "fn") {
    p = p +
      theme(
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0)
      )
  }
  if (interactive()) {
    print(p)
  }
  p
}

#' Unfortunate partial duplication as above function is for BenchmarkAggr objects but turns out
#' I want one for "normal" data.frames as well. Should have seen that coming.
#'
plot_aggr_scores = function(
  xdf,
  type = "box",
  eval_measure_id = "harrell_c",
  tuning_measure_id = "harrell_c",
  dodge = FALSE,
  flip = FALSE
) {
  checkmate::assert_data_table(xdf)

  measure_label = msr_tbl[id == eval_measure_id, label]
  tuning_measure_label = msr_tbl[id == tuning_measure_id, label]

  plot_type_label = switch(type, mean = "Mean ± SE", box = "Boxplot")

  minimize = msr_tbl[id == eval_measure_id, minimize]

  if (minimize) {
    direction_label = "lower is better"
  } else {
    direction_label = "higher is better"
  }

  this_df = data.table::copy(xdf) |>
    dplyr::filter(grepl(pattern = .env$tuning_measure_id, x = .data$tune_measure)) |>
    dplyr::filter(!is.na(.data[[eval_measure_id]])) |>
    dplyr::filter(is.finite(.data[[eval_measure_id]]))

  if (nrow(this_df) == 0) {
    cli::cli_abort(
      "Combination of eval measure {.val {eval_measure_id}} and tuning measure {.val {tuning_measure_id}} has no scores in data"
    )
  }
  p = ggplot(
    this_df,
    aes(x = learner_id, y = .data[[eval_measure_id]], color = learner_group, fill = learner_group)
  ) +
    geom_boxplot(alpha = 1 / 4, key_glyph = "rect") +
    scale_color_manual(values = palette_groups, aesthetics = c("color", "fill"), name = NULL)

  p = p +
    labs(
      title = measure_label,
      subtitle = glue::glue("{plot_type_label} of aggregated scores across all tasks ({direction_label})"),
      x = NULL,
      y = measure_label,
      color = NULL,
      fill = NULL,
      caption = glue::glue("Tuning measure: {tuning_measure_label}")
    )

  if (dodge) {
    p = p + scale_x_discrete(guide = guide_axis(n.dodge = 2))
  }
  if (flip) {
    p = p + coord_flip() + scale_x_discrete(limits = rev)
  }
  p = p + theme_minimal(base_size = 15)
  p = p +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      plot.title.position = "plot"
    )

  print(p)
}

#' Analogous plotting function but for by-dataset plots, slightly different, could be consolidated though
plot_scores = function(
  scores,
  eval_measure_id = "harrel_c",
  tuning_measure_id = "harrel_c",
  dodge = FALSE,
  flip = TRUE,
  ncol = 8
) {
  checkmate::assert_data_table(scores)
  checkmate::assert_subset(eval_measure_id, choices = msr_tbl$id)
  checkmate::assert_subset(tuning_measure_id, choices = c("isbs", "harrell_c"))

  if (msr_tbl[id == eval_measure_id, minimize]) {
    direction_label = "lower is better"
  } else {
    direction_label = "higher is better"
  }

  p = scores |>
    dplyr::filter(grepl(pattern = .env$tuning_measure_id, x = .data$tune_measure)) |>
    ggplot(aes(y = learner_id, x = .data[[eval_measure_id]], color = learner_group, fill = learner_group)) +
    facet_wrap(vars(task_id), scales = "free_x", ncol = ncol) +
    geom_boxplot(alpha = 1 / 4) +
    scale_color_manual(values = palette_groups, aesthetics = c("color", "fill")) +
    labs(
      title = msr_tbl[id == eval_measure_id, label],
      subtitle = glue::glue("Scores per dataset across outer resampling iterations ({direction_label})"),
      x = "Score",
      y = NULL,
      color = NULL,
      fill = NULL,
      caption = glue::glue("Tuning measure: {msr_tbl[id == tuning_measure_id, label]}"),
    ) +
    theme_minimal(
      base_size = 11 #,
      # Would want to use custom fonts but reproducibility... :(
      # base_family = "Fira Sans"
    ) +
    theme(
      legend.position = "top",
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_blank(),
      plot.title.position = "plot",
      # axis.text.x = element_text(size = rel(.7)),
      axis.text.y = element_text(size = rel(.7)),
      strip.text = element_text(size = rel(1.1))
    )

  # if (interactive()) {
  #   print(p)
  # }

  p
}

save_plot = function(p, name, height = 6, width = 9, formats = c("png", "pdf"), dpi = 300) {
  if (interactive()) {
    print(p)
  }

  for (format in formats) {
    filename = fs::path(plot_path, name, ext = format)
    cli::cli_alert_info("Saving {.file {fs::path_rel(filename)}} / {.val {format}}")

    ggsave(
      filename = filename,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
  }
}
